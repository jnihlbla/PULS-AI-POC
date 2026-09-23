000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF027700.                                                
000400 AUTHOR.         ANDERS HENRIKSSON.                                       
000500 DATE-WRITTEN.   02/04/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.ERRBUNDLOCATE                                    
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE T01LSEL                                
001500*        THE PROGRAM READS   TABLE T01TRAW                                
001600*        THE PROGRAM READS   TABLE T01TBUN                                
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: WF0277T                                             
002000*        REQUEST:     WZ01REQU                                            
002100*                     WF0277I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    WZ01RESP                                            
002500*                     WF0277O1                                            
002510*                                                                         
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)  VALUE 'WF027700'.             
003100                                                                          
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003400 77  KDRC-DISPLAY                PIC Z(5).                                
003500                                                                          
003600*    --- CONSTANTS                                                        
003700 77  YES                         PIC X      VALUE 'J'.                    
003800 77  NOO                         PIC X      VALUE 'N'.                    
003900                                                                          
004000 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
004100 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004200 77  WS-ADRESS                   PIC X(50)                                
004300                    VALUE 'CARPARTS.BILLIT.ERRBUNDLOCATE'.                
004400                                                                          
004500 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004600     88  KEYS-OK                            VALUE 'J'.                    
004700     88  KEYS-WRONG                         VALUE 'N'.                    
004800                                                                          
004900 77  SYSTEM-SW                   PIC X      VALUE SPACE.                  
005000     88  SYSTEM-YES                         VALUE 'J'.                    
005100     88  SYSTEM-NOO                         VALUE 'N'.                    
005200                                                                          
005300 77  DATUM-SW                    PIC X      VALUE SPACE.                  
005400     88  DATUM-YES                          VALUE 'J'.                    
005500     88  DATUM-NOO                          VALUE 'N'.                    
005600                                                                          
005700 77  TID-SW                      PIC X      VALUE SPACE.                  
005800     88  TID-YES                            VALUE 'J'.                    
005900     88  TID-NOO                            VALUE 'N'.                    
006000                                                                          
006100*    --- WORK FIELDS                                                      
006200 01  WS-IX-MOD                   PIC S9(4)  VALUE ZERO    BINARY.         
006300 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
006400 01  WS-COUNTER-HELA             PIC S9(7)  VALUE ZERO    COMP-3.         
006500 01  WS-COUNTER-SYSTEM           PIC S9(7)  VALUE ZERO    COMP-3.         
006600 01  WS-COUNTER-DATUM            PIC S9(7)  VALUE ZERO    COMP-3.         
006700 01  WS-COUNTER-TID              PIC S9(7)  VALUE ZERO    COMP-3.         
006800 01  WS-COUNTER-SYSTEM-DATUM     PIC S9(7)  VALUE ZERO    COMP-3.         
006900 01  WS-COUNTER-SYSTEM-TID       PIC S9(7)  VALUE ZERO    COMP-3.         
007000 01  WS-COUNTER-DATUM-TID        PIC S9(7)  VALUE ZERO    COMP-3.         
007100 01  WS-COUNTER-SYSTEM-DATUM-TID PIC S9(7)  VALUE ZERO    COMP-3.         
007200 01  WS-COUNTER-ANTIBUNT         PIC S9(7)  VALUE ZERO    COMP-3.         
007300 01  WS-COUNTER-FELIBUNT         PIC S9(7)  VALUE ZERO    COMP-3.         
007400 01  WS-BELEGRAD-1               PIC X(35)  VALUE SPACE.                  
007500 01  WS-IDLEGSEL                 PIC X(4)   VALUE SPACE.                  
007600 01  WS-CURRENT-DATE             PIC X(8)   VALUE SPACE.                  
007700 01  WS-DATUM                    PIC X(8)   VALUE SPACE.                  
007800 01  WS-CURRENT-TIME             PIC X(6)   VALUE SPACE.                  
007900 01  WS-TID                      PIC S9(6)  VALUE ZERO COMP-3.            
008000 01  WS-TID2                     PIC S9(10) VALUE ZERO COMP-3.            
008100 01  WS-TID3                     PIC 9(10)  VALUE ZERO.                   
008200                                                                          
008300*    --- MAPPING FIELDS                                                   
008400 01  MAP-IDBUNDLE-LINE           PIC X(15)  VALUE SPACE.                  
008500 01  MAP-DAREGDAT-LINE           PIC X(8)   VALUE SPACE.                  
008600 01  MAP-TIREGTID-LINE           PIC S9(10) VALUE ZERO COMP-3.            
008700 01  MAP-IDREF-LINE              PIC X(15)  VALUE SPACE.                  
008800 01  MAP-DAREFDAT-LINE           PIC X(8)   VALUE SPACE.                  
008900 01  MAP-IDREFRAD-LINE           PIC S9(5)  VALUE ZERO COMP-3.            
009000 01  MAP-IDSYSTEM-SEND-LINE      PIC X(4)   VALUE SPACE.                  
009100                                                                          
009200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009300 01  GENERAL-SUBPROGRAMS.                                                 
009400     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
009500     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
009600                                                                          
009700*    --- PARAMETERS TO ABEND                                              
009800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
010200                                                                          
010300 01  MESSAGE-CODES.                                                       
010400     03  ERROR-CODES.                                                     
010500         05  ERR-MORE-LINES-EXIST    PIC X(3)   VALUE '011'.              
010600         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
010700         05  ERR-INVALID-FIELDS      PIC X(3)   VALUE '023'.              
010800         05  ERR-MUST-BE-NUMERIC     PIC X(3)   VALUE '024'.              
010900         05  NOT-FOUND               PIC X(3)   VALUE '025'.              
011000         05  ERR-MUST-BE-ENTERED     PIC X(3)   VALUE '026'.              
011100         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
011200         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
011300         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
011400     EJECT                                                                
011500                                                                          
011600*01  -COPY WZ01SUB                                                        
011700     EJECT                                                                
011800                                                                          
011900*                                                                         
012000 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
012100 01  REQU-AREA.                                                           
012200*    03 -COPY WZ01REQU                                                    
012300*    03 -COPY WF0277I1                                                    
012400     EJECT                                                                
012500                                                                          
012600 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
012700 01  RESP-AREA.                                                           
012800*    03 -COPY WZ01RESP                                                    
012900*    03 -COPY WF0277O1                                                    
013000     EJECT                                                                
013100                                                                          
013200 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
013300     SKIP3                                                                
013400*    -COPY WZ20DATE                                                       
013500     EJECT                                                                
013600                                                                          
013700 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
013800     SKIP3                                                                
013900*    -COPY WZ20DAYS                                                       
014000     EJECT                                                                
014100                                                                          
014200 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
014300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
014400                                                                          
014500 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
014600 01  DB2-WS.                                                              
014700     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
014800         88  CURSOR-OK                      VALUE 000.                    
014900         88  LINES-FOUND                    VALUE 000.                    
015000         88  LINES-MISSING                  VALUE 100.                    
015100         88  RESOURCE-WRONG                 VALUE 904.                    
015200                                                                          
015300     03  GOOD-SQLCODECODES.                                               
015400         05  GOOD-SQLCODE OCCURS 5                                        
015500             INDEXED BY SQLCODE-IX PIC 9(3).                              
015600                                                                          
015700 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
015800*01  -COPY T01LSEL -PRE T01LSEL-                                          
015900     EJECT                                                                
016000                                                                          
016100 01  FILLER                      PIC X(16)   VALUE 'T01TRAW-AREA'.        
016200*01  -COPY T01TRAW -PRE T01TRAW-                                          
016300     EJECT                                                                
016400                                                                          
016500 01  FILLER                      PIC X(16)   VALUE 'T01TBUN-AREA'.        
016600*01  -COPY T01TBUN -PRE T01TBUN-                                          
016700     EJECT                                                                
016800                                                                          
016900     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
017000     EJECT                                                                
017100     EXEC SQL INCLUDE T01TRAW END-EXEC.                                   
017200     EJECT                                                                
017300     EXEC SQL INCLUDE T01TBUN END-EXEC.                                   
017400     EJECT                                                                
017500                                                                          
017600 LINKAGE SECTION.                                                         
017700 PROCEDURE DIVISION.                                                      
017800 MAIN SECTION.                                                            
017900                                                                          
018000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
018100     IF SUB-KDRC = ZERO                                                   
018200       PERFORM A-INIT                                                     
018300       PERFORM B-CHECK-KEYS                                               
018400       IF KEYS-OK                                                         
018500         PERFORM BB-CHECK-KEY-RELATION                                    
018600       END-IF                                                             
018700       IF KEYS-OK                                                         
018800         PERFORM F-READ-SHOW-INFO                                         
018900       END-IF                                                             
019000       PERFORM S02-RETURN-RESPONSE                                        
019100     END-IF                                                               
019200     MOVE ZERO TO RETURN-CODE                                             
019300     GOBACK                                                               
019400     .                                                                    
019500     EJECT                                                                
019600                                                                          
019700 A-INIT SECTION.                                                          
019800     INITIALIZE GOOD-SQLCODECODES                                         
019900     MOVE ALL '+' TO RESP-AREA                                            
020000     MOVE SPACE TO RESP-IDMSG-ERROR                                       
020100     MOVE SPACE TO RESP-IDMSG-INFO                                        
020200     MOVE SPACE TO RESP-IDELMT-ERROR                                      
020300     MOVE ZERO                  TO WS-COUNTER-HELA                        
020400                                   WS-COUNTER-SYSTEM                      
020500                                   WS-COUNTER-DATUM                       
020600                                   WS-COUNTER-TID                         
020700                                   WS-COUNTER-SYSTEM-DATUM                
020800                                   WS-COUNTER-SYSTEM-TID                  
020900                                   WS-COUNTER-DATUM-TID                   
021000                                   WS-COUNTER-SYSTEM-DATUM-TID            
021100                                   WS-COUNTER-ANTIBUNT                    
021200                                   WS-COUNTER-FELIBUNT                    
021300                                   RESP-KVRADER                           
021400     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
021500     MOVE FUNCTION CURRENT-DATE (9:6) TO WS-CURRENT-TIME                  
021600     .                                                                    
021700     EJECT                                                                
021800                                                                          
021900*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
022000 B-CHECK-KEYS SECTION.                                                    
022100     MOVE YES TO KEYS-SW                                                  
022200     IF REQU-KDPGMACT = WS-SEARCH                                         
022300     AND REQU-IDMSGVER NUMERIC                                            
022400     AND (REQU-FLASC = 'Y' OR REQU-FLASC = 'N')                           
022500     AND REQU-IDLEGSEL-KEY > SPACE                                        
022600       CONTINUE                                                           
022700     ELSE                                                                 
022800       MOVE NOO TO KEYS-SW                                                
022900     END-IF                                                               
023000                                                                          
023100     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
023200       MOVE NOO TO KEYS-SW                                                
023300     END-IF                                                               
023400                                                                          
023500     IF KEYS-WRONG                                                        
023600       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
023700       IF REQU-KDPGMACT = 'S'                                             
023800         CONTINUE                                                         
023900       ELSE                                                               
024000         MOVE SYSTEM-ERROR       TO RESP-IDMSG-ERROR                      
024100         MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                     
024200       END-IF                                                             
024300       IF REQU-FLASC = 'Y' OR = 'N'                                       
024400         CONTINUE                                                         
024500       ELSE                                                               
024600         MOVE SYSTEM-ERROR       TO RESP-IDMSG-ERROR                      
024700         MOVE 'FLASC'            TO RESP-IDELMT-ERROR                     
024800       END-IF                                                             
024900       IF REQU-IDMSGVER NUMERIC                                           
025000         CONTINUE                                                         
025100       ELSE                                                               
025200         MOVE SYSTEM-ERROR        TO RESP-IDMSG-ERROR                     
025300         MOVE 'IDMSGVER'          TO RESP-IDELMT-ERROR                    
025400       END-IF                                                             
025500       IF REQU-IDUSER = SPACE OR = ALL '+'                                
025600         MOVE SYSTEM-ERROR        TO RESP-IDMSG-ERROR                     
025700         MOVE 'IDUSER'            TO RESP-IDELMT-ERROR                    
025800       ELSE                                                               
025900         CONTINUE                                                         
026000       END-IF                                                             
026100     END-IF                                                               
026200                                                                          
026300     IF KEYS-OK                                                           
026400       PERFORM DB2-SELECT-T01LSEL-TAB                                     
026500       IF LINES-FOUND                                                     
026600         CONTINUE                                                         
026700       ELSE                                                               
026800         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
026900         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
027000         MOVE NOO        TO KEYS-SW                                       
027100       END-IF                                                             
027200     END-IF                                                               
027300     .                                                                    
027400     EJECT                                                                
027500                                                                          
027600*** - CHECK RELATION BETWEEN REQUSTED KEYS                                
027700 BB-CHECK-KEY-RELATION SECTION.                                           
027800     MOVE NOO TO SYSTEM-SW                                                
027900                 DATUM-SW                                                 
028000                 TID-SW                                                   
028100     IF REQU-IDSYSTEM-SEND = SPACE OR = ALL '+'                           
028200       CONTINUE                                                           
028300     ELSE                                                                 
028400       MOVE YES TO SYSTEM-SW                                              
028500     END-IF                                                               
028600                                                                          
028700     IF REQU-DADATUM = SPACE OR = ALL '+'                                 
028800       CONTINUE                                                           
028900     ELSE                                                                 
029000       IF REQU-DADATUM NUMERIC                                            
029100         IF REQU-DADATUM < WS-CURRENT-DATE OR = WS-CURRENT-DATE           
029200         AND REQU-DADATUM NUMERIC                                         
029300           MOVE REQU-DADATUM  TO DATE-TIDATE                              
029400           MOVE 'YYYYMMDD'  TO DATE-KDDATFMT                              
029500           CALL WZ20DATE USING DATE-WZ20DATE                              
029600           IF DATE-KDRC > ZERO                                            
029700             MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                  
029800             MOVE 'DADATUM'        TO RESP-IDELMT-ERROR                   
029900             MOVE NOO TO KEYS-SW                                          
030000           ELSE                                                           
030100             MOVE YES TO DATUM-SW                                         
030200             MOVE REQU-DADATUM TO WS-DATUM                                
030300           END-IF                                                         
030400         ELSE                                                             
030500           MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                    
030600           MOVE 'DADATUM'        TO RESP-IDELMT-ERROR                     
030700           MOVE NOO TO KEYS-SW                                            
030800         END-IF                                                           
030900       ELSE                                                               
031000         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
031100         MOVE 'DADATUM'           TO RESP-IDELMT-ERROR                    
031200         MOVE NOO TO KEYS-SW                                              
031300       END-IF                                                             
031400     END-IF                                                               
031500                                                                          
031600     IF REQU-TIHHMMSS = ZERO OR = ALL '+'                                 
031700       CONTINUE                                                           
031800     ELSE                                                                 
031900       IF REQU-TIHHMMSS NOT NUMERIC                                       
032000         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
032100         MOVE 'TIHHMMSS' TO RESP-IDELMT-ERROR                             
032200         MOVE NOO TO KEYS-SW                                              
032300       ELSE                                                               
032400         MOVE YES TO TID-SW                                               
032500         IF REQU-DADATUM = SPACE OR = ALL '+'                             
032600           IF REQU-TIHHMMSS < WS-CURRENT-TIME                             
032700             MOVE WS-CURRENT-DATE (1:8) TO DAYS-TIDATE2                   
032800             MOVE 'YYYYMMDD' TO DAYS-KDDATFMT2                            
032900             MOVE 1 TO DAYS-KVDAYS                                        
033000             MOVE 'WEEKDAYS' TO DAYS-IDCALEND                             
033100             MOVE SPACE TO DAYS-TIDATE1                                   
033200             MOVE 'YYYYMMDD' TO DAYS-KDDATFMT1                            
033300             CALL WZ20DAYS USING                                          
033400                  DAYS-WZ20DAYS                                           
033500             IF DAYS-KDRC = ZERO                                          
033600               MOVE DAYS-TIDATE1  TO WS-DATUM                             
033700               MOVE REQU-TIHHMMSS TO WS-TID                               
033800               COMPUTE WS-TID3 = WS-TID * 100                             
033900               MOVE WS-TID3       TO WS-TID2                              
034000             ELSE                                                         
034100               MOVE ERR-INVALID-FIELDS TO RESP-IDMSG-ERROR                
034200               MOVE 'TIHHMMSS' TO RESP-IDELMT-ERROR                       
034300               MOVE NOO TO KEYS-SW                                        
034400             END-IF                                                       
034500           ELSE                                                           
034600             MOVE WS-CURRENT-DATE TO WS-DATUM                             
034700             MOVE REQU-TIHHMMSS   TO WS-TID                               
034800             COMPUTE WS-TID3 = WS-TID * 100                               
034900             MOVE WS-TID3         TO WS-TID2                              
035000           END-IF                                                         
035100         ELSE                                                             
035200           MOVE REQU-TIHHMMSS TO WS-TID                                   
035300           COMPUTE WS-TID3 = WS-TID * 100                                 
035400           MOVE WS-TID3       TO WS-TID2                                  
035500         END-IF                                                           
035600       END-IF                                                             
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
036100*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
036200 F-READ-SHOW-INFO SECTION.                                                
036300     MOVE REQU-IDLEGSEL-KEY      TO RESP-IDLEGSEL-KEY                     
036400     MOVE REQU-IDMSGVER          TO RESP-IDMSGVER                         
036500     MOVE REQU-FLASC             TO RESP-FLASC                            
036600     MOVE REQU-IDSYSTEM-SEND     TO RESP-IDSYSTEM-SEND                    
036700     MOVE REQU-DADATUM           TO RESP-DADATUM                          
036800     MOVE REQU-TIHHMMSS          TO RESP-TIHHMMSS                         
036900     MOVE WS-BELEGRAD-1          TO RESP-BELEGRAD-1                       
037000                                                                          
037100     PERFORM FA-READ-BASICDATA                                            
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037500*** - CHECK WHICH REQUESTED KEY                                           
037600 FA-READ-BASICDATA SECTION.                                               
037700     IF SYSTEM-NOO AND DATUM-NOO AND TID-NOO                              
037800       PERFORM FAA-HELSOKNING                                             
037900     END-IF                                                               
038000                                                                          
038100     IF SYSTEM-YES AND DATUM-NOO AND TID-NOO                              
038200       PERFORM FAB-SYSTEMSOKNING                                          
038300     END-IF                                                               
038400                                                                          
038500     IF SYSTEM-YES AND DATUM-YES AND TID-NOO                              
038600       PERFORM FAC-SYSTEM-DATUMSOKNING                                    
038700     END-IF                                                               
038800                                                                          
038900     IF SYSTEM-YES AND DATUM-NOO AND TID-YES                              
039000       PERFORM FAD-SYSTEM-TIDSOKNING                                      
039100     END-IF                                                               
039200                                                                          
039300     IF SYSTEM-YES AND DATUM-YES AND TID-YES                              
039400       PERFORM FAE-SYSTEM-DATUM-TIDSOKNING                                
039500     END-IF                                                               
039600                                                                          
039700     IF SYSTEM-NOO AND DATUM-YES AND TID-NOO                              
039800       PERFORM FAF-DATUMSOKNING                                           
039900     END-IF                                                               
040000                                                                          
040100     IF SYSTEM-NOO AND DATUM-YES AND TID-YES                              
040200       PERFORM FAG-DATUM-TIDSOKNING                                       
040300     END-IF                                                               
040400                                                                          
040500     IF SYSTEM-NOO AND DATUM-NOO AND TID-YES                              
040600       PERFORM FAH-TIDSOKNING                                             
040700     END-IF                                                               
040800                                                                          
040900     MOVE WS-IX              TO RESP-KVRADER                              
041000     .                                                                    
041100     EJECT                                                                
041200                                                                          
041300*** - HANDLE SOKNING ALL ERRORS                                           
041400 FAA-HELSOKNING SECTION.                                                  
041500     PERFORM DB2-COUNT-CRS-HELA                                           
041600     IF WS-COUNTER-HELA = ZERO                                            
041700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
041800     ELSE                                                                 
041900       MOVE WS-COUNTER-HELA TO RESP-KVRADER                               
042000       IF WS-COUNTER-HELA > WS-MAX-LINES                                  
042100         MOVE ERR-MORE-LINES-EXIST TO RESP-IDMSG-ERROR                    
042200       END-IF                                                             
042300     END-IF                                                               
042400                                                                          
042500     IF RESP-IDMSG-ERROR = SPACE                                          
042600       IF REQU-FLASC = 'N'                                                
042700         PERFORM DB2-DCL-OPN-T01TRAW-CRS-1                                
042800         PERFORM DB2-FETCH-T01TRAW-CRS-1                                  
042900         MOVE ZERO TO WS-IX                                               
043000         PERFORM UNTIL LINES-MISSING                                      
043100           PERFORM DB2-COUNT-BUNDLEROWS                                   
043200           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
043300           PERFORM S03-MOVE-TO-RESPOND                                    
043400           PERFORM DB2-FETCH-T01TRAW-CRS-1                                
043500         END-PERFORM                                                      
043600         PERFORM DB2-CLOSE-T01TRAW-CRS-1                                  
043700       ELSE                                                               
043800         PERFORM DB2-DCL-OPN-T01TRAW-CRS-1-ASC                            
043900         PERFORM DB2-FETCH-T01TRAW-CRS-1-ASC                              
044000         MOVE ZERO TO WS-IX                                               
044100         PERFORM UNTIL LINES-MISSING                                      
044200           PERFORM DB2-COUNT-BUNDLEROWS                                   
044300           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
044400           PERFORM S03-MOVE-TO-RESPOND                                    
044500           PERFORM DB2-FETCH-T01TRAW-CRS-1-ASC                            
044600         END-PERFORM                                                      
044700         PERFORM DB2-CLOSE-T01TRAW-CRS-1-ASC                              
044800       END-IF                                                             
044900     END-IF                                                               
045000     .                                                                    
045100     EJECT                                                                
045200                                                                          
045300*** - HANDLE SOKNING ERRORS IN SYSTEM                                     
045400 FAB-SYSTEMSOKNING SECTION.                                               
045500     PERFORM DB2-COUNT-CRS-SYSTEM                                         
045600     IF WS-COUNTER-SYSTEM = ZERO                                          
045700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
045800     ELSE                                                                 
045900       MOVE WS-COUNTER-SYSTEM TO RESP-KVRADER                             
046000       IF WS-COUNTER-SYSTEM > WS-MAX-LINES                                
046100         MOVE ERR-MORE-LINES-EXIST TO RESP-IDMSG-ERROR                    
046200       END-IF                                                             
046300     END-IF                                                               
046400                                                                          
046500     IF RESP-IDMSG-ERROR = SPACE                                          
046600       IF REQU-FLASC = 'N'                                                
046700         PERFORM DB2-DCL-OPN-T01TRAW-CRS-2                                
046800         PERFORM DB2-FETCH-T01TRAW-CRS-2                                  
046900         MOVE ZERO TO WS-IX                                               
047000         PERFORM UNTIL LINES-MISSING                                      
047100           PERFORM DB2-COUNT-BUNDLEROWS                                   
047200           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
047300           PERFORM S03-MOVE-TO-RESPOND                                    
047400           PERFORM DB2-FETCH-T01TRAW-CRS-2                                
047500         END-PERFORM                                                      
047600         PERFORM DB2-CLOSE-T01TRAW-CRS-2                                  
047700       ELSE                                                               
047800         PERFORM DB2-DCL-OPN-T01TRAW-CRS-2-ASC                            
047900         PERFORM DB2-FETCH-T01TRAW-CRS-2-ASC                              
048000         MOVE ZERO TO WS-IX                                               
048100         PERFORM UNTIL LINES-MISSING                                      
048200           PERFORM DB2-COUNT-BUNDLEROWS                                   
048300           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
048400           PERFORM S03-MOVE-TO-RESPOND                                    
048500           PERFORM DB2-FETCH-T01TRAW-CRS-2-ASC                            
048600         END-PERFORM                                                      
048700         PERFORM DB2-CLOSE-T01TRAW-CRS-2-ASC                              
048800       END-IF                                                             
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200                                                                          
049300*** - HANDLE SYSTEM DATUM SOKNING AFTER ERRORS                            
049400 FAC-SYSTEM-DATUMSOKNING SECTION.                                         
049500     PERFORM DB2-COUNT-CRS-SYSTEM-DATUM                                   
049600     IF WS-COUNTER-SYSTEM-DATUM = ZERO                                    
049700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
049800     ELSE                                                                 
049900       MOVE WS-COUNTER-SYSTEM-DATUM TO RESP-KVRADER                       
050000       IF WS-COUNTER-SYSTEM-DATUM > WS-MAX-LINES                          
050100         MOVE ERR-MORE-LINES-EXIST TO RESP-IDMSG-ERROR                    
050200       END-IF                                                             
050300     END-IF                                                               
050400                                                                          
050500     IF RESP-IDMSG-ERROR = SPACE                                          
050600       IF REQU-FLASC = 'N'                                                
050700         PERFORM DB2-DCL-OPN-T01TRAW-CRS-3                                
050800         PERFORM DB2-FETCH-T01TRAW-CRS-3                                  
050900         MOVE ZERO TO WS-IX                                               
051000         PERFORM UNTIL LINES-MISSING                                      
051100           PERFORM DB2-COUNT-BUNDLEROWS                                   
051200           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
051300           PERFORM S03-MOVE-TO-RESPOND                                    
051400           PERFORM DB2-FETCH-T01TRAW-CRS-3                                
051500         END-PERFORM                                                      
051600         PERFORM DB2-CLOSE-T01TRAW-CRS-3                                  
051700       ELSE                                                               
051800         PERFORM DB2-DCL-OPN-T01TRAW-CRS-3-ASC                            
051900         PERFORM DB2-FETCH-T01TRAW-CRS-3-ASC                              
052000         MOVE ZERO TO WS-IX                                               
052100         PERFORM UNTIL LINES-MISSING                                      
052200           PERFORM DB2-COUNT-BUNDLEROWS                                   
052300           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
052400           PERFORM S03-MOVE-TO-RESPOND                                    
052500           PERFORM DB2-FETCH-T01TRAW-CRS-3-ASC                            
052600         END-PERFORM                                                      
052700         PERFORM DB2-CLOSE-T01TRAW-CRS-3-ASC                              
052800       END-IF                                                             
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200                                                                          
053300*** - HANDLE KEY SYSTEM TID SOKNING AFTER ERRORS                          
053400 FAD-SYSTEM-TIDSOKNING SECTION.                                           
053500     PERFORM DB2-COUNT-CRS-SYSTEM-TID                                     
053600     IF WS-COUNTER-SYSTEM-TID = ZERO                                      
053700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
053800     ELSE                                                                 
053900       MOVE WS-COUNTER-SYSTEM-TID TO RESP-KVRADER                         
054000       IF WS-COUNTER-SYSTEM-TID > WS-MAX-LINES                            
054100         MOVE ERR-MORE-LINES-EXIST TO RESP-IDMSG-ERROR                    
054200       END-IF                                                             
054300     END-IF                                                               
054400                                                                          
054500     IF RESP-IDMSG-ERROR = SPACE                                          
054600       IF REQU-FLASC = 'N'                                                
054700         PERFORM DB2-DCL-OPN-T01TRAW-CRS-4                                
054800         PERFORM DB2-FETCH-T01TRAW-CRS-4                                  
054900         MOVE ZERO TO WS-IX                                               
055000         PERFORM UNTIL LINES-MISSING                                      
055100           PERFORM DB2-COUNT-BUNDLEROWS                                   
055200           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
055300           PERFORM S03-MOVE-TO-RESPOND                                    
055400           PERFORM DB2-FETCH-T01TRAW-CRS-4                                
055500         END-PERFORM                                                      
055600         PERFORM DB2-CLOSE-T01TRAW-CRS-4                                  
055700       ELSE                                                               
055800         PERFORM DB2-DCL-OPN-T01TRAW-CRS-4-ASC                            
055900         PERFORM DB2-FETCH-T01TRAW-CRS-4-ASC                              
056000         MOVE ZERO TO WS-IX                                               
056100         PERFORM UNTIL LINES-MISSING                                      
056200           PERFORM DB2-COUNT-BUNDLEROWS                                   
056300           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
056400           PERFORM S03-MOVE-TO-RESPOND                                    
056500           PERFORM DB2-FETCH-T01TRAW-CRS-4-ASC                            
056600         END-PERFORM                                                      
056700         PERFORM DB2-CLOSE-T01TRAW-CRS-4-ASC                              
056800       END-IF                                                             
056900     END-IF                                                               
057000     .                                                                    
057100     EJECT                                                                
057200                                                                          
057300*** - HANDLE SYSTEM DATUM TID SOKNING AFTER ERRORS                        
057400 FAE-SYSTEM-DATUM-TIDSOKNING SECTION.                                     
057500     PERFORM DB2-COUNT-CRS-SYSTEM-DATUM-TID                               
057600     IF WS-COUNTER-SYSTEM-DATUM-TID = ZERO                                
057700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
057800     ELSE                                                                 
057900       MOVE WS-COUNTER-SYSTEM-DATUM-TID TO RESP-KVRADER                   
058000       IF WS-COUNTER-SYSTEM-DATUM-TID > WS-MAX-LINES                      
058100         MOVE ERR-MORE-LINES-EXIST TO RESP-IDMSG-ERROR                    
058200       END-IF                                                             
058300     END-IF                                                               
058400                                                                          
058500     IF RESP-IDMSG-ERROR = SPACE                                          
058600       IF REQU-FLASC = 'N'                                                
058700         PERFORM DB2-DCL-OPN-T01TRAW-CRS-5                                
058800         PERFORM DB2-FETCH-T01TRAW-CRS-5                                  
058900         MOVE ZERO TO WS-IX                                               
059000         PERFORM UNTIL LINES-MISSING                                      
059100           PERFORM DB2-COUNT-BUNDLEROWS                                   
059200           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
059300           PERFORM S03-MOVE-TO-RESPOND                                    
059400           PERFORM DB2-FETCH-T01TRAW-CRS-5                                
059500         END-PERFORM                                                      
059600         PERFORM DB2-CLOSE-T01TRAW-CRS-5                                  
059700       ELSE                                                               
059800         PERFORM DB2-DCL-OPN-T01TRAW-CRS-5-ASC                            
059900         PERFORM DB2-FETCH-T01TRAW-CRS-5-ASC                              
060000         MOVE ZERO TO WS-IX                                               
060100         PERFORM UNTIL LINES-MISSING                                      
060200           PERFORM DB2-COUNT-BUNDLEROWS                                   
060300           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
060400           PERFORM S03-MOVE-TO-RESPOND                                    
060500           PERFORM DB2-FETCH-T01TRAW-CRS-5-ASC                            
060600         END-PERFORM                                                      
060700         PERFORM DB2-CLOSE-T01TRAW-CRS-5-ASC                              
060800       END-IF                                                             
060900     END-IF                                                               
061000     .                                                                    
061100     EJECT                                                                
061200                                                                          
061300*** - HANDLE DATUM SOKNING AFTER ERRORS                                   
061400 FAF-DATUMSOKNING SECTION.                                                
061500     PERFORM DB2-COUNT-CRS-DATUM                                          
061600     IF WS-COUNTER-DATUM = ZERO                                           
061700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
061800     ELSE                                                                 
061900       MOVE WS-COUNTER-DATUM TO RESP-KVRADER                              
062000       IF WS-COUNTER-DATUM > WS-MAX-LINES                                 
062100         MOVE ERR-MORE-LINES-EXIST TO RESP-IDMSG-ERROR                    
062200       END-IF                                                             
062300     END-IF                                                               
062400                                                                          
062500     IF RESP-IDMSG-ERROR = SPACE                                          
062600       IF REQU-FLASC = 'N'                                                
062700         PERFORM DB2-DCL-OPN-T01TRAW-CRS-6                                
062800         PERFORM DB2-FETCH-T01TRAW-CRS-6                                  
062900         MOVE ZERO TO WS-IX                                               
063000         PERFORM UNTIL LINES-MISSING                                      
063100           PERFORM DB2-COUNT-BUNDLEROWS                                   
063200           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
063300           PERFORM S03-MOVE-TO-RESPOND                                    
063400           PERFORM DB2-FETCH-T01TRAW-CRS-6                                
063500         END-PERFORM                                                      
063600         PERFORM DB2-CLOSE-T01TRAW-CRS-6                                  
063700       ELSE                                                               
063800         PERFORM DB2-DCL-OPN-T01TRAW-CRS-6-ASC                            
063900         PERFORM DB2-FETCH-T01TRAW-CRS-6-ASC                              
064000         MOVE ZERO TO WS-IX                                               
064100         PERFORM UNTIL LINES-MISSING                                      
064200           PERFORM DB2-COUNT-BUNDLEROWS                                   
064300           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
064400           PERFORM S03-MOVE-TO-RESPOND                                    
064500           PERFORM DB2-FETCH-T01TRAW-CRS-6-ASC                            
064600         END-PERFORM                                                      
064700         PERFORM DB2-CLOSE-T01TRAW-CRS-6-ASC                              
064800       END-IF                                                             
064900     END-IF                                                               
065000     .                                                                    
065100     EJECT                                                                
065200                                                                          
065300*** - HANDLE DATUM TID SOKNING AFTER ERRORS                               
065400 FAG-DATUM-TIDSOKNING SECTION.                                            
065500     PERFORM DB2-COUNT-CRS-DATUM-TID                                      
065600     IF WS-COUNTER-DATUM-TID = ZERO                                       
065700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
065800     ELSE                                                                 
065900       MOVE WS-COUNTER-DATUM-TID TO RESP-KVRADER                          
066000       IF WS-COUNTER-DATUM-TID > WS-MAX-LINES                             
066100         MOVE ERR-MORE-LINES-EXIST TO RESP-IDMSG-ERROR                    
066200       END-IF                                                             
066300     END-IF                                                               
066400                                                                          
066500     IF RESP-IDMSG-ERROR = SPACE                                          
066600       IF REQU-FLASC = 'N'                                                
066700         PERFORM DB2-DCL-OPN-T01TRAW-CRS-7                                
066800         PERFORM DB2-FETCH-T01TRAW-CRS-7                                  
066900         MOVE ZERO TO WS-IX                                               
067000         PERFORM UNTIL LINES-MISSING                                      
067100           PERFORM DB2-COUNT-BUNDLEROWS                                   
067200           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
067300           PERFORM S03-MOVE-TO-RESPOND                                    
067400           PERFORM DB2-FETCH-T01TRAW-CRS-7                                
067500         END-PERFORM                                                      
067600         PERFORM DB2-CLOSE-T01TRAW-CRS-7                                  
067700       ELSE                                                               
067800         PERFORM DB2-DCL-OPN-T01TRAW-CRS-7-ASC                            
067900         PERFORM DB2-FETCH-T01TRAW-CRS-7-ASC                              
068000         MOVE ZERO TO WS-IX                                               
068100         PERFORM UNTIL LINES-MISSING                                      
068200           PERFORM DB2-COUNT-BUNDLEROWS                                   
068300           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
068400           PERFORM S03-MOVE-TO-RESPOND                                    
068500           PERFORM DB2-FETCH-T01TRAW-CRS-7-ASC                            
068600         END-PERFORM                                                      
068700         PERFORM DB2-CLOSE-T01TRAW-CRS-7-ASC                              
068800       END-IF                                                             
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200                                                                          
069300*** - HANDLE TID SOKNING AFTER ERRORS                                     
069400 FAH-TIDSOKNING SECTION.                                                  
069500     PERFORM DB2-COUNT-CRS-TID                                            
069600     IF WS-COUNTER-TID = ZERO                                             
069700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
069800     ELSE                                                                 
069900       MOVE WS-COUNTER-TID TO RESP-KVRADER                                
070000       IF WS-COUNTER-TID > WS-MAX-LINES                                   
070100         MOVE ERR-MORE-LINES-EXIST TO RESP-IDMSG-ERROR                    
070200       END-IF                                                             
070300     END-IF                                                               
070400                                                                          
070500     IF RESP-IDMSG-ERROR = SPACE                                          
070600       IF REQU-FLASC = 'N'                                                
070700         PERFORM DB2-DCL-OPN-T01TRAW-CRS-8                                
070800         PERFORM DB2-FETCH-T01TRAW-CRS-8                                  
070900         MOVE ZERO TO WS-IX                                               
071000         PERFORM UNTIL LINES-MISSING                                      
071100           PERFORM DB2-COUNT-BUNDLEROWS                                   
071200           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
071300           PERFORM S03-MOVE-TO-RESPOND                                    
071400           PERFORM DB2-FETCH-T01TRAW-CRS-8                                
071500         END-PERFORM                                                      
071600         PERFORM DB2-CLOSE-T01TRAW-CRS-8                                  
071700       ELSE                                                               
071800         PERFORM DB2-DCL-OPN-T01TRAW-CRS-8-ASC                            
071900         PERFORM DB2-FETCH-T01TRAW-CRS-8-ASC                              
072000         MOVE ZERO TO WS-IX                                               
072100         PERFORM UNTIL LINES-MISSING                                      
072200           PERFORM DB2-COUNT-BUNDLEROWS                                   
072300           PERFORM DB2-COUNT-BUNDLEROWS-ERRORS                            
072400           PERFORM S03-MOVE-TO-RESPOND                                    
072500           PERFORM DB2-FETCH-T01TRAW-CRS-8-ASC                            
072600         END-PERFORM                                                      
072700         PERFORM DB2-CLOSE-T01TRAW-CRS-8-ASC                              
072800       END-IF                                                             
072900     END-IF                                                               
073000     .                                                                    
073100     EJECT                                                                
073200                                                                          
073300*   --- DISPATCHER SECTION START                                          
073400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
073500     MOVE 'GETARG'             TO SUB-KDFUNC                              
073600     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
073700     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
073800                                                                          
073900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
074000                                                                          
074100     IF SUB-KDRC > 0                                                      
074200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
074300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
074400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
074500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
074600     END-IF                                                               
074700     .                                                                    
074800     EJECT                                                                
074900                                                                          
075000 S02-RETURN-RESPONSE SECTION.                                             
075100     MOVE 'RETURN'             TO SUB-KDFUNC                              
075200                                                                          
075300     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
075400                              - ((WS-MAX-LINES - WS-IX)                   
075500                              * LENGTH OF RESP-TABELLRAD)                 
075600                                                                          
075700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
075800                                                                          
075900     IF SUB-KDRC > 0                                                      
076000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
076100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
076200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
076300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
076400     END-IF                                                               
076500     .                                                                    
076600     EJECT                                                                
076700                                                                          
076800*   --- MOVE TO OUTPUT SECTION START                                      
076900*** - MOVE DATA TO RESPOND WHEN CURRENT LINE,                             
077000***   WHEN COMING LINE MODIFY CURRENT LINE.                               
077100 S03-MOVE-TO-RESPOND SECTION.                                             
077200     ADD 1 TO WS-IX                                                       
077300     MOVE MAP-IDBUNDLE-LINE      TO RESP-IDBUNDLE-LINE(WS-IX)             
077400     MOVE MAP-DAREGDAT-LINE      TO RESP-DAREGDAT-LINE(WS-IX)             
077500     MOVE MAP-TIREGTID-LINE      TO RESP-TIREGTID-LINE(WS-IX)             
077600     MOVE MAP-IDREF-LINE         TO RESP-IDREF-LINE(WS-IX)                
077700     MOVE MAP-DAREFDAT-LINE      TO RESP-DAREFDAT-LINE(WS-IX)             
077800     MOVE MAP-IDREFRAD-LINE      TO RESP-IDREFRAD-LINE(WS-IX)             
077900     MOVE MAP-IDSYSTEM-SEND-LINE TO RESP-IDSYSTEM-SEND-LINE(WS-IX)        
078000     MOVE WS-COUNTER-ANTIBUNT    TO RESP-ANTIBUNT-LINE(WS-IX)             
078100     MOVE WS-COUNTER-FELIBUNT    TO RESP-FELIBUNT-LINE(WS-IX)             
078200     .                                                                    
078300     EJECT                                                                
078400                                                                          
078500*   --- DB2 SECTIONS                                                      
078600*** - CHECK THAT THE REQUESTED LEGAL SELLER EXIST                         
078700 DB2-SELECT-T01LSEL-TAB SECTION.                                          
078800     MOVE 000100 TO GOOD-SQLCODECODES                                     
078900                                                                          
079000     EXEC SQL                                                             
079100           SELECT  IDLEGSEL                                               
079200                 , BELEGRAD_1                                             
079300                                                                          
079400           INTO   :WS-IDLEGSEL                                            
079500                , :WS-BELEGRAD-1                                          
079600                                                                          
079700           FROM    T01LSEL                                                
079800                                                                          
079900           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
080000               AND KDSTATUS = 001                                         
080100     END-EXEC                                                             
080200                                                                          
080300     MOVE SQLCODE TO SQLCODE-WS                                           
080400     PERFORM DB2-STATUS-CHECK                                             
080500     .                                                                    
080600     EJECT                                                                
080700                                                                          
080800* * * * * * * * * *   - CURSOR-HELA- * * * * * * * * * * *                
080900 DB2-COUNT-CRS-HELA SECTION.                                              
081000     EXEC SQL                                                             
081100                                                                          
081200           SELECT COUNT(*)                                                
081300                                                                          
081400           INTO  :WS-COUNTER-HELA                                         
081500                                                                          
081600           FROM   T01TRAW A                                               
081700           ,      T01TBUN B                                               
081800                                                                          
081900           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
082000           AND    A.IDLEGSEL = B.IDLEGSEL                                 
082100           AND    A.IDBUNDLE = B.IDBUNDLE                                 
082200           AND    A.DAREGDAT = B.DAREGDAT                                 
082300           AND    A.TIREGTID = B.TIREGTID                                 
082400           AND    B.FLFEL    = 'J'                                        
082500                                                                          
082600     END-EXEC                                                             
082700                                                                          
082800     MOVE 000100  TO GOOD-SQLCODECODES                                    
082900                                                                          
083000     MOVE SQLCODE TO SQLCODE-WS                                           
083100     PERFORM DB2-STATUS-CHECK                                             
083200     .                                                                    
083300     EJECT                                                                
083400                                                                          
083500 DB2-DCL-OPN-T01TRAW-CRS-1 SECTION.                                       
083600     MOVE 000100 TO GOOD-SQLCODECODES                                     
083700                                                                          
083800     EXEC SQL                                                             
083900         DECLARE T01TRAW-CRS-1 CURSOR WITH HOLD FOR                       
084000                                                                          
084100           SELECT  A.IDBUNDLE                                             
084200                 , A.DAREGDAT                                             
084300                 , A.TIREGTID                                             
084400                 , A.IDREF                                                
084500                 , A.DAREFDAT                                             
084600                 , A.IDREFRAD                                             
084700                 , A.IDSYSTEM_SEND                                        
084800                                                                          
084900           FROM    T01TRAW A                                              
085000           ,       T01TBUN B                                              
085100                                                                          
085200           WHERE   A.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
085300           AND     A.IDLEGSEL = B.IDLEGSEL                                
085400           AND     A.IDBUNDLE = B.IDBUNDLE                                
085500           AND     A.DAREGDAT = B.DAREGDAT                                
085600           AND     A.TIREGTID = B.TIREGTID                                
085700           AND     B.FLFEL    = 'J'                                       
085800                                                                          
085900           ORDER BY A.IDLEGSEL                                            
086000                  , A.DAREGDAT DESC                                       
086100                  , A.TIREGTID DESC                                       
086200                  , A.IDBUNDLE                                            
086300     END-EXEC                                                             
086400                                                                          
086500     MOVE 000100  TO GOOD-SQLCODECODES                                    
086600                                                                          
086700     EXEC SQL                                                             
086800        OPEN T01TRAW-CRS-1                                                
086900     END-EXEC                                                             
087000                                                                          
087100     MOVE SQLCODE TO SQLCODE-WS                                           
087200     PERFORM DB2-STATUS-CHECK                                             
087300     .                                                                    
087400     EJECT                                                                
087500                                                                          
087600 DB2-FETCH-T01TRAW-CRS-1 SECTION.                                         
087700     MOVE 000100  TO GOOD-SQLCODECODES                                    
087800                                                                          
087900     EXEC SQL                                                             
088000                                                                          
088100         FETCH T01TRAW-CRS-1                                              
088200                                                                          
088300         INTO :MAP-IDBUNDLE-LINE                                          
088400            , :MAP-DAREGDAT-LINE                                          
088500            , :MAP-TIREGTID-LINE                                          
088600            , :MAP-IDREF-LINE                                             
088700            , :MAP-DAREFDAT-LINE                                          
088800            , :MAP-IDREFRAD-LINE                                          
088900            , :MAP-IDSYSTEM-SEND-LINE                                     
089000     END-EXEC                                                             
089100                                                                          
089200     MOVE SQLCODE TO SQLCODE-WS                                           
089300     PERFORM DB2-STATUS-CHECK                                             
089400     .                                                                    
089500     EJECT                                                                
089600                                                                          
089700 DB2-CLOSE-T01TRAW-CRS-1 SECTION.                                         
089800     EXEC SQL                                                             
089900        CLOSE T01TRAW-CRS-1                                               
090000     END-EXEC                                                             
090100     .                                                                    
090200     EJECT                                                                
090300                                                                          
090400 DB2-DCL-OPN-T01TRAW-CRS-1-ASC SECTION.                                   
090500     MOVE 000100 TO GOOD-SQLCODECODES                                     
090600                                                                          
090700     EXEC SQL                                                             
090800         DECLARE T01TRAW-CRS-1-ASC CURSOR WITH HOLD FOR                   
090900                                                                          
091000           SELECT  A.IDBUNDLE                                             
091100                 , A.DAREGDAT                                             
091200                 , A.TIREGTID                                             
091300                 , A.IDREF                                                
091400                 , A.DAREFDAT                                             
091500                 , A.IDREFRAD                                             
091600                 , A.IDSYSTEM_SEND                                        
091700                                                                          
091800           FROM    T01TRAW A                                              
091900           ,       T01TBUN B                                              
092000                                                                          
092100           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
092200           AND    A.IDLEGSEL = B.IDLEGSEL                                 
092300           AND    A.IDBUNDLE = B.IDBUNDLE                                 
092400           AND    A.DAREGDAT = B.DAREGDAT                                 
092500           AND    A.TIREGTID = B.TIREGTID                                 
092600           AND    B.FLFEL    = 'J'                                        
092700                                                                          
092800           ORDER BY A.IDLEGSEL                                            
092900                  , A.DAREGDAT                                            
093000                  , A.TIREGTID                                            
093100                  , A.IDBUNDLE                                            
093200     END-EXEC                                                             
093300                                                                          
093400     MOVE 000100  TO GOOD-SQLCODECODES                                    
093500                                                                          
093600     EXEC SQL                                                             
093700        OPEN T01TRAW-CRS-1-ASC                                            
093800     END-EXEC                                                             
093900                                                                          
094000     MOVE SQLCODE TO SQLCODE-WS                                           
094100     PERFORM DB2-STATUS-CHECK                                             
094200     .                                                                    
094300     EJECT                                                                
094400                                                                          
094500 DB2-FETCH-T01TRAW-CRS-1-ASC SECTION.                                     
094600     MOVE 000100  TO GOOD-SQLCODECODES                                    
094700                                                                          
094800     EXEC SQL                                                             
094900                                                                          
095000         FETCH T01TRAW-CRS-1-ASC                                          
095100                                                                          
095200         INTO :MAP-IDBUNDLE-LINE                                          
095300            , :MAP-DAREGDAT-LINE                                          
095400            , :MAP-TIREGTID-LINE                                          
095500            , :MAP-IDREF-LINE                                             
095600            , :MAP-DAREFDAT-LINE                                          
095700            , :MAP-IDREFRAD-LINE                                          
095800            , :MAP-IDSYSTEM-SEND-LINE                                     
095900     END-EXEC                                                             
096000                                                                          
096100     MOVE SQLCODE TO SQLCODE-WS                                           
096200     PERFORM DB2-STATUS-CHECK                                             
096300     .                                                                    
096400     EJECT                                                                
096500                                                                          
096600 DB2-CLOSE-T01TRAW-CRS-1-ASC SECTION.                                     
096700     EXEC SQL                                                             
096800        CLOSE T01TRAW-CRS-1-ASC                                           
096900     END-EXEC                                                             
097000     .                                                                    
097100     EJECT                                                                
097200                                                                          
097300****** COUNT ROWS IN BUNDLE *********                                     
097400 DB2-COUNT-BUNDLEROWS SECTION.                                            
097500     EXEC SQL                                                             
097600                                                                          
097700        SELECT COUNT(*)                                                   
097800                                                                          
097900        INTO  :WS-COUNTER-ANTIBUNT                                        
098000                                                                          
098100        FROM   T01TRAW                                                    
098200                                                                          
098300        WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                            
098400        AND      IDBUNDLE = :MAP-IDBUNDLE-LINE                            
098500        AND      DAREGDAT = :MAP-DAREGDAT-LINE                            
098600        AND      TIREGTID = :MAP-TIREGTID-LINE                            
098700                                                                          
098800     END-EXEC                                                             
098900     MOVE 000100  TO GOOD-SQLCODECODES                                    
099000                                                                          
099100     MOVE SQLCODE TO SQLCODE-WS                                           
099200     PERFORM DB2-STATUS-CHECK                                             
099300     .                                                                    
099400     EJECT                                                                
099500                                                                          
099600******* COUNT ROWS OF ERROR IN BUNDLE *********                           
099700 DB2-COUNT-BUNDLEROWS-ERRORS SECTION.                                     
099800     EXEC SQL                                                             
099900                                                                          
100000        SELECT COUNT(*)                                                   
100100                                                                          
100200        INTO  :WS-COUNTER-FELIBUNT                                        
100300                                                                          
100400        FROM   T01TRAW A                                                  
100500        ,      T01TBUN B                                                  
100600                                                                          
100700        WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                            
100800        AND    A.IDLEGSEL = B.IDLEGSEL                                    
100900        AND    A.IDBUNDLE = B.IDBUNDLE                                    
101000        AND    A.DAREGDAT = B.DAREGDAT                                    
101100        AND    A.TIREGTID = B.TIREGTID                                    
101200        AND    A.IDBUNDLE = :MAP-IDBUNDLE-LINE                            
101300        AND    A.DAREGDAT = :MAP-DAREGDAT-LINE                            
101400        AND    A.TIREGTID = :MAP-TIREGTID-LINE                            
101500        AND    B.FLFEL    = 'J'                                           
101600                                                                          
101700     END-EXEC                                                             
101800     MOVE 000100  TO GOOD-SQLCODECODES                                    
101900                                                                          
102000     MOVE SQLCODE TO SQLCODE-WS                                           
102100     PERFORM DB2-STATUS-CHECK                                             
102200     .                                                                    
102300     EJECT                                                                
102400                                                                          
102500* * * * * * * * * *   - CURSOR-SYSTEM- *  * * * * * * * * * * *           
102600 DB2-COUNT-CRS-SYSTEM SECTION.                                            
102700     EXEC SQL                                                             
102800                                                                          
102900           SELECT COUNT(*)                                                
103000                                                                          
103100           INTO  :WS-COUNTER-SYSTEM                                       
103200                                                                          
103300           FROM   T01TRAW A                                               
103400           ,      T01TBUN B                                               
103500                                                                          
103600           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
103700           AND    A.IDLEGSEL = B.IDLEGSEL                                 
103800           AND    A.IDBUNDLE = B.IDBUNDLE                                 
103900           AND    A.DAREGDAT = B.DAREGDAT                                 
104000           AND    A.TIREGTID = B.TIREGTID                                 
104100           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
104200           AND    B.FLFEL    = 'J'                                        
104300                                                                          
104400     END-EXEC                                                             
104500                                                                          
104600     MOVE 000100  TO GOOD-SQLCODECODES                                    
104700                                                                          
104800     MOVE SQLCODE TO SQLCODE-WS                                           
104900     PERFORM DB2-STATUS-CHECK                                             
105000     .                                                                    
105100     EJECT                                                                
105200                                                                          
105300 DB2-DCL-OPN-T01TRAW-CRS-2 SECTION.                                       
105400     MOVE 000100 TO GOOD-SQLCODECODES                                     
105500                                                                          
105600     EXEC SQL                                                             
105700         DECLARE T01TRAW-CRS-2 CURSOR WITH HOLD FOR                       
105800                                                                          
105900           SELECT  A.IDBUNDLE                                             
106000                 , A.DAREGDAT                                             
106100                 , A.TIREGTID                                             
106200                 , A.IDREF                                                
106300                 , A.DAREFDAT                                             
106400                 , A.IDREFRAD                                             
106500                 , A.IDSYSTEM_SEND                                        
106600                                                                          
106700           FROM    T01TRAW A                                              
106800           ,       T01TBUN B                                              
106900                                                                          
107000           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
107100           AND    A.IDLEGSEL = B.IDLEGSEL                                 
107200           AND    A.IDBUNDLE = B.IDBUNDLE                                 
107300           AND    A.DAREGDAT = B.DAREGDAT                                 
107400           AND    A.TIREGTID = B.TIREGTID                                 
107500           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
107600           AND    B.FLFEL    = 'J'                                        
107700                                                                          
107800           ORDER BY A.IDLEGSEL                                            
107900                  , A.DAREGDAT DESC                                       
108000                  , A.TIREGTID DESC                                       
108100                  , A.IDBUNDLE                                            
108200     END-EXEC                                                             
108300                                                                          
108400     MOVE 000100  TO GOOD-SQLCODECODES                                    
108500                                                                          
108600     EXEC SQL                                                             
108700        OPEN T01TRAW-CRS-2                                                
108800     END-EXEC                                                             
108900                                                                          
109000     MOVE SQLCODE TO SQLCODE-WS                                           
109100     PERFORM DB2-STATUS-CHECK                                             
109200     .                                                                    
109300     EJECT                                                                
109400                                                                          
109500 DB2-FETCH-T01TRAW-CRS-2 SECTION.                                         
109600     MOVE 000100  TO GOOD-SQLCODECODES                                    
109700                                                                          
109800     EXEC SQL                                                             
109900                                                                          
110000         FETCH T01TRAW-CRS-2                                              
110100                                                                          
110200         INTO :MAP-IDBUNDLE-LINE                                          
110300            , :MAP-DAREGDAT-LINE                                          
110400            , :MAP-TIREGTID-LINE                                          
110500            , :MAP-IDREF-LINE                                             
110600            , :MAP-DAREFDAT-LINE                                          
110700            , :MAP-IDREFRAD-LINE                                          
110800            , :MAP-IDSYSTEM-SEND-LINE                                     
110900     END-EXEC                                                             
111000                                                                          
111100     MOVE SQLCODE TO SQLCODE-WS                                           
111200     PERFORM DB2-STATUS-CHECK                                             
111300     .                                                                    
111400     EJECT                                                                
111500                                                                          
111600 DB2-CLOSE-T01TRAW-CRS-2 SECTION.                                         
111700     EXEC SQL                                                             
111800        CLOSE T01TRAW-CRS-2                                               
111900     END-EXEC                                                             
112000     .                                                                    
112100     EJECT                                                                
112200                                                                          
112300 DB2-DCL-OPN-T01TRAW-CRS-2-ASC SECTION.                                   
112400     MOVE 000100 TO GOOD-SQLCODECODES                                     
112500                                                                          
112600     EXEC SQL                                                             
112700         DECLARE T01TRAW-CRS-2-ASC CURSOR WITH HOLD FOR                   
112800                                                                          
112900           SELECT  A.IDBUNDLE                                             
113000                 , A.DAREGDAT                                             
113100                 , A.TIREGTID                                             
113200                 , A.IDREF                                                
113300                 , A.DAREFDAT                                             
113400                 , A.IDREFRAD                                             
113500                 , A.IDSYSTEM_SEND                                        
113600                                                                          
113700           FROM    T01TRAW A                                              
113800           ,       T01TBUN B                                              
113900                                                                          
114000           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
114100           AND    A.IDLEGSEL = B.IDLEGSEL                                 
114200           AND    A.IDBUNDLE = B.IDBUNDLE                                 
114300           AND    A.DAREGDAT = B.DAREGDAT                                 
114400           AND    A.TIREGTID = B.TIREGTID                                 
114500           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
114600           AND    B.FLFEL    = 'J'                                        
114700                                                                          
114800           ORDER BY A.IDLEGSEL                                            
114900                  , A.DAREGDAT                                            
115000                  , A.TIREGTID                                            
115100                  , A.IDBUNDLE                                            
115200     END-EXEC                                                             
115300                                                                          
115400     MOVE 000100  TO GOOD-SQLCODECODES                                    
115500                                                                          
115600     EXEC SQL                                                             
115700        OPEN T01TRAW-CRS-2-ASC                                            
115800     END-EXEC                                                             
115900                                                                          
116000     MOVE SQLCODE TO SQLCODE-WS                                           
116100     PERFORM DB2-STATUS-CHECK                                             
116200     .                                                                    
116300     EJECT                                                                
116400                                                                          
116500 DB2-FETCH-T01TRAW-CRS-2-ASC SECTION.                                     
116600     MOVE 000100  TO GOOD-SQLCODECODES                                    
116700                                                                          
116800     EXEC SQL                                                             
116900                                                                          
117000         FETCH T01TRAW-CRS-2-ASC                                          
117100                                                                          
117200         INTO :MAP-IDBUNDLE-LINE                                          
117300            , :MAP-DAREGDAT-LINE                                          
117400            , :MAP-TIREGTID-LINE                                          
117500            , :MAP-IDREF-LINE                                             
117600            , :MAP-DAREFDAT-LINE                                          
117700            , :MAP-IDREFRAD-LINE                                          
117800            , :MAP-IDSYSTEM-SEND-LINE                                     
117900     END-EXEC                                                             
118000                                                                          
118100     MOVE SQLCODE TO SQLCODE-WS                                           
118200     PERFORM DB2-STATUS-CHECK                                             
118300     .                                                                    
118400     EJECT                                                                
118500                                                                          
118600 DB2-CLOSE-T01TRAW-CRS-2-ASC SECTION.                                     
118700     EXEC SQL                                                             
118800        CLOSE T01TRAW-CRS-2-ASC                                           
118900     END-EXEC                                                             
119000     .                                                                    
119100     EJECT                                                                
119200                                                                          
119300* * * * * * * * * *   - CURSOR-SYSTEM-DATUM-  * * * * * * * * *           
119400 DB2-COUNT-CRS-SYSTEM-DATUM SECTION.                                      
119500     EXEC SQL                                                             
119600                                                                          
119700           SELECT COUNT(*)                                                
119800                                                                          
119900           INTO  :WS-COUNTER-SYSTEM-DATUM                                 
120000                                                                          
120100           FROM   T01TRAW A                                               
120200           ,      T01TBUN B                                               
120300                                                                          
120400           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
120500           AND    A.IDLEGSEL = B.IDLEGSEL                                 
120600           AND    A.IDBUNDLE = B.IDBUNDLE                                 
120700           AND    A.DAREGDAT = B.DAREGDAT                                 
120800           AND    A.TIREGTID = B.TIREGTID                                 
120900           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
121000           AND    B.DAREGDAT >= :WS-DATUM                                 
121100           AND    B.FLFEL    = 'J'                                        
121200                                                                          
121300     END-EXEC                                                             
121400                                                                          
121500     MOVE 000100  TO GOOD-SQLCODECODES                                    
121600                                                                          
121700     MOVE SQLCODE TO SQLCODE-WS                                           
121800     PERFORM DB2-STATUS-CHECK                                             
121900     .                                                                    
122000     EJECT                                                                
122100                                                                          
122200 DB2-DCL-OPN-T01TRAW-CRS-3 SECTION.                                       
122300     MOVE 000100 TO GOOD-SQLCODECODES                                     
122400                                                                          
122500     EXEC SQL                                                             
122600         DECLARE T01TRAW-CRS-3 CURSOR WITH HOLD FOR                       
122700                                                                          
122800           SELECT  A.IDBUNDLE                                             
122900                 , A.DAREGDAT                                             
123000                 , A.TIREGTID                                             
123100                 , A.IDREF                                                
123200                 , A.DAREFDAT                                             
123300                 , A.IDREFRAD                                             
123400                 , A.IDSYSTEM_SEND                                        
123500                                                                          
123600           FROM    T01TRAW A                                              
123700           ,       T01TBUN B                                              
123800                                                                          
123900           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
124000           AND    A.IDLEGSEL = B.IDLEGSEL                                 
124100           AND    A.IDBUNDLE = B.IDBUNDLE                                 
124200           AND    A.DAREGDAT = B.DAREGDAT                                 
124300           AND    A.TIREGTID = B.TIREGTID                                 
124400           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
124500           AND    B.DAREGDAT >= :WS-DATUM                                 
124600           AND    B.FLFEL    = 'J'                                        
124700                                                                          
124800           ORDER BY A.IDLEGSEL                                            
124900                  , A.DAREGDAT DESC                                       
125000                  , A.TIREGTID DESC                                       
125100                  , A.IDBUNDLE                                            
125200     END-EXEC                                                             
125300                                                                          
125400     MOVE 000100  TO GOOD-SQLCODECODES                                    
125500                                                                          
125600     EXEC SQL                                                             
125700        OPEN T01TRAW-CRS-3                                                
125800     END-EXEC                                                             
125900                                                                          
126000     MOVE SQLCODE TO SQLCODE-WS                                           
126100     PERFORM DB2-STATUS-CHECK                                             
126200     .                                                                    
126300     EJECT                                                                
126400                                                                          
126500 DB2-FETCH-T01TRAW-CRS-3 SECTION.                                         
126600     MOVE 000100  TO GOOD-SQLCODECODES                                    
126700                                                                          
126800     EXEC SQL                                                             
126900                                                                          
127000         FETCH T01TRAW-CRS-3                                              
127100                                                                          
127200         INTO :MAP-IDBUNDLE-LINE                                          
127300            , :MAP-DAREGDAT-LINE                                          
127400            , :MAP-TIREGTID-LINE                                          
127500            , :MAP-IDREF-LINE                                             
127600            , :MAP-DAREFDAT-LINE                                          
127700            , :MAP-IDREFRAD-LINE                                          
127800            , :MAP-IDSYSTEM-SEND-LINE                                     
127900     END-EXEC                                                             
128000                                                                          
128100     MOVE SQLCODE TO SQLCODE-WS                                           
128200     PERFORM DB2-STATUS-CHECK                                             
128300     .                                                                    
128400     EJECT                                                                
128500                                                                          
128600 DB2-CLOSE-T01TRAW-CRS-3 SECTION.                                         
128700     EXEC SQL                                                             
128800        CLOSE T01TRAW-CRS-3                                               
128900     END-EXEC                                                             
129000     .                                                                    
129100     EJECT                                                                
129200                                                                          
129300 DB2-DCL-OPN-T01TRAW-CRS-3-ASC SECTION.                                   
129400     MOVE 000100 TO GOOD-SQLCODECODES                                     
129500                                                                          
129600     EXEC SQL                                                             
129700         DECLARE T01TRAW-CRS-3-ASC CURSOR WITH HOLD FOR                   
129800                                                                          
129900           SELECT  A.IDBUNDLE                                             
130000                 , A.DAREGDAT                                             
130100                 , A.TIREGTID                                             
130200                 , A.IDREF                                                
130300                 , A.DAREFDAT                                             
130400                 , A.IDREFRAD                                             
130500                 , A.IDSYSTEM_SEND                                        
130600                                                                          
130700           FROM    T01TRAW A                                              
130800           ,       T01TBUN B                                              
130900                                                                          
131000           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
131100           AND    A.IDLEGSEL = B.IDLEGSEL                                 
131200           AND    A.IDBUNDLE = B.IDBUNDLE                                 
131300           AND    A.DAREGDAT = B.DAREGDAT                                 
131400           AND    A.TIREGTID = B.TIREGTID                                 
131500           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
131600           AND    B.DAREGDAT >= :WS-DATUM                                 
131700           AND    B.FLFEL    = 'J'                                        
131800                                                                          
131900           ORDER BY A.IDLEGSEL                                            
132000                  , A.DAREGDAT                                            
132100                  , A.TIREGTID                                            
132200                  , A.IDBUNDLE                                            
132300     END-EXEC                                                             
132400                                                                          
132500     MOVE 000100  TO GOOD-SQLCODECODES                                    
132600                                                                          
132700     EXEC SQL                                                             
132800        OPEN T01TRAW-CRS-3-ASC                                            
132900     END-EXEC                                                             
133000                                                                          
133100     MOVE SQLCODE TO SQLCODE-WS                                           
133200     PERFORM DB2-STATUS-CHECK                                             
133300     .                                                                    
133400     EJECT                                                                
133500                                                                          
133600 DB2-FETCH-T01TRAW-CRS-3-ASC SECTION.                                     
133700     MOVE 000100  TO GOOD-SQLCODECODES                                    
133800                                                                          
133900     EXEC SQL                                                             
134000                                                                          
134100         FETCH T01TRAW-CRS-3-ASC                                          
134200                                                                          
134300         INTO :MAP-IDBUNDLE-LINE                                          
134400            , :MAP-DAREGDAT-LINE                                          
134500            , :MAP-TIREGTID-LINE                                          
134600            , :MAP-IDREF-LINE                                             
134700            , :MAP-DAREFDAT-LINE                                          
134800            , :MAP-IDREFRAD-LINE                                          
134900            , :MAP-IDSYSTEM-SEND-LINE                                     
135000     END-EXEC                                                             
135100                                                                          
135200     MOVE SQLCODE TO SQLCODE-WS                                           
135300     PERFORM DB2-STATUS-CHECK                                             
135400     .                                                                    
135500     EJECT                                                                
135600                                                                          
135700 DB2-CLOSE-T01TRAW-CRS-3-ASC SECTION.                                     
135800     EXEC SQL                                                             
135900        CLOSE T01TRAW-CRS-3-ASC                                           
136000     END-EXEC                                                             
136100     .                                                                    
136200     EJECT                                                                
136300                                                                          
136400* * * * * * * * * *   - CURSOR-SYSTEM-TID- *  * * * * * * * * *           
136500 DB2-COUNT-CRS-SYSTEM-TID SECTION.                                        
136600     EXEC SQL                                                             
136700                                                                          
136800           SELECT COUNT(*)                                                
136900                                                                          
137000           INTO  :WS-COUNTER-SYSTEM-TID                                   
137100                                                                          
137200           FROM   T01TRAW A                                               
137300           ,      T01TBUN B                                               
137400                                                                          
137500           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
137600           AND    A.IDLEGSEL = B.IDLEGSEL                                 
137700           AND    A.IDBUNDLE = B.IDBUNDLE                                 
137800           AND    A.DAREGDAT = B.DAREGDAT                                 
137900           AND    A.TIREGTID = B.TIREGTID                                 
138000           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
138100           AND  ( B.DAREGDAT = :WS-DATUM                                  
138200           AND    B.TIREGTID > :WS-TID2                                   
138300           OR     B.DAREGDAT > :WS-DATUM )                                
138400           AND    B.FLFEL    = 'J'                                        
138500                                                                          
138600     END-EXEC                                                             
138700                                                                          
138800     MOVE 000100  TO GOOD-SQLCODECODES                                    
138900                                                                          
139000     MOVE SQLCODE TO SQLCODE-WS                                           
139100     PERFORM DB2-STATUS-CHECK                                             
139200     .                                                                    
139300     EJECT                                                                
139400                                                                          
139500 DB2-DCL-OPN-T01TRAW-CRS-4 SECTION.                                       
139600     MOVE 000100 TO GOOD-SQLCODECODES                                     
139700                                                                          
139800     EXEC SQL                                                             
139900         DECLARE T01TRAW-CRS-4 CURSOR WITH HOLD FOR                       
140000                                                                          
140100           SELECT  A.IDBUNDLE                                             
140200                 , A.DAREGDAT                                             
140300                 , A.TIREGTID                                             
140400                 , A.IDREF                                                
140500                 , A.DAREFDAT                                             
140600                 , A.IDREFRAD                                             
140700                 , A.IDSYSTEM_SEND                                        
140800                                                                          
140900           FROM    T01TRAW A                                              
141000           ,       T01TBUN B                                              
141100                                                                          
141200           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
141300           AND    A.IDLEGSEL = B.IDLEGSEL                                 
141400           AND    A.IDBUNDLE = B.IDBUNDLE                                 
141500           AND    A.DAREGDAT = B.DAREGDAT                                 
141600           AND    A.TIREGTID = B.TIREGTID                                 
141700           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
141800           AND  ( B.DAREGDAT = :WS-DATUM                                  
141900           AND    B.TIREGTID > :WS-TID2                                   
142000           OR     B.DAREGDAT > :WS-DATUM )                                
142100           AND    B.FLFEL    = 'J'                                        
142200                                                                          
142300           ORDER BY A.IDLEGSEL                                            
142400                  , A.DAREGDAT DESC                                       
142500                  , A.TIREGTID DESC                                       
142600                  , A.IDBUNDLE                                            
142700     END-EXEC                                                             
142800                                                                          
142900     MOVE 000100  TO GOOD-SQLCODECODES                                    
143000                                                                          
143100     EXEC SQL                                                             
143200        OPEN T01TRAW-CRS-4                                                
143300     END-EXEC                                                             
143400                                                                          
143500     MOVE SQLCODE TO SQLCODE-WS                                           
143600     PERFORM DB2-STATUS-CHECK                                             
143700     .                                                                    
143800     EJECT                                                                
143900                                                                          
144000 DB2-FETCH-T01TRAW-CRS-4 SECTION.                                         
144100     MOVE 000100  TO GOOD-SQLCODECODES                                    
144200                                                                          
144300     EXEC SQL                                                             
144400                                                                          
144500         FETCH T01TRAW-CRS-4                                              
144600                                                                          
144700         INTO :MAP-IDBUNDLE-LINE                                          
144800            , :MAP-DAREGDAT-LINE                                          
144900            , :MAP-TIREGTID-LINE                                          
145000            , :MAP-IDREF-LINE                                             
145100            , :MAP-DAREFDAT-LINE                                          
145200            , :MAP-IDREFRAD-LINE                                          
145300            , :MAP-IDSYSTEM-SEND-LINE                                     
145400     END-EXEC                                                             
145500                                                                          
145600     MOVE SQLCODE TO SQLCODE-WS                                           
145700     PERFORM DB2-STATUS-CHECK                                             
145800     .                                                                    
145900     EJECT                                                                
146000                                                                          
146100 DB2-CLOSE-T01TRAW-CRS-4 SECTION.                                         
146200     EXEC SQL                                                             
146300        CLOSE T01TRAW-CRS-4                                               
146400     END-EXEC                                                             
146500     .                                                                    
146600     EJECT                                                                
146700                                                                          
146800 DB2-DCL-OPN-T01TRAW-CRS-4-ASC SECTION.                                   
146900     MOVE 000100 TO GOOD-SQLCODECODES                                     
147000                                                                          
147100     EXEC SQL                                                             
147200         DECLARE T01TRAW-CRS-4-ASC CURSOR WITH HOLD FOR                   
147300                                                                          
147400           SELECT  A.IDBUNDLE                                             
147500                 , A.DAREGDAT                                             
147600                 , A.TIREGTID                                             
147700                 , A.IDREF                                                
147800                 , A.DAREFDAT                                             
147900                 , A.IDREFRAD                                             
148000                 , A.IDSYSTEM_SEND                                        
148100                                                                          
148200           FROM    T01TRAW A                                              
148300           ,       T01TBUN B                                              
148400                                                                          
148500           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
148600           AND    A.IDLEGSEL = B.IDLEGSEL                                 
148700           AND    A.IDBUNDLE = B.IDBUNDLE                                 
148800           AND    A.DAREGDAT = B.DAREGDAT                                 
148900           AND    A.TIREGTID = B.TIREGTID                                 
149000           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
149100           AND  ( B.DAREGDAT = :WS-DATUM                                  
149200           AND    B.TIREGTID > :WS-TID2                                   
149300           OR     B.DAREGDAT > :WS-DATUM )                                
149400           AND    B.FLFEL    = 'J'                                        
149500                                                                          
149600           ORDER BY A.IDLEGSEL                                            
149700                  , A.DAREGDAT                                            
149800                  , A.TIREGTID                                            
149900                  , A.IDBUNDLE                                            
150000     END-EXEC                                                             
150100                                                                          
150200     MOVE 000100  TO GOOD-SQLCODECODES                                    
150300                                                                          
150400     EXEC SQL                                                             
150500        OPEN T01TRAW-CRS-4-ASC                                            
150600     END-EXEC                                                             
150700                                                                          
150800     MOVE SQLCODE TO SQLCODE-WS                                           
150900     PERFORM DB2-STATUS-CHECK                                             
151000     .                                                                    
151100     EJECT                                                                
151200                                                                          
151300 DB2-FETCH-T01TRAW-CRS-4-ASC SECTION.                                     
151400     MOVE 000100  TO GOOD-SQLCODECODES                                    
151500                                                                          
151600     EXEC SQL                                                             
151700                                                                          
151800         FETCH T01TRAW-CRS-4-ASC                                          
151900                                                                          
152000         INTO :MAP-IDBUNDLE-LINE                                          
152100            , :MAP-DAREGDAT-LINE                                          
152200            , :MAP-TIREGTID-LINE                                          
152300            , :MAP-IDREF-LINE                                             
152400            , :MAP-DAREFDAT-LINE                                          
152500            , :MAP-IDREFRAD-LINE                                          
152600            , :MAP-IDSYSTEM-SEND-LINE                                     
152700     END-EXEC                                                             
152800                                                                          
152900     MOVE SQLCODE TO SQLCODE-WS                                           
153000     PERFORM DB2-STATUS-CHECK                                             
153100     .                                                                    
153200     EJECT                                                                
153300                                                                          
153400 DB2-CLOSE-T01TRAW-CRS-4-ASC SECTION.                                     
153500     EXEC SQL                                                             
153600        CLOSE T01TRAW-CRS-4-ASC                                           
153700     END-EXEC                                                             
153800     .                                                                    
153900     EJECT                                                                
154000                                                                          
154100* * * * * * * * * *   - CURSOR-SYSTEM-DATUM-TID-* * * * * * * *           
154200 DB2-COUNT-CRS-SYSTEM-DATUM-TID SECTION.                                  
154300     EXEC SQL                                                             
154400                                                                          
154500           SELECT COUNT(*)                                                
154600                                                                          
154700           INTO  :WS-COUNTER-SYSTEM-DATUM-TID                             
154800                                                                          
154900           FROM   T01TRAW A                                               
155000           ,      T01TBUN B                                               
155100                                                                          
155200           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
155300           AND    A.IDLEGSEL = B.IDLEGSEL                                 
155400           AND    A.IDBUNDLE = B.IDBUNDLE                                 
155500           AND    A.DAREGDAT = B.DAREGDAT                                 
155600           AND    A.TIREGTID = B.TIREGTID                                 
155700           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
155800           AND  ( B.DAREGDAT = :WS-DATUM                                  
155900           AND    B.TIREGTID > :WS-TID2                                   
156000           OR     B.DAREGDAT > :WS-DATUM )                                
156100           AND    B.FLFEL    = 'J'                                        
156200                                                                          
156300     END-EXEC                                                             
156400                                                                          
156500     MOVE 000100  TO GOOD-SQLCODECODES                                    
156600                                                                          
156700     MOVE SQLCODE TO SQLCODE-WS                                           
156800     PERFORM DB2-STATUS-CHECK                                             
156900     .                                                                    
157000     EJECT                                                                
157100                                                                          
157200 DB2-DCL-OPN-T01TRAW-CRS-5 SECTION.                                       
157300     MOVE 000100 TO GOOD-SQLCODECODES                                     
157400                                                                          
157500     EXEC SQL                                                             
157600         DECLARE T01TRAW-CRS-5 CURSOR WITH HOLD FOR                       
157700                                                                          
157800           SELECT  A.IDBUNDLE                                             
157900                 , A.DAREGDAT                                             
158000                 , A.TIREGTID                                             
158100                 , A.IDREF                                                
158200                 , A.DAREFDAT                                             
158300                 , A.IDREFRAD                                             
158400                 , A.IDSYSTEM_SEND                                        
158500                                                                          
158600           FROM    T01TRAW A                                              
158700           ,       T01TBUN B                                              
158800                                                                          
158900           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
159000           AND    A.IDLEGSEL = B.IDLEGSEL                                 
159100           AND    A.IDBUNDLE = B.IDBUNDLE                                 
159200           AND    A.DAREGDAT = B.DAREGDAT                                 
159300           AND    A.TIREGTID = B.TIREGTID                                 
159400           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
159500           AND  ( B.DAREGDAT = :WS-DATUM                                  
159600           AND    B.TIREGTID > :WS-TID2                                   
159700           OR     B.DAREGDAT > :WS-DATUM )                                
159800           AND    B.FLFEL    = 'J'                                        
159900                                                                          
160000           ORDER BY A.IDLEGSEL                                            
160100                  , A.DAREGDAT DESC                                       
160200                  , A.TIREGTID DESC                                       
160300                  , A.IDBUNDLE                                            
160400     END-EXEC                                                             
160500                                                                          
160600     MOVE 000100  TO GOOD-SQLCODECODES                                    
160700                                                                          
160800     EXEC SQL                                                             
160900        OPEN T01TRAW-CRS-5                                                
161000     END-EXEC                                                             
161100                                                                          
161200     MOVE SQLCODE TO SQLCODE-WS                                           
161300     PERFORM DB2-STATUS-CHECK                                             
161400     .                                                                    
161500     EJECT                                                                
161600                                                                          
161700 DB2-FETCH-T01TRAW-CRS-5 SECTION.                                         
161800     MOVE 000100  TO GOOD-SQLCODECODES                                    
161900                                                                          
162000     EXEC SQL                                                             
162100                                                                          
162200         FETCH T01TRAW-CRS-5                                              
162300                                                                          
162400         INTO :MAP-IDBUNDLE-LINE                                          
162500            , :MAP-DAREGDAT-LINE                                          
162600            , :MAP-TIREGTID-LINE                                          
162700            , :MAP-IDREF-LINE                                             
162800            , :MAP-DAREFDAT-LINE                                          
162900            , :MAP-IDREFRAD-LINE                                          
163000            , :MAP-IDSYSTEM-SEND-LINE                                     
163100     END-EXEC                                                             
163200                                                                          
163300     MOVE SQLCODE TO SQLCODE-WS                                           
163400     PERFORM DB2-STATUS-CHECK                                             
163500     .                                                                    
163600     EJECT                                                                
163700                                                                          
163800 DB2-CLOSE-T01TRAW-CRS-5 SECTION.                                         
163900     EXEC SQL                                                             
164000        CLOSE T01TRAW-CRS-5                                               
164100     END-EXEC                                                             
164200     .                                                                    
164300     EJECT                                                                
164400                                                                          
164500 DB2-DCL-OPN-T01TRAW-CRS-5-ASC SECTION.                                   
164600     MOVE 000100 TO GOOD-SQLCODECODES                                     
164700                                                                          
164800     EXEC SQL                                                             
164900         DECLARE T01TRAW-CRS-5-ASC CURSOR WITH HOLD FOR                   
165000                                                                          
165100           SELECT  A.IDBUNDLE                                             
165200                 , A.DAREGDAT                                             
165300                 , A.TIREGTID                                             
165400                 , A.IDREF                                                
165500                 , A.DAREFDAT                                             
165600                 , A.IDREFRAD                                             
165700                 , A.IDSYSTEM_SEND                                        
165800                                                                          
165900           FROM    T01TRAW A                                              
166000           ,       T01TBUN B                                              
166100                                                                          
166200           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
166300           AND    A.IDLEGSEL = B.IDLEGSEL                                 
166400           AND    A.IDBUNDLE = B.IDBUNDLE                                 
166500           AND    A.DAREGDAT = B.DAREGDAT                                 
166600           AND    A.TIREGTID = B.TIREGTID                                 
166700           AND    A.IDSYSTEM_SEND = :REQU-IDSYSTEM-SEND                   
166800           AND  ( B.DAREGDAT = :WS-DATUM                                  
166900           AND    B.TIREGTID > :WS-TID2                                   
167000           OR     B.DAREGDAT > :WS-DATUM )                                
167100           AND    B.FLFEL    = 'J'                                        
167200                                                                          
167300           ORDER BY A.IDLEGSEL                                            
167400                  , A.DAREGDAT                                            
167500                  , A.TIREGTID                                            
167600                  , A.IDBUNDLE                                            
167700     END-EXEC                                                             
167800                                                                          
167900     MOVE 000100  TO GOOD-SQLCODECODES                                    
168000                                                                          
168100     EXEC SQL                                                             
168200        OPEN T01TRAW-CRS-5-ASC                                            
168300     END-EXEC                                                             
168400                                                                          
168500     MOVE SQLCODE TO SQLCODE-WS                                           
168600     PERFORM DB2-STATUS-CHECK                                             
168700     .                                                                    
168800     EJECT                                                                
168900                                                                          
169000 DB2-FETCH-T01TRAW-CRS-5-ASC SECTION.                                     
169100     MOVE 000100  TO GOOD-SQLCODECODES                                    
169200                                                                          
169300     EXEC SQL                                                             
169400                                                                          
169500         FETCH T01TRAW-CRS-5-ASC                                          
169600                                                                          
169700         INTO :MAP-IDBUNDLE-LINE                                          
169800            , :MAP-DAREGDAT-LINE                                          
169900            , :MAP-TIREGTID-LINE                                          
170000            , :MAP-IDREF-LINE                                             
170100            , :MAP-DAREFDAT-LINE                                          
170200            , :MAP-IDREFRAD-LINE                                          
170300            , :MAP-IDSYSTEM-SEND-LINE                                     
170400     END-EXEC                                                             
170500                                                                          
170600     MOVE SQLCODE TO SQLCODE-WS                                           
170700     PERFORM DB2-STATUS-CHECK                                             
170800     .                                                                    
170900     EJECT                                                                
171000                                                                          
171100 DB2-CLOSE-T01TRAW-CRS-5-ASC SECTION.                                     
171200     EXEC SQL                                                             
171300        CLOSE T01TRAW-CRS-5-ASC                                           
171400     END-EXEC                                                             
171500     .                                                                    
171600     EJECT                                                                
171700                                                                          
171800* * * * * * * * * *   - CURSOR-DATUM- * * * * * * * * * * * * *           
171900 DB2-COUNT-CRS-DATUM SECTION.                                             
172000     EXEC SQL                                                             
172100                                                                          
172200           SELECT COUNT(*)                                                
172300                                                                          
172400           INTO  :WS-COUNTER-DATUM                                        
172500                                                                          
172600           FROM   T01TRAW A                                               
172700           ,      T01TBUN B                                               
172800                                                                          
172900           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
173000           AND    A.IDLEGSEL = B.IDLEGSEL                                 
173100           AND    A.IDBUNDLE = B.IDBUNDLE                                 
173200           AND    A.DAREGDAT = B.DAREGDAT                                 
173300           AND    A.TIREGTID = B.TIREGTID                                 
173400           AND    B.DAREGDAT >= :WS-DATUM                                 
173500           AND    B.FLFEL    = 'J'                                        
173600                                                                          
173700     END-EXEC                                                             
173800                                                                          
173900     MOVE 000100  TO GOOD-SQLCODECODES                                    
174000                                                                          
174100     MOVE SQLCODE TO SQLCODE-WS                                           
174200     PERFORM DB2-STATUS-CHECK                                             
174300     .                                                                    
174400     EJECT                                                                
174500                                                                          
174600 DB2-DCL-OPN-T01TRAW-CRS-6 SECTION.                                       
174700     MOVE 000100 TO GOOD-SQLCODECODES                                     
174800                                                                          
174900     EXEC SQL                                                             
175000         DECLARE T01TRAW-CRS-6 CURSOR WITH HOLD FOR                       
175100                                                                          
175200           SELECT  A.IDBUNDLE                                             
175300                 , A.DAREGDAT                                             
175400                 , A.TIREGTID                                             
175500                 , A.IDREF                                                
175600                 , A.DAREFDAT                                             
175700                 , A.IDREFRAD                                             
175800                 , A.IDSYSTEM_SEND                                        
175900                                                                          
176000           FROM    T01TRAW A                                              
176100           ,       T01TBUN B                                              
176200                                                                          
176300           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
176400           AND    A.IDLEGSEL = B.IDLEGSEL                                 
176500           AND    A.IDBUNDLE = B.IDBUNDLE                                 
176600           AND    A.DAREGDAT = B.DAREGDAT                                 
176700           AND    A.TIREGTID = B.TIREGTID                                 
176800           AND    B.DAREGDAT >= :WS-DATUM                                 
176900           AND    B.FLFEL    = 'J'                                        
177000                                                                          
177100           ORDER BY A.IDLEGSEL                                            
177200                  , A.DAREGDAT DESC                                       
177300                  , A.TIREGTID DESC                                       
177400                  , A.IDBUNDLE                                            
177500     END-EXEC                                                             
177600                                                                          
177700     MOVE 000100  TO GOOD-SQLCODECODES                                    
177800                                                                          
177900     EXEC SQL                                                             
178000        OPEN T01TRAW-CRS-6                                                
178100     END-EXEC                                                             
178200                                                                          
178300     MOVE SQLCODE TO SQLCODE-WS                                           
178400     PERFORM DB2-STATUS-CHECK                                             
178500     .                                                                    
178600     EJECT                                                                
178700                                                                          
178800 DB2-FETCH-T01TRAW-CRS-6 SECTION.                                         
178900     MOVE 000100  TO GOOD-SQLCODECODES                                    
179000                                                                          
179100     EXEC SQL                                                             
179200                                                                          
179300         FETCH T01TRAW-CRS-6                                              
179400                                                                          
179500         INTO :MAP-IDBUNDLE-LINE                                          
179600            , :MAP-DAREGDAT-LINE                                          
179700            , :MAP-TIREGTID-LINE                                          
179800            , :MAP-IDREF-LINE                                             
179900            , :MAP-DAREFDAT-LINE                                          
180000            , :MAP-IDREFRAD-LINE                                          
180100            , :MAP-IDSYSTEM-SEND-LINE                                     
180200     END-EXEC                                                             
180300                                                                          
180400     MOVE SQLCODE TO SQLCODE-WS                                           
180500     PERFORM DB2-STATUS-CHECK                                             
180600     .                                                                    
180700     EJECT                                                                
180800                                                                          
180900 DB2-CLOSE-T01TRAW-CRS-6 SECTION.                                         
181000     EXEC SQL                                                             
181100        CLOSE T01TRAW-CRS-6                                               
181200     END-EXEC                                                             
181300     .                                                                    
181400     EJECT                                                                
181500                                                                          
181600 DB2-DCL-OPN-T01TRAW-CRS-6-ASC SECTION.                                   
181700     MOVE 000100 TO GOOD-SQLCODECODES                                     
181800                                                                          
181900     EXEC SQL                                                             
182000         DECLARE T01TRAW-CRS-6-ASC CURSOR WITH HOLD FOR                   
182100                                                                          
182200           SELECT  A.IDBUNDLE                                             
182300                 , A.DAREGDAT                                             
182400                 , A.TIREGTID                                             
182500                 , A.IDREF                                                
182600                 , A.DAREFDAT                                             
182700                 , A.IDREFRAD                                             
182800                 , A.IDSYSTEM_SEND                                        
182900                                                                          
183000           FROM    T01TRAW A                                              
183100           ,       T01TBUN B                                              
183200                                                                          
183300           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
183400           AND    A.IDLEGSEL = B.IDLEGSEL                                 
183500           AND    A.IDBUNDLE = B.IDBUNDLE                                 
183600           AND    A.DAREGDAT = B.DAREGDAT                                 
183700           AND    A.TIREGTID = B.TIREGTID                                 
183800           AND    B.DAREGDAT >= :WS-DATUM                                 
183900           AND    B.FLFEL    = 'J'                                        
184000                                                                          
184100           ORDER BY A.IDLEGSEL                                            
184200                  , A.DAREGDAT                                            
184300                  , A.TIREGTID                                            
184400                  , A.IDBUNDLE                                            
184500     END-EXEC                                                             
184600                                                                          
184700     MOVE 000100  TO GOOD-SQLCODECODES                                    
184800                                                                          
184900     EXEC SQL                                                             
185000        OPEN T01TRAW-CRS-6-ASC                                            
185100     END-EXEC                                                             
185200                                                                          
185300     MOVE SQLCODE TO SQLCODE-WS                                           
185400     PERFORM DB2-STATUS-CHECK                                             
185500     .                                                                    
185600     EJECT                                                                
185700                                                                          
185800 DB2-FETCH-T01TRAW-CRS-6-ASC SECTION.                                     
185900     MOVE 000100  TO GOOD-SQLCODECODES                                    
186000                                                                          
186100     EXEC SQL                                                             
186200                                                                          
186300         FETCH T01TRAW-CRS-6-ASC                                          
186400                                                                          
186500         INTO :MAP-IDBUNDLE-LINE                                          
186600            , :MAP-DAREGDAT-LINE                                          
186700            , :MAP-TIREGTID-LINE                                          
186800            , :MAP-IDREF-LINE                                             
186900            , :MAP-DAREFDAT-LINE                                          
187000            , :MAP-IDREFRAD-LINE                                          
187100            , :MAP-IDSYSTEM-SEND-LINE                                     
187200     END-EXEC                                                             
187300                                                                          
187400     MOVE SQLCODE TO SQLCODE-WS                                           
187500     PERFORM DB2-STATUS-CHECK                                             
187600     .                                                                    
187700     EJECT                                                                
187800                                                                          
187900 DB2-CLOSE-T01TRAW-CRS-6-ASC SECTION.                                     
188000     EXEC SQL                                                             
188100        CLOSE T01TRAW-CRS-6-ASC                                           
188200     END-EXEC                                                             
188300     .                                                                    
188400     EJECT                                                                
188500                                                                          
188600* * * * * * * * * *   - CURSOR-DATUM-TID- * * * * * * * * * * *           
188700 DB2-COUNT-CRS-DATUM-TID SECTION.                                         
188800     EXEC SQL                                                             
188900                                                                          
189000           SELECT COUNT(*)                                                
189100                                                                          
189200           INTO  :WS-COUNTER-DATUM-TID                                    
189300                                                                          
189400           FROM   T01TRAW A                                               
189500           ,      T01TBUN B                                               
189600                                                                          
189700           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
189800           AND    A.IDLEGSEL = B.IDLEGSEL                                 
189900           AND    A.IDBUNDLE = B.IDBUNDLE                                 
190000           AND    A.DAREGDAT = B.DAREGDAT                                 
190100           AND    A.TIREGTID = B.TIREGTID                                 
190200           AND  ( B.DAREGDAT = :WS-DATUM                                  
190300           AND    B.TIREGTID > :WS-TID2                                   
190400           OR     B.DAREGDAT > :WS-DATUM )                                
190500           AND    B.FLFEL    = 'J'                                        
190600                                                                          
190700     END-EXEC                                                             
190800                                                                          
190900     MOVE 000100  TO GOOD-SQLCODECODES                                    
191000                                                                          
191100     MOVE SQLCODE TO SQLCODE-WS                                           
191200     PERFORM DB2-STATUS-CHECK                                             
191300     .                                                                    
191400     EJECT                                                                
191500                                                                          
191600 DB2-DCL-OPN-T01TRAW-CRS-7 SECTION.                                       
191700     MOVE 000100 TO GOOD-SQLCODECODES                                     
191800                                                                          
191900     EXEC SQL                                                             
192000         DECLARE T01TRAW-CRS-7 CURSOR WITH HOLD FOR                       
192100                                                                          
192200           SELECT  A.IDBUNDLE                                             
192300                 , A.DAREGDAT                                             
192400                 , A.TIREGTID                                             
192500                 , A.IDREF                                                
192600                 , A.DAREFDAT                                             
192700                 , A.IDREFRAD                                             
192800                 , A.IDSYSTEM_SEND                                        
192900                                                                          
193000           FROM    T01TRAW A                                              
193100           ,       T01TBUN B                                              
193200                                                                          
193300           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
193400           AND    A.IDLEGSEL = B.IDLEGSEL                                 
193500           AND    A.IDBUNDLE = B.IDBUNDLE                                 
193600           AND    A.DAREGDAT = B.DAREGDAT                                 
193700           AND    A.TIREGTID = B.TIREGTID                                 
193800           AND  ( B.DAREGDAT = :WS-DATUM                                  
193900           AND    B.TIREGTID > :WS-TID2                                   
194000           OR     B.DAREGDAT > :WS-DATUM )                                
194100           AND    B.FLFEL    = 'J'                                        
194200                                                                          
194300           ORDER BY A.IDLEGSEL                                            
194400                  , A.DAREGDAT DESC                                       
194500                  , A.TIREGTID DESC                                       
194600                  , A.IDBUNDLE                                            
194700     END-EXEC                                                             
194800                                                                          
194900     MOVE 000100  TO GOOD-SQLCODECODES                                    
195000                                                                          
195100     EXEC SQL                                                             
195200        OPEN T01TRAW-CRS-7                                                
195300     END-EXEC                                                             
195400                                                                          
195500     MOVE SQLCODE TO SQLCODE-WS                                           
195600     PERFORM DB2-STATUS-CHECK                                             
195700     .                                                                    
195800     EJECT                                                                
195900                                                                          
196000 DB2-FETCH-T01TRAW-CRS-7 SECTION.                                         
196100     MOVE 000100  TO GOOD-SQLCODECODES                                    
196200                                                                          
196300     EXEC SQL                                                             
196400                                                                          
196500         FETCH T01TRAW-CRS-7                                              
196600                                                                          
196700         INTO :MAP-IDBUNDLE-LINE                                          
196800            , :MAP-DAREGDAT-LINE                                          
196900            , :MAP-TIREGTID-LINE                                          
197000            , :MAP-IDREF-LINE                                             
197100            , :MAP-DAREFDAT-LINE                                          
197200            , :MAP-IDREFRAD-LINE                                          
197300            , :MAP-IDSYSTEM-SEND-LINE                                     
197400     END-EXEC                                                             
197500                                                                          
197600     MOVE SQLCODE TO SQLCODE-WS                                           
197700     PERFORM DB2-STATUS-CHECK                                             
197800     .                                                                    
197900     EJECT                                                                
198000                                                                          
198100 DB2-CLOSE-T01TRAW-CRS-7 SECTION.                                         
198200     EXEC SQL                                                             
198300        CLOSE T01TRAW-CRS-7                                               
198400     END-EXEC                                                             
198500     .                                                                    
198600     EJECT                                                                
198700                                                                          
198800 DB2-DCL-OPN-T01TRAW-CRS-7-ASC SECTION.                                   
198900     MOVE 000100 TO GOOD-SQLCODECODES                                     
199000                                                                          
199100     EXEC SQL                                                             
199200         DECLARE T01TRAW-CRS-7-ASC CURSOR WITH HOLD FOR                   
199300                                                                          
199400           SELECT  A.IDBUNDLE                                             
199500                 , A.DAREGDAT                                             
199600                 , A.TIREGTID                                             
199700                 , A.IDREF                                                
199800                 , A.DAREFDAT                                             
199900                 , A.IDREFRAD                                             
200000                 , A.IDSYSTEM_SEND                                        
200100                                                                          
200200           FROM    T01TRAW A                                              
200300           ,       T01TBUN B                                              
200400                                                                          
200500           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
200600           AND    A.IDLEGSEL = B.IDLEGSEL                                 
200700           AND    A.IDBUNDLE = B.IDBUNDLE                                 
200800           AND    A.DAREGDAT = B.DAREGDAT                                 
200900           AND    A.TIREGTID = B.TIREGTID                                 
201000           AND  ( B.DAREGDAT = :WS-DATUM                                  
201100           AND    B.TIREGTID > :WS-TID2                                   
201200           OR     B.DAREGDAT > :WS-DATUM )                                
201300           AND    B.FLFEL    = 'J'                                        
201400                                                                          
201500           ORDER BY A.IDLEGSEL                                            
201600                  , A.DAREGDAT                                            
201700                  , A.TIREGTID                                            
201800                  , A.IDBUNDLE                                            
201900     END-EXEC                                                             
202000                                                                          
202100     MOVE 000100  TO GOOD-SQLCODECODES                                    
202200                                                                          
202300     EXEC SQL                                                             
202400        OPEN T01TRAW-CRS-7-ASC                                            
202500     END-EXEC                                                             
202600                                                                          
202700     MOVE SQLCODE TO SQLCODE-WS                                           
202800     PERFORM DB2-STATUS-CHECK                                             
202900     .                                                                    
203000     EJECT                                                                
203100                                                                          
203200 DB2-FETCH-T01TRAW-CRS-7-ASC SECTION.                                     
203300     MOVE 000100  TO GOOD-SQLCODECODES                                    
203400                                                                          
203500     EXEC SQL                                                             
203600                                                                          
203700         FETCH T01TRAW-CRS-7-ASC                                          
203800                                                                          
203900         INTO :MAP-IDBUNDLE-LINE                                          
204000            , :MAP-DAREGDAT-LINE                                          
204100            , :MAP-TIREGTID-LINE                                          
204200            , :MAP-IDREF-LINE                                             
204300            , :MAP-DAREFDAT-LINE                                          
204400            , :MAP-IDREFRAD-LINE                                          
204500            , :MAP-IDSYSTEM-SEND-LINE                                     
204600     END-EXEC                                                             
204700                                                                          
204800     MOVE SQLCODE TO SQLCODE-WS                                           
204900     PERFORM DB2-STATUS-CHECK                                             
205000     .                                                                    
205100     EJECT                                                                
205200                                                                          
205300 DB2-CLOSE-T01TRAW-CRS-7-ASC SECTION.                                     
205400     EXEC SQL                                                             
205500        CLOSE T01TRAW-CRS-7-ASC                                           
205600     END-EXEC                                                             
205700     .                                                                    
205800     EJECT                                                                
205900                                                                          
206000* * * * * * * * * *   - CURSOR-TID- * * * * * * * * * * * * * *           
206100 DB2-COUNT-CRS-TID SECTION.                                               
206200     EXEC SQL                                                             
206300                                                                          
206400           SELECT COUNT(*)                                                
206500                                                                          
206600           INTO  :WS-COUNTER-TID                                          
206700                                                                          
206800           FROM   T01TRAW A                                               
206900           ,      T01TBUN B                                               
207000                                                                          
207100           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
207200           AND    A.IDLEGSEL = B.IDLEGSEL                                 
207300           AND    A.IDBUNDLE = B.IDBUNDLE                                 
207400           AND    A.DAREGDAT = B.DAREGDAT                                 
207500           AND    A.TIREGTID = B.TIREGTID                                 
207600           AND  ( B.DAREGDAT = :WS-DATUM                                  
207700           AND    B.TIREGTID > :WS-TID2                                   
207800           OR     B.DAREGDAT > :WS-DATUM )                                
207900           AND    B.FLFEL    = 'J'                                        
208000                                                                          
208100     END-EXEC                                                             
208200                                                                          
208300     MOVE 000100  TO GOOD-SQLCODECODES                                    
208400                                                                          
208500     MOVE SQLCODE TO SQLCODE-WS                                           
208600     PERFORM DB2-STATUS-CHECK                                             
208700     .                                                                    
208800     EJECT                                                                
208900                                                                          
209000 DB2-DCL-OPN-T01TRAW-CRS-8 SECTION.                                       
209100     MOVE 000100 TO GOOD-SQLCODECODES                                     
209200                                                                          
209300     EXEC SQL                                                             
209400         DECLARE T01TRAW-CRS-8 CURSOR WITH HOLD FOR                       
209500                                                                          
209600           SELECT  A.IDBUNDLE                                             
209700                 , A.DAREGDAT                                             
209800                 , A.TIREGTID                                             
209900                 , A.IDREF                                                
210000                 , A.DAREFDAT                                             
210100                 , A.IDREFRAD                                             
210200                 , A.IDSYSTEM_SEND                                        
210300                                                                          
210400           FROM    T01TRAW A                                              
210500           ,       T01TBUN B                                              
210600                                                                          
210700           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
210800           AND    A.IDLEGSEL = B.IDLEGSEL                                 
210900           AND    A.IDBUNDLE = B.IDBUNDLE                                 
211000           AND    A.DAREGDAT = B.DAREGDAT                                 
211100           AND    A.TIREGTID = B.TIREGTID                                 
211200           AND  ( B.DAREGDAT = :WS-DATUM                                  
211300           AND    B.TIREGTID > :WS-TID2                                   
211400           OR     B.DAREGDAT > :WS-DATUM )                                
211500           AND    B.FLFEL    = 'J'                                        
211600                                                                          
211700           ORDER BY A.IDLEGSEL                                            
211800                  , A.DAREGDAT DESC                                       
211900                  , A.TIREGTID DESC                                       
212000                  , A.IDBUNDLE                                            
212100     END-EXEC                                                             
212200                                                                          
212300     MOVE 000100  TO GOOD-SQLCODECODES                                    
212400                                                                          
212500     EXEC SQL                                                             
212600        OPEN T01TRAW-CRS-8                                                
212700     END-EXEC                                                             
212800                                                                          
212900     MOVE SQLCODE TO SQLCODE-WS                                           
213000     PERFORM DB2-STATUS-CHECK                                             
213100     .                                                                    
213200     EJECT                                                                
213300                                                                          
213400 DB2-FETCH-T01TRAW-CRS-8 SECTION.                                         
213500     MOVE 000100  TO GOOD-SQLCODECODES                                    
213600                                                                          
213700     EXEC SQL                                                             
213800                                                                          
213900         FETCH T01TRAW-CRS-8                                              
214000                                                                          
214100         INTO :MAP-IDBUNDLE-LINE                                          
214200            , :MAP-DAREGDAT-LINE                                          
214300            , :MAP-TIREGTID-LINE                                          
214400            , :MAP-IDREF-LINE                                             
214500            , :MAP-DAREFDAT-LINE                                          
214600            , :MAP-IDREFRAD-LINE                                          
214700            , :MAP-IDSYSTEM-SEND-LINE                                     
214800     END-EXEC                                                             
214900                                                                          
215000     MOVE SQLCODE TO SQLCODE-WS                                           
215100     PERFORM DB2-STATUS-CHECK                                             
215200     .                                                                    
215300     EJECT                                                                
215400                                                                          
215500 DB2-CLOSE-T01TRAW-CRS-8 SECTION.                                         
215600     EXEC SQL                                                             
215700        CLOSE T01TRAW-CRS-8                                               
215800     END-EXEC                                                             
215900     .                                                                    
216000     EJECT                                                                
216100                                                                          
216200 DB2-DCL-OPN-T01TRAW-CRS-8-ASC SECTION.                                   
216300     MOVE 000100 TO GOOD-SQLCODECODES                                     
216400                                                                          
216500     EXEC SQL                                                             
216600         DECLARE T01TRAW-CRS-8-ASC CURSOR WITH HOLD FOR                   
216700                                                                          
216800           SELECT  A.IDBUNDLE                                             
216900                 , A.DAREGDAT                                             
217000                 , A.TIREGTID                                             
217100                 , A.IDREF                                                
217200                 , A.DAREFDAT                                             
217300                 , A.IDREFRAD                                             
217400                 , A.IDSYSTEM_SEND                                        
217500                                                                          
217600           FROM    T01TRAW A                                              
217700           ,       T01TBUN B                                              
217800                                                                          
217900           WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
218000           AND    A.IDLEGSEL = B.IDLEGSEL                                 
218100           AND    A.IDBUNDLE = B.IDBUNDLE                                 
218200           AND    A.DAREGDAT = B.DAREGDAT                                 
218300           AND    A.TIREGTID = B.TIREGTID                                 
218400           AND  ( B.DAREGDAT = :WS-DATUM                                  
218500           AND    B.TIREGTID > :WS-TID2                                   
218600           OR     B.DAREGDAT > :WS-DATUM )                                
218700           AND    B.FLFEL    = 'J'                                        
218800                                                                          
218900           ORDER BY A.IDLEGSEL                                            
219000                  , A.DAREGDAT                                            
219100                  , A.TIREGTID                                            
219200                  , A.IDBUNDLE                                            
219300     END-EXEC                                                             
219400                                                                          
219500     MOVE 000100  TO GOOD-SQLCODECODES                                    
219600                                                                          
219700     EXEC SQL                                                             
219800        OPEN T01TRAW-CRS-8-ASC                                            
219900     END-EXEC                                                             
220000                                                                          
220100     MOVE SQLCODE TO SQLCODE-WS                                           
220200     PERFORM DB2-STATUS-CHECK                                             
220300     .                                                                    
220400     EJECT                                                                
220500                                                                          
220600 DB2-FETCH-T01TRAW-CRS-8-ASC SECTION.                                     
220700     MOVE 000100  TO GOOD-SQLCODECODES                                    
220800                                                                          
220900     EXEC SQL                                                             
221000                                                                          
221100         FETCH T01TRAW-CRS-8-ASC                                          
221200                                                                          
221300         INTO :MAP-IDBUNDLE-LINE                                          
221400            , :MAP-DAREGDAT-LINE                                          
221500            , :MAP-TIREGTID-LINE                                          
221600            , :MAP-IDREF-LINE                                             
221700            , :MAP-DAREFDAT-LINE                                          
221800            , :MAP-IDREFRAD-LINE                                          
221900            , :MAP-IDSYSTEM-SEND-LINE                                     
222000     END-EXEC                                                             
222100                                                                          
222200     MOVE SQLCODE TO SQLCODE-WS                                           
222300     PERFORM DB2-STATUS-CHECK                                             
222400     .                                                                    
222500     EJECT                                                                
222600                                                                          
222700 DB2-CLOSE-T01TRAW-CRS-8-ASC SECTION.                                     
222800     EXEC SQL                                                             
222900        CLOSE T01TRAW-CRS-8-ASC                                           
223000     END-EXEC                                                             
223100     .                                                                    
223200     EJECT                                                                
223300                                                                          
223400 DB2-STATUS-CHECK  SECTION.                                               
223500     SET SQLCODE-IX TO 1                                                  
223600     SEARCH GOOD-SQLCODE                                                  
223700       AT END                                                             
223800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
223900          DELIMITED BY SIZE INTO ERROR-TEXT                               
224000          CALL ABEND USING RKOD-ABEND-DB2                                 
224100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
224200          CONTINUE                                                        
224300     END-SEARCH                                                           
224400     .                                                                    
