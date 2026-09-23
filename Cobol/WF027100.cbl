000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF027100.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/03/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.INTRULELOCATE                                    
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE T01LSEL                                
001500*        THE PROGRAM READS   TABLE T01COCO                                
001600*        THE PROGRAM READS   TABLE T01INRE                                
001700*        THE PROGRAM READS   TABLE T01SECO                                
001800*        THE PROGRAM READS   TABLE T01RECO                                
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: WF0271T                                             
002200*                                                                         
002300*        REQUEST:     WZ01REQU                                            
002400*                     WF0271I1                                            
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        RESPONSE:    WZ01RESP                                            
002800*                     WF0271O1                                            
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 DATA DIVISION.                                                           
003300                                                                          
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)  VALUE 'WF027100'.             
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100*    --- CONSTANTS                                                        
004200 77  YES                         PIC X      VALUE 'Y'.                    
004300 77  NOO                         PIC X      VALUE 'N'.                    
004400                                                                          
004500 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
004600 77  WS-CURRENT                  PIC S9(3)  VALUE +001    COMP-3.         
004700 77  WS-COMING                   PIC S9(3)  VALUE +002    COMP-3.         
004800 77  WS-ACTIVE                   PIC X(8)   VALUE '00000000'.             
004900 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
005000 77  WS-NOT-REG                  PIC X(8)   VALUE 'NOT REG.'.             
005100 77  WS-ADRESS                   PIC X(50)                                
005200                         VALUE 'CARPARTS.BILLIT.INTRULELOCATE'.           
005300                                                                          
005400 77  KEYS-SW                     PIC X      VALUE SPACE.                  
005500     88  KEYS-OK                            VALUE 'Y'.                    
005600     88  KEYS-WRONG                         VALUE 'N'.                    
005700                                                                          
005800*    --- WORK FIELDS                                                      
005900 01  WS-IDLANDX3-KEY             PIC X(3).                                
006000 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
006100 01  WS-XI                       PIC S9(9)  VALUE ZERO    BINARY.         
006200 01  WS-KEY1-MARKED              PIC X(1)   VALUE SPACE.                  
006300 01  WS-EXISTING                 PIC X(1)   VALUE SPACE.                  
006400 01  WS-COUNTER-FAA              PIC S9(7)  VALUE ZERO    COMP-3.         
006500 01  WS-COUNTER-FAB              PIC S9(7)  VALUE ZERO    COMP-3.         
006600 01  WS-COUNTER-FAC              PIC S9(7)  VALUE ZERO    COMP-3.         
006610 01  WS-COUNTER-FAD              PIC S9(7)  VALUE ZERO    COMP-3.         
006700 01  WS-COUNTER-4                PIC S9(7)  VALUE ZERO    COMP-3.         
006800 01  WS-KDSTATUS                 PIC S9(3)   VALUE ZERO COMP-3.           
006900                                                                          
007000*    --- MAPPING FIELDS                                                   
007100 01  MAP-KDSTATUS-LINE           PIC S9(3)  VALUE ZERO    COMP-3.         
007200 01  MAP-KDSTATUS-LINE-2         PIC S9(3)  VALUE ZERO    COMP-3.         
007300 01  MAP-IDLANDX3-SEND-LINE      PIC X(3)   VALUE SPACE.                  
007400 01  MAP-IDLANDX3-SEND-LINE-2    PIC X(3)   VALUE SPACE.                  
007500 01  MAP-BELAND-SEND-LINE        PIC X(35)  VALUE SPACE.                  
007600 01  MAP-BELAND-SEND-LINE-2      PIC X(35)  VALUE SPACE.                  
007700 01  MAP-IDLANDX3-REC-LINE       PIC X(3)   VALUE SPACE.                  
007800 01  MAP-IDLANDX3-REC-LINE-2     PIC X(3)   VALUE SPACE.                  
007900 01  MAP-BELAND-REC-LINE         PIC X(35)  VALUE SPACE.                  
008000 01  MAP-BELAND-REC-LINE-2       PIC X(35)  VALUE SPACE.                  
008100 01  MAP-DAREGDAT-LINE           PIC X(8)   VALUE SPACE.                  
008200 01  MAP-DAREGDAT-LINE-2         PIC X(8)   VALUE SPACE.                  
008300 01  MAP-DAUPPDAT-LINE           PIC X(8)   VALUE SPACE.                  
008400 01  MAP-DAUPPDAT-LINE-2         PIC X(8)   VALUE SPACE.                  
008500 01  MAP-IDUSER-LINE             PIC X(8)   VALUE SPACE.                  
008600 01  MAP-IDUSER-LINE-2           PIC X(8)   VALUE SPACE.                  
008700                                                                          
008800 01  COCO-IDLANDX3               PIC X(3)   VALUE SPACE.                  
008900 01  COCO-BELAND                 PIC X(35)  VALUE SPACE.                  
009000                                                                          
009100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009200 01  GENERAL-SUBPROGRAMS.                                                 
009300     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
009400     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
009500                                                                          
009600*    --- PARAMETERS TO ABEND                                              
009700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
010100                                                                          
010200 01  MESSAGE-CODES.                                                       
010300     03  ERROR-CODES.                                                     
010400         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
010500         05  ERR-INVALID-FIELD       PIC X(3)   VALUE '023'.              
010600         05  ERR-MUST-BE-NUMERIC     PIC X(3)   VALUE '024'.              
010700         05  NOT-FOUND               PIC X(3)   VALUE '025'.              
010800         05  MUST-BE-ENTERED         PIC X(3)   VALUE '026'.              
010900         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
011000         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
011100         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
011110     03  INFO-CODES.                                                      
011120         05  INF-MORE-LINES-EXIST    PIC X(3)   VALUE '104'.              
011200                                                                          
011300*01  -COPY WZ01SUB                                                        
011400     EJECT                                                                
011500*                                                                         
011600 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
011700 01  REQU-AREA.                                                           
011800*    03 -COPY WZ01REQU                                                    
011900*    03 -COPY WF0271I1                                                    
012000     EJECT                                                                
012100                                                                          
012200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
012300 01  RESP-AREA.                                                           
012400*    03 -COPY WZ01RESP                                                    
012500*    03 -COPY WF0271O1                                                    
012600     EJECT                                                                
012700                                                                          
012800 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
012900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013000                                                                          
013100 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
013200 01  DB2-WS.                                                              
013300     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
013400         88  CURSOR-OK                      VALUE 000.                    
013500         88  LINES-FOUND                    VALUE 000.                    
013600         88  LINES-MISSING                  VALUE 100.                    
013700         88  RESOURCE-WRONG                 VALUE 904.                    
013800     03  GOOD-SQLCODECODES.                                               
013900         05  GOOD-SQLCODE OCCURS 5                                        
014000             INDEXED BY SQLCODE-IX PIC 9(3).                              
014100                                                                          
014200 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
014300                                                                          
014400*01  -COPY T01LSEL -PRE T01LSEL-                                          
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'T01COCO-AREA'.        
014700                                                                          
014800*01  -COPY T01COCO -PRE T01COCO-                                          
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'T01INRE-AREA'.        
015100                                                                          
015200*01  -COPY T01INRE -PRE T01INRE-                                          
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'T01SECO-AREA'.        
015500                                                                          
015600*01  -COPY T01SECO -PRE T01SECO-                                          
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)   VALUE 'T01RECO-AREA'.        
015900                                                                          
016000*01  -COPY T01RECO -PRE T01RECO-                                          
016100     EJECT                                                                
016200       EXEC SQL INCLUDE T01LSEL END-EXEC.                                 
016300     EJECT                                                                
016400       EXEC SQL INCLUDE T01COCO END-EXEC.                                 
016500     EJECT                                                                
016600       EXEC SQL INCLUDE T01INRE END-EXEC.                                 
016700     EJECT                                                                
016800       EXEC SQL INCLUDE T01SECO END-EXEC.                                 
016900     EJECT                                                                
017000       EXEC SQL INCLUDE T01RECO END-EXEC.                                 
017100     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300                                                                          
017400 PROCEDURE DIVISION.                                                      
017500 MAIN SECTION.                                                            
017600                                                                          
017700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017800     IF SUB-KDRC = ZERO                                                   
017900       PERFORM A-INIT                                                     
018000       PERFORM B-CHECK-KEYS                                               
018100       IF KEYS-OK                                                         
018200         PERFORM BA-CHECK-KEY-RELATION                                    
018300       END-IF                                                             
018400       IF KEYS-OK                                                         
018500         PERFORM F-READ-SHOW-INFO                                         
018600       END-IF                                                             
018700       PERFORM S02-RETURN-RESPONSE                                        
018800     END-IF                                                               
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300                                                                          
019400 A-INIT SECTION.                                                          
019500     INITIALIZE GOOD-SQLCODECODES                                         
019600     MOVE ALL '+' TO RESP-AREA                                            
019700     MOVE SPACE TO RESP-IDMSG-ERROR                                       
019800     MOVE SPACE TO RESP-IDMSG-INFO                                        
019900     MOVE SPACE TO RESP-IDELMT-ERROR                                      
020000     MOVE ZERO TO RESP-KVRADER                                            
020100     .                                                                    
020200                                                                          
020300*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
020400 B-CHECK-KEYS SECTION.                                                    
020500     MOVE YES TO KEYS-SW                                                  
020600                                                                          
020700     IF REQU-IDMSGVER NUMERIC                                             
020800       IF REQU-IDLEGSEL-KEY > SPACE                                       
020900       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
021000       AND REQU-KDPGMACT = WS-SEARCH                                      
021100         CONTINUE                                                         
021200       ELSE                                                               
021300         MOVE NOO TO KEYS-SW                                              
021400       END-IF                                                             
021500     ELSE                                                                 
021600       MOVE NOO TO KEYS-SW                                                
021700     END-IF                                                               
021800                                                                          
021900     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
022000       MOVE NOO TO KEYS-SW                                                
022100     END-IF                                                               
022200                                                                          
022300     IF KEYS-WRONG                                                        
022400       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
022500       IF REQU-IDMSGVER NUMERIC                                           
022600         CONTINUE                                                         
022700       ELSE                                                               
022800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
022900         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
023000       END-IF                                                             
023100       IF REQU-KDPGMACT = WS-SEARCH                                       
023200         CONTINUE                                                         
023300       ELSE                                                               
023400         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
023500         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
023600       END-IF                                                             
023700       IF REQU-IDUSER = SPACE OR = ALL '+'                                
023800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
023900         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
024000       END-IF                                                             
024100     END-IF                                                               
024200     IF KEYS-OK                                                           
024300       PERFORM DB2-SELECT-T01LSEL-TAB                                     
024400       IF LINES-FOUND                                                     
024500         CONTINUE                                                         
024600       ELSE                                                               
024700         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
024800         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
024900         MOVE NOO TO KEYS-SW                                              
025000       END-IF                                                             
025100     END-IF                                                               
025200     .                                                                    
025300                                                                          
025400*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
025500 BA-CHECK-KEY-RELATION SECTION.                                           
025600     IF REQU-IDLANDX3-SEND-KEY = SPACE OR = ALL '+'                       
025700       IF REQU-IDLANDX3-REC-KEY = SPACE OR = ALL '+'                      
025800*        MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                         
025900*        MOVE 'IDLANDX3'      TO RESP-IDELMT-ERROR                        
026000*        MOVE NOO TO KEYS-SW                                              
026010         CONTINUE                                                         
026100       ELSE                                                               
026200         PERFORM DB2-SELECT-T01RECO-TAB                                   
026300         IF LINES-FOUND                                                   
026400           CONTINUE                                                       
026500         ELSE                                                             
026600           MOVE NOT-FOUND      TO RESP-IDMSG-ERROR                        
026700           MOVE 'IDLANDX3-REC' TO RESP-IDELMT-ERROR                       
026800           MOVE NOO TO KEYS-SW                                            
026900         END-IF                                                           
027000       END-IF                                                             
027100     ELSE                                                                 
027200       IF REQU-IDLANDX3-REC-KEY = SPACE OR = ALL '+'                      
027300         PERFORM DB2-SELECT-T01SECO-TAB                                   
027400         IF LINES-FOUND                                                   
027500           CONTINUE                                                       
027600         ELSE                                                             
027700           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
027800           MOVE 'IDLANDX3-SEND' TO RESP-IDELMT-ERROR                      
027900           MOVE NOO TO KEYS-SW                                            
028000         END-IF                                                           
028100       ELSE                                                               
028200         PERFORM DB2-SELECT-T01SECO-TAB                                   
028300         IF LINES-FOUND                                                   
028400           PERFORM DB2-SELECT-T01RECO-TAB                                 
028500           IF LINES-FOUND                                                 
028600             PERFORM DB2-SELECT-T01INRE-TAB                               
028700             IF LINES-FOUND                                               
028800               CONTINUE                                                   
028900             ELSE                                                         
029000               MOVE NOT-FOUND      TO RESP-IDMSG-ERROR                    
029100               MOVE 'INTERSTATE'   TO RESP-IDELMT-ERROR                   
029200               MOVE NOO TO KEYS-SW                                        
029300             END-IF                                                       
029400           ELSE                                                           
029500             MOVE NOT-FOUND      TO RESP-IDMSG-ERROR                      
029600             MOVE 'IDLANDX3-REC' TO RESP-IDELMT-ERROR                     
029700             MOVE NOO TO KEYS-SW                                          
029800           END-IF                                                         
029900         ELSE                                                             
030000           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
030100           MOVE 'IDLANDX3-SEND' TO RESP-IDELMT-ERROR                      
030200           MOVE NOO TO KEYS-SW                                            
030300         END-IF                                                           
030400       END-IF                                                             
030500     END-IF                                                               
030600     .                                                                    
030700                                                                          
030800*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
030900 F-READ-SHOW-INFO SECTION.                                                
031000     MOVE REQU-IDLEGSEL-KEY      TO RESP-IDLEGSEL-KEY                     
031100     MOVE REQU-IDLANDX3-SEND-KEY TO RESP-IDLANDX3-SEND-KEY                
031200     MOVE REQU-IDLANDX3-REC-KEY  TO RESP-IDLANDX3-REC-KEY                 
031300     MOVE REQU-IDMSGVER          TO RESP-IDMSGVER                         
031400     MOVE T01LSEL-BELEGRAD-1     TO RESP-BELEGRAD-1                       
031500                                                                          
031600     PERFORM FA-READ-BASICDATA                                            
031700     .                                                                    
031800                                                                          
031900*** - CHECK WHICH REQUESTED KEY                                           
032000 FA-READ-BASICDATA SECTION.                                               
032100     MOVE ZERO TO WS-COUNTER-FAA                                          
032200     MOVE ZERO TO WS-COUNTER-FAB                                          
032300     MOVE ZERO TO WS-COUNTER-FAC                                          
032310     MOVE ZERO TO WS-COUNTER-FAD                                          
032400     MOVE 'N'  TO WS-KEY1-MARKED                                          
032500                                                                          
032600     IF  REQU-IDLANDX3-SEND-KEY > SPACE                                   
032700     AND REQU-IDLANDX3-SEND-KEY NOT = ALL '+'                             
032800     AND REQU-IDLANDX3-REC-KEY > SPACE                                    
032900     AND REQU-IDLANDX3-REC-KEY NOT = ALL '+'                              
033000       PERFORM FAA-SEARCH-SEND-REC-COUNTRY                                
033100     ELSE                                                                 
033200       IF  REQU-IDLANDX3-SEND-KEY > SPACE                                 
033300       AND REQU-IDLANDX3-SEND-KEY NOT = ALL '+'                           
033400         PERFORM FAB-SEARCH-SEND-COUNTRY                                  
033500       ELSE                                                               
033510         IF  REQU-IDLANDX3-REC-KEY  > SPACE                               
033520         AND REQU-IDLANDX3-REC-KEY  NOT = ALL '+'                         
033600           PERFORM FAC-SEARCH-REC-COUNTRY                                 
033610         ELSE                                                             
033620           PERFORM FAD-SEARCH-SEND-REC-BLANK                              
033700         END-IF                                                           
033710       END-IF                                                             
033800     END-IF                                                               
033900                                                                          
034000     MOVE WS-IX TO RESP-KVRADER                                           
034100     .                                                                    
034200                                                                          
034300*** - HANDLE BOTH SENDING AND RECEIVING COUNTRY                           
034400 FAA-SEARCH-SEND-REC-COUNTRY SECTION.                                     
034500     PERFORM DB2-COUNT-CRS-1                                              
034600                                                                          
034700     IF WS-COUNTER-FAA = ZERO                                             
034800       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
034900     ELSE                                                                 
035000       IF WS-COUNTER-FAA > WS-MAX-LINES                                   
035100         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
035200       END-IF                                                             
035300     END-IF                                                               
035400                                                                          
035500     IF RESP-IDMSG-ERROR = SPACE                                          
035600       PERFORM DB2-COUNT-CRS-4                                            
035700       IF WS-COUNTER-4 = ZERO                                             
035800         MOVE ZERO TO WS-IX                                               
035900       ELSE                                                               
036000         MOVE ZERO TO WS-XI                                               
036100         PERFORM DB2-DCL-OPN-T01INRE-CRS-1                                
036200         PERFORM DB2-FETCH-T01INRE-CRS-1                                  
036300         MOVE ZERO TO WS-IX                                               
036400         PERFORM UNTIL LINES-MISSING                                      
036500           ADD 1 TO WS-XI                                                 
036600           PERFORM S03-MOVE-TO-RESPOND                                    
036700           PERFORM DB2-FETCH-T01INRE-CRS-1                                
036800         END-PERFORM                                                      
036900         PERFORM DB2-CLOSE-T01INRE-CRS-1                                  
037000       END-IF                                                             
037100                                                                          
037200       IF WS-XI = ZERO                                                    
037300         PERFORM S03-MOVE-TO-RESPOND                                      
037400       END-IF                                                             
037500     END-IF                                                               
037600     .                                                                    
037700                                                                          
037800*** - HANDLE REQUESTED SENDING COUNTRY                                    
037900 FAB-SEARCH-SEND-COUNTRY SECTION.                                         
038000     PERFORM DB2-COUNT-CRS-2                                              
038100                                                                          
038200     IF WS-COUNTER-FAB = ZERO                                             
038300       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
038400     ELSE                                                                 
038500       IF WS-COUNTER-FAB > WS-MAX-LINES                                   
038600         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
038700       END-IF                                                             
038800     END-IF                                                               
038900                                                                          
039000     IF RESP-IDMSG-ERROR = SPACE                                          
039100       PERFORM DB2-COUNT-CRS-4                                            
039200       IF WS-COUNTER-4 = ZERO                                             
039300         MOVE ZERO TO WS-IX                                               
039400       ELSE                                                               
039500         PERFORM DB2-DCL-OPN-T01INRE-CRS-2A                               
039600         PERFORM DB2-FETCH-T01INRE-CRS-2A                                 
039700         PERFORM UNTIL LINES-MISSING                                      
039800           PERFORM S03-MOVE-TO-RESPOND                                    
039900           PERFORM DB2-FETCH-T01INRE-CRS-2A                               
040000         END-PERFORM                                                      
040100         PERFORM DB2-CLOSE-T01INRE-CRS-2A                                 
040200       END-IF                                                             
040300                                                                          
040400       IF WS-IX = ZERO                                                    
040500         PERFORM DB2-DCL-OPN-T01RECO-CRS                                  
040600         PERFORM DB2-FETCH-T01RECO-CRS                                    
040700         PERFORM UNTIL LINES-MISSING                                      
040800           PERFORM S03-MOVE-TO-RESPOND                                    
040900           PERFORM DB2-FETCH-T01RECO-CRS                                  
041000         END-PERFORM                                                      
041100         PERFORM DB2-CLOSE-T01RECO-CRS                                    
041200       END-IF                                                             
041300     END-IF                                                               
041400     .                                                                    
041500                                                                          
041600*** - HANDLE REQUESTED RECEIVING COUNTRY                                  
041700 FAC-SEARCH-REC-COUNTRY SECTION.                                          
041800     PERFORM DB2-COUNT-CRS-3                                              
041900                                                                          
042000     IF WS-COUNTER-FAC = ZERO                                             
042100       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
042200     ELSE                                                                 
042300       IF WS-COUNTER-FAC > WS-MAX-LINES                                   
042400         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
042500       END-IF                                                             
042600     END-IF                                                               
042700                                                                          
042800     IF RESP-IDMSG-ERROR = SPACE                                          
042900       PERFORM DB2-DCL-OPN-T01INRE-CRS-3A                                 
043000       PERFORM DB2-FETCH-T01INRE-CRS-3A                                   
043100       PERFORM UNTIL LINES-MISSING                                        
043200         PERFORM S03-MOVE-TO-RESPOND                                      
043300         PERFORM DB2-FETCH-T01INRE-CRS-3A                                 
043400       END-PERFORM                                                        
043500       PERFORM DB2-CLOSE-T01INRE-CRS-3A                                   
043600                                                                          
043700       IF WS-IX = ZERO                                                    
043800         PERFORM DB2-DCL-OPN-T01SECO-CRS                                  
043900         PERFORM DB2-FETCH-T01SECO-CRS                                    
044000         PERFORM UNTIL LINES-MISSING                                      
044100           PERFORM S03-MOVE-TO-RESPOND                                    
044200           PERFORM DB2-FETCH-T01SECO-CRS                                  
044300         END-PERFORM                                                      
044400         PERFORM DB2-CLOSE-T01SECO-CRS                                    
044500       END-IF                                                             
044600     END-IF                                                               
044700     .                                                                    
044800                                                                          
044810*** - HANDLE WHEN SENDING AND RECEIVING COUNTRY ARE SPACES                
044820 FAD-SEARCH-SEND-REC-BLANK SECTION.                                       
044830     PERFORM DB2-COUNT-CRS-5                                              
044840                                                                          
044850     IF WS-COUNTER-FAD = ZERO                                             
044860       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
044870     ELSE                                                                 
044880       IF WS-COUNTER-FAD > WS-MAX-LINES                                   
044890         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
044891         CONTINUE                                                         
044892       END-IF                                                             
044893     END-IF                                                               
044894                                                                          
044895     IF RESP-IDMSG-ERROR = SPACE                                          
044897       IF WS-COUNTER-FAD = ZERO                                           
044898         MOVE ZERO TO WS-IX                                               
044899       ELSE                                                               
044900         PERFORM DB2-DCL-OPN-T01INRE-CRS-5                                
044901         PERFORM DB2-FETCH-T01INRE-CRS-5                                  
044902         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
044903           PERFORM S03-MOVE-TO-RESPOND                                    
044904           PERFORM DB2-FETCH-T01INRE-CRS-5                                
044905         END-PERFORM                                                      
044906         IF NOT LINES-MISSING                                             
044907           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
044908         ELSE                                                             
044909           MOVE SPACE                 TO RESP-IDMSG-INFO                  
044910         END-IF                                                           
044911         PERFORM DB2-CLOSE-T01INRE-CRS-5                                  
044912       END-IF                                                             
044913                                                                          
044914       IF WS-IX = ZERO                                                    
044915         PERFORM DB2-DCL-OPN-T01RECO-CRS                                  
044916         PERFORM DB2-FETCH-T01RECO-CRS                                    
044917         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
044918           PERFORM S03-MOVE-TO-RESPOND                                    
044919           PERFORM DB2-FETCH-T01RECO-CRS                                  
044920         END-PERFORM                                                      
044921         IF NOT LINES-MISSING                                             
044922           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
044923         ELSE                                                             
044924           MOVE SPACE                 TO RESP-IDMSG-INFO                  
044925         END-IF                                                           
044926         PERFORM DB2-CLOSE-T01RECO-CRS                                    
044927       END-IF                                                             
044928     END-IF                                                               
044929     .                                                                    
044930                                                                          
044940*   --- DISPATCHER SECTION START                                          
045000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
045100     MOVE 'GETARG'             TO SUB-KDFUNC                              
045200     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
045300     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
045400                                                                          
045500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
045600                                                                          
045700     IF SUB-KDRC > 0                                                      
045800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
045900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
046000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
046100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
046200     END-IF                                                               
046300     .                                                                    
046400                                                                          
046500 S02-RETURN-RESPONSE SECTION.                                             
046600     MOVE 'RETURN'             TO SUB-KDFUNC                              
046700                                                                          
046800     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
046900                              - ((WS-MAX-LINES - WS-IX)                   
047000                              * LENGTH OF RESP-TABELLRAD)                 
047100                                                                          
047200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
047300                                                                          
047400     IF SUB-KDRC > 0                                                      
047500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
047600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
047700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
047800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
047900     END-IF                                                               
048000     .                                                                    
048100                                                                          
048200*** - CHECK IF RELATION BETWEEN SENDING/RECEIVING COUNTRY                 
048300***   (TABLE T01INRE) WHEN CURRENT  LINE.                                 
048400*** - MOVE TO RESPOND AND COUNT NUMBER OF HITS WHEN CURRENT LINE.         
048500***   WHEN COMING LINE MODIFY CURRENT LINE.                               
048600 S03-MOVE-TO-RESPOND SECTION.                                             
048700     PERFORM S04-SELECT-SEARCH-RESPOND                                    
048800     IF WS-EXISTING = 'N'                                                 
048900       MOVE 100 TO SQLCODE-WS                                             
049000     ELSE                                                                 
049100       MOVE 000 TO SQLCODE-WS                                             
049200     END-IF                                                               
049300     IF LINES-FOUND                                                       
049400       PERFORM S05-SELECT-SEARCH-RESPOND                                  
049500       IF MAP-KDSTATUS-LINE = WS-CURRENT                                  
049600         IF WS-IX = ZERO                                                  
049700           ADD 1 TO WS-IX                                                 
049800           MOVE MAP-IDLANDX3-SEND-LINE TO                                 
049900                                  RESP-IDLANDX3-SEND-LINE(WS-IX)          
050000           MOVE MAP-BELAND-SEND-LINE TO                                   
050100                                  RESP-BELAND-SEND-LINE(WS-IX)            
050200           MOVE MAP-IDLANDX3-REC-LINE TO                                  
050300                                  RESP-IDLANDX3-REC-LINE(WS-IX)           
050400           MOVE MAP-BELAND-REC-LINE TO                                    
050500                                  RESP-BELAND-REC-LINE(WS-IX)             
050600           MOVE MAP-DAREGDAT-LINE TO RESP-DAREGDAT-LINE(WS-IX)            
050700           MOVE MAP-DAUPPDAT-LINE TO RESP-DAUPPDAT-LINE(WS-IX)            
050800           MOVE MAP-IDUSER-LINE TO RESP-IDUSER-LINE(WS-IX)                
050900           PERFORM DB2-CHECK-COMING                                       
051000           IF WS-KDSTATUS = WS-COMING                                     
051100             MOVE YES           TO RESP-FLCOMING-LINE(WS-IX)              
051200           ELSE                                                           
051300             MOVE NOO           TO RESP-FLCOMING-LINE(WS-IX)              
051400           END-IF                                                         
051500         ELSE                                                             
051600           IF MAP-IDLANDX3-SEND-LINE =                                    
051700                           RESP-IDLANDX3-SEND-LINE(WS-IX)                 
051800           AND MAP-IDLANDX3-REC-LINE =                                    
051900                           RESP-IDLANDX3-REC-LINE(WS-IX)                  
052000             CONTINUE                                                     
052100           ELSE                                                           
052200             ADD 1 TO WS-IX                                               
052300             MOVE MAP-IDLANDX3-SEND-LINE TO                               
052400                                    RESP-IDLANDX3-SEND-LINE(WS-IX)        
052500             MOVE MAP-BELAND-SEND-LINE TO                                 
052600                                    RESP-BELAND-SEND-LINE(WS-IX)          
052700             MOVE MAP-IDLANDX3-REC-LINE TO                                
052800                                    RESP-IDLANDX3-REC-LINE(WS-IX)         
052900             MOVE MAP-BELAND-REC-LINE TO                                  
053000                                    RESP-BELAND-REC-LINE(WS-IX)           
053100             MOVE MAP-DAREGDAT-LINE TO RESP-DAREGDAT-LINE(WS-IX)          
053200             MOVE MAP-DAUPPDAT-LINE TO RESP-DAUPPDAT-LINE(WS-IX)          
053300             MOVE MAP-IDUSER-LINE TO RESP-IDUSER-LINE(WS-IX)              
053400             PERFORM DB2-CHECK-COMING                                     
053500             IF WS-KDSTATUS = WS-COMING                                   
053600               MOVE YES           TO RESP-FLCOMING-LINE(WS-IX)            
053700             ELSE                                                         
053800               MOVE NOO           TO RESP-FLCOMING-LINE(WS-IX)            
053900             END-IF                                                       
054000           END-IF                                                         
054100         END-IF                                                           
054200       END-IF                                                             
054300     ELSE                                                                 
054400       PERFORM S05-SELECT-SEARCH-RESPOND                                  
054500       IF MAP-KDSTATUS-LINE-2 = WS-CURRENT                                
054600         MOVE SPACE TO MAP-DAREGDAT-LINE-2                                
054700                       MAP-DAUPPDAT-LINE-2                                
054800         MOVE WS-NOT-REG TO MAP-IDUSER-LINE-2                             
054900         IF WS-IX = ZERO                                                  
055000           ADD 1 TO WS-IX                                                 
055100           MOVE MAP-IDLANDX3-SEND-LINE-2 TO                               
055200                                  RESP-IDLANDX3-SEND-LINE(WS-IX)          
055300           MOVE MAP-BELAND-SEND-LINE-2 TO                                 
055400                                    RESP-BELAND-SEND-LINE(WS-IX)          
055500           MOVE MAP-IDLANDX3-REC-LINE-2 TO                                
055600                                   RESP-IDLANDX3-REC-LINE(WS-IX)          
055700           MOVE MAP-BELAND-REC-LINE-2 TO                                  
055800                                    RESP-BELAND-REC-LINE(WS-IX)           
055900           MOVE MAP-DAREGDAT-LINE-2 TO RESP-DAREGDAT-LINE(WS-IX)          
056000           MOVE MAP-DAUPPDAT-LINE-2 TO RESP-DAUPPDAT-LINE(WS-IX)          
056100           MOVE MAP-IDUSER-LINE-2 TO RESP-IDUSER-LINE(WS-IX)              
056200           MOVE NOO             TO RESP-FLCOMING-LINE(WS-IX)              
056300         ELSE                                                             
056400           IF MAP-IDLANDX3-SEND-LINE =                                    
056500                              RESP-IDLANDX3-SEND-LINE(WS-IX)              
056600           AND MAP-IDLANDX3-REC-LINE =                                    
056700                              RESP-IDLANDX3-REC-LINE(WS-IX)               
056800             CONTINUE                                                     
056900           ELSE                                                           
057000             ADD 1 TO WS-IX                                               
057100             MOVE MAP-IDLANDX3-SEND-LINE-2 TO                             
057200                                    RESP-IDLANDX3-SEND-LINE(WS-IX)        
057300             MOVE MAP-BELAND-SEND-LINE-2 TO                               
057400                                      RESP-BELAND-SEND-LINE(WS-IX)        
057500             MOVE MAP-IDLANDX3-REC-LINE-2 TO                              
057600                                     RESP-IDLANDX3-REC-LINE(WS-IX)        
057700             MOVE MAP-BELAND-REC-LINE-2 TO                                
057800                                      RESP-BELAND-REC-LINE(WS-IX)         
057900             MOVE MAP-DAREGDAT-LINE-2 TO RESP-DAREGDAT-LINE(WS-IX)        
058000             MOVE MAP-DAUPPDAT-LINE-2 TO RESP-DAUPPDAT-LINE(WS-IX)        
058100             MOVE MAP-IDUSER-LINE-2 TO RESP-IDUSER-LINE(WS-IX)            
058200             MOVE NOO             TO RESP-FLCOMING-LINE(WS-IX)            
058300           END-IF                                                         
058400         END-IF                                                           
058500       END-IF                                                             
058600     END-IF                                                               
058700     .                                                                    
058800                                                                          
058900 S04-SELECT-SEARCH-RESPOND SECTION.                                       
059000     IF WS-COUNTER-FAA > ZERO                                             
059100       IF WS-XI > ZERO                                                    
059200         PERFORM DB2-SELECT-T01INRE-ALL                                   
059300       ELSE                                                               
059400         MOVE 100 TO SQLCODE-WS                                           
059500       END-IF                                                             
059600     END-IF                                                               
059700                                                                          
059800     IF WS-COUNTER-FAB > ZERO                                             
059900       PERFORM DB2-SELECT-T01INRE-SECO                                    
060000       IF LINES-FOUND                                                     
060100         MOVE 'Y' TO WS-EXISTING                                          
060200       ELSE                                                               
060300         MOVE 'N' TO WS-EXISTING                                          
060400       END-IF                                                             
060500       MOVE MAP-IDLANDX3-SEND-LINE TO MAP-IDLANDX3-SEND-LINE-2            
060600       MOVE MAP-IDLANDX3-REC-LINE-2 TO MAP-IDLANDX3-REC-LINE              
060700       MOVE MAP-KDSTATUS-LINE-2     TO MAP-KDSTATUS-LINE                  
060800       MOVE MAP-DAREGDAT-LINE-2     TO MAP-DAREGDAT-LINE                  
060900       MOVE MAP-DAUPPDAT-LINE-2     TO MAP-DAUPPDAT-LINE                  
061000       MOVE MAP-IDUSER-LINE-2       TO MAP-IDUSER-LINE                    
061100                                                                          
061200       MOVE MAP-IDLANDX3-SEND-LINE TO WS-IDLANDX3-KEY                     
061300       PERFORM DB2-SELECT-T01COCO-TAB                                     
061400       MOVE COCO-BELAND             TO MAP-BELAND-SEND-LINE               
061500       MOVE MAP-BELAND-SEND-LINE    TO MAP-BELAND-SEND-LINE-2             
061600                                                                          
061700       MOVE MAP-IDLANDX3-REC-LINE-2 TO WS-IDLANDX3-KEY                    
061800       PERFORM DB2-SELECT-T01COCO-TAB                                     
061900       MOVE COCO-BELAND             TO MAP-BELAND-REC-LINE-2              
062000       MOVE MAP-BELAND-REC-LINE-2 TO MAP-BELAND-REC-LINE                  
062100     END-IF                                                               
062200                                                                          
062300     IF WS-COUNTER-FAC > ZERO                                             
062400       PERFORM DB2-SELECT-T01INRE-RECO                                    
062500       IF LINES-FOUND                                                     
062600         MOVE 'Y' TO WS-EXISTING                                          
062700       ELSE                                                               
062800         MOVE 'N' TO WS-EXISTING                                          
062900       END-IF                                                             
063000       MOVE MAP-IDLANDX3-SEND-LINE-2 TO MAP-IDLANDX3-SEND-LINE            
063100       MOVE MAP-IDLANDX3-REC-LINE    TO MAP-IDLANDX3-REC-LINE-2           
063200       MOVE MAP-KDSTATUS-LINE-2     TO MAP-KDSTATUS-LINE                  
063300       MOVE MAP-DAREGDAT-LINE-2     TO MAP-DAREGDAT-LINE                  
063400       MOVE MAP-DAUPPDAT-LINE-2     TO MAP-DAUPPDAT-LINE                  
063500       MOVE MAP-IDUSER-LINE-2       TO MAP-IDUSER-LINE                    
063600                                                                          
063700       MOVE MAP-IDLANDX3-SEND-LINE-2 TO WS-IDLANDX3-KEY                   
063800       PERFORM DB2-SELECT-T01COCO-TAB                                     
063900       MOVE COCO-BELAND             TO MAP-BELAND-SEND-LINE-2             
064000       MOVE MAP-BELAND-SEND-LINE-2 TO MAP-BELAND-SEND-LINE                
064100                                                                          
064200       MOVE MAP-IDLANDX3-REC-LINE TO WS-IDLANDX3-KEY                      
064300       PERFORM DB2-SELECT-T01COCO-TAB                                     
064400       MOVE COCO-BELAND             TO MAP-BELAND-REC-LINE                
064500       MOVE MAP-BELAND-REC-LINE     TO MAP-BELAND-REC-LINE-2              
064600     END-IF                                                               
064601                                                                          
064610     IF WS-COUNTER-FAD > ZERO                                             
064620       PERFORM DB2-SELECT-T01INRE-SECO                                    
064630       IF LINES-FOUND                                                     
064640         MOVE 'Y' TO WS-EXISTING                                          
064650       ELSE                                                               
064660         MOVE 'N' TO WS-EXISTING                                          
064670       END-IF                                                             
064680       MOVE MAP-IDLANDX3-SEND-LINE TO MAP-IDLANDX3-SEND-LINE-2            
064690       MOVE MAP-IDLANDX3-REC-LINE-2 TO MAP-IDLANDX3-REC-LINE              
064691       MOVE MAP-KDSTATUS-LINE-2     TO MAP-KDSTATUS-LINE                  
064692       MOVE MAP-DAREGDAT-LINE-2     TO MAP-DAREGDAT-LINE                  
064693       MOVE MAP-DAUPPDAT-LINE-2     TO MAP-DAUPPDAT-LINE                  
064694       MOVE MAP-IDUSER-LINE-2       TO MAP-IDUSER-LINE                    
064695                                                                          
064696       MOVE MAP-IDLANDX3-SEND-LINE TO WS-IDLANDX3-KEY                     
064697       PERFORM DB2-SELECT-T01COCO-TAB                                     
064698       MOVE COCO-BELAND             TO MAP-BELAND-SEND-LINE               
064699       MOVE MAP-BELAND-SEND-LINE    TO MAP-BELAND-SEND-LINE-2             
064700                                                                          
064701       MOVE MAP-IDLANDX3-REC-LINE-2 TO WS-IDLANDX3-KEY                    
064702       PERFORM DB2-SELECT-T01COCO-TAB                                     
064703       MOVE COCO-BELAND             TO MAP-BELAND-REC-LINE-2              
064704       MOVE MAP-BELAND-REC-LINE-2 TO MAP-BELAND-REC-LINE                  
064705     END-IF                                                               
064706                                                                          
064710     .                                                                    
064800                                                                          
064900 S05-SELECT-SEARCH-RESPOND SECTION.                                       
065000     IF WS-COUNTER-FAA > ZERO                                             
065100       IF WS-IX = ZERO                                                    
065200         MOVE WS-CURRENT TO MAP-KDSTATUS-LINE-2                           
065300       END-IF                                                             
065400       PERFORM DB2-SELECT-T01RECO-TAB                                     
065500       MOVE MAP-IDLANDX3-REC-LINE     TO MAP-IDLANDX3-REC-LINE-2          
065600       PERFORM DB2-SELECT-T01SECO-TAB                                     
065700       MOVE MAP-IDLANDX3-SEND-LINE     TO MAP-IDLANDX3-SEND-LINE-2        
065800                                                                          
065900       MOVE MAP-IDLANDX3-SEND-LINE  TO WS-IDLANDX3-KEY                    
066000       PERFORM DB2-SELECT-T01COCO-TAB                                     
066100       MOVE COCO-BELAND             TO MAP-BELAND-SEND-LINE               
066200       MOVE MAP-BELAND-SEND-LINE    TO MAP-BELAND-SEND-LINE-2             
066300                                                                          
066400       MOVE MAP-IDLANDX3-REC-LINE   TO WS-IDLANDX3-KEY                    
066500       PERFORM DB2-SELECT-T01COCO-TAB                                     
066600       MOVE COCO-BELAND             TO MAP-BELAND-REC-LINE                
066700       MOVE MAP-BELAND-REC-LINE     TO MAP-BELAND-REC-LINE-2              
066800     END-IF                                                               
066900                                                                          
067000     IF WS-COUNTER-FAB > ZERO                                             
067100       PERFORM DB2-SELECT-T01RECO-FAB                                     
067200       MOVE MAP-IDLANDX3-REC-LINE     TO MAP-IDLANDX3-REC-LINE-2          
067300       MOVE MAP-BELAND-REC-LINE       TO MAP-BELAND-REC-LINE-2            
067400     END-IF                                                               
067500                                                                          
067600     IF WS-COUNTER-FAC > ZERO                                             
067700       PERFORM DB2-SELECT-T01SECO-FAC                                     
067800       MOVE MAP-IDLANDX3-SEND-LINE     TO MAP-IDLANDX3-SEND-LINE-2        
067900       MOVE MAP-BELAND-SEND-LINE       TO MAP-BELAND-SEND-LINE-2          
068000     END-IF                                                               
068001                                                                          
068010     IF WS-COUNTER-FAD > ZERO                                             
068020       PERFORM DB2-SELECT-T01RECO-FAB                                     
068030       MOVE MAP-IDLANDX3-REC-LINE     TO MAP-IDLANDX3-REC-LINE-2          
068040       MOVE MAP-BELAND-REC-LINE       TO MAP-BELAND-REC-LINE-2            
068050     END-IF                                                               
068060     .                                                                    
068200                                                                          
068300*   --- DB2 SECTIONS                                                      
068400 DB2-SELECT-T01LSEL-TAB SECTION.                                          
068500     MOVE 000100 TO GOOD-SQLCODECODES                                     
068600                                                                          
068700     EXEC SQL                                                             
068800           SELECT BELEGRAD_1                                              
068900                                                                          
069000           INTO  :T01LSEL-BELEGRAD-1                                      
069100                                                                          
069200           FROM   T01LSEL                                                 
069300                                                                          
069400           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
069500           AND    KDSTATUS = :WS-CURRENT                                  
069600     END-EXEC                                                             
069700                                                                          
069800     MOVE SQLCODE TO SQLCODE-WS                                           
069900     PERFORM DB2-STATUS-CHECK                                             
070000     .                                                                    
070100                                                                          
070200 DB2-SELECT-T01COCO-TAB SECTION.                                          
070300     MOVE 000100 TO GOOD-SQLCODECODES                                     
070400                                                                          
070500     EXEC SQL                                                             
070600           SELECT IDLANDX3                                                
070700               ,  BELAND                                                  
070800                                                                          
070900           INTO  :COCO-IDLANDX3                                           
071000               , :COCO-BELAND                                             
071100                                                                          
071200           FROM   T01COCO                                                 
071300                                                                          
071400           WHERE  IDLANDX3 = :WS-IDLANDX3-KEY                             
071500     END-EXEC                                                             
071600                                                                          
071700     MOVE SQLCODE TO SQLCODE-WS                                           
071800     PERFORM DB2-STATUS-CHECK                                             
071900     .                                                                    
072000                                                                          
072100 DB2-SELECT-T01SECO-TAB SECTION.                                          
072200     MOVE 000100 TO GOOD-SQLCODECODES                                     
072300                                                                          
072400     EXEC SQL                                                             
072500           SELECT IDLANDX3                                                
072600                , KDSTATUS                                                
072700                                                                          
072800           INTO  :MAP-IDLANDX3-SEND-LINE                                  
072900               , :MAP-KDSTATUS-LINE-2                                     
073000                                                                          
073100           FROM   T01SECO                                                 
073200                                                                          
073300           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
073400           AND    IDLANDX3 = :REQU-IDLANDX3-SEND-KEY                      
073500           AND    KDSTATUS = :WS-CURRENT                                  
073600           AND    DADELDAT = :WS-ACTIVE                                   
073700     END-EXEC                                                             
073800                                                                          
073900     MOVE SQLCODE TO SQLCODE-WS                                           
074000     PERFORM DB2-STATUS-CHECK                                             
074100     .                                                                    
074200                                                                          
074300 DB2-SELECT-T01RECO-TAB SECTION.                                          
074400     MOVE 000100 TO GOOD-SQLCODECODES                                     
074500                                                                          
074600     EXEC SQL                                                             
074700           SELECT IDLANDX3                                                
074800                , KDSTATUS                                                
074900                                                                          
075000           INTO  :MAP-IDLANDX3-REC-LINE                                   
075100               , :MAP-KDSTATUS-LINE-2                                     
075200                                                                          
075300           FROM   T01RECO                                                 
075400                                                                          
075500           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
075600           AND    IDLANDX3 = :REQU-IDLANDX3-REC-KEY                       
075700           AND    KDSTATUS = :WS-CURRENT                                  
075800           AND    DADELDAT = :WS-ACTIVE                                   
075900     END-EXEC                                                             
076000                                                                          
076100     MOVE SQLCODE TO SQLCODE-WS                                           
076200     PERFORM DB2-STATUS-CHECK                                             
076300     .                                                                    
076400                                                                          
076500 DB2-SELECT-T01INRE-TAB SECTION.                                          
076600     MOVE 000100 TO GOOD-SQLCODECODES                                     
076700                                                                          
076800     EXEC SQL                                                             
076900           SELECT IDLANDX3_SEND                                           
077000                                                                          
077100           INTO  :T01INRE-IDLANDX3-SEND                                   
077200                                                                          
077300           FROM   T01INRE                                                 
077400                                                                          
077500           WHERE  IDLEGSEL      = :REQU-IDLEGSEL-KEY                      
077600           AND    IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                 
077700           AND    IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                  
077800           AND    KDSTATUS      = :WS-CURRENT                             
077900           AND    DADELDAT      = :WS-ACTIVE                              
078000     END-EXEC                                                             
078100                                                                          
078200     MOVE SQLCODE TO SQLCODE-WS                                           
078300     PERFORM DB2-STATUS-CHECK                                             
078400     .                                                                    
078500                                                                          
078600 DB2-SELECT-T01SECO-FAC SECTION.                                          
078700     MOVE 000100 TO GOOD-SQLCODECODES                                     
078800                                                                          
078900     EXEC SQL                                                             
079000           SELECT IDLANDX3                                                
079100                                                                          
079200           INTO  :MAP-IDLANDX3-SEND-LINE                                  
079300                                                                          
079400           FROM   T01SECO                                                 
079500                                                                          
079600           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
079700           AND    IDLANDX3 = :MAP-IDLANDX3-SEND-LINE                      
079800           AND    KDSTATUS = :WS-CURRENT                                  
079900           AND    DADELDAT = :WS-ACTIVE                                   
080000     END-EXEC                                                             
080100                                                                          
080200     MOVE SQLCODE TO SQLCODE-WS                                           
080300     PERFORM DB2-STATUS-CHECK                                             
080400     .                                                                    
080500                                                                          
080600 DB2-SELECT-T01RECO-FAB SECTION.                                          
080700     MOVE 000100 TO GOOD-SQLCODECODES                                     
080800                                                                          
080900     EXEC SQL                                                             
081000           SELECT IDLANDX3                                                
081100                                                                          
081200           INTO  :MAP-IDLANDX3-REC-LINE                                   
081300                                                                          
081400           FROM   T01RECO                                                 
081500                                                                          
081600           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
081700           AND    IDLANDX3 = :MAP-IDLANDX3-REC-LINE                       
081800           AND    KDSTATUS = :WS-CURRENT                                  
081900           AND    DADELDAT = :WS-ACTIVE                                   
082000     END-EXEC                                                             
082100                                                                          
082200     MOVE SQLCODE TO SQLCODE-WS                                           
082300     PERFORM DB2-STATUS-CHECK                                             
082400     .                                                                    
082500                                                                          
082600 DB2-SELECT-T01INRE-ALL SECTION.                                          
082700     MOVE 000100 TO GOOD-SQLCODECODES                                     
082800                                                                          
082900     EXEC SQL                                                             
083000           SELECT IDLANDX3_SEND                                           
083100                                                                          
083200           INTO  :T01INRE-IDLANDX3-SEND                                   
083300                                                                          
083400           FROM   T01INRE                                                 
083500                                                                          
083600           WHERE  IDLEGSEL      = :REQU-IDLEGSEL-KEY                      
083700           AND    IDLANDX3_SEND = :MAP-IDLANDX3-SEND-LINE                 
083800           AND    IDLANDX3_REC  = :MAP-IDLANDX3-REC-LINE                  
083900           AND    KDSTATUS      = :WS-CURRENT                             
084000           AND    DADELDAT      = :WS-ACTIVE                              
084100     END-EXEC                                                             
084200                                                                          
084300     MOVE SQLCODE TO SQLCODE-WS                                           
084400     PERFORM DB2-STATUS-CHECK                                             
084500     .                                                                    
084600                                                                          
084700 DB2-SELECT-T01INRE-RECO SECTION.                                         
084800     MOVE 000100 TO GOOD-SQLCODECODES                                     
084900                                                                          
085000     EXEC SQL                                                             
085100           SELECT IDUSER                                                  
085200                , IDUSER                                                  
085300                , DAREGDAT                                                
085400                , DAREGDAT                                                
085500                , DAUPPDAT                                                
085600                , DAUPPDAT                                                
085700                                                                          
085800           INTO  :MAP-IDUSER-LINE                                         
085900               , :MAP-IDUSER-LINE-2                                       
086000               , :MAP-DAREGDAT-LINE                                       
086100               , :MAP-DAREGDAT-LINE-2                                     
086200               , :MAP-DAUPPDAT-LINE                                       
086300               , :MAP-DAUPPDAT-LINE-2                                     
086400                                                                          
086500           FROM   T01INRE                                                 
086600                                                                          
086700           WHERE  IDLEGSEL      = :REQU-IDLEGSEL-KEY                      
086800           AND    IDLANDX3_REC  = :MAP-IDLANDX3-REC-LINE                  
086900           AND    IDLANDX3_SEND = :MAP-IDLANDX3-SEND-LINE-2               
087000           AND    KDSTATUS      = :WS-CURRENT                             
087100           AND    DADELDAT      = :WS-ACTIVE                              
087200     END-EXEC                                                             
087300                                                                          
087400     MOVE SQLCODE TO SQLCODE-WS                                           
087500     PERFORM DB2-STATUS-CHECK                                             
087600     .                                                                    
087700                                                                          
087800 DB2-SELECT-T01INRE-SECO SECTION.                                         
087900     MOVE 000100 TO GOOD-SQLCODECODES                                     
088000                                                                          
088100     EXEC SQL                                                             
088200           SELECT IDUSER                                                  
088300                , IDUSER                                                  
088400                , DAREGDAT                                                
088500                , DAREGDAT                                                
088600                , DAUPPDAT                                                
088700                , DAUPPDAT                                                
088800                                                                          
088900           INTO  :MAP-IDUSER-LINE                                         
089000               , :MAP-IDUSER-LINE-2                                       
089100               , :MAP-DAREGDAT-LINE                                       
089200               , :MAP-DAREGDAT-LINE-2                                     
089300               , :MAP-DAUPPDAT-LINE                                       
089400               , :MAP-DAUPPDAT-LINE-2                                     
089500                                                                          
089600           FROM   T01INRE                                                 
089700                                                                          
089800           WHERE  IDLEGSEL      = :REQU-IDLEGSEL-KEY                      
089900           AND    IDLANDX3_SEND = :MAP-IDLANDX3-SEND-LINE                 
090000           AND    IDLANDX3_REC  = :MAP-IDLANDX3-REC-LINE-2                
090100           AND    KDSTATUS      = :WS-CURRENT                             
090200           AND    DADELDAT      = :WS-ACTIVE                              
090300     END-EXEC                                                             
090400                                                                          
090500     MOVE SQLCODE TO SQLCODE-WS                                           
090600     PERFORM DB2-STATUS-CHECK                                             
090700     .                                                                    
090800                                                                          
090900* * * * * * * * * *   - CURSOR-1 -   * * * * * * * * * * * * * * *        
091000 DB2-COUNT-CRS-1 SECTION.                                                 
091100     EXEC SQL                                                             
091200           SELECT COUNT(*)                                                
091300                                                                          
091400           INTO  :WS-COUNTER-FAA                                          
091500                                                                          
091600           FROM   T01SECO                                                 
091700                                                                          
091800           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
091900           AND     KDSTATUS = :WS-CURRENT                                 
092000           AND     DADELDAT = :WS-ACTIVE                                  
092100           AND     IDLANDX3 = :REQU-IDLANDX3-SEND-KEY                     
092200     END-EXEC                                                             
092300                                                                          
092400     MOVE 000100  TO GOOD-SQLCODECODES                                    
092500                                                                          
092600     MOVE SQLCODE TO SQLCODE-WS                                           
092700     PERFORM DB2-STATUS-CHECK                                             
092800     .                                                                    
092900                                                                          
093000 DB2-DCL-OPN-T01INRE-CRS-1 SECTION.                                       
093100     MOVE 000100 TO GOOD-SQLCODECODES                                     
093200                                                                          
093300     EXEC SQL                                                             
093400         DECLARE T01INRE-CRS-1 CURSOR WITH HOLD FOR                       
093500                                                                          
093600           SELECT  KDSTATUS                                               
093700                 , IDLANDX3_SEND                                          
093800                 , IDLANDX3_REC                                           
093900                 , DAREGDAT                                               
094000                 , DAUPPDAT                                               
094100                 , IDUSER                                                 
094200                                                                          
094300           FROM    T01INRE                                                
094400                                                                          
094500           WHERE   IDLEGSEL      = :REQU-IDLEGSEL-KEY                     
094600           AND     IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                
094700           AND     IDLANDX3_REC  = :REQU-IDLANDX3-REC-KEY                 
094800           AND     KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING            
094900           AND     DADELDAT      = :WS-ACTIVE                             
095000                                                                          
095100           ORDER BY IDLEGSEL                                              
095200                  , IDLANDX3_SEND                                         
095300                  , IDLANDX3_REC                                          
095400     END-EXEC                                                             
095500                                                                          
095600     MOVE 000100  TO GOOD-SQLCODECODES                                    
095700                                                                          
095800     EXEC SQL                                                             
095900       OPEN T01INRE-CRS-1                                                 
096000     END-EXEC                                                             
096100                                                                          
096200     MOVE SQLCODE TO SQLCODE-WS                                           
096300     PERFORM DB2-STATUS-CHECK                                             
096400     .                                                                    
096500                                                                          
096600 DB2-FETCH-T01INRE-CRS-1 SECTION.                                         
096700     MOVE 000100  TO GOOD-SQLCODECODES                                    
096800                                                                          
096900     EXEC SQL                                                             
097000         FETCH T01INRE-CRS-1                                              
097100                                                                          
097200         INTO :MAP-KDSTATUS-LINE                                          
097300            , :MAP-IDLANDX3-SEND-LINE                                     
097400            , :MAP-IDLANDX3-REC-LINE                                      
097500            , :MAP-DAREGDAT-LINE                                          
097600            , :MAP-DAUPPDAT-LINE                                          
097700            , :MAP-IDUSER-LINE                                            
097800     END-EXEC                                                             
097900                                                                          
098000     MOVE SQLCODE TO SQLCODE-WS                                           
098100     PERFORM DB2-STATUS-CHECK                                             
098200     .                                                                    
098300                                                                          
098400 DB2-CLOSE-T01INRE-CRS-1 SECTION.                                         
098500     EXEC SQL                                                             
098600       CLOSE T01INRE-CRS-1                                                
098700     END-EXEC                                                             
098800     .                                                                    
098900                                                                          
099000* * * * * * * * * *   - CURSOR-2 -   * * * * * * * * * * * * * * *        
099100 DB2-COUNT-CRS-2 SECTION.                                                 
099200     MOVE 000100  TO GOOD-SQLCODECODES                                    
099300                                                                          
099400     EXEC SQL                                                             
099500          SELECT COUNT(*)                                                 
099600                                                                          
099700          INTO  :WS-COUNTER-FAB                                           
099800                                                                          
099900          FROM   T01SECO                                                  
100000                                                                          
100100          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
100200          AND    IDLANDX3 = :REQU-IDLANDX3-SEND-KEY                       
100300          AND    KDSTATUS = :WS-CURRENT                                   
100400          AND    DADELDAT = :WS-ACTIVE                                    
100500     END-EXEC                                                             
100600                                                                          
100700     MOVE SQLCODE TO SQLCODE-WS                                           
100800     PERFORM DB2-STATUS-CHECK                                             
100900     .                                                                    
101000                                                                          
101100 DB2-DCL-OPN-T01INRE-CRS-2A SECTION.                                      
101200     MOVE 000100 TO GOOD-SQLCODECODES                                     
101300                                                                          
101400     EXEC SQL                                                             
101500         DECLARE T01INRE-CRS-2A CURSOR WITH HOLD FOR                      
101600                                                                          
101700          SELECT  DISTINCT A.KDSTATUS                                     
101800                , A.IDLANDX3_SEND                                         
101900                , B.IDLANDX3                                              
102000                , A.DAREGDAT                                              
102100                , A.DAUPPDAT                                              
102200                , A.IDUSER                                                
102300                                                                          
102400          FROM   T01INRE A                                                
102500               , T01RECO B                                                
102600                                                                          
102700          WHERE  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
102800          AND    A.IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                
102900          AND    A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING            
103000          AND    B.KDSTATUS = :WS-CURRENT                                 
103100          AND    B.DADELDAT = :WS-ACTIVE                                  
103200          AND    A.DADELDAT = :WS-ACTIVE                                  
103300          AND (  A.IDLANDX3_REC <> B.IDLANDX3                             
103400          OR     A.IDLANDX3_REC =  B.IDLANDX3 )                           
103500                                                                          
103600     END-EXEC                                                             
103700                                                                          
103800     MOVE 000100  TO GOOD-SQLCODECODES                                    
103900                                                                          
104000     EXEC SQL                                                             
104100       OPEN T01INRE-CRS-2A                                                
104200     END-EXEC                                                             
104300                                                                          
104400     MOVE SQLCODE TO SQLCODE-WS                                           
104500     PERFORM DB2-STATUS-CHECK                                             
104600     .                                                                    
104700                                                                          
104800 DB2-FETCH-T01INRE-CRS-2A SECTION.                                        
104900     MOVE 000100  TO GOOD-SQLCODECODES                                    
105000                                                                          
105100     EXEC SQL                                                             
105200         FETCH T01INRE-CRS-2A                                             
105300                                                                          
105400         INTO :MAP-KDSTATUS-LINE-2                                        
105500            , :MAP-IDLANDX3-SEND-LINE                                     
105600            , :MAP-IDLANDX3-REC-LINE-2                                    
105700            , :MAP-DAREGDAT-LINE-2                                        
105800            , :MAP-DAUPPDAT-LINE-2                                        
105900            , :MAP-IDUSER-LINE-2                                          
106000     END-EXEC                                                             
106100                                                                          
106200     MOVE SQLCODE TO SQLCODE-WS                                           
106300     PERFORM DB2-STATUS-CHECK                                             
106400     .                                                                    
106500                                                                          
106600 DB2-CLOSE-T01INRE-CRS-2A SECTION.                                        
106700     EXEC SQL                                                             
106800       CLOSE T01INRE-CRS-2A                                               
106900     END-EXEC                                                             
107000     .                                                                    
107100                                                                          
107200* * * * * * * * * *   - CURSOR-4 -   * * * * * * * * * * * * * * *        
107300 DB2-COUNT-CRS-4 SECTION.                                                 
107400     EXEC SQL                                                             
107500           SELECT COUNT(*)                                                
107600                                                                          
107700           INTO  :WS-COUNTER-4                                            
107800                                                                          
107900           FROM   T01INRE                                                 
108000                                                                          
108100           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
108200           AND     KDSTATUS = :WS-CURRENT                                 
108300           AND     DADELDAT = :WS-ACTIVE                                  
108400           AND     IDLANDX3_SEND = :REQU-IDLANDX3-SEND-KEY                
108500     END-EXEC                                                             
108600                                                                          
108700     MOVE 000100  TO GOOD-SQLCODECODES                                    
108800                                                                          
108900     MOVE SQLCODE TO SQLCODE-WS                                           
109000     PERFORM DB2-STATUS-CHECK                                             
109100     .                                                                    
109200                                                                          
109300* * * * * * * * * *   - CURSOR-3 -   * * * * * * * * * * * * * * *        
109400 DB2-COUNT-CRS-3 SECTION.                                                 
109500     MOVE 000100  TO GOOD-SQLCODECODES                                    
109600                                                                          
109700     EXEC SQL                                                             
109800          SELECT COUNT(*)                                                 
109900                                                                          
110000          INTO  :WS-COUNTER-FAC                                           
110100                                                                          
110200          FROM   T01RECO                                                  
110300                                                                          
110400          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
110500          AND    IDLANDX3 = :REQU-IDLANDX3-REC-KEY                        
110600          AND    KDSTATUS = :WS-CURRENT                                   
110700          AND    DADELDAT = :WS-ACTIVE                                    
110800     END-EXEC                                                             
110900                                                                          
111000     MOVE SQLCODE TO SQLCODE-WS                                           
111100     PERFORM DB2-STATUS-CHECK                                             
111200     .                                                                    
111300                                                                          
111400 DB2-DCL-OPN-T01INRE-CRS-3A SECTION.                                      
111500     MOVE 000100 TO GOOD-SQLCODECODES                                     
111600                                                                          
111700     EXEC SQL                                                             
111800         DECLARE T01INRE-CRS-3A CURSOR WITH HOLD FOR                      
111900                                                                          
112000          SELECT  DISTINCT A.KDSTATUS                                     
112100                , A.IDLANDX3_REC                                          
112200                , B.IDLANDX3                                              
112300                , A.DAREGDAT                                              
112400                , A.DAUPPDAT                                              
112500                , A.IDUSER                                                
112600                                                                          
112700          FROM   T01INRE A                                                
112800               , T01SECO B                                                
112900                                                                          
113000          WHERE B.IDLEGSEL = :REQU-IDLEGSEL-KEY                           
113100          AND A.IDLANDX3_REC = :REQU-IDLANDX3-REC-KEY                     
113200          AND A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING               
113300          AND B.KDSTATUS = :WS-CURRENT                                    
113400          AND B.DADELDAT = :WS-ACTIVE                                     
113500          AND A.DADELDAT = :WS-ACTIVE                                     
113600          AND ( A.IDLANDX3_SEND <> B.IDLANDX3                             
113700          OR  A.IDLANDX3_SEND =  B.IDLANDX3 )                             
113800                                                                          
113900     END-EXEC                                                             
114000                                                                          
114100     MOVE 000100  TO GOOD-SQLCODECODES                                    
114200                                                                          
114300     EXEC SQL                                                             
114400       OPEN T01INRE-CRS-3A                                                
114500     END-EXEC                                                             
114600                                                                          
114700     MOVE SQLCODE TO SQLCODE-WS                                           
114800     PERFORM DB2-STATUS-CHECK                                             
114900     .                                                                    
115000                                                                          
115100 DB2-FETCH-T01INRE-CRS-3A SECTION.                                        
115200     MOVE 000100  TO GOOD-SQLCODECODES                                    
115300                                                                          
115400     EXEC SQL                                                             
115500         FETCH T01INRE-CRS-3A                                             
115600                                                                          
115700         INTO :MAP-KDSTATUS-LINE-2                                        
115800            , :MAP-IDLANDX3-REC-LINE                                      
115900            , :MAP-IDLANDX3-SEND-LINE-2                                   
116000            , :MAP-DAREGDAT-LINE-2                                        
116100            , :MAP-DAUPPDAT-LINE-2                                        
116200            , :MAP-IDUSER-LINE-2                                          
116300     END-EXEC                                                             
116400                                                                          
116500     MOVE SQLCODE TO SQLCODE-WS                                           
116600     PERFORM DB2-STATUS-CHECK                                             
116700     .                                                                    
116800                                                                          
116900 DB2-CLOSE-T01INRE-CRS-3A SECTION.                                        
117000     EXEC SQL                                                             
117100       CLOSE T01INRE-CRS-3A                                               
117200     END-EXEC                                                             
117300     .                                                                    
117400                                                                          
117410* * * * * * * * * *   - CURSOR-5 -   * * * * * * * * * * * * * * *        
117420 DB2-COUNT-CRS-5 SECTION.                                                 
099400     EXEC SQL                                                             
099500          SELECT COUNT(*)                                                 
099600                                                                          
099700          INTO  :WS-COUNTER-FAD                                           
099800                                                                          
099900          FROM   T01SECO                                                  
100000                                                                          
100100          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
100300          AND    KDSTATUS = :WS-CURRENT                                   
100400          AND    DADELDAT = :WS-ACTIVE                                    
100500     END-EXEC                                                             
117450                                                                          
117504                                                                          
117505     MOVE 000100  TO GOOD-SQLCODECODES                                    
117506                                                                          
117507     MOVE SQLCODE TO SQLCODE-WS                                           
117508     PERFORM DB2-STATUS-CHECK                                             
117509     .                                                                    
117510                                                                          
117511 DB2-DCL-OPN-T01INRE-CRS-5 SECTION.                                       
117512     MOVE 000100 TO GOOD-SQLCODECODES                                     
117513                                                                          
117514     EXEC SQL                                                             
117515         DECLARE T01INRE-CRS-5 CURSOR WITH HOLD FOR                       
117516                                                                          
117517           SELECT  DISTINCT A.KDSTATUS                                    
117518                 , A.IDLANDX3_SEND                                        
117519                 , B.IDLANDX3                                             
117520                 , A.DAREGDAT                                             
117521                 , A.DAUPPDAT                                             
117522                 , A.IDUSER                                               
117523                                                                          
117524           FROM   T01INRE A                                               
117525                , T01SECO B                                               
117526                , T01RECO C                                               
117527                                                                          
117528           WHERE   A.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
117529           AND     B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
117530           AND     C.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
117531           AND     A.IDLANDX3_SEND = C.IDLANDX3                           
117532           AND     A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
117533           AND     B.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
117534           AND     C.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
117535           AND     A.DADELDAT = :WS-ACTIVE                                
117536           AND     B.DADELDAT = :WS-ACTIVE                                
117537           AND     C.DADELDAT = :WS-ACTIVE                                
117538           AND     (  A.IDLANDX3_REC <> B.IDLANDX3                        
117539           OR      A.IDLANDX3_REC =  B.IDLANDX3 )                         
117540                                                                          
117549     END-EXEC                                                             
117550                                                                          
117551     MOVE 000100  TO GOOD-SQLCODECODES                                    
117552                                                                          
117553     EXEC SQL                                                             
117554       OPEN T01INRE-CRS-5                                                 
117555     END-EXEC                                                             
117556                                                                          
117557     MOVE SQLCODE TO SQLCODE-WS                                           
117558     PERFORM DB2-STATUS-CHECK                                             
117559     .                                                                    
117560                                                                          
117561 DB2-FETCH-T01INRE-CRS-5 SECTION.                                         
117562     MOVE 000100  TO GOOD-SQLCODECODES                                    
117563                                                                          
117564     EXEC SQL                                                             
117565         FETCH T01INRE-CRS-5                                              
117566                                                                          
117567         INTO :MAP-KDSTATUS-LINE-2                                        
117568            , :MAP-IDLANDX3-SEND-LINE                                     
117569            , :MAP-IDLANDX3-REC-LINE-2                                    
117570            , :MAP-DAREGDAT-LINE-2                                        
117571            , :MAP-DAUPPDAT-LINE-2                                        
117572            , :MAP-IDUSER-LINE-2                                          
117573     END-EXEC                                                             
117574                                                                          
117575     MOVE SQLCODE TO SQLCODE-WS                                           
117576     PERFORM DB2-STATUS-CHECK                                             
117577     .                                                                    
117578                                                                          
117579 DB2-CLOSE-T01INRE-CRS-5 SECTION.                                         
117580     EXEC SQL                                                             
117581       CLOSE T01INRE-CRS-5                                                
117582     END-EXEC                                                             
117583     .                                                                    
117584                                                                          
117590 DB2-DCL-OPN-T01SECO-CRS SECTION.                                         
117600     MOVE 000100 TO GOOD-SQLCODECODES                                     
117700                                                                          
117800     EXEC SQL                                                             
117900         DECLARE T01SECO-CRS CURSOR WITH HOLD FOR                         
118000                                                                          
118100          SELECT  KDSTATUS                                                
118200                , IDLANDX3                                                
118300                , DAREGDAT                                                
118400                , DAUPPDAT                                                
118500                , IDUSER                                                  
118600                                                                          
118700          FROM   T01SECO                                                  
118800                                                                          
118900          WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                             
119000          AND KDSTATUS = :WS-CURRENT                                      
119100          AND DADELDAT = :WS-ACTIVE                                       
119200                                                                          
119300          ORDER BY IDLEGSEL                                               
119400                 , IDLANDX3                                               
119500     END-EXEC                                                             
119600                                                                          
119700     MOVE 000100  TO GOOD-SQLCODECODES                                    
119800                                                                          
119900     EXEC SQL                                                             
120000       OPEN T01SECO-CRS                                                   
120100     END-EXEC                                                             
120200                                                                          
120300     MOVE SQLCODE TO SQLCODE-WS                                           
120400     PERFORM DB2-STATUS-CHECK                                             
120500     .                                                                    
120600                                                                          
120700 DB2-FETCH-T01SECO-CRS SECTION.                                           
120800     MOVE 000100  TO GOOD-SQLCODECODES                                    
120900                                                                          
121000     EXEC SQL                                                             
121100         FETCH T01SECO-CRS                                                
121200                                                                          
121300         INTO :MAP-KDSTATUS-LINE-2                                        
121400            , :MAP-IDLANDX3-SEND-LINE-2                                   
121500            , :MAP-DAREGDAT-LINE-2                                        
121600            , :MAP-DAUPPDAT-LINE-2                                        
121700            , :MAP-IDUSER-LINE-2                                          
121800     END-EXEC                                                             
121900                                                                          
122000     MOVE SQLCODE TO SQLCODE-WS                                           
122100     PERFORM DB2-STATUS-CHECK                                             
122200     .                                                                    
122300                                                                          
122400 DB2-CLOSE-T01SECO-CRS SECTION.                                           
122500     EXEC SQL                                                             
122600       CLOSE T01SECO-CRS                                                  
122700     END-EXEC                                                             
122800     .                                                                    
122900                                                                          
123000 DB2-DCL-OPN-T01RECO-CRS SECTION.                                         
123100     MOVE 000100 TO GOOD-SQLCODECODES                                     
123200                                                                          
123300     EXEC SQL                                                             
123400         DECLARE T01RECO-CRS CURSOR WITH HOLD FOR                         
123500                                                                          
123600          SELECT  KDSTATUS                                                
123700                , IDLANDX3                                                
123800                , DAREGDAT                                                
123900                , DAUPPDAT                                                
124000                , IDUSER                                                  
124100                                                                          
124200          FROM   T01RECO                                                  
124300                                                                          
124400          WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                             
124500          AND KDSTATUS = :WS-CURRENT                                      
124600          AND DADELDAT = :WS-ACTIVE                                       
124700                                                                          
124800          ORDER BY IDLEGSEL                                               
124900                 , IDLANDX3                                               
125000     END-EXEC                                                             
125100                                                                          
125200     MOVE 000100  TO GOOD-SQLCODECODES                                    
125300                                                                          
125400     EXEC SQL                                                             
125500       OPEN T01RECO-CRS                                                   
125600     END-EXEC                                                             
125700                                                                          
125800     MOVE SQLCODE TO SQLCODE-WS                                           
125900     PERFORM DB2-STATUS-CHECK                                             
126000     .                                                                    
126100                                                                          
126200 DB2-FETCH-T01RECO-CRS SECTION.                                           
126300     MOVE 000100  TO GOOD-SQLCODECODES                                    
126400                                                                          
126500     EXEC SQL                                                             
126600         FETCH T01RECO-CRS                                                
126700                                                                          
126800         INTO :MAP-KDSTATUS-LINE-2                                        
126900            , :MAP-IDLANDX3-REC-LINE-2                                    
127000            , :MAP-DAREGDAT-LINE-2                                        
127100            , :MAP-DAUPPDAT-LINE-2                                        
127200            , :MAP-IDUSER-LINE-2                                          
127300     END-EXEC                                                             
127400                                                                          
127500     MOVE SQLCODE TO SQLCODE-WS                                           
127600     PERFORM DB2-STATUS-CHECK                                             
127700     .                                                                    
127800                                                                          
127900 DB2-CLOSE-T01RECO-CRS SECTION.                                           
128000     EXEC SQL                                                             
128100       CLOSE T01RECO-CRS                                                  
128200     END-EXEC                                                             
128300     .                                                                    
128400                                                                          
128500 DB2-CHECK-COMING SECTION.                                                
128600     MOVE 000100  TO GOOD-SQLCODECODES                                    
128700     EXEC SQL                                                             
128800           SELECT MAX(KDSTATUS)                                           
128900                                                                          
129000           INTO  :WS-KDSTATUS                                             
129100                                                                          
129200           FROM   T01INRE                                                 
129300                                                                          
129400           WHERE  IDLEGSEL      = :REQU-IDLEGSEL-KEY                      
129500           AND    IDLANDX3_SEND = :MAP-IDLANDX3-SEND-LINE                 
129600           AND    IDLANDX3_REC  = :MAP-IDLANDX3-REC-LINE                  
129700           AND    DADELDAT      = :WS-ACTIVE                              
129800     END-EXEC                                                             
129900     MOVE SQLCODE TO SQLCODE-WS                                           
130000     PERFORM DB2-STATUS-CHECK                                             
130100     .                                                                    
130200                                                                          
130300 DB2-STATUS-CHECK  SECTION.                                               
130400     SET SQLCODE-IX TO 1                                                  
130500     SEARCH GOOD-SQLCODE                                                  
130600       AT END                                                             
130700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
130800          DELIMITED BY SIZE INTO ERROR-TEXT                               
130900          CALL ABEND USING RKOD-ABEND-DB2                                 
131000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
131100          CONTINUE                                                        
131200     END-SEARCH                                                           
131300     .                                                                    
