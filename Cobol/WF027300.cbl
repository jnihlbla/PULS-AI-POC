000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF027300.                                                
000400 AUTHOR.         ANDERS HENRIKSSON.                                       
000500 DATE-WRITTEN.   02/03/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.BUSINRELATLOCATE                                 
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE T01LSEL                                
001500*        THE PROGRAM READS   TABLE T01CUGR                                
001600*        THE PROGRAM READS   TABLE T01BURE                                
001700*        THE PROGRAM READS   TABLE T01DOTY                                
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: WF0273T                                             
002100*        REQUEST:     WZ01REQU                                            
002200*                     WF0273I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    WZ01RESP                                            
002600*                     WF0273O1                                            
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)  VALUE 'WF027300'.             
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600                                                                          
003700*    --- CONSTANTS                                                        
003800 77  YES                         PIC X      VALUE 'Y'.                    
003900 77  NOO                         PIC X      VALUE 'N'.                    
004000                                                                          
004100 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
004200 77  WS-CURRENT                  PIC S9(3)  VALUE +001    COMP-3.         
004300 77  WS-COMING                   PIC S9(3)  VALUE +002    COMP-3.         
004400 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004500 77  WS-NOT-REG                  PIC X(8)   VALUE 'NOT REG.'.             
004600 77  WS-ADRESS                   PIC X(50)                                
004700                    VALUE 'CARPARTS.BILLIT.BUSINRELATLOCATE'.             
004800                                                                          
004900 77  KEYS-SW                     PIC X      VALUE SPACE.                  
005000     88  KEYS-OK                            VALUE 'Y'.                    
005100     88  KEYS-WRONG                         VALUE 'N'.                    
005200                                                                          
005300*    --- WORK FIELDS                                                      
005400 01  WS-IX-MOD                   PIC S9(4)  VALUE ZERO    BINARY.         
005500 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
005600 01  WS-IX2                      PIC S9(9)  VALUE ZERO    BINARY.         
005700 01  WS-COUNTER-T01BURE          PIC S9(5)  VALUE ZERO    COMP-3.         
005800 01  WS-COUNTER-FAB              PIC S9(5)  VALUE ZERO    COMP-3.         
005900 01  WS-COUNTER-FAC              PIC S9(5)  VALUE ZERO    COMP-3.         
006000 01  WS-COUNTER-FAD              PIC S9(5)  VALUE ZERO    COMP-3.         
006100 01  WS-COUNTER-FAE              PIC S9(5)  VALUE ZERO    COMP-3.         
006200 01  WS-COUNTER-FAF              PIC S9(5)  VALUE ZERO    COMP-3.         
006300 01  WS-KEY1-MARKED              PIC X(1)   VALUE SPACE.                  
006400 01  WS-BELEGRAD-1               PIC X(35)  VALUE SPACE.                  
006500 01  WS-IDLEGSEL                 PIC X(4)   VALUE SPACE.                  
006600 01  WS-KDFINDOC                 PIC X(4)   VALUE SPACE.                  
006700                                                                          
006800*    --- MAPPING FIELDS                                                   
006900 01  MAP-KDFINDOC-LINE           PIC X(4)   VALUE SPACE.                  
007000 01  MAP-KDFINDOC-LINE-2         PIC X(4)   VALUE SPACE.                  
007100 01  MAP-KDPARTTY-LINE           PIC X(3)   VALUE SPACE.                  
007200 01  MAP-KDPARTTY-LINE-2         PIC X(3)   VALUE SPACE.                  
007300 01  MAP-KDPARTGR-LINE           PIC X(15)  VALUE SPACE.                  
007400 01  MAP-KDPARTGR-LINE-2         PIC X(15)  VALUE SPACE.                  
007500 01  MAP-KDSTATUS-LINE           PIC S9(3)  VALUE ZERO    COMP-3.         
007600 01  MAP-KDSTATUS-LINE-2         PIC S9(3)  VALUE ZERO    COMP-3.         
007700 01  MAP-DAREGDAT-LINE           PIC X(8)   VALUE SPACE.                  
007800 01  MAP-DAREGDAT-LINE-2         PIC X(8)   VALUE SPACE.                  
007900 01  MAP-DAUPPDAT-LINE           PIC X(8)   VALUE SPACE.                  
008000 01  MAP-DAUPPDAT-LINE-2         PIC X(8)   VALUE SPACE.                  
008100 01  MAP-IDUSER-LINE             PIC X(8)   VALUE SPACE.                  
008200 01  MAP-IDUSER-LINE-2           PIC X(8)   VALUE SPACE.                  
008300                                                                          
008400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008500 01  GENERAL-SUBPROGRAMS.                                                 
008600     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
008700     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
008800                                                                          
008900*    --- PARAMETERS TO ABEND                                              
009000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009300 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
009400                                                                          
009500 01  MESSAGE-CODES.                                                       
009600     03  ERROR-CODES.                                                     
009700         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
009800         05  IS-INVALID              PIC X(3)   VALUE '023'.              
009900         05  MUST-BE-NUMERIC         PIC X(3)   VALUE '024'.              
010000         05  NOT-FOUND               PIC X(3)   VALUE '025'.              
010100         05  MUST-BE-ENTERED         PIC X(3)   VALUE '026'.              
010200         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
010300         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
010400         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
010500     EJECT                                                                
010600                                                                          
010700*01  -COPY WZ01SUB                                                        
010800     EJECT                                                                
010900*                                                                         
011000 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
011100 01  REQU-AREA.                                                           
011200*    03 -COPY WZ01REQU                                                    
011300*    03 -COPY WF0273I1                                                    
011400     EJECT                                                                
011500                                                                          
011600 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
011700 01  RESP-AREA.                                                           
011800*    03 -COPY WZ01RESP                                                    
011900*    03 -COPY WF0273O1                                                    
012000     EJECT                                                                
012100                                                                          
012200 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
012300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012400                                                                          
012500 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
012600 01  DB2-WS.                                                              
012700     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
012800         88  CURSOR-OK                      VALUE 000.                    
012900         88  LINES-FOUND                    VALUE 000.                    
013000         88  LINES-MISSING                  VALUE 100.                    
013100         88  RESOURCE-WRONG                 VALUE 904.                    
013200                                                                          
013300     03  GOOD-SQLCODECODES.                                               
013400         05  GOOD-SQLCODE OCCURS 5                                        
013500             INDEXED BY SQLCODE-IX PIC 9(3).                              
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
013800*01  -COPY T01LSEL -PRE T01LSEL-                                          
013900     EJECT                                                                
014000                                                                          
014100 01  FILLER                      PIC X(16)   VALUE 'T01CUGR-AREA'.        
014200*01  -COPY T01CUGR -PRE T01CUGR-                                          
014300     EJECT                                                                
014400                                                                          
014500 01  FILLER                      PIC X(16)   VALUE 'T01DOTY-AREA'.        
014600*01  -COPY T01DOTY -PRE T01DOTY-                                          
014700     EJECT                                                                
014800                                                                          
014900 01  FILLER                      PIC X(16)   VALUE 'T01BURE-AREA'.        
015000*01  -COPY T01BURE -PRE T01BURE-                                          
015100     EJECT                                                                
015200                                                                          
015300     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
015400     EJECT                                                                
015500     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
015600     EJECT                                                                
015700     EXEC SQL INCLUDE T01DOTY END-EXEC.                                   
015800     EJECT                                                                
015900     EXEC SQL INCLUDE T01BURE END-EXEC.                                   
016000     EJECT                                                                
016100                                                                          
016200 LINKAGE SECTION.                                                         
016300 PROCEDURE DIVISION.                                                      
016400 MAIN SECTION.                                                            
016500                                                                          
016600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
016700     IF SUB-KDRC = ZERO                                                   
016800       PERFORM A-INIT                                                     
016900       PERFORM B-CHECK-KEYS                                               
017000       IF KEYS-OK                                                         
017100         PERFORM F-READ-SHOW-INFO                                         
017200       END-IF                                                             
017300       PERFORM S02-RETURN-RESPONSE                                        
017400     END-IF                                                               
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900                                                                          
018000 A-INIT SECTION.                                                          
018100     INITIALIZE GOOD-SQLCODECODES                                         
018200     MOVE ALL '+' TO RESP-AREA                                            
018300     MOVE SPACE TO RESP-IDMSG-ERROR                                       
018400     MOVE SPACE TO RESP-IDMSG-INFO                                        
018500     MOVE SPACE TO RESP-IDELMT-ERROR                                      
018600     MOVE ZERO                  TO WS-COUNTER-T01BURE                     
018700                                   WS-COUNTER-FAB                         
018800                                   WS-COUNTER-FAC                         
018900                                   WS-COUNTER-FAD                         
019000                                   WS-COUNTER-FAE                         
019100                                   WS-COUNTER-FAF                         
019200                                   RESP-KVRADER                           
019300     .                                                                    
019400     EJECT                                                                
019500                                                                          
019600*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
019700 B-CHECK-KEYS SECTION.                                                    
019800     MOVE YES TO KEYS-SW                                                  
019900     IF REQU-KDPGMACT = WS-SEARCH                                         
020000     AND REQU-IDMSGVER NUMERIC                                            
020100     AND REQU-IDLEGSEL-KEY > SPACE                                        
020200       CONTINUE                                                           
020300     ELSE                                                                 
020400       MOVE NOO TO KEYS-SW                                                
020500     END-IF                                                               
020600                                                                          
020700     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
020800       MOVE NOO TO KEYS-SW                                                
020900     ELSE                                                                 
021000       CONTINUE                                                           
021100     END-IF                                                               
021200                                                                          
021300     IF KEYS-WRONG                                                        
021400       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
021500       IF REQU-KDPGMACT = 'S'                                             
021600         CONTINUE                                                         
021700       ELSE                                                               
021800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
021900         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
022000       END-IF                                                             
022100       IF REQU-IDMSGVER NUMERIC                                           
022200         CONTINUE                                                         
022300       ELSE                                                               
022400         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
022500         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
022600       END-IF                                                             
022700       IF REQU-IDUSER = SPACE OR = ALL '+'                                
022800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
022900         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
023000       ELSE                                                               
023100         CONTINUE                                                         
023200       END-IF                                                             
023300     END-IF                                                               
023400                                                                          
023500     IF KEYS-OK                                                           
023600       PERFORM DB2-SELECT-T01LSEL-TAB                                     
023700       IF LINES-FOUND                                                     
023800         CONTINUE                                                         
023900       ELSE                                                               
024000         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
024100         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
024200         MOVE NOO TO KEYS-SW                                              
024300       END-IF                                                             
024400     END-IF                                                               
024500                                                                          
024600     PERFORM BB-CHECK-KEY-RELATION                                        
024700     .                                                                    
024800     EJECT                                                                
024900                                                                          
025000*** - CHECK RELATION BETWEEN REQUSTED KEYS                                
025100 BB-CHECK-KEY-RELATION SECTION.                                           
025200     IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                            
025300       IF REQU-KDFINDOC-KEY = SPACE OR = ALL '+'                          
025400*        MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                         
025500*        MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
025600*        MOVE NOO TO KEYS-SW                                              
025700         CONTINUE                                                         
025800       ELSE                                                               
025900         IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                        
026000           CONTINUE                                                       
026100         ELSE                                                             
026200           MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
026300           MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                      
026400           MOVE NOO TO KEYS-SW                                            
026500         END-IF                                                           
026600       END-IF                                                             
026700     ELSE                                                                 
026800       IF REQU-KDFINDOC-KEY = SPACE OR = ALL '+'                          
026900         IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                        
027000           MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
027100           MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                      
027200           MOVE NOO TO KEYS-SW                                            
027300         ELSE                                                             
027400           CONTINUE                                                       
027500         END-IF                                                           
027600       ELSE                                                               
027700         IF REQU-KDPARTGR-KEY NOT = SPACE OR NOT = ALL '+'                
027800           IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                      
027900             MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
028000             MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                    
028100             MOVE NOO TO KEYS-SW                                          
028200           ELSE                                                           
028300             CONTINUE                                                     
028400           END-IF                                                         
028500         END-IF                                                           
028600       END-IF                                                             
028700     END-IF                                                               
028800     .                                                                    
028900     EJECT                                                                
029000                                                                          
029100*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
029200 F-READ-SHOW-INFO SECTION.                                                
029300     MOVE REQU-IDLEGSEL-KEY TO RESP-IDLEGSEL-KEY                          
029400     MOVE REQU-KDFINDOC-KEY TO RESP-KDFINDOC-KEY                          
029500     MOVE REQU-KDPARTTY-KEY TO RESP-KDPARTTY-KEY                          
029600     MOVE REQU-KDPARTGR-KEY TO RESP-KDPARTGR-KEY                          
029700     MOVE REQU-IDMSGVER     TO RESP-IDMSGVER                              
029800     MOVE WS-BELEGRAD-1     TO RESP-BELEGRAD-1                            
029900                                                                          
030000     PERFORM FA-READ-BASICDATA                                            
030100     .                                                                    
030200     EJECT                                                                
030300                                                                          
030400*** - CHECK WHICH REQUESTED KEY                                           
030500 FA-READ-BASICDATA SECTION.                                               
030600     MOVE ZERO TO WS-COUNTER-T01BURE                                      
030700     MOVE SPACE TO WS-KEY1-MARKED                                         
030800     IF REQU-KDFINDOC-KEY = SPACE OR = ALL '+'                            
030900       IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                          
031000         IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                        
031100           PERFORM FAF-HANDLE-KDPART-KEYSY-BLNK                           
031200*        MOVE NOO TO KEYS-SW                                              
031300*        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
031400         END-IF                                                           
031500       ELSE                                                               
031600         IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                        
031700           PERFORM FAE-HANDLE-KDPARTTY-KEY-2                              
031800         ELSE                                                             
031900           PERFORM FAD-HANDLE-KDPARTGR-KEY-2                              
032000         END-IF                                                           
032100       END-IF                                                             
032200     ELSE                                                                 
032300       IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                          
032400         PERFORM FAC-HANDLE-KDFINDOC-KEY                                  
032500       ELSE                                                               
032600         IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                        
032700           PERFORM FAB-HANDLE-KDPARTTY-KEY                                
032800         ELSE                                                             
032900           PERFORM FAA-HANDLE-KDPARTGR-KEY                                
033000         END-IF                                                           
033100       END-IF                                                             
033200     END-IF                                                               
033300                                                                          
033400     IF WS-IX2 > ZERO                                                     
033500       MOVE WS-IX2              TO RESP-KVRADER                           
033600     ELSE                                                                 
033700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
033800       IF WS-COUNTER-T01BURE > ZERO                                       
033900         MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
034000       END-IF                                                             
034100       IF WS-COUNTER-FAB > ZERO                                           
034200         MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
034300       END-IF                                                             
034400       IF WS-COUNTER-FAC > ZERO                                           
034500         MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
034600       END-IF                                                             
034700       IF WS-COUNTER-FAD > ZERO                                           
034800         MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                        
034900       END-IF                                                             
035000       IF WS-COUNTER-FAE > ZERO                                           
035100         MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                        
035200       END-IF                                                             
035300       IF WS-COUNTER-FAF > ZERO                                           
035400         MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
035500       END-IF                                                             
035600     END-IF                                                               
035700     .                                                                    
035800     EJECT                                                                
035900                                                                          
036000*** - HANDLE KEY KDPARTGR                                                 
036100 FAA-HANDLE-KDPARTGR-KEY SECTION.                                         
036200     PERFORM DB2-COUNT-CRS-FAA                                            
036300     IF WS-COUNTER-T01BURE = ZERO                                         
036400       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
036500     ELSE                                                                 
036600       IF WS-COUNTER-T01BURE > WS-MAX-LINES                               
036700         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
036800       END-IF                                                             
036900     END-IF                                                               
037000                                                                          
037100     IF RESP-IDMSG-ERROR = SPACE                                          
037200       PERFORM DB2-DCL-OPN-T01BURE-CRS-1                                  
037300       PERFORM DB2-FETCH-T01BURE-CRS-1                                    
037400       MOVE ZERO TO WS-IX2                                                
037500       MOVE 1 TO WS-IX                                                    
037600       PERFORM UNTIL LINES-MISSING                                        
037700         PERFORM S03-MOVE-TO-RESPOND                                      
037800         PERFORM DB2-FETCH-T01BURE-CRS-1                                  
037900       END-PERFORM                                                        
038000       PERFORM DB2-CLOSE-T01BURE-CRS-1                                    
038100                                                                          
038200       IF WS-IX2 = ZERO                                                   
038300         PERFORM DB2-DCL-OPN-T01DOTY-CRS-3                                
038400         PERFORM DB2-FETCH-T01DOTY-CRS-3                                  
038500         PERFORM UNTIL LINES-MISSING                                      
038600           PERFORM S03-MOVE-TO-RESPOND                                    
038700           PERFORM DB2-FETCH-T01DOTY-CRS-3                                
038800         END-PERFORM                                                      
038900         PERFORM DB2-CLOSE-T01DOTY-CRS-3                                  
039000       END-IF                                                             
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400                                                                          
039500*** - HANDLE KEY KDPARTTY                                                 
039600 FAB-HANDLE-KDPARTTY-KEY SECTION.                                         
039700     PERFORM DB2-COUNT-CRS-FAB                                            
039800     IF WS-COUNTER-FAB = ZERO                                             
039900       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
040000     ELSE                                                                 
040100       IF WS-COUNTER-FAB > WS-MAX-LINES                                   
040200         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
040300       END-IF                                                             
040400     END-IF                                                               
040500                                                                          
040600     IF RESP-IDMSG-ERROR = SPACE                                          
040700       PERFORM DB2-DCL-OPN-T01BURE-CRS-2A                                 
040800       PERFORM DB2-FETCH-T01BURE-CRS-2A                                   
040900       MOVE ZERO TO WS-IX2                                                
041000       MOVE 1    TO WS-IX                                                 
041100       PERFORM UNTIL LINES-MISSING                                        
041200         PERFORM S03-MOVE-TO-RESPOND                                      
041300         PERFORM DB2-FETCH-T01BURE-CRS-2A                                 
041400       END-PERFORM                                                        
041500       PERFORM DB2-CLOSE-T01BURE-CRS-2A                                   
041600                                                                          
041700       IF WS-IX2 = ZERO                                                   
041800         PERFORM DB2-DCL-OPN-T01DOTY-CRS-2                                
041900         PERFORM DB2-FETCH-T01DOTY-CRS-2                                  
042000         PERFORM UNTIL LINES-MISSING                                      
042100           PERFORM S03-MOVE-TO-RESPOND                                    
042200           PERFORM DB2-FETCH-T01DOTY-CRS-2                                
042300         END-PERFORM                                                      
042400         PERFORM DB2-CLOSE-T01DOTY-CRS-2                                  
042500       END-IF                                                             
042600                                                                          
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000                                                                          
043100*** - HANDLE KEY KDFINDOC                                                 
043200 FAC-HANDLE-KDFINDOC-KEY SECTION.                                         
043300     PERFORM DB2-COUNT-CRS-FAC                                            
043400     IF WS-COUNTER-FAC = ZERO                                             
043500       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
043600     ELSE                                                                 
043700       IF WS-COUNTER-FAC > WS-MAX-LINES                                   
043800         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
043900       END-IF                                                             
044000     END-IF                                                               
044100                                                                          
044200     IF RESP-IDMSG-ERROR = SPACE                                          
044300       PERFORM DB2-DCL-OPN-T01BURE-CRS-3A                                 
044400       PERFORM DB2-FETCH-T01BURE-CRS-3A                                   
044500       MOVE ZERO TO WS-IX2                                                
044600       MOVE 1    TO WS-IX                                                 
044700       PERFORM UNTIL LINES-MISSING                                        
044800         PERFORM S03-MOVE-TO-RESPOND                                      
044900         PERFORM DB2-FETCH-T01BURE-CRS-3A                                 
045000       END-PERFORM                                                        
045100       PERFORM DB2-CLOSE-T01BURE-CRS-3A                                   
045200                                                                          
045300       IF WS-IX2 = ZERO                                                   
045400         PERFORM DB2-DCL-OPN-T01DOTY-CRS-1                                
045500         PERFORM DB2-FETCH-T01DOTY-CRS-1                                  
045600         PERFORM UNTIL LINES-MISSING                                      
045700           PERFORM S03-MOVE-TO-RESPOND                                    
045800           PERFORM DB2-FETCH-T01DOTY-CRS-1                                
045900         END-PERFORM                                                      
046000         PERFORM DB2-CLOSE-T01DOTY-CRS-1                                  
046100       END-IF                                                             
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500                                                                          
046600*** - HANDLE KEY KDPARTGR-2                                               
046700 FAD-HANDLE-KDPARTGR-KEY-2 SECTION.                                       
046800     PERFORM DB2-COUNT-CRS-FAD                                            
046900     IF WS-COUNTER-FAD = ZERO                                             
047000       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
047100     ELSE                                                                 
047200       IF WS-COUNTER-FAD > WS-MAX-LINES                                   
047300         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
047400       END-IF                                                             
047500     END-IF                                                               
047600                                                                          
047700     IF RESP-IDMSG-ERROR = SPACE                                          
047800                                                                          
047900       PERFORM DB2-DCL-OPN-T01BURE-CRS-4A                                 
048000       PERFORM DB2-FETCH-T01BURE-CRS-4A                                   
048100       MOVE ZERO TO WS-IX2                                                
048200       MOVE 1    TO WS-IX                                                 
048300       PERFORM UNTIL LINES-MISSING                                        
048400         PERFORM S03-MOVE-TO-RESPOND                                      
048500         PERFORM DB2-FETCH-T01BURE-CRS-4A                                 
048600       END-PERFORM                                                        
048700       PERFORM DB2-CLOSE-T01BURE-CRS-4A                                   
048800                                                                          
048900       IF WS-IX2 = ZERO                                                   
049000         PERFORM DB2-DCL-OPN-T01CUGR-CRS-2                                
049100         PERFORM DB2-FETCH-T01CUGR-CRS-2                                  
049200         PERFORM UNTIL LINES-MISSING                                      
049300           PERFORM S03-MOVE-TO-RESPOND                                    
049400           PERFORM DB2-FETCH-T01CUGR-CRS-2                                
049500         END-PERFORM                                                      
049600         PERFORM DB2-CLOSE-T01CUGR-CRS-2                                  
049700       END-IF                                                             
049800     END-IF                                                               
049900     .                                                                    
050000     EJECT                                                                
050100                                                                          
050200*** - HANDLE KEY KDPARTTY-2                                               
050300 FAE-HANDLE-KDPARTTY-KEY-2 SECTION.                                       
050400     PERFORM DB2-COUNT-CRS-FAE                                            
050500     IF WS-COUNTER-FAE = ZERO                                             
050600       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
050700     ELSE                                                                 
050800       IF WS-COUNTER-FAE > WS-MAX-LINES                                   
050900         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
051000       END-IF                                                             
051100     END-IF                                                               
051200                                                                          
051300     IF RESP-IDMSG-ERROR = SPACE                                          
051400       PERFORM DB2-DCL-OPN-T01BURE-CRS-5A                                 
051500       PERFORM DB2-FETCH-T01BURE-CRS-5A                                   
051600       MOVE ZERO TO WS-IX2                                                
051700       MOVE 1    TO WS-IX                                                 
051800       PERFORM UNTIL LINES-MISSING                                        
051900         PERFORM S03-MOVE-TO-RESPOND                                      
052000         PERFORM DB2-FETCH-T01BURE-CRS-5A                                 
052100       END-PERFORM                                                        
052200       PERFORM DB2-CLOSE-T01BURE-CRS-5A                                   
052300                                                                          
052400       IF WS-IX2 = ZERO                                                   
052500         PERFORM DB2-DCL-OPN-T01CUGR-CRS-1                                
052600         PERFORM DB2-FETCH-T01CUGR-CRS-1                                  
052700         PERFORM UNTIL LINES-MISSING                                      
052800           PERFORM S03-MOVE-TO-RESPOND                                    
052900           PERFORM DB2-FETCH-T01CUGR-CRS-1                                
053000         END-PERFORM                                                      
053100         PERFORM DB2-CLOSE-T01CUGR-CRS-1                                  
053200       END-IF                                                             
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600                                                                          
053700*** - HANDLE WHEN KDFINDOC KDPARTY KDPARTGR ARE SPACES                    
053800 FAF-HANDLE-KDPART-KEYSY-BLNK SECTION.                                    
053900     PERFORM DB2-COUNT-CRS-FAF                                            
054000     IF WS-COUNTER-FAF = ZERO                                             
054100       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
054200     ELSE                                                                 
054300       IF WS-COUNTER-FAF > WS-MAX-LINES                                   
054400         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
054500       END-IF                                                             
054600     END-IF                                                               
054700                                                                          
054800     IF RESP-IDMSG-ERROR = SPACE                                          
054900       PERFORM DB2-DCL-OPN-T01BURE-CRS-6A                                 
055000       PERFORM DB2-FETCH-T01BURE-CRS-6A                                   
055100       MOVE ZERO TO WS-IX2                                                
055200       MOVE 1    TO WS-IX                                                 
055300       PERFORM UNTIL LINES-MISSING                                        
055400         PERFORM S03-MOVE-TO-RESPOND                                      
055500         PERFORM DB2-FETCH-T01BURE-CRS-6A                                 
055600       END-PERFORM                                                        
055700       PERFORM DB2-CLOSE-T01BURE-CRS-6A                                   
055800                                                                          
055900       IF WS-IX2 = ZERO                                                   
056000         PERFORM DB2-DCL-OPN-T01DOTY-CRS-6B                               
056100         PERFORM DB2-FETCH-T01DOTY-CRS-6B                                 
056200         PERFORM UNTIL LINES-MISSING                                      
056300           PERFORM S03-MOVE-TO-RESPOND                                    
056400           PERFORM DB2-FETCH-T01DOTY-CRS-6B                               
056500         END-PERFORM                                                      
056600         PERFORM DB2-CLOSE-T01DOTY-CRS-6B                                 
056700       END-IF                                                             
056800     END-IF                                                               
056900     .                                                                    
057000     EJECT                                                                
057100                                                                          
057200*   --- DISPATCHER SECTION START                                          
057300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
057400     MOVE 'GETARG'             TO SUB-KDFUNC                              
057500     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
057600     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
057700                                                                          
057800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
057900                                                                          
058000     IF SUB-KDRC > 0                                                      
058100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
058200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
058300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
058400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058500     END-IF                                                               
058600     .                                                                    
058700     EJECT                                                                
058800                                                                          
058900 S02-RETURN-RESPONSE SECTION.                                             
059000     MOVE 'RETURN'             TO SUB-KDFUNC                              
059100                                                                          
059200     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
059300                              - ((WS-MAX-LINES - WS-IX2)                  
059400                              * LENGTH OF RESP-TABELLRAD)                 
059500                                                                          
059600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
059700                                                                          
059800     IF SUB-KDRC > 0                                                      
059900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
060000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
060100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
060200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060300     END-IF                                                               
060400     .                                                                    
060500     EJECT                                                                
060600                                                                          
060700*   --- MOVE TO OUTPUT SECTION START                                      
060800*** - MOVE DATA TO RESPOND WHEN CURRENT LINE,                             
060900***   WHEN COMING LINE MODIFY CURRENT LINE.                               
061000 S03-MOVE-TO-RESPOND SECTION.                                             
061100     PERFORM S04-SELECT-SEARCH-RESPOND                                    
061200     IF LINES-FOUND                                                       
061300       IF MAP-KDSTATUS-LINE = WS-CURRENT                                  
061400       AND MAP-KDSTATUS-LINE-2 = WS-CURRENT                               
061500         IF  MAP-KDFINDOC-LINE = RESP-KDFINDOC-LINE(WS-IX)                
061600         AND MAP-KDPARTTY-LINE = RESP-KDPARTTY-LINE(WS-IX)                
061700         AND MAP-KDPARTGR-LINE = RESP-KDPARTGR-LINE(WS-IX)                
061800           CONTINUE                                                       
061900         ELSE                                                             
062000           ADD 1 TO WS-IX2                                                
062100           MOVE MAP-KDFINDOC-LINE TO RESP-KDFINDOC-LINE(WS-IX)            
062200           MOVE MAP-KDPARTTY-LINE TO RESP-KDPARTTY-LINE(WS-IX)            
062300           MOVE MAP-KDPARTGR-LINE TO RESP-KDPARTGR-LINE(WS-IX)            
062400           MOVE MAP-DAREGDAT-LINE TO RESP-DAREGDAT-LINE(WS-IX)            
062500           MOVE MAP-DAUPPDAT-LINE TO RESP-DAUPPDAT-LINE(WS-IX)            
062600           MOVE MAP-IDUSER-LINE TO RESP-IDUSER-LINE(WS-IX)                
062700           IF WS-KEY1-MARKED = 'Y'                                        
062800             MOVE YES             TO RESP-FLCOMING-LINE(WS-IX)            
062900             MOVE 'N'             TO WS-KEY1-MARKED                       
063000           ELSE                                                           
063100             MOVE NOO             TO RESP-FLCOMING-LINE(WS-IX)            
063200           END-IF                                                         
063300           ADD 1 TO WS-IX                                                 
063400         END-IF                                                           
063500       ELSE                                                               
063600         IF  MAP-KDFINDOC-LINE = RESP-KDFINDOC-LINE(WS-IX)                
063700         AND MAP-KDPARTTY-LINE = RESP-KDPARTTY-LINE(WS-IX)                
063800         AND MAP-KDPARTGR-LINE = RESP-KDPARTGR-LINE(WS-IX)                
063900           CONTINUE                                                       
064000         ELSE                                                             
064100           IF MAP-KDSTATUS-LINE = WS-COMING                               
064200             MOVE 'Y' TO WS-KEY1-MARKED                                   
064300           END-IF                                                         
064400           IF MAP-KDSTATUS-LINE-2 = WS-COMING                             
064500             MOVE 'Y' TO WS-KEY1-MARKED                                   
064600           END-IF                                                         
064700         END-IF                                                           
064800       END-IF                                                             
064900     ELSE                                                                 
065000       IF MAP-KDSTATUS-LINE-2 = WS-CURRENT                                
065100         MOVE SPACE      TO MAP-DAREGDAT-LINE-2                           
065200                            MAP-DAUPPDAT-LINE-2                           
065300         MOVE WS-NOT-REG TO MAP-IDUSER-LINE-2                             
065400         IF  MAP-KDFINDOC-LINE = RESP-KDFINDOC-LINE(WS-IX)                
065500         AND MAP-KDPARTTY-LINE = RESP-KDPARTTY-LINE(WS-IX)                
065600         AND MAP-KDPARTGR-LINE = RESP-KDPARTGR-LINE(WS-IX)                
065700           CONTINUE                                                       
065800         ELSE                                                             
065900           ADD 1 TO WS-IX2                                                
066000           MOVE MAP-KDFINDOC-LINE-2 TO RESP-KDFINDOC-LINE(WS-IX)          
066100           MOVE MAP-KDPARTTY-LINE-2 TO RESP-KDPARTTY-LINE(WS-IX)          
066200           MOVE MAP-KDPARTGR-LINE-2 TO RESP-KDPARTGR-LINE(WS-IX)          
066300           MOVE MAP-DAREGDAT-LINE-2 TO RESP-DAREGDAT-LINE(WS-IX)          
066400           MOVE MAP-DAUPPDAT-LINE-2 TO RESP-DAUPPDAT-LINE(WS-IX)          
066500           MOVE MAP-IDUSER-LINE-2 TO RESP-IDUSER-LINE(WS-IX)              
066600           MOVE NOO               TO RESP-FLCOMING-LINE(WS-IX)            
066700           ADD 1 TO WS-IX                                                 
066800         END-IF                                                           
066900       END-IF                                                             
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300                                                                          
067400 S04-SELECT-SEARCH-RESPOND SECTION.                                       
067500     IF WS-COUNTER-T01BURE > ZERO                                         
067600       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
067700         PERFORM DB2-SELECT-T01BURE-FAA-2                                 
067800         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
067900         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
068000         MOVE MAP-KDPARTGR-LINE TO MAP-KDPARTGR-LINE-2                    
068100       ELSE                                                               
068200         PERFORM DB2-SELECT-T01BURE-FAA                                   
068300         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
068400         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
068500         MOVE MAP-KDPARTGR-LINE TO MAP-KDPARTGR-LINE-2                    
068600       END-IF                                                             
068700     END-IF                                                               
068800                                                                          
068900     IF WS-COUNTER-FAB > ZERO                                             
069000       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
069100         PERFORM DB2-SELECT-T01BURE-FAB-2                                 
069200         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
069300         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
069400         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
069500       ELSE                                                               
069600         PERFORM DB2-SELECT-T01BURE-FAB                                   
069700         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
069800         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
069900         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
070000       END-IF                                                             
070100     END-IF                                                               
070200                                                                          
070300     IF WS-COUNTER-FAC > ZERO                                             
070400       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
070500         PERFORM DB2-SELECT-T01BURE-FAC-2                                 
070600         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
070700         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
070800         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
070900       ELSE                                                               
071000         PERFORM DB2-SELECT-T01BURE-FAC                                   
071100         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
071200         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
071300         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
071400       END-IF                                                             
071500     END-IF                                                               
071600                                                                          
071700     IF WS-COUNTER-FAD > ZERO                                             
071800       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
071900         PERFORM DB2-SELECT-T01BURE-FAD-2                                 
072000         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
072100         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
072200         MOVE MAP-KDPARTGR-LINE TO MAP-KDPARTGR-LINE-2                    
072300       ELSE                                                               
072400         PERFORM DB2-SELECT-T01BURE-FAD                                   
072500         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
072600         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
072700         MOVE MAP-KDPARTGR-LINE TO MAP-KDPARTGR-LINE-2                    
072800       END-IF                                                             
072900     END-IF                                                               
073000                                                                          
073100     IF WS-COUNTER-FAE > ZERO                                             
073200       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
073300         PERFORM DB2-SELECT-T01BURE-FAE-2                                 
073400         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
073500         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
073600         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
073700       ELSE                                                               
073800         PERFORM DB2-SELECT-T01BURE-FAE                                   
073900         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
074000         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
074100         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
074200       END-IF                                                             
074300     END-IF                                                               
074400                                                                          
074500     IF WS-COUNTER-FAF > ZERO                                             
074600       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
074700         PERFORM DB2-SELECT-T01BURE-FAC-2                                 
074800         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
074900         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
075000         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
075100       ELSE                                                               
075200         PERFORM DB2-SELECT-T01BURE-FAC                                   
075300         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
075400         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
075500         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
075600       END-IF                                                             
075700     END-IF                                                               
075800     .                                                                    
075900     EJECT                                                                
076000                                                                          
076100*   --- DB2 SECTIONS                                                      
076200*** - CHECK THAT THE REQUESTED LEGAL SELLER EXIST                         
076300 DB2-SELECT-T01LSEL-TAB SECTION.                                          
076400     MOVE 000100 TO GOOD-SQLCODECODES                                     
076500                                                                          
076600     EXEC SQL                                                             
076700           SELECT  IDLEGSEL                                               
076800                 , BELEGRAD_1                                             
076900                                                                          
077000           INTO   :WS-IDLEGSEL                                            
077100                , :WS-BELEGRAD-1                                          
077200                                                                          
077300           FROM    T01LSEL                                                
077400                                                                          
077500           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
077600               AND KDSTATUS = :WS-CURRENT                                 
077700     END-EXEC                                                             
077800                                                                          
077900     MOVE SQLCODE TO SQLCODE-WS                                           
078000     PERFORM DB2-STATUS-CHECK                                             
078100     .                                                                    
078200     EJECT                                                                
078300                                                                          
078400 DB2-SELECT-T01BURE-FAA SECTION.                                          
078500     MOVE 000100 TO GOOD-SQLCODECODES                                     
078600                                                                          
078700     EXEC SQL                                                             
078800           SELECT  KDSTATUS                                               
078900                 , DAREGDAT                                               
079000                 , DAUPPDAT                                               
079100                 , IDUSER                                                 
079200                                                                          
079300           INTO    :MAP-KDSTATUS-LINE-2                                   
079400                 , :MAP-DAREGDAT-LINE                                     
079500                 , :MAP-DAUPPDAT-LINE                                     
079600                 , :MAP-IDUSER-LINE                                       
079700                                                                          
079800           FROM    T01BURE                                                
079900                                                                          
080000           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
080100                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
080200                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
080300                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
080400                AND DADELDAT = '00000000'                                 
080500                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
080600     END-EXEC                                                             
080700                                                                          
080800     MOVE SQLCODE TO SQLCODE-WS                                           
080900     PERFORM DB2-STATUS-CHECK                                             
081000     .                                                                    
081100     EJECT                                                                
081200                                                                          
081300 DB2-SELECT-T01BURE-FAA-2 SECTION.                                        
081400     MOVE 000100 TO GOOD-SQLCODECODES                                     
081500                                                                          
081600     EXEC SQL                                                             
081700           SELECT  KDSTATUS                                               
081800                 , DAREGDAT                                               
081900                 , DAUPPDAT                                               
082000                 , IDUSER                                                 
082100                                                                          
082200           INTO    :MAP-KDSTATUS-LINE                                     
082300                 , :MAP-DAREGDAT-LINE                                     
082400                 , :MAP-DAUPPDAT-LINE                                     
082500                 , :MAP-IDUSER-LINE                                       
082600                                                                          
082700           FROM    T01BURE                                                
082800                                                                          
082900           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
083000                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
083100                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
083200                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
083300                AND DADELDAT = '00000000'                                 
083400                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
083500     END-EXEC                                                             
083600                                                                          
083700     MOVE SQLCODE TO SQLCODE-WS                                           
083800     PERFORM DB2-STATUS-CHECK                                             
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200 DB2-SELECT-T01BURE-FAB SECTION.                                          
084300     MOVE 000100 TO GOOD-SQLCODECODES                                     
084400                                                                          
084500     EXEC SQL                                                             
084600           SELECT  KDFINDOC                                               
084700                 , DAREGDAT                                               
084800                 , DAUPPDAT                                               
084900                 , IDUSER                                                 
085000                                                                          
085100           INTO    :WS-KDFINDOC                                           
085200                 , :MAP-DAREGDAT-LINE                                     
085300                 , :MAP-DAUPPDAT-LINE                                     
085400                 , :MAP-IDUSER-LINE                                       
085500                                                                          
085600           FROM    T01BURE                                                
085700                                                                          
085800           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
085900                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
086000                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
086100                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
086200                AND DADELDAT = '00000000'                                 
086300                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
086400     END-EXEC                                                             
086500                                                                          
086600     MOVE SQLCODE TO SQLCODE-WS                                           
086700     PERFORM DB2-STATUS-CHECK                                             
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100 DB2-SELECT-T01BURE-FAB-2 SECTION.                                        
087200     MOVE 000100 TO GOOD-SQLCODECODES                                     
087300                                                                          
087400     EXEC SQL                                                             
087500           SELECT  KDFINDOC                                               
087600                 , DAREGDAT                                               
087700                 , DAUPPDAT                                               
087800                 , IDUSER                                                 
087900                                                                          
088000           INTO    :WS-KDFINDOC                                           
088100                 , :MAP-DAREGDAT-LINE                                     
088200                 , :MAP-DAUPPDAT-LINE                                     
088300                 , :MAP-IDUSER-LINE                                       
088400                                                                          
088500           FROM    T01BURE                                                
088600                                                                          
088700           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
088800                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
088900                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
089000                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
089100                AND DADELDAT = '00000000'                                 
089200                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
089300     END-EXEC                                                             
089400                                                                          
089500     MOVE SQLCODE TO SQLCODE-WS                                           
089600     PERFORM DB2-STATUS-CHECK                                             
089700     .                                                                    
089800     EJECT                                                                
089900                                                                          
090000 DB2-SELECT-T01BURE-FAC SECTION.                                          
090100     MOVE 000100 TO GOOD-SQLCODECODES                                     
090200                                                                          
090300     EXEC SQL                                                             
090400           SELECT  KDFINDOC                                               
090500                 , DAREGDAT                                               
090600                 , DAUPPDAT                                               
090700                 , IDUSER                                                 
090800                                                                          
090900           INTO    :WS-KDFINDOC                                           
091000                 , :MAP-DAREGDAT-LINE                                     
091100                 , :MAP-DAUPPDAT-LINE                                     
091200                 , :MAP-IDUSER-LINE                                       
091300                                                                          
091400           FROM    T01BURE                                                
091500                                                                          
091600           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
091700                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
091800                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
091900                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
092000                AND DADELDAT = '00000000'                                 
092100                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
092200     END-EXEC                                                             
092300                                                                          
092400     MOVE SQLCODE TO SQLCODE-WS                                           
092500     PERFORM DB2-STATUS-CHECK                                             
092600     .                                                                    
092700     EJECT                                                                
092800                                                                          
092900 DB2-SELECT-T01BURE-FAC-2 SECTION.                                        
093000     MOVE 000100 TO GOOD-SQLCODECODES                                     
093100                                                                          
093200     EXEC SQL                                                             
093300           SELECT  KDFINDOC                                               
093400                 , DAREGDAT                                               
093500                 , DAUPPDAT                                               
093600                 , IDUSER                                                 
093700                                                                          
093800           INTO    :WS-KDFINDOC                                           
093900                 , :MAP-DAREGDAT-LINE                                     
094000                 , :MAP-DAUPPDAT-LINE                                     
094100                 , :MAP-IDUSER-LINE                                       
094200                                                                          
094300           FROM    T01BURE                                                
094400                                                                          
094500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
094600                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
094700                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
094800                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
094900                AND DADELDAT = '00000000'                                 
095000                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
095100     END-EXEC                                                             
095200                                                                          
095300     MOVE SQLCODE TO SQLCODE-WS                                           
095400     PERFORM DB2-STATUS-CHECK                                             
095500     .                                                                    
095600     EJECT                                                                
095700                                                                          
095800 DB2-SELECT-T01BURE-FAD SECTION.                                          
095900     MOVE 000100 TO GOOD-SQLCODECODES                                     
096000                                                                          
096100     EXEC SQL                                                             
096200           SELECT  KDFINDOC                                               
096300                 , DAREGDAT                                               
096400                 , DAUPPDAT                                               
096500                 , IDUSER                                                 
096600                                                                          
096700           INTO    :WS-KDFINDOC                                           
096800                 , :MAP-DAREGDAT-LINE                                     
096900                 , :MAP-DAUPPDAT-LINE                                     
097000                 , :MAP-IDUSER-LINE                                       
097100                                                                          
097200           FROM    T01BURE                                                
097300                                                                          
097400           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
097500                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
097600                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
097700                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
097800                AND DADELDAT = '00000000'                                 
097900                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
098000     END-EXEC                                                             
098100                                                                          
098200     MOVE SQLCODE TO SQLCODE-WS                                           
098300     PERFORM DB2-STATUS-CHECK                                             
098400     .                                                                    
098500     EJECT                                                                
098600                                                                          
098700 DB2-SELECT-T01BURE-FAD-2 SECTION.                                        
098800     MOVE 000100 TO GOOD-SQLCODECODES                                     
098900                                                                          
099000     EXEC SQL                                                             
099100           SELECT  KDFINDOC                                               
099200                 , DAREGDAT                                               
099300                 , DAUPPDAT                                               
099400                 , IDUSER                                                 
099500                                                                          
099600           INTO    :WS-KDFINDOC                                           
099700                 , :MAP-DAREGDAT-LINE                                     
099800                 , :MAP-DAUPPDAT-LINE                                     
099900                 , :MAP-IDUSER-LINE                                       
100000                                                                          
100100           FROM    T01BURE                                                
100200                                                                          
100300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
100400                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
100500                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
100600                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
100700                AND DADELDAT = '00000000'                                 
100800                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
100900     END-EXEC                                                             
101000                                                                          
101100     MOVE SQLCODE TO SQLCODE-WS                                           
101200     PERFORM DB2-STATUS-CHECK                                             
101300     .                                                                    
101400     EJECT                                                                
101500                                                                          
101600 DB2-SELECT-T01BURE-FAE SECTION.                                          
101700     MOVE 000100 TO GOOD-SQLCODECODES                                     
101800                                                                          
101900     EXEC SQL                                                             
102000           SELECT  KDFINDOC                                               
102100                 , DAREGDAT                                               
102200                 , DAUPPDAT                                               
102300                 , IDUSER                                                 
102400                                                                          
102500           INTO    :WS-KDFINDOC                                           
102600                 , :MAP-DAREGDAT-LINE                                     
102700                 , :MAP-DAUPPDAT-LINE                                     
102800                 , :MAP-IDUSER-LINE                                       
102900                                                                          
103000           FROM    T01BURE                                                
103100                                                                          
103200           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
103300                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
103400                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
103500                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
103600                AND DADELDAT = '00000000'                                 
103700                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
103800     END-EXEC                                                             
103900                                                                          
104000     MOVE SQLCODE TO SQLCODE-WS                                           
104100     PERFORM DB2-STATUS-CHECK                                             
104200     .                                                                    
104300     EJECT                                                                
104400                                                                          
104500 DB2-SELECT-T01BURE-FAE-2 SECTION.                                        
104600     MOVE 000100 TO GOOD-SQLCODECODES                                     
104700                                                                          
104800     EXEC SQL                                                             
104900           SELECT  KDFINDOC                                               
105000                 , DAREGDAT                                               
105100                 , DAUPPDAT                                               
105200                 , IDUSER                                                 
105300                                                                          
105400           INTO    :WS-KDFINDOC                                           
105500                 , :MAP-DAREGDAT-LINE                                     
105600                 , :MAP-DAUPPDAT-LINE                                     
105700                 , :MAP-IDUSER-LINE                                       
105800                                                                          
105900           FROM    T01BURE                                                
106000                                                                          
106100           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
106200                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
106300                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
106400                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
106500                AND DADELDAT = '00000000'                                 
106600                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
106700     END-EXEC                                                             
106800                                                                          
106900     MOVE SQLCODE TO SQLCODE-WS                                           
107000     PERFORM DB2-STATUS-CHECK                                             
107100     .                                                                    
107200     EJECT                                                                
107300                                                                          
107400* * * * * * * * * *   - CURSOR-FAA- * * * * * * * * * * * * * *           
107500 DB2-COUNT-CRS-FAA SECTION.                                               
107600     EXEC SQL                                                             
107700                                                                          
107800           SELECT COUNT(*)                                                
107900                                                                          
108000           INTO  :WS-COUNTER-T01BURE                                      
108100                                                                          
108200           FROM   T01CUGR A                                               
108300                , T01DOTY B                                               
108400                                                                          
108500           WHERE    A.IDLEGSEL   = :REQU-IDLEGSEL-KEY                     
108600                AND A.IDLEGSEL   = B.IDLEGSEL                             
108700                AND ( B.KDFINDOC = :REQU-KDFINDOC-KEY                     
108800                OR  A.KDPARTTY   = :REQU-KDPARTTY-KEY                     
108900                OR  A.KDPARTGR   = :REQU-KDPARTGR-KEY )                   
109000                AND A.DADELDAT   = '00000000'                             
109100                AND A.KDSTATUS   = :WS-CURRENT                            
109200                                                                          
109300     END-EXEC                                                             
109400                                                                          
109500     MOVE 000100  TO GOOD-SQLCODECODES                                    
109600                                                                          
109700     MOVE SQLCODE TO SQLCODE-WS                                           
109800     PERFORM DB2-STATUS-CHECK                                             
109900     .                                                                    
110000     EJECT                                                                
110100                                                                          
110200 DB2-DCL-OPN-T01DOTY-CRS-3 SECTION.                                       
110300     MOVE 000100 TO GOOD-SQLCODECODES                                     
110400                                                                          
110500     EXEC SQL                                                             
110600         DECLARE T01DOTY-CRS-3 CURSOR WITH HOLD FOR                       
110700                                                                          
110800           SELECT  A.KDFINDOC                                             
110900                 , B.KDPARTTY                                             
111000                 , B.KDPARTGR                                             
111100                 , A.KDSTATUS                                             
111200                 , A.DAREGDAT                                             
111300                 , A.DAUPPDAT                                             
111400                 , A.IDUSER                                               
111500                                                                          
111600           FROM    T01DOTY A                                              
111700                 , T01CUGR B                                              
111800                                                                          
111900           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
112000                AND A.IDLEGSEL = B.IDLEGSEL                               
112100                AND A.DADELDAT = '00000000'                               
112200                AND B.DADELDAT = '00000000'                               
112300                AND A.KDSTATUS = :WS-CURRENT                              
112400                AND B.KDSTATUS = :WS-CURRENT                              
112500                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
112600                AND B.KDPARTTY = :REQU-KDPARTTY-KEY                       
112700                AND B.KDPARTGR = :REQU-KDPARTGR-KEY                       
112800                                                                          
112900           ORDER BY A.IDLEGSEL                                            
113000                  , A.KDFINDOC                                            
113100                  , B.KDPARTTY                                            
113200                  , B.KDPARTGR                                            
113300     END-EXEC                                                             
113400                                                                          
113500     MOVE 000100  TO GOOD-SQLCODECODES                                    
113600                                                                          
113700     EXEC SQL                                                             
113800        OPEN T01DOTY-CRS-3                                                
113900     END-EXEC                                                             
114000                                                                          
114100     MOVE SQLCODE TO SQLCODE-WS                                           
114200     PERFORM DB2-STATUS-CHECK                                             
114300     .                                                                    
114400     EJECT                                                                
114500                                                                          
114600 DB2-FETCH-T01DOTY-CRS-3 SECTION.                                         
114700     MOVE 000100  TO GOOD-SQLCODECODES                                    
114800                                                                          
114900     EXEC SQL                                                             
115000                                                                          
115100         FETCH T01DOTY-CRS-3                                              
115200                                                                          
115300         INTO :MAP-KDFINDOC-LINE                                          
115400            , :MAP-KDPARTTY-LINE                                          
115500            , :MAP-KDPARTGR-LINE                                          
115600            , :MAP-KDSTATUS-LINE-2                                        
115700            , :MAP-DAREGDAT-LINE-2                                        
115800            , :MAP-DAUPPDAT-LINE-2                                        
115900            , :MAP-IDUSER-LINE-2                                          
116000     END-EXEC                                                             
116100                                                                          
116200     MOVE SQLCODE TO SQLCODE-WS                                           
116300     PERFORM DB2-STATUS-CHECK                                             
116400     .                                                                    
116500     EJECT                                                                
116600                                                                          
116700 DB2-CLOSE-T01DOTY-CRS-3 SECTION.                                         
116800     EXEC SQL                                                             
116900        CLOSE T01DOTY-CRS-3                                               
117000     END-EXEC                                                             
117100     .                                                                    
117200     EJECT                                                                
117300                                                                          
117400                                                                          
117500 DB2-DCL-OPN-T01BURE-CRS-1 SECTION.                                       
117600     MOVE 000100 TO GOOD-SQLCODECODES                                     
117700                                                                          
117800     EXEC SQL                                                             
117900         DECLARE T01BURE-CRS-1 CURSOR WITH HOLD FOR                       
118000                                                                          
118100           SELECT  KDFINDOC                                               
118200                 , KDPARTTY                                               
118300                 , KDPARTGR                                               
118400                 , KDSTATUS                                               
118500                 , KDSTATUS                                               
118600                 , DAREGDAT                                               
118700                 , DAUPPDAT                                               
118800                 , IDUSER                                                 
118900                                                                          
119000           FROM    T01BURE                                                
119100                                                                          
119200           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
119300                AND KDFINDOC = :REQU-KDFINDOC-KEY                         
119400                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
119500                AND KDPARTGR = :REQU-KDPARTGR-KEY                         
119600                AND DADELDAT = '00000000'                                 
119700                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
119800                                                                          
119900           ORDER BY IDLEGSEL                                              
120000                  , KDFINDOC                                              
120100                  , KDPARTTY                                              
120200                  , KDPARTGR                                              
120300                  , KDSTATUS DESC                                         
120400     END-EXEC                                                             
120500                                                                          
120600     MOVE 000100  TO GOOD-SQLCODECODES                                    
120700                                                                          
120800     EXEC SQL                                                             
120900        OPEN T01BURE-CRS-1                                                
121000     END-EXEC                                                             
121100                                                                          
121200     MOVE SQLCODE TO SQLCODE-WS                                           
121300     PERFORM DB2-STATUS-CHECK                                             
121400     .                                                                    
121500     EJECT                                                                
121600                                                                          
121700 DB2-FETCH-T01BURE-CRS-1 SECTION.                                         
121800     MOVE 000100  TO GOOD-SQLCODECODES                                    
121900                                                                          
122000     EXEC SQL                                                             
122100                                                                          
122200         FETCH T01BURE-CRS-1                                              
122300                                                                          
122400         INTO :MAP-KDFINDOC-LINE                                          
122500            , :MAP-KDPARTTY-LINE                                          
122600            , :MAP-KDPARTGR-LINE                                          
122700            , :MAP-KDSTATUS-LINE                                          
122800            , :MAP-KDSTATUS-LINE-2                                        
122900            , :MAP-DAREGDAT-LINE                                          
123000            , :MAP-DAUPPDAT-LINE                                          
123100            , :MAP-IDUSER-LINE                                            
123200     END-EXEC                                                             
123300                                                                          
123400     MOVE SQLCODE TO SQLCODE-WS                                           
123500     PERFORM DB2-STATUS-CHECK                                             
123600     .                                                                    
123700     EJECT                                                                
123800                                                                          
123900 DB2-CLOSE-T01BURE-CRS-1 SECTION.                                         
124000     EXEC SQL                                                             
124100        CLOSE T01BURE-CRS-1                                               
124200     END-EXEC                                                             
124300     .                                                                    
124400     EJECT                                                                
124500                                                                          
124600* * * * * * * * * *   - CURSOR-FAB-  * * * * * * * * * * * * *            
124700 DB2-COUNT-CRS-FAB SECTION.                                               
124800     EXEC SQL                                                             
124900                                                                          
125000          SELECT COUNT(*)                                                 
125100                                                                          
125200          INTO  :WS-COUNTER-FAB                                           
125300                                                                          
125400          FROM   T01DOTY A                                                
125500               , T01CUGR B                                                
125600                                                                          
125700          WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
125800             AND   A.IDLEGSEL = B.IDLEGSEL                                
125900             AND ( A.KDFINDOC = :REQU-KDFINDOC-KEY                        
126000             OR    B.KDPARTTY = :REQU-KDPARTTY-KEY )                      
126100             AND   A.KDSTATUS = :WS-CURRENT                               
126200             AND   A.DADELDAT = '00000000'                                
126300                                                                          
126400     END-EXEC                                                             
126500                                                                          
126600     MOVE 000100  TO GOOD-SQLCODECODES                                    
126700                                                                          
126800     MOVE SQLCODE TO SQLCODE-WS                                           
126900     PERFORM DB2-STATUS-CHECK                                             
127000     .                                                                    
127100     EJECT                                                                
127200                                                                          
127300 DB2-DCL-OPN-T01DOTY-CRS-2 SECTION.                                       
127400     MOVE 000100 TO GOOD-SQLCODECODES                                     
127500                                                                          
127600     EXEC SQL                                                             
127700         DECLARE T01DOTY-CRS-2 CURSOR WITH HOLD FOR                       
127800                                                                          
127900           SELECT  A.KDFINDOC                                             
128000                 , B.KDPARTTY                                             
128100                 , B.KDPARTGR                                             
128200                 , A.KDSTATUS                                             
128300                 , A.DAREGDAT                                             
128400                 , A.DAUPPDAT                                             
128500                 , A.IDUSER                                               
128600                                                                          
128700           FROM    T01DOTY A                                              
128800                 , T01CUGR B                                              
128900                                                                          
129000           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
129100                AND A.IDLEGSEL = B.IDLEGSEL                               
129200                AND A.DADELDAT = '00000000'                               
129300                AND B.DADELDAT = '00000000'                               
129400                AND A.KDSTATUS = :WS-CURRENT                              
129500                AND B.KDSTATUS = :WS-CURRENT                              
129600                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
129700                AND B.KDPARTTY = :REQU-KDPARTTY-KEY                       
129800                                                                          
129900           ORDER BY A.IDLEGSEL                                            
130000                  , A.KDFINDOC                                            
130100                  , B.KDPARTTY                                            
130200                  , B.KDPARTGR                                            
130300     END-EXEC                                                             
130400                                                                          
130500     MOVE 000100  TO GOOD-SQLCODECODES                                    
130600                                                                          
130700     EXEC SQL                                                             
130800        OPEN T01DOTY-CRS-2                                                
130900     END-EXEC                                                             
131000                                                                          
131100     MOVE SQLCODE TO SQLCODE-WS                                           
131200     PERFORM DB2-STATUS-CHECK                                             
131300     .                                                                    
131400     EJECT                                                                
131500                                                                          
131600 DB2-FETCH-T01DOTY-CRS-2 SECTION.                                         
131700     MOVE 000100  TO GOOD-SQLCODECODES                                    
131800                                                                          
131900     EXEC SQL                                                             
132000                                                                          
132100         FETCH T01DOTY-CRS-2                                              
132200                                                                          
132300         INTO :MAP-KDFINDOC-LINE                                          
132400            , :MAP-KDPARTTY-LINE                                          
132500            , :MAP-KDPARTGR-LINE-2                                        
132600            , :MAP-KDSTATUS-LINE-2                                        
132700            , :MAP-DAREGDAT-LINE-2                                        
132800            , :MAP-DAUPPDAT-LINE-2                                        
132900            , :MAP-IDUSER-LINE-2                                          
133000     END-EXEC                                                             
133100                                                                          
133200     MOVE SQLCODE TO SQLCODE-WS                                           
133300     PERFORM DB2-STATUS-CHECK                                             
133400     .                                                                    
133500     EJECT                                                                
133600                                                                          
133700 DB2-CLOSE-T01DOTY-CRS-2 SECTION.                                         
133800     EXEC SQL                                                             
133900        CLOSE T01DOTY-CRS-2                                               
134000     END-EXEC                                                             
134100     .                                                                    
134200     EJECT                                                                
134300                                                                          
134400 DB2-DCL-OPN-T01BURE-CRS-2A SECTION.                                      
134500     MOVE 000100 TO GOOD-SQLCODECODES                                     
134600                                                                          
134700     EXEC SQL                                                             
134800         DECLARE T01BURE-CRS-2A CURSOR WITH HOLD FOR                      
134900                                                                          
135000          SELECT   DISTINCT A.KDFINDOC                                    
135100                 , B.KDPARTTY                                             
135200                 , B.KDPARTGR                                             
135300                 , A.KDSTATUS                                             
135400                 , B.KDSTATUS                                             
135500                 , B.DAREGDAT                                             
135600                 , B.DAUPPDAT                                             
135700                 , B.IDUSER                                               
135800                                                                          
135900          FROM     T01BURE A                                              
136000                 , T01CUGR B                                              
136100                                                                          
136200          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
136300             AND   A.IDLEGSEL = B.IDLEGSEL                                
136400             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
136500             AND   B.KDPARTTY = :REQU-KDPARTTY-KEY                        
136600             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
136700             AND   B.KDSTATUS = :WS-CURRENT                               
136800             AND   B.DADELDAT = '00000000'                                
136900             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
137000             AND   A.IDLEGSEL = B.IDLEGSEL                                
137100             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
137200             AND   B.KDPARTTY = :REQU-KDPARTTY-KEY                        
137300             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
137400             AND   B.KDSTATUS = :WS-CURRENT                               
137500             AND   B.DADELDAT = '00000000'                                
137600             AND   A.KDPARTGR <> B.KDPARTGR )                             
137700                                                                          
137800     END-EXEC                                                             
137900                                                                          
138000     MOVE 000100  TO GOOD-SQLCODECODES                                    
138100                                                                          
138200     EXEC SQL                                                             
138300        OPEN T01BURE-CRS-2A                                               
138400     END-EXEC                                                             
138500                                                                          
138600     MOVE SQLCODE TO SQLCODE-WS                                           
138700     PERFORM DB2-STATUS-CHECK                                             
138800     .                                                                    
138900     EJECT                                                                
139000                                                                          
139100 DB2-FETCH-T01BURE-CRS-2A SECTION.                                        
139200     MOVE 000100  TO GOOD-SQLCODECODES                                    
139300                                                                          
139400     EXEC SQL                                                             
139500                                                                          
139600         FETCH T01BURE-CRS-2A                                             
139700                                                                          
139800         INTO :MAP-KDFINDOC-LINE                                          
139900            , :MAP-KDPARTTY-LINE                                          
140000            , :MAP-KDPARTGR-LINE-2                                        
140100            , :MAP-KDSTATUS-LINE                                          
140200            , :MAP-KDSTATUS-LINE-2                                        
140300            , :MAP-DAREGDAT-LINE                                          
140400            , :MAP-DAREGDAT-LINE-2                                        
140500            , :MAP-DAUPPDAT-LINE                                          
140600            , :MAP-DAUPPDAT-LINE-2                                        
140700            , :MAP-IDUSER-LINE                                            
140800            , :MAP-IDUSER-LINE-2                                          
140900     END-EXEC                                                             
141000                                                                          
141100     MOVE SQLCODE TO SQLCODE-WS                                           
141200     PERFORM DB2-STATUS-CHECK                                             
141300     .                                                                    
141400     EJECT                                                                
141500                                                                          
141600 DB2-CLOSE-T01BURE-CRS-2A SECTION.                                        
141700     EXEC SQL                                                             
141800        CLOSE T01BURE-CRS-2A                                              
141900     END-EXEC                                                             
142000     .                                                                    
142100     EJECT                                                                
142200                                                                          
142300* * * * * * * * * *   - CURSOR-FAC-  * * * * * * * * * * * * *            
142400 DB2-COUNT-CRS-FAC SECTION.                                               
142500     EXEC SQL                                                             
142600                                                                          
142700          SELECT COUNT(*)                                                 
142800                                                                          
142900          INTO  :WS-COUNTER-FAC                                           
143000                                                                          
143100          FROM   T01DOTY                                                  
143200                                                                          
143300          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
143400             AND   KDSTATUS = :WS-CURRENT                                 
143500             AND   DADELDAT = '00000000'                                  
143600             AND   KDFINDOC = :REQU-KDFINDOC-KEY                          
143700                                                                          
143800     END-EXEC                                                             
143900                                                                          
144000     MOVE 000100  TO GOOD-SQLCODECODES                                    
144100                                                                          
144200     MOVE SQLCODE TO SQLCODE-WS                                           
144300     PERFORM DB2-STATUS-CHECK                                             
144400     .                                                                    
144500     EJECT                                                                
144600                                                                          
144700 DB2-DCL-OPN-T01DOTY-CRS-1 SECTION.                                       
144800     MOVE 000100 TO GOOD-SQLCODECODES                                     
144900                                                                          
145000     EXEC SQL                                                             
145100         DECLARE T01DOTY-CRS-1 CURSOR WITH HOLD FOR                       
145200                                                                          
145300           SELECT  A.KDFINDOC                                             
145400                 , B.KDPARTTY                                             
145500                 , B.KDPARTGR                                             
145600                 , A.KDSTATUS                                             
145700                 , A.DAREGDAT                                             
145800                 , A.DAUPPDAT                                             
145900                 , A.IDUSER                                               
146000                                                                          
146100           FROM    T01DOTY A                                              
146200                 , T01CUGR B                                              
146300                                                                          
146400           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
146500                AND A.IDLEGSEL = B.IDLEGSEL                               
146600                AND A.DADELDAT = '00000000'                               
146700                AND B.DADELDAT = '00000000'                               
146800                AND A.KDSTATUS = :WS-CURRENT                              
146900                AND B.KDSTATUS = :WS-CURRENT                              
147000                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
147100                                                                          
147200           ORDER BY A.IDLEGSEL                                            
147300                  , A.KDFINDOC                                            
147400                  , B.KDPARTTY                                            
147500                  , B.KDPARTGR                                            
147600     END-EXEC                                                             
147700                                                                          
147800     MOVE 000100  TO GOOD-SQLCODECODES                                    
147900                                                                          
148000     EXEC SQL                                                             
148100        OPEN T01DOTY-CRS-1                                                
148200     END-EXEC                                                             
148300                                                                          
148400     MOVE SQLCODE TO SQLCODE-WS                                           
148500     PERFORM DB2-STATUS-CHECK                                             
148600     .                                                                    
148700     EJECT                                                                
148800                                                                          
148900 DB2-FETCH-T01DOTY-CRS-1 SECTION.                                         
149000     MOVE 000100  TO GOOD-SQLCODECODES                                    
149100                                                                          
149200     EXEC SQL                                                             
149300                                                                          
149400         FETCH T01DOTY-CRS-1                                              
149500                                                                          
149600         INTO :MAP-KDFINDOC-LINE                                          
149700            , :MAP-KDPARTTY-LINE-2                                        
149800            , :MAP-KDPARTGR-LINE-2                                        
149900            , :MAP-KDSTATUS-LINE-2                                        
150000            , :MAP-DAREGDAT-LINE-2                                        
150100            , :MAP-DAUPPDAT-LINE-2                                        
150200            , :MAP-IDUSER-LINE-2                                          
150300     END-EXEC                                                             
150400                                                                          
150500     MOVE SQLCODE TO SQLCODE-WS                                           
150600     PERFORM DB2-STATUS-CHECK                                             
150700     .                                                                    
150800     EJECT                                                                
150900                                                                          
151000 DB2-CLOSE-T01DOTY-CRS-1 SECTION.                                         
151100     EXEC SQL                                                             
151200        CLOSE T01DOTY-CRS-1                                               
151300     END-EXEC                                                             
151400     .                                                                    
151500     EJECT                                                                
151600                                                                          
151700 DB2-DCL-OPN-T01BURE-CRS-3A SECTION.                                      
151800     MOVE 000100 TO GOOD-SQLCODECODES                                     
151900                                                                          
152000     EXEC SQL                                                             
152100         DECLARE T01BURE-CRS-3A CURSOR WITH HOLD FOR                      
152200                                                                          
152300          SELECT   DISTINCT A.KDFINDOC                                    
152400                 , B.KDPARTTY                                             
152500                 , B.KDPARTGR                                             
152600                 , A.KDSTATUS                                             
152700                 , B.KDSTATUS                                             
152800                 , B.DAREGDAT                                             
152900                 , B.DAUPPDAT                                             
153000                 , B.IDUSER                                               
153100                                                                          
153200          FROM     T01BURE A                                              
153300                 , T01CUGR B                                              
153400                                                                          
153500          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
153600             AND   A.IDLEGSEL = B.IDLEGSEL                                
153700             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
153800             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
153900             AND   B.KDSTATUS = :WS-CURRENT                               
154000             AND   B.DADELDAT = '00000000'                                
154100             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
154200             AND   A.IDLEGSEL = B.IDLEGSEL                                
154300             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
154400             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
154500             AND   B.KDSTATUS = :WS-CURRENT                               
154600             AND   B.DADELDAT = '00000000'                                
154700             AND ( B.KDPARTTY <> A.KDPARTTY                               
154800             OR    B.KDPARTGR <> A.KDPARTGR ))                            
154900             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
155000             AND   A.IDLEGSEL = B.IDLEGSEL                                
155100             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
155200             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
155300             AND   B.KDSTATUS = :WS-CURRENT                               
155400             AND   B.DADELDAT = '00000000'                                
155500             AND   B.KDPARTTY <> A.KDPARTTY                               
155600             AND   B.KDPARTGR <> A.KDPARTGR )                             
155700                                                                          
155800     END-EXEC                                                             
155900                                                                          
156000     MOVE 000100  TO GOOD-SQLCODECODES                                    
156100                                                                          
156200     EXEC SQL                                                             
156300        OPEN T01BURE-CRS-3A                                               
156400     END-EXEC                                                             
156500                                                                          
156600     MOVE SQLCODE TO SQLCODE-WS                                           
156700     PERFORM DB2-STATUS-CHECK                                             
156800     .                                                                    
156900     EJECT                                                                
157000                                                                          
157100 DB2-FETCH-T01BURE-CRS-3A SECTION.                                        
157200     MOVE 000100  TO GOOD-SQLCODECODES                                    
157300                                                                          
157400     EXEC SQL                                                             
157500                                                                          
157600         FETCH T01BURE-CRS-3A                                             
157700                                                                          
157800         INTO :MAP-KDFINDOC-LINE                                          
157900            , :MAP-KDPARTTY-LINE-2                                        
158000            , :MAP-KDPARTGR-LINE-2                                        
158100            , :MAP-KDSTATUS-LINE                                          
158200            , :MAP-KDSTATUS-LINE-2                                        
158300            , :MAP-DAREGDAT-LINE                                          
158400            , :MAP-DAREGDAT-LINE-2                                        
158500            , :MAP-DAUPPDAT-LINE                                          
158600            , :MAP-DAUPPDAT-LINE-2                                        
158700            , :MAP-IDUSER-LINE                                            
158800            , :MAP-IDUSER-LINE-2                                          
158900     END-EXEC                                                             
159000                                                                          
159100     MOVE SQLCODE TO SQLCODE-WS                                           
159200     PERFORM DB2-STATUS-CHECK                                             
159300     .                                                                    
159400     EJECT                                                                
159500                                                                          
159600 DB2-CLOSE-T01BURE-CRS-3A SECTION.                                        
159700     EXEC SQL                                                             
159800        CLOSE T01BURE-CRS-3A                                              
159900     END-EXEC                                                             
160000     .                                                                    
160100     EJECT                                                                
160200                                                                          
160300* * * * * * * * * *   - CURSOR-FAD-  * * * * * * * * * * * * *            
160400 DB2-COUNT-CRS-FAD SECTION.                                               
160500     EXEC SQL                                                             
160600                                                                          
160700          SELECT COUNT(*)                                                 
160800                                                                          
160900          INTO  :WS-COUNTER-FAD                                           
161000                                                                          
161100          FROM   T01CUGR A                                                
161200                                                                          
161300          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
161400             AND   KDPARTTY = :REQU-KDPARTTY-KEY                          
161500             AND   KDPARTGR = :REQU-KDPARTGR-KEY                          
161600             AND   KDSTATUS = :WS-CURRENT                                 
161700             AND   DADELDAT = '00000000'                                  
161800                                                                          
161900     END-EXEC                                                             
162000                                                                          
162100     MOVE 000100  TO GOOD-SQLCODECODES                                    
162200                                                                          
162300     MOVE SQLCODE TO SQLCODE-WS                                           
162400     PERFORM DB2-STATUS-CHECK                                             
162500     .                                                                    
162600     EJECT                                                                
162700                                                                          
162800 DB2-DCL-OPN-T01CUGR-CRS-2 SECTION.                                       
162900     MOVE 000100 TO GOOD-SQLCODECODES                                     
163000                                                                          
163100     EXEC SQL                                                             
163200         DECLARE T01CUGR-CRS-2 CURSOR WITH HOLD FOR                       
163300                                                                          
163400           SELECT  B.KDFINDOC                                             
163500                 , A.KDPARTTY                                             
163600                 , A.KDPARTGR                                             
163700                 , A.KDSTATUS                                             
163800                 , A.DAREGDAT                                             
163900                 , A.DAUPPDAT                                             
164000                 , A.IDUSER                                               
164100                                                                          
164200           FROM    T01CUGR A                                              
164300                 , T01DOTY B                                              
164400                                                                          
164500           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
164600                AND A.IDLEGSEL = B.IDLEGSEL                               
164700                AND A.DADELDAT = '00000000'                               
164800                AND B.DADELDAT = '00000000'                               
164900                AND A.KDSTATUS = :WS-CURRENT                              
165000                AND B.KDSTATUS = :WS-CURRENT                              
165100                AND ( A.KDPARTTY = :REQU-KDPARTTY-KEY                     
165200                OR  A.KDPARTGR = :REQU-KDPARTTY-KEY )                     
165300                                                                          
165400           ORDER BY A.IDLEGSEL                                            
165500                  , B.KDFINDOC                                            
165600                  , A.KDPARTTY                                            
165700                  , A.KDPARTGR                                            
165800     END-EXEC                                                             
165900                                                                          
166000     MOVE 000100  TO GOOD-SQLCODECODES                                    
166100                                                                          
166200     EXEC SQL                                                             
166300        OPEN T01CUGR-CRS-2                                                
166400     END-EXEC                                                             
166500                                                                          
166600     MOVE SQLCODE TO SQLCODE-WS                                           
166700     PERFORM DB2-STATUS-CHECK                                             
166800     .                                                                    
166900     EJECT                                                                
167000                                                                          
167100 DB2-FETCH-T01CUGR-CRS-2 SECTION.                                         
167200     MOVE 000100  TO GOOD-SQLCODECODES                                    
167300                                                                          
167400     EXEC SQL                                                             
167500                                                                          
167600         FETCH T01CUGR-CRS-2                                              
167700                                                                          
167800         INTO :MAP-KDFINDOC-LINE-2                                        
167900            , :MAP-KDPARTTY-LINE                                          
168000            , :MAP-KDPARTGR-LINE                                          
168100            , :MAP-KDSTATUS-LINE-2                                        
168200            , :MAP-DAREGDAT-LINE-2                                        
168300            , :MAP-DAUPPDAT-LINE-2                                        
168400            , :MAP-IDUSER-LINE-2                                          
168500     END-EXEC                                                             
168600                                                                          
168700     MOVE SQLCODE TO SQLCODE-WS                                           
168800     PERFORM DB2-STATUS-CHECK                                             
168900     .                                                                    
169000     EJECT                                                                
169100                                                                          
169200 DB2-CLOSE-T01CUGR-CRS-2 SECTION.                                         
169300     EXEC SQL                                                             
169400        CLOSE T01CUGR-CRS-2                                               
169500     END-EXEC                                                             
169600     .                                                                    
169700     EJECT                                                                
169800                                                                          
169900                                                                          
170000 DB2-DCL-OPN-T01BURE-CRS-4A SECTION.                                      
170100     MOVE 000100 TO GOOD-SQLCODECODES                                     
170200                                                                          
170300     EXEC SQL                                                             
170400         DECLARE T01BURE-CRS-4A CURSOR WITH HOLD FOR                      
170500                                                                          
170600          SELECT   DISTINCT B.KDFINDOC                                    
170700                 , A.KDPARTTY                                             
170800                 , A.KDPARTGR                                             
170900                 , A.KDSTATUS                                             
171000                 , B.KDSTATUS                                             
171100                 , B.DAREGDAT                                             
171200                 , B.DAUPPDAT                                             
171300                 , B.IDUSER                                               
171400                                                                          
171500          FROM     T01BURE A                                              
171600                 , T01DOTY B                                              
171700                                                                          
171800          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
171900             AND   A.IDLEGSEL = B.IDLEGSEL                                
172000             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
172100             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
172200             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
172300             AND   B.DADELDAT = '00000000'                                
172400             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
172500             AND   A.IDLEGSEL = B.IDLEGSEL                                
172600             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
172700             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
172800             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
172900             AND   B.DADELDAT = '00000000'                                
173000             AND ( A.KDFINDOC <> B.KDFINDOC                               
173100             OR    A.KDFINDOC =  B.KDFINDOC ))                            
173200             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
173300             AND   A.IDLEGSEL = B.IDLEGSEL                                
173400             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
173500             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
173600             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
173700             AND   B.DADELDAT = '00000000'                                
173800             AND   A.KDFINDOC <> B.KDFINDOC                               
173900             AND   A.KDFINDOC =  B.KDFINDOC )                             
174000                                                                          
174100     END-EXEC                                                             
174200                                                                          
174300     MOVE 000100  TO GOOD-SQLCODECODES                                    
174400                                                                          
174500     EXEC SQL                                                             
174600        OPEN T01BURE-CRS-4A                                               
174700     END-EXEC                                                             
174800                                                                          
174900     MOVE SQLCODE TO SQLCODE-WS                                           
175000     PERFORM DB2-STATUS-CHECK                                             
175100     .                                                                    
175200     EJECT                                                                
175300                                                                          
175400 DB2-FETCH-T01BURE-CRS-4A SECTION.                                        
175500     MOVE 000100  TO GOOD-SQLCODECODES                                    
175600                                                                          
175700     EXEC SQL                                                             
175800                                                                          
175900         FETCH T01BURE-CRS-4A                                             
176000                                                                          
176100         INTO :MAP-KDFINDOC-LINE-2                                        
176200            , :MAP-KDPARTTY-LINE                                          
176300            , :MAP-KDPARTGR-LINE                                          
176400            , :MAP-KDSTATUS-LINE                                          
176500            , :MAP-KDSTATUS-LINE-2                                        
176600            , :MAP-DAREGDAT-LINE                                          
176700            , :MAP-DAREGDAT-LINE-2                                        
176800            , :MAP-DAUPPDAT-LINE                                          
176900            , :MAP-DAUPPDAT-LINE-2                                        
177000            , :MAP-IDUSER-LINE                                            
177100            , :MAP-IDUSER-LINE-2                                          
177200     END-EXEC                                                             
177300                                                                          
177400     MOVE SQLCODE TO SQLCODE-WS                                           
177500     PERFORM DB2-STATUS-CHECK                                             
177600     .                                                                    
177700     EJECT                                                                
177800                                                                          
177900 DB2-CLOSE-T01BURE-CRS-4A SECTION.                                        
178000     EXEC SQL                                                             
178100        CLOSE T01BURE-CRS-4A                                              
178200     END-EXEC                                                             
178300     .                                                                    
178400     EJECT                                                                
178500                                                                          
178600* * * * * * * * * *   - CURSOR-FAE-  * * * * * * * * * * * * *            
178700 DB2-COUNT-CRS-FAE SECTION.                                               
178800     EXEC SQL                                                             
178900                                                                          
179000          SELECT COUNT(*)                                                 
179100                                                                          
179200          INTO  :WS-COUNTER-FAE                                           
179300                                                                          
179400          FROM   T01CUGR A                                                
179500                                                                          
179600          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
179700             AND   KDPARTTY = :REQU-KDPARTTY-KEY                          
179800             AND   KDSTATUS = :WS-CURRENT                                 
179900             AND   DADELDAT = '00000000'                                  
180000                                                                          
180100     END-EXEC                                                             
180200                                                                          
180300     MOVE 000100  TO GOOD-SQLCODECODES                                    
180400                                                                          
180500     MOVE SQLCODE TO SQLCODE-WS                                           
180600     PERFORM DB2-STATUS-CHECK                                             
180700     .                                                                    
180800     EJECT                                                                
180900                                                                          
181000 DB2-DCL-OPN-T01CUGR-CRS-1 SECTION.                                       
181100     MOVE 000100 TO GOOD-SQLCODECODES                                     
181200                                                                          
181300     EXEC SQL                                                             
181400         DECLARE T01CUGR-CRS-1 CURSOR WITH HOLD FOR                       
181500                                                                          
181600           SELECT  B.KDFINDOC                                             
181700                 , A.KDPARTTY                                             
181800                 , A.KDPARTGR                                             
181900                 , A.KDSTATUS                                             
182000                 , A.DAREGDAT                                             
182100                 , A.DAUPPDAT                                             
182200                 , A.IDUSER                                               
182300                                                                          
182400           FROM    T01CUGR A                                              
182500                 , T01DOTY B                                              
182600                                                                          
182700           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
182800                AND A.IDLEGSEL = B.IDLEGSEL                               
182900                AND A.KDPARTTY = :REQU-KDPARTTY-KEY                       
183000                AND A.DADELDAT = '00000000'                               
183100                AND B.DADELDAT = '00000000'                               
183200                AND A.KDSTATUS = :WS-CURRENT                              
183300                AND B.KDSTATUS = :WS-CURRENT                              
183400                                                                          
183500           ORDER BY A.IDLEGSEL                                            
183600                  , B.KDFINDOC                                            
183700                  , A.KDPARTTY                                            
183800                  , A.KDPARTGR                                            
183900     END-EXEC                                                             
184000                                                                          
184100     MOVE 000100  TO GOOD-SQLCODECODES                                    
184200                                                                          
184300     EXEC SQL                                                             
184400        OPEN T01CUGR-CRS-1                                                
184500     END-EXEC                                                             
184600                                                                          
184700     MOVE SQLCODE TO SQLCODE-WS                                           
184800     PERFORM DB2-STATUS-CHECK                                             
184900     .                                                                    
185000     EJECT                                                                
185100                                                                          
185200 DB2-FETCH-T01CUGR-CRS-1 SECTION.                                         
185300     MOVE 000100  TO GOOD-SQLCODECODES                                    
185400                                                                          
185500     EXEC SQL                                                             
185600                                                                          
185700         FETCH T01CUGR-CRS-1                                              
185800                                                                          
185900         INTO :MAP-KDFINDOC-LINE-2                                        
186000            , :MAP-KDPARTTY-LINE                                          
186100            , :MAP-KDPARTGR-LINE-2                                        
186200            , :MAP-KDSTATUS-LINE-2                                        
186300            , :MAP-DAREGDAT-LINE-2                                        
186400            , :MAP-DAUPPDAT-LINE-2                                        
186500            , :MAP-IDUSER-LINE-2                                          
186600     END-EXEC                                                             
186700                                                                          
186800     MOVE SQLCODE TO SQLCODE-WS                                           
186900     PERFORM DB2-STATUS-CHECK                                             
187000     .                                                                    
187100     EJECT                                                                
187200                                                                          
187300 DB2-CLOSE-T01CUGR-CRS-1 SECTION.                                         
187400     EXEC SQL                                                             
187500        CLOSE T01CUGR-CRS-1                                               
187600     END-EXEC                                                             
187700     .                                                                    
187800     EJECT                                                                
187900                                                                          
188000 DB2-DCL-OPN-T01BURE-CRS-5A SECTION.                                      
188100     MOVE 000100 TO GOOD-SQLCODECODES                                     
188200                                                                          
188300     EXEC SQL                                                             
188400         DECLARE T01BURE-CRS-5A CURSOR WITH HOLD FOR                      
188500                                                                          
188600          SELECT   DISTINCT B.KDFINDOC                                    
188700                 , A.KDPARTTY                                             
188800                 , A.KDPARTGR                                             
188900                 , A.KDSTATUS                                             
189000                 , B.KDSTATUS                                             
189100                 , B.DAREGDAT                                             
189200                 , B.DAUPPDAT                                             
189300                 , B.IDUSER                                               
189400                                                                          
189500          FROM     T01BURE A                                              
189600                 , T01DOTY B                                              
189700                                                                          
189800          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
189900             AND   A.IDLEGSEL = B.IDLEGSEL                                
190000             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
190100             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
190200             AND   B.DADELDAT = '00000000'                                
190300             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
190400             AND   A.IDLEGSEL = B.IDLEGSEL                                
190500             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
190600             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
190700             AND   B.DADELDAT = '00000000'                                
190800             AND ( A.KDFINDOC <> B.KDFINDOC                               
190900             OR    A.KDFINDOC =  B.KDFINDOC ))                            
191000             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
191100             AND   A.IDLEGSEL = B.IDLEGSEL                                
191200             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
191300             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
191400             AND   B.DADELDAT = '00000000'                                
191500             AND   A.KDFINDOC <> B.KDFINDOC                               
191600             AND   A.KDFINDOC =  B.KDFINDOC )                             
191700                                                                          
191800     END-EXEC                                                             
191900                                                                          
192000     MOVE 000100  TO GOOD-SQLCODECODES                                    
192100                                                                          
192200     EXEC SQL                                                             
192300        OPEN T01BURE-CRS-5A                                               
192400     END-EXEC                                                             
192500                                                                          
192600     MOVE SQLCODE TO SQLCODE-WS                                           
192700     PERFORM DB2-STATUS-CHECK                                             
192800     .                                                                    
192900     EJECT                                                                
193000                                                                          
193100 DB2-FETCH-T01BURE-CRS-5A SECTION.                                        
193200     MOVE 000100  TO GOOD-SQLCODECODES                                    
193300                                                                          
193400     EXEC SQL                                                             
193500                                                                          
193600         FETCH T01BURE-CRS-5A                                             
193700                                                                          
193800         INTO :MAP-KDFINDOC-LINE-2                                        
193900            , :MAP-KDPARTTY-LINE                                          
194000            , :MAP-KDPARTGR-LINE-2                                        
194100            , :MAP-KDSTATUS-LINE                                          
194200            , :MAP-KDSTATUS-LINE-2                                        
194300            , :MAP-DAREGDAT-LINE                                          
194400            , :MAP-DAREGDAT-LINE-2                                        
194500            , :MAP-DAUPPDAT-LINE                                          
194600            , :MAP-DAUPPDAT-LINE-2                                        
194700            , :MAP-IDUSER-LINE                                            
194800            , :MAP-IDUSER-LINE-2                                          
194900     END-EXEC                                                             
195000                                                                          
195100     MOVE SQLCODE TO SQLCODE-WS                                           
195200     PERFORM DB2-STATUS-CHECK                                             
195300     .                                                                    
195400     EJECT                                                                
195500                                                                          
195600 DB2-CLOSE-T01BURE-CRS-5A SECTION.                                        
195700     EXEC SQL                                                             
195800        CLOSE T01BURE-CRS-5A                                              
195900     END-EXEC                                                             
196000     .                                                                    
196100     EJECT                                                                
196200                                                                          
196300* * * * * * * * * *   - CURSOR-FAF-  * * * * * * * * * * * * *            
196400 DB2-COUNT-CRS-FAF SECTION.                                               
196500     EXEC SQL                                                             
196600                                                                          
196700          SELECT COUNT(*)                                                 
196800                                                                          
196900          INTO  :WS-COUNTER-FAF                                           
197000                                                                          
197100          FROM   T01DOTY                                                  
197200                                                                          
197300          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
197400             AND   KDSTATUS = :WS-CURRENT                                 
197500             AND   DADELDAT = '00000000'                                  
197600                                                                          
197700     END-EXEC                                                             
197800                                                                          
197900     MOVE 000100  TO GOOD-SQLCODECODES                                    
198000                                                                          
198100     MOVE SQLCODE TO SQLCODE-WS                                           
198200     PERFORM DB2-STATUS-CHECK                                             
198300     .                                                                    
198400     EJECT                                                                
198500                                                                          
198600 DB2-DCL-OPN-T01BURE-CRS-6A SECTION.                                      
198700     MOVE 000100 TO GOOD-SQLCODECODES                                     
198800                                                                          
198900     EXEC SQL                                                             
199000         DECLARE T01BURE-CRS-6A CURSOR WITH HOLD FOR                      
199100                                                                          
199200          SELECT   DISTINCT A.KDFINDOC                                    
199300                 , B.KDPARTTY                                             
199400                 , B.KDPARTGR                                             
199500                 , A.KDSTATUS                                             
199600                 , B.KDSTATUS                                             
199700                 , B.DAREGDAT                                             
199800                 , B.DAUPPDAT                                             
199900                 , B.IDUSER                                               
200000                                                                          
200100          FROM     T01BURE A                                              
200200                 , T01CUGR B                                              
200300                 , T01DOTY C                                              
200400                                                                          
200500          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
200600             AND   A.IDLEGSEL = B.IDLEGSEL                                
200700             AND   A.IDLEGSEL = C.IDLEGSEL                                
200800             AND   A.KDFINDOC = C.KDFINDOC                                
200900             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
201000             AND   B.KDSTATUS = :WS-CURRENT                               
201100             AND   B.DADELDAT = '00000000'                                
201200             AND   C.KDSTATUS = :WS-CURRENT                               
201300             AND   C.DADELDAT = '00000000'                                
201400             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
201500             AND   A.IDLEGSEL = B.IDLEGSEL                                
201600             AND   A.IDLEGSEL = C.IDLEGSEL                                
201700             AND   A.KDFINDOC = C.KDFINDOC                                
201800             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
201900             AND   B.KDSTATUS = :WS-CURRENT                               
202000             AND   B.DADELDAT = '00000000'                                
202100             AND   C.KDSTATUS = :WS-CURRENT                               
202200             AND   C.DADELDAT = '00000000'                                
202300             AND ( B.KDPARTTY <> A.KDPARTTY                               
202400             OR    B.KDPARTGR <> A.KDPARTGR ))                            
202500             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
202600             AND   A.IDLEGSEL = B.IDLEGSEL                                
202700             AND   A.IDLEGSEL = C.IDLEGSEL                                
202800             AND   A.KDFINDOC = C.KDFINDOC                                
202900             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
203000             AND   B.KDSTATUS = :WS-CURRENT                               
203100             AND   B.DADELDAT = '00000000'                                
203200             AND   C.KDSTATUS = :WS-CURRENT                               
203300             AND   C.DADELDAT = '00000000'                                
203400             AND   B.KDPARTTY <> A.KDPARTTY                               
203500             AND   B.KDPARTGR <> A.KDPARTGR )                             
203600                                                                          
203700     END-EXEC                                                             
203800                                                                          
203900     MOVE 000100  TO GOOD-SQLCODECODES                                    
204000                                                                          
204100     EXEC SQL                                                             
204200        OPEN T01BURE-CRS-6A                                               
204300     END-EXEC                                                             
204400                                                                          
204500     MOVE SQLCODE TO SQLCODE-WS                                           
204600     PERFORM DB2-STATUS-CHECK                                             
204700     .                                                                    
204800     EJECT                                                                
204900                                                                          
205000 DB2-FETCH-T01BURE-CRS-6A SECTION.                                        
205100     MOVE 000100  TO GOOD-SQLCODECODES                                    
205200                                                                          
205300     EXEC SQL                                                             
205400                                                                          
205500         FETCH T01BURE-CRS-6A                                             
205600                                                                          
205700         INTO :MAP-KDFINDOC-LINE                                          
205800            , :MAP-KDPARTTY-LINE-2                                        
205900            , :MAP-KDPARTGR-LINE-2                                        
206000            , :MAP-KDSTATUS-LINE                                          
206100            , :MAP-KDSTATUS-LINE-2                                        
206200            , :MAP-DAREGDAT-LINE                                          
206300            , :MAP-DAREGDAT-LINE-2                                        
206400            , :MAP-DAUPPDAT-LINE                                          
206500            , :MAP-DAUPPDAT-LINE-2                                        
206600            , :MAP-IDUSER-LINE                                            
206700            , :MAP-IDUSER-LINE-2                                          
206800     END-EXEC                                                             
206900                                                                          
207000     MOVE SQLCODE TO SQLCODE-WS                                           
207100     PERFORM DB2-STATUS-CHECK                                             
207200     .                                                                    
207300     EJECT                                                                
207400                                                                          
207500 DB2-CLOSE-T01BURE-CRS-6A SECTION.                                        
207600     EXEC SQL                                                             
207700        CLOSE T01BURE-CRS-6A                                              
207800     END-EXEC                                                             
207900     .                                                                    
208000     EJECT                                                                
208100                                                                          
208200 DB2-DCL-OPN-T01DOTY-CRS-6B SECTION.                                      
208300     MOVE 000100 TO GOOD-SQLCODECODES                                     
208400                                                                          
208500     EXEC SQL                                                             
208600         DECLARE T01DOTY-CRS-6B CURSOR WITH HOLD FOR                      
208700                                                                          
208800           SELECT  A.KDFINDOC                                             
208900                 , B.KDPARTTY                                             
209000                 , B.KDPARTGR                                             
209100                 , A.KDSTATUS                                             
209200                 , A.DAREGDAT                                             
209300                 , A.DAUPPDAT                                             
209400                 , A.IDUSER                                               
209500                                                                          
209600           FROM    T01DOTY A                                              
209700                 , T01CUGR B                                              
209800                                                                          
209900           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
210000                AND A.IDLEGSEL = B.IDLEGSEL                               
210100                AND A.DADELDAT = '00000000'                               
210200                AND B.DADELDAT = '00000000'                               
210300                AND A.KDSTATUS = :WS-CURRENT                              
210400                AND B.KDSTATUS = :WS-CURRENT                              
210500                                                                          
210600           ORDER BY A.IDLEGSEL                                            
210700                  , A.KDFINDOC                                            
210800                  , B.KDPARTTY                                            
210900                  , B.KDPARTGR                                            
211000     END-EXEC                                                             
211100                                                                          
211200     MOVE 000100  TO GOOD-SQLCODECODES                                    
211300                                                                          
211400     EXEC SQL                                                             
211500        OPEN T01DOTY-CRS-6B                                               
211600     END-EXEC                                                             
211700                                                                          
211800     MOVE SQLCODE TO SQLCODE-WS                                           
211900     PERFORM DB2-STATUS-CHECK                                             
212000     .                                                                    
212100     EJECT                                                                
212200                                                                          
212300 DB2-FETCH-T01DOTY-CRS-6B SECTION.                                        
212400     MOVE 000100  TO GOOD-SQLCODECODES                                    
212500                                                                          
212600     EXEC SQL                                                             
212700                                                                          
212800         FETCH T01DOTY-CRS-6B                                             
212900                                                                          
213000         INTO :MAP-KDFINDOC-LINE                                          
213100            , :MAP-KDPARTTY-LINE-2                                        
213200            , :MAP-KDPARTGR-LINE-2                                        
213300            , :MAP-KDSTATUS-LINE-2                                        
213400            , :MAP-DAREGDAT-LINE-2                                        
213500            , :MAP-DAUPPDAT-LINE-2                                        
213600            , :MAP-IDUSER-LINE-2                                          
213700     END-EXEC                                                             
213800                                                                          
213900     MOVE SQLCODE TO SQLCODE-WS                                           
214000     PERFORM DB2-STATUS-CHECK                                             
214100     .                                                                    
214200     EJECT                                                                
214300                                                                          
214400 DB2-CLOSE-T01DOTY-CRS-6B SECTION.                                        
214500     EXEC SQL                                                             
214600        CLOSE T01DOTY-CRS-6B                                              
214700     END-EXEC                                                             
214800     .                                                                    
214900     EJECT                                                                
215000                                                                          
215100 DB2-STATUS-CHECK  SECTION.                                               
215200     SET SQLCODE-IX TO 1                                                  
215300     SEARCH GOOD-SQLCODE                                                  
215400       AT END                                                             
215500          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
215600          DELIMITED BY SIZE INTO ERROR-TEXT                               
215700          CALL ABEND USING RKOD-ABEND-DB2                                 
215800       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
215900          CONTINUE                                                        
216000     END-SEARCH                                                           
216100     .                                                                    
