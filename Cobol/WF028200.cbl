000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF028200.                                                
000400 AUTHOR.         ANDERS HENRIKSSON.                                       
000500 DATE-WRITTEN.   04/11/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.PAYINSTRLOCATE                                   
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE T01LSEL                                
001500*        THE PROGRAM READS   TABLE T01CURR                                
001600*        THE PROGRAM READS   TABLE T01CUGR                                
001700*        THE PROGRAM READS   TABLE T01DOTY                                
001800*        THE PROGRAM READS   TABLE T01PAIN                                
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: WF0282T                                             
002200*        REQUEST:     WZ01REQU                                            
002300*                     WF0282I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    WZ01RESP                                            
002700*                     WF0282O1                                            
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200 77  IDPGM                       PIC X(08)  VALUE 'WF028200'.             
003300                                                                          
003400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700                                                                          
003800*    --- CONSTANTS                                                        
003900 77  YES                         PIC X      VALUE 'Y'.                    
004000 77  NOO                         PIC X      VALUE 'N'.                    
004100                                                                          
004200 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
004300 77  WS-CURRENT                  PIC S9(3)  VALUE +001    COMP-3.         
004400 77  WS-COMING                   PIC S9(3)  VALUE +002    COMP-3.         
004500 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004600 77  WS-NOT-REG                  PIC X(8)   VALUE 'NOT REG.'.             
004700 77  WS-ADRESS                   PIC X(50)                                
004800                    VALUE 'CARPARTS.BILLIT.PAYINSTRLOCATE'.               
004900                                                                          
005000 77  KEYS-SW                     PIC X      VALUE SPACE.                  
005100     88  KEYS-OK                            VALUE 'Y'.                    
005200     88  KEYS-WRONG                         VALUE 'N'.                    
005300                                                                          
005400*    --- WORK FIELDS                                                      
005500 01  WS-IX-MOD                   PIC S9(4)  VALUE ZERO    BINARY.         
005600 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
005700 01  WS-COUNTER-T01PAIN          PIC S9(5)  VALUE ZERO    COMP-3.         
005800 01  WS-COUNTER-FAB              PIC S9(5)  VALUE ZERO    COMP-3.         
005900 01  WS-COUNTER-FAC              PIC S9(5)  VALUE ZERO    COMP-3.         
006000 01  WS-COUNTER-FAD              PIC S9(5)  VALUE ZERO    COMP-3.         
006100 01  WS-COUNTER-FAE              PIC S9(5)  VALUE ZERO    COMP-3.         
006200 01  WS-COUNTER-FAF              PIC S9(5)  VALUE ZERO    COMP-3.         
006300 01  WS-COUNTER-FAG              PIC S9(5)  VALUE ZERO    COMP-3.         
006400 01  WS-COUNTER-FAH              PIC S9(5)  VALUE ZERO    COMP-3.         
006500 01  WS-COUNTER-FAI              PIC S9(5)  VALUE ZERO    COMP-3.         
006600 01  WS-COUNTER-FAJ              PIC S9(5)  VALUE ZERO    COMP-3.         
006700 01  WS-COUNTER-FAK              PIC S9(5)  VALUE ZERO    COMP-3.         
006800 01  WS-COUNTER-FAL              PIC S9(5)  VALUE ZERO    COMP-3.         
006900 01  WS-KEY1-MARKED              PIC X(1)   VALUE SPACE.                  
007000 01  WS-BELEGRAD-1               PIC X(35)  VALUE SPACE.                  
007100 01  WS-IDLEGSEL                 PIC X(4)   VALUE SPACE.                  
007200 01  WS-KDFINDOC                 PIC X(4)   VALUE SPACE.                  
007300 01  WS-KDVALISO                 PIC X(3)   VALUE SPACE.                  
007400                                                                          
007500*    --- MAPPING FIELDS                                                   
007600 01  MAP-KDFINDOC-LINE           PIC X(4)   VALUE SPACE.                  
007700 01  MAP-KDFINDOC-LINE-2         PIC X(4)   VALUE SPACE.                  
007800 01  MAP-KDPARTTY-LINE           PIC X(3)   VALUE SPACE.                  
007900 01  MAP-KDPARTTY-LINE-2         PIC X(3)   VALUE SPACE.                  
008000 01  MAP-KDPARTGR-LINE           PIC X(15)  VALUE SPACE.                  
008100 01  MAP-KDPARTGR-LINE-2         PIC X(15)  VALUE SPACE.                  
008200 01  MAP-KDVALISO-LINE           PIC X(3)   VALUE SPACE.                  
008300 01  MAP-KDVALISO-LINE-2         PIC X(3)   VALUE SPACE.                  
008400 01  MAP-KDSTATUS-LINE           PIC S9(3)  VALUE ZERO    COMP-3.         
008500 01  MAP-KDSTATUS-LINE-2         PIC S9(3)  VALUE ZERO    COMP-3.         
008600 01  MAP-DAREGDAT-LINE           PIC X(8)   VALUE SPACE.                  
008700 01  MAP-DAREGDAT-LINE-2         PIC X(8)   VALUE SPACE.                  
008800 01  MAP-DAUPPDAT-LINE           PIC X(8)   VALUE SPACE.                  
008900 01  MAP-DAUPPDAT-LINE-2         PIC X(8)   VALUE SPACE.                  
009000 01  MAP-IDUSER-LINE             PIC X(8)   VALUE SPACE.                  
009100 01  MAP-IDUSER-LINE-2           PIC X(8)   VALUE SPACE.                  
009200 01  MAP-IDLEGSEL-LINE           PIC X(4)   VALUE SPACE.                  
009300                                                                          
009400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009500 01  GENERAL-SUBPROGRAMS.                                                 
009600     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
009700     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
009800                                                                          
009900*    --- PARAMETERS TO ABEND                                              
010000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010300 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
010400                                                                          
010500 01  MESSAGE-CODES.                                                       
010600     03  ERROR-CODES.                                                     
010700         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
010800         05  IS-INVALID              PIC X(3)   VALUE '023'.              
010900         05  MUST-BE-NUMERIC         PIC X(3)   VALUE '024'.              
011000         05  NOT-FOUND               PIC X(3)   VALUE '025'.              
011100         05  MUST-BE-ENTERED         PIC X(3)   VALUE '026'.              
011200         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
011300         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
011400         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
011500     03  INFO-CODES.                                                      
011600         05  INF-MORE-LINES-EXIST    PIC X(3)   VALUE '104'.              
011700     EJECT                                                                
011800                                                                          
011900*01  -COPY WZ01SUB                                                        
012000     EJECT                                                                
012100*                                                                         
012200 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
012300 01  REQU-AREA.                                                           
012400*    03 -COPY WZ01REQU                                                    
012500*    03 -COPY WF0282I1                                                    
012600     EJECT                                                                
012700                                                                          
012800 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
012900 01  RESP-AREA.                                                           
013000*    03 -COPY WZ01RESP                                                    
013100*    03 -COPY WF0282O1                                                    
013200     EJECT                                                                
013300                                                                          
013400 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
013500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013600                                                                          
013700 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
013800 01  DB2-WS.                                                              
013900     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
014000         88  CURSOR-OK                      VALUE 000.                    
014100         88  LINES-FOUND                    VALUE 000.                    
014200         88  LINES-MISSING                  VALUE 100.                    
014300         88  RESOURCE-WRONG                 VALUE 904.                    
014400                                                                          
014500     03  GOOD-SQLCODECODES.                                               
014600         05  GOOD-SQLCODE OCCURS 5                                        
014700             INDEXED BY SQLCODE-IX PIC 9(3).                              
014800                                                                          
014900 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
015000*01  -COPY T01LSEL -PRE T01LSEL-                                          
015100     EJECT                                                                
015200                                                                          
015300 01  FILLER                      PIC X(16)   VALUE 'T01CUGR-AREA'.        
015400*01  -COPY T01CUGR -PRE T01CUGR-                                          
015500     EJECT                                                                
015600                                                                          
015700 01  FILLER                      PIC X(16)   VALUE 'T01CURR-AREA'.        
015800*01  -COPY T01CURR -PRE T01CURR-                                          
015900     EJECT                                                                
016000                                                                          
016100 01  FILLER                      PIC X(16)   VALUE 'T01DOTY-AREA'.        
016200*01  -COPY T01DOTY -PRE T01DOTY-                                          
016300     EJECT                                                                
016400                                                                          
016500 01  FILLER                      PIC X(16)   VALUE 'T01PAIN-AREA'.        
016600*01  -COPY T01PAIN -PRE T01PAIN-                                          
016700     EJECT                                                                
016800                                                                          
016900     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
017000     EJECT                                                                
017100     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
017200     EJECT                                                                
017300     EXEC SQL INCLUDE T01CURR END-EXEC.                                   
017400     EJECT                                                                
017500     EXEC SQL INCLUDE T01DOTY END-EXEC.                                   
017600     EJECT                                                                
017700     EXEC SQL INCLUDE T01PAIN END-EXEC.                                   
017800     EJECT                                                                
017900                                                                          
018000 LINKAGE SECTION.                                                         
018100 PROCEDURE DIVISION.                                                      
018200 MAIN SECTION.                                                            
018300                                                                          
018400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
018500     IF SUB-KDRC = ZERO                                                   
018600       PERFORM A-INIT                                                     
018700       PERFORM B-CHECK-KEYS                                               
018800       IF KEYS-OK                                                         
018900         PERFORM F-READ-SHOW-INFO                                         
019000       END-IF                                                             
019100       PERFORM S02-RETURN-RESPONSE                                        
019200     END-IF                                                               
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700                                                                          
019800 A-INIT SECTION.                                                          
019900     INITIALIZE GOOD-SQLCODECODES                                         
020000     MOVE ALL '+' TO RESP-AREA                                            
020100     MOVE SPACE TO RESP-IDMSG-ERROR                                       
020200     MOVE SPACE TO RESP-IDMSG-INFO                                        
020300     MOVE SPACE TO RESP-IDELMT-ERROR                                      
020400     MOVE ZERO                  TO WS-COUNTER-T01PAIN                     
020500                                   WS-COUNTER-FAB                         
020600                                   WS-COUNTER-FAC                         
020700                                   WS-COUNTER-FAD                         
020800                                   WS-COUNTER-FAE                         
020900                                   WS-COUNTER-FAF                         
021000                                   WS-COUNTER-FAG                         
021100                                   WS-COUNTER-FAH                         
021200                                   WS-COUNTER-FAI                         
021300                                   WS-COUNTER-FAJ                         
021400                                   WS-COUNTER-FAK                         
021500                                   WS-COUNTER-FAL                         
021600                                   RESP-KVRADER                           
021700     .                                                                    
021800     EJECT                                                                
021900                                                                          
022000*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
022100 B-CHECK-KEYS SECTION.                                                    
022200     MOVE YES TO KEYS-SW                                                  
022300     IF REQU-KDPGMACT = WS-SEARCH                                         
022400     AND REQU-IDMSGVER NUMERIC                                            
022500     AND REQU-IDLEGSEL-KEY > SPACE                                        
022600       CONTINUE                                                           
022700     ELSE                                                                 
022800       MOVE NOO TO KEYS-SW                                                
022900     END-IF                                                               
023000                                                                          
023100     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
023200       MOVE NOO TO KEYS-SW                                                
023300     ELSE                                                                 
023400       CONTINUE                                                           
023500     END-IF                                                               
023600                                                                          
023700     IF KEYS-WRONG                                                        
023800       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
023900       IF REQU-KDPGMACT = 'S'                                             
024000         CONTINUE                                                         
024100       ELSE                                                               
024200         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
024300         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
024400       END-IF                                                             
024500       IF REQU-IDMSGVER NUMERIC                                           
024600         CONTINUE                                                         
024700       ELSE                                                               
024800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
024900         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
025000       END-IF                                                             
025100       IF REQU-IDUSER = SPACE OR = ALL '+'                                
025200         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
025300         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
025400       ELSE                                                               
025500         CONTINUE                                                         
025600       END-IF                                                             
025700     END-IF                                                               
025800                                                                          
025900     IF KEYS-OK                                                           
026000       PERFORM DB2-SELECT-T01LSEL-TAB                                     
026100       IF LINES-FOUND                                                     
026200         CONTINUE                                                         
026300       ELSE                                                               
026400         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
026500         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
026600         MOVE NOO TO KEYS-SW                                              
026700       END-IF                                                             
026800     END-IF                                                               
026900                                                                          
027000     PERFORM BB-CHECK-KEY-RELATION                                        
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400*** - CHECK RELATION BETWEEN REQUSTED KEYS                                
027500 BB-CHECK-KEY-RELATION SECTION.                                           
027600     IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                            
027700       IF REQU-KDFINDOC-KEY = SPACE OR = ALL '+'                          
027800         IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                        
027900*          MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
028000*          MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                      
028100*          MOVE NOO TO KEYS-SW                                            
028200           CONTINUE                                                       
028300         ELSE                                                             
028400           IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                      
028500             CONTINUE                                                     
028600           ELSE                                                           
028700             MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
028800             MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                    
028900             MOVE NOO TO KEYS-SW                                          
029000           END-IF                                                         
029100         END-IF                                                           
029200       ELSE                                                               
029300         IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                        
029400           CONTINUE                                                       
029500         ELSE                                                             
029600           MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
029700           MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                      
029800           MOVE NOO TO KEYS-SW                                            
029900         END-IF                                                           
030000       END-IF                                                             
030100     ELSE                                                                 
030200       IF REQU-KDFINDOC-KEY = SPACE OR = ALL '+'                          
030300         IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                        
030400           IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                      
030500             MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
030600             MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                    
030700             MOVE NOO TO KEYS-SW                                          
030800           ELSE                                                           
030900             IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                    
031000               MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
031100               MOVE 'KDPARTGR'      TO RESP-IDELMT-ERROR                  
031200               MOVE NOO TO KEYS-SW                                        
031300             ELSE                                                         
031400               MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
031500               MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                  
031600               MOVE NOO TO KEYS-SW                                        
031700             END-IF                                                       
031800           END-IF                                                         
031900         ELSE                                                             
032000           IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                      
032100             MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
032200             MOVE 'KDPARTGR'      TO RESP-IDELMT-ERROR                    
032300             MOVE NOO TO KEYS-SW                                          
032400           END-IF                                                         
032500         END-IF                                                           
032600       ELSE                                                               
032700         IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                        
032800           MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
032900           MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                      
033000           MOVE NOO TO KEYS-SW                                            
033100         ELSE                                                             
033200           CONTINUE                                                       
033300         END-IF                                                           
033400       END-IF                                                             
033500     END-IF                                                               
033600                                                                          
033700     IF KEYS-OK                                                           
033800       IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                          
033900         CONTINUE                                                         
034000       ELSE                                                               
034100         PERFORM DB2-SELECT-T01CURR-TAB                                   
034200         IF LINES-FOUND                                                   
034300           CONTINUE                                                       
034400         ELSE                                                             
034500           MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                            
034600           MOVE 'KDVALISO' TO RESP-IDELMT-ERROR                           
034700           MOVE NOO TO KEYS-SW                                            
034800         END-IF                                                           
034900       END-IF                                                             
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300                                                                          
035400*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
035500 F-READ-SHOW-INFO SECTION.                                                
035600     MOVE REQU-IDLEGSEL-KEY TO RESP-IDLEGSEL-KEY                          
035700     MOVE REQU-KDFINDOC-KEY TO RESP-KDFINDOC-KEY                          
035800     MOVE REQU-KDVALISO-KEY TO RESP-KDVALISO-KEY                          
035900     MOVE REQU-KDPARTTY-KEY TO RESP-KDPARTTY-KEY                          
036000     MOVE REQU-KDPARTGR-KEY TO RESP-KDPARTGR-KEY                          
036100     MOVE REQU-IDMSGVER     TO RESP-IDMSGVER                              
036200     MOVE WS-BELEGRAD-1     TO RESP-BELEGRAD-1                            
036300                                                                          
036400     PERFORM FA-READ-BASICDATA                                            
036500     .                                                                    
036600     EJECT                                                                
036700                                                                          
036800*** - CHECK WHICH REQUESTED KEY                                           
036900 FA-READ-BASICDATA SECTION.                                               
037000     MOVE ZERO TO WS-COUNTER-T01PAIN                                      
037100     MOVE SPACE TO WS-KEY1-MARKED                                         
037200     IF REQU-KDFINDOC-KEY = SPACE OR = ALL '+'                            
037300       IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                          
037400         IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                        
037500*          MOVE NOO TO KEYS-SW                                            
037600*          MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                   
037700           PERFORM FAL-HANDLE-BLANK-KEYS                                  
037800         ELSE                                                             
037900           PERFORM FAK-HANDLE-KDVALISO-KEY                                
038000         END-IF                                                           
038100       ELSE                                                               
038200         IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                        
038300           IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                      
038400             PERFORM FAE-HANDLE-KDPARTTY-KEY-2                            
038500           ELSE                                                           
038600             PERFORM FAD-HANDLE-KDPARTGR-KEY-2                            
038700           END-IF                                                         
038800         ELSE                                                             
038900           IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                      
039000             PERFORM FAF-HANDLE-KDPARTTY-KEY-3                            
039100           ELSE                                                           
039200             PERFORM FAG-HANDLE-KDPARTGR-KEY-3                            
039300           END-IF                                                         
039400         END-IF                                                           
039500       END-IF                                                             
039600     ELSE                                                                 
039700       IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                          
039800         IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                        
039900           PERFORM FAC-HANDLE-KDFINDOC-KEY                                
040000         ELSE                                                             
040100           IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                      
040200             PERFORM FAB-HANDLE-KDPARTTY-KEY                              
040300           ELSE                                                           
040400             PERFORM FAA-HANDLE-KDPARTGR-KEY                              
040500           END-IF                                                         
040600         END-IF                                                           
040700       ELSE                                                               
040800         IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                        
040900           PERFORM FAH-HANDLE-KDFINDOC-KEY-4                              
041000         ELSE                                                             
041100           IF REQU-KDPARTGR-KEY = SPACE OR = ALL '+'                      
041200             PERFORM FAI-HANDLE-KDPARTTY-KEY-4                            
041300           ELSE                                                           
041400             PERFORM FAJ-HANDLE-KDPARTGR-KEY-4                            
041500           END-IF                                                         
041600         END-IF                                                           
041700       END-IF                                                             
041800     END-IF                                                               
041900                                                                          
042000     IF WS-IX > ZERO                                                      
042100       MOVE WS-IX               TO RESP-KVRADER                           
042200     ELSE                                                                 
042300       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
042400       IF WS-COUNTER-T01PAIN > ZERO                                       
042500         MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
042600       END-IF                                                             
042700       IF WS-COUNTER-FAB > ZERO                                           
042800         MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
042900       END-IF                                                             
043000       IF WS-COUNTER-FAC > ZERO                                           
043100         MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
043200       END-IF                                                             
043300       IF WS-COUNTER-FAD > ZERO                                           
043400         MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                        
043500       END-IF                                                             
043600       IF WS-COUNTER-FAE > ZERO                                           
043700         MOVE 'KDPARTTY'      TO RESP-IDELMT-ERROR                        
043800       END-IF                                                             
043900       IF WS-COUNTER-FAF > ZERO                                           
044000         MOVE 'KDVALISO'      TO RESP-IDELMT-ERROR                        
044100       END-IF                                                             
044200       IF WS-COUNTER-FAG > ZERO                                           
044300         MOVE 'KDVALISO'      TO RESP-IDELMT-ERROR                        
044400       END-IF                                                             
044500       IF WS-COUNTER-FAH > ZERO                                           
044600         MOVE 'KDVALISO'      TO RESP-IDELMT-ERROR                        
044700       END-IF                                                             
044800       IF WS-COUNTER-FAI > ZERO                                           
044900         MOVE 'KDVALISO'      TO RESP-IDELMT-ERROR                        
045000       END-IF                                                             
045100       IF WS-COUNTER-FAJ > ZERO                                           
045200         MOVE 'KDVALISO'      TO RESP-IDELMT-ERROR                        
045300       END-IF                                                             
045400       IF WS-COUNTER-FAK > ZERO                                           
045500         MOVE 'KDVALISO'      TO RESP-IDELMT-ERROR                        
045600       END-IF                                                             
045700       IF WS-COUNTER-FAL > ZERO                                           
045800         MOVE 'KDFINDOC'      TO RESP-IDELMT-ERROR                        
045900       END-IF                                                             
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300                                                                          
046400*** - HANDLE KEY KDPARTGR                                                 
046500 FAA-HANDLE-KDPARTGR-KEY SECTION.                                         
046600     PERFORM DB2-COUNT-CRS-FAA                                            
046700     IF WS-COUNTER-T01PAIN = ZERO                                         
046800       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
046900     ELSE                                                                 
047000       IF WS-COUNTER-T01PAIN > WS-MAX-LINES                               
047100         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
047200       END-IF                                                             
047300     END-IF                                                               
047400                                                                          
047500     IF RESP-IDMSG-ERROR = SPACE                                          
047600       PERFORM DB2-DCL-OPN-T01PAIN-CRS-1                                  
047700       PERFORM DB2-FETCH-T01PAIN-CRS-1                                    
047800       MOVE ZERO TO WS-IX                                                 
047900       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
048000         PERFORM S03-MOVE-TO-RESPOND                                      
048100         PERFORM DB2-FETCH-T01PAIN-CRS-1                                  
048200       END-PERFORM                                                        
048300       IF NOT LINES-MISSING                                               
048400         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
048500       ELSE                                                               
048600         MOVE SPACE                 TO RESP-IDMSG-INFO                    
048700       END-IF                                                             
048800       PERFORM DB2-CLOSE-T01PAIN-CRS-1                                    
048900                                                                          
049000       IF WS-IX = ZERO                                                    
049100         PERFORM DB2-DCL-OPN-T01DOTY-CRS-3                                
049200         PERFORM DB2-FETCH-T01DOTY-CRS-3                                  
049300         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
049400           PERFORM S03-MOVE-TO-RESPOND                                    
049500           PERFORM DB2-FETCH-T01DOTY-CRS-3                                
049600         END-PERFORM                                                      
049700         IF NOT LINES-MISSING                                             
049800           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
049900         ELSE                                                             
050000           MOVE SPACE                 TO RESP-IDMSG-INFO                  
050100         END-IF                                                           
050200         PERFORM DB2-CLOSE-T01DOTY-CRS-3                                  
050300       END-IF                                                             
050400     END-IF                                                               
050500     .                                                                    
050600     EJECT                                                                
050700                                                                          
050800*** - HANDLE KEY KDPARTTY                                                 
050900 FAB-HANDLE-KDPARTTY-KEY SECTION.                                         
051000     PERFORM DB2-COUNT-CRS-FAB                                            
051100     IF WS-COUNTER-FAB = ZERO                                             
051200       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
051300     ELSE                                                                 
051400       IF WS-COUNTER-FAB > WS-MAX-LINES                                   
051500         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
051600       END-IF                                                             
051700     END-IF                                                               
051800                                                                          
051900     IF RESP-IDMSG-ERROR = SPACE                                          
052000       PERFORM DB2-DCL-OPN-T01PAIN-CRS-2A                                 
052100       PERFORM DB2-FETCH-T01PAIN-CRS-2A                                   
052200       MOVE ZERO TO WS-IX                                                 
052300       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
052400         PERFORM S03-MOVE-TO-RESPOND                                      
052500         PERFORM DB2-FETCH-T01PAIN-CRS-2A                                 
052600       END-PERFORM                                                        
052700       IF NOT LINES-MISSING                                               
052800         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
052900       ELSE                                                               
053000         MOVE SPACE                 TO RESP-IDMSG-INFO                    
053100       END-IF                                                             
053200       PERFORM DB2-CLOSE-T01PAIN-CRS-2A                                   
053300                                                                          
053400       IF WS-IX = ZERO                                                    
053500         PERFORM DB2-DCL-OPN-T01DOTY-CRS-2                                
053600         PERFORM DB2-FETCH-T01DOTY-CRS-2                                  
053700         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
053800           PERFORM S03-MOVE-TO-RESPOND                                    
053900           PERFORM DB2-FETCH-T01DOTY-CRS-2                                
054000         END-PERFORM                                                      
054100         IF NOT LINES-MISSING                                             
054200           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
054300         ELSE                                                             
054400           MOVE SPACE                 TO RESP-IDMSG-INFO                  
054500         END-IF                                                           
054600         PERFORM DB2-CLOSE-T01DOTY-CRS-2                                  
054700       END-IF                                                             
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100                                                                          
055200*** - HANDLE KEY KDFINDOC                                                 
055300 FAC-HANDLE-KDFINDOC-KEY SECTION.                                         
055400     PERFORM DB2-COUNT-CRS-FAC                                            
055500     IF WS-COUNTER-FAC = ZERO                                             
055600       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
055700     ELSE                                                                 
055800       IF WS-COUNTER-FAC > WS-MAX-LINES                                   
055900         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
056000       END-IF                                                             
056100     END-IF                                                               
056200                                                                          
056300     IF RESP-IDMSG-ERROR = SPACE                                          
056400       PERFORM DB2-DCL-OPN-T01PAIN-CRS-3A                                 
056500       PERFORM DB2-FETCH-T01PAIN-CRS-3A                                   
056600       MOVE ZERO TO WS-IX                                                 
056700       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
056800         PERFORM S03-MOVE-TO-RESPOND                                      
056900         PERFORM DB2-FETCH-T01PAIN-CRS-3A                                 
057000       END-PERFORM                                                        
057100       IF NOT LINES-MISSING                                               
057200         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
057300       ELSE                                                               
057400         MOVE SPACE                 TO RESP-IDMSG-INFO                    
057500       END-IF                                                             
057600       PERFORM DB2-CLOSE-T01PAIN-CRS-3A                                   
057700                                                                          
057800       IF WS-IX = ZERO                                                    
057900         PERFORM DB2-DCL-OPN-T01DOTY-CRS-1                                
058000         PERFORM DB2-FETCH-T01DOTY-CRS-1                                  
058100         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
058200           PERFORM S03-MOVE-TO-RESPOND                                    
058300           PERFORM DB2-FETCH-T01DOTY-CRS-1                                
058400         END-PERFORM                                                      
058500         IF NOT LINES-MISSING                                             
058600           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
058700         ELSE                                                             
058800           MOVE SPACE                 TO RESP-IDMSG-INFO                  
058900         END-IF                                                           
059000         PERFORM DB2-CLOSE-T01DOTY-CRS-1                                  
059100       END-IF                                                             
059200     END-IF                                                               
059300     .                                                                    
059400     EJECT                                                                
059500                                                                          
059600*** - HANDLE KEY KDPARTGR-2                                               
059700 FAD-HANDLE-KDPARTGR-KEY-2 SECTION.                                       
059800     PERFORM DB2-COUNT-CRS-FAD                                            
059900     IF WS-COUNTER-FAD = ZERO                                             
060000       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
060100     ELSE                                                                 
060200       IF WS-COUNTER-FAD > WS-MAX-LINES                                   
060300         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
060400       END-IF                                                             
060500     END-IF                                                               
060600                                                                          
060700     IF RESP-IDMSG-ERROR = SPACE                                          
060800                                                                          
060900       PERFORM DB2-DCL-OPN-T01PAIN-CRS-4A                                 
061000       PERFORM DB2-FETCH-T01PAIN-CRS-4A                                   
061100       MOVE ZERO TO WS-IX                                                 
061200       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
061300         PERFORM S03-MOVE-TO-RESPOND                                      
061400         PERFORM DB2-FETCH-T01PAIN-CRS-4A                                 
061500       END-PERFORM                                                        
061600       IF NOT LINES-MISSING                                               
061700         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
061800       ELSE                                                               
061900         MOVE SPACE                 TO RESP-IDMSG-INFO                    
062000       END-IF                                                             
062100       PERFORM DB2-CLOSE-T01PAIN-CRS-4A                                   
062200                                                                          
062300       IF WS-IX = ZERO                                                    
062400         PERFORM DB2-DCL-OPN-T01CUGR-CRS-2                                
062500         PERFORM DB2-FETCH-T01CUGR-CRS-2                                  
062600         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
062700           PERFORM S03-MOVE-TO-RESPOND                                    
062800           PERFORM DB2-FETCH-T01CUGR-CRS-2                                
062900         END-PERFORM                                                      
063000         IF NOT LINES-MISSING                                             
063100           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
063200         ELSE                                                             
063300           MOVE SPACE                 TO RESP-IDMSG-INFO                  
063400         END-IF                                                           
063500         PERFORM DB2-CLOSE-T01CUGR-CRS-2                                  
063600       END-IF                                                             
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000                                                                          
064100*** - HANDLE KEY KDPARTTY-2                                               
064200 FAE-HANDLE-KDPARTTY-KEY-2 SECTION.                                       
064300     PERFORM DB2-COUNT-CRS-FAE                                            
064400     IF WS-COUNTER-FAE = ZERO                                             
064500       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
064600     ELSE                                                                 
064700       IF WS-COUNTER-FAE > WS-MAX-LINES                                   
064800         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
064900       END-IF                                                             
065000     END-IF                                                               
065100                                                                          
065200     IF RESP-IDMSG-ERROR = SPACE                                          
065300       PERFORM DB2-DCL-OPN-T01PAIN-CRS-5A                                 
065400       PERFORM DB2-FETCH-T01PAIN-CRS-5A                                   
065500       MOVE ZERO TO WS-IX                                                 
065600       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
065700         PERFORM S03-MOVE-TO-RESPOND                                      
065800         PERFORM DB2-FETCH-T01PAIN-CRS-5A                                 
065900       END-PERFORM                                                        
066000       IF NOT LINES-MISSING                                               
066100         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
066200       ELSE                                                               
066300         MOVE SPACE                 TO RESP-IDMSG-INFO                    
066400       END-IF                                                             
066500       PERFORM DB2-CLOSE-T01PAIN-CRS-5A                                   
066600                                                                          
066700       IF WS-IX = ZERO                                                    
066800         PERFORM DB2-DCL-OPN-T01CUGR-CRS-1                                
066900         PERFORM DB2-FETCH-T01CUGR-CRS-1                                  
067000         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
067100           PERFORM S03-MOVE-TO-RESPOND                                    
067200           PERFORM DB2-FETCH-T01CUGR-CRS-1                                
067300         END-PERFORM                                                      
067400         IF NOT LINES-MISSING                                             
067500           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
067600         ELSE                                                             
067700           MOVE SPACE                 TO RESP-IDMSG-INFO                  
067800         END-IF                                                           
067900         PERFORM DB2-CLOSE-T01CUGR-CRS-1                                  
068000       END-IF                                                             
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400                                                                          
068500*** - HANDLE KEY KDPARTTY-3                                               
068600 FAF-HANDLE-KDPARTTY-KEY-3 SECTION.                                       
068700     PERFORM DB2-COUNT-CRS-FAF                                            
068800     IF WS-COUNTER-FAF = ZERO                                             
068900       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
069000     ELSE                                                                 
069100       IF WS-COUNTER-FAF > WS-MAX-LINES                                   
069200         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
069300       END-IF                                                             
069400     END-IF                                                               
069500                                                                          
069600     IF RESP-IDMSG-ERROR = SPACE                                          
069700       PERFORM DB2-DCL-OPN-T01PAIN-CRS-6A                                 
069800       PERFORM DB2-FETCH-T01PAIN-CRS-6A                                   
069900       MOVE ZERO TO WS-IX                                                 
070000       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
070100         PERFORM S03-MOVE-TO-RESPOND                                      
070200         PERFORM DB2-FETCH-T01PAIN-CRS-6A                                 
070300       END-PERFORM                                                        
070400       IF NOT LINES-MISSING                                               
070500         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
070600       ELSE                                                               
070700         MOVE SPACE                 TO RESP-IDMSG-INFO                    
070800       END-IF                                                             
070900       PERFORM DB2-CLOSE-T01PAIN-CRS-6A                                   
071000                                                                          
071100       IF WS-IX = ZERO                                                    
071200         PERFORM DB2-DCL-OPN-T01CUGR-CRS-3                                
071300         PERFORM DB2-FETCH-T01CUGR-CRS-3                                  
071400         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
071500           PERFORM S03-MOVE-TO-RESPOND                                    
071600           PERFORM DB2-FETCH-T01CUGR-CRS-3                                
071700         END-PERFORM                                                      
071800         IF NOT LINES-MISSING                                             
071900           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
072000         ELSE                                                             
072100           MOVE SPACE                 TO RESP-IDMSG-INFO                  
072200         END-IF                                                           
072300         PERFORM DB2-CLOSE-T01CUGR-CRS-3                                  
072400       END-IF                                                             
072500     END-IF                                                               
072600     .                                                                    
072700     EJECT                                                                
072800                                                                          
072900*** - HANDLE KEY KDPARTGR-3                                               
073000 FAG-HANDLE-KDPARTGR-KEY-3 SECTION.                                       
073100     PERFORM DB2-COUNT-CRS-FAG                                            
073200     IF WS-COUNTER-FAG = ZERO                                             
073300       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
073400     ELSE                                                                 
073500       IF WS-COUNTER-FAG > WS-MAX-LINES                                   
073600         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
073700       END-IF                                                             
073800     END-IF                                                               
073900                                                                          
074000     IF RESP-IDMSG-ERROR = SPACE                                          
074100                                                                          
074200       PERFORM DB2-DCL-OPN-T01PAIN-CRS-7A                                 
074300       PERFORM DB2-FETCH-T01PAIN-CRS-7A                                   
074400       MOVE ZERO TO WS-IX                                                 
074500       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
074600         PERFORM S03-MOVE-TO-RESPOND                                      
074700         PERFORM DB2-FETCH-T01PAIN-CRS-7A                                 
074800       END-PERFORM                                                        
074900       IF NOT LINES-MISSING                                               
075000         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
075100       ELSE                                                               
075200         MOVE SPACE                 TO RESP-IDMSG-INFO                    
075300       END-IF                                                             
075400       PERFORM DB2-CLOSE-T01PAIN-CRS-7A                                   
075500                                                                          
075600       IF WS-IX = ZERO                                                    
075700         PERFORM DB2-DCL-OPN-T01CUGR-CRS-4                                
075800         PERFORM DB2-FETCH-T01CUGR-CRS-4                                  
075900         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
076000           PERFORM S03-MOVE-TO-RESPOND                                    
076100           PERFORM DB2-FETCH-T01CUGR-CRS-4                                
076200         END-PERFORM                                                      
076300         IF NOT LINES-MISSING                                             
076400           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
076500         ELSE                                                             
076600           MOVE SPACE                 TO RESP-IDMSG-INFO                  
076700         END-IF                                                           
076800         PERFORM DB2-CLOSE-T01CUGR-CRS-4                                  
076900       END-IF                                                             
077000     END-IF                                                               
077100     .                                                                    
077200     EJECT                                                                
077300                                                                          
077400*** - HANDLE KEY KDFINDOC-4                                               
077500 FAH-HANDLE-KDFINDOC-KEY-4 SECTION.                                       
077600     PERFORM DB2-COUNT-CRS-FAH                                            
077700     IF WS-COUNTER-FAH = ZERO                                             
077800       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
077900     ELSE                                                                 
078000       IF WS-COUNTER-FAH > WS-MAX-LINES                                   
078100         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
078200       END-IF                                                             
078300     END-IF                                                               
078400                                                                          
078500     IF RESP-IDMSG-ERROR = SPACE                                          
078600       PERFORM DB2-DCL-OPN-T01PAIN-CRS-8A                                 
078700       PERFORM DB2-FETCH-T01PAIN-CRS-8A                                   
078800       MOVE ZERO TO WS-IX                                                 
078900       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
079000         PERFORM S03-MOVE-TO-RESPOND                                      
079100         PERFORM DB2-FETCH-T01PAIN-CRS-8A                                 
079200       END-PERFORM                                                        
079300       IF NOT LINES-MISSING                                               
079400         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
079500       ELSE                                                               
079600         MOVE SPACE                 TO RESP-IDMSG-INFO                    
079700       END-IF                                                             
079800       PERFORM DB2-CLOSE-T01PAIN-CRS-8A                                   
079900                                                                          
080000       IF WS-IX = ZERO                                                    
080100         PERFORM DB2-DCL-OPN-T01DOTY-CRS-4                                
080200         PERFORM DB2-FETCH-T01DOTY-CRS-4                                  
080300         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
080400           PERFORM S03-MOVE-TO-RESPOND                                    
080500           PERFORM DB2-FETCH-T01DOTY-CRS-4                                
080600         END-PERFORM                                                      
080700         IF NOT LINES-MISSING                                             
080800           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
080900         ELSE                                                             
081000           MOVE SPACE                 TO RESP-IDMSG-INFO                  
081100         END-IF                                                           
081200         PERFORM DB2-CLOSE-T01DOTY-CRS-4                                  
081300       END-IF                                                             
081400     END-IF                                                               
081500     .                                                                    
081600     EJECT                                                                
081700                                                                          
081800*** - HANDLE KEY KDPARTTY-4                                               
081900 FAI-HANDLE-KDPARTTY-KEY-4 SECTION.                                       
082000     PERFORM DB2-COUNT-CRS-FAI                                            
082100     IF WS-COUNTER-FAI = ZERO                                             
082200       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
082300     ELSE                                                                 
082400       IF WS-COUNTER-FAI > WS-MAX-LINES                                   
082500         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
082600       END-IF                                                             
082700     END-IF                                                               
082800                                                                          
082900     IF RESP-IDMSG-ERROR = SPACE                                          
083000       PERFORM DB2-DCL-OPN-T01PAIN-CRS-9A                                 
083100       PERFORM DB2-FETCH-T01PAIN-CRS-9A                                   
083200       MOVE ZERO TO WS-IX                                                 
083300       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
083400         PERFORM S03-MOVE-TO-RESPOND                                      
083500         PERFORM DB2-FETCH-T01PAIN-CRS-9A                                 
083600       END-PERFORM                                                        
083700       IF NOT LINES-MISSING                                               
083800         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
083900       ELSE                                                               
084000         MOVE SPACE                 TO RESP-IDMSG-INFO                    
084100       END-IF                                                             
084200       PERFORM DB2-CLOSE-T01PAIN-CRS-9A                                   
084300                                                                          
084400       IF WS-IX = ZERO                                                    
084500         PERFORM DB2-DCL-OPN-T01DOTY-CRS-5                                
084600         PERFORM DB2-FETCH-T01DOTY-CRS-5                                  
084700         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
084800           PERFORM S03-MOVE-TO-RESPOND                                    
084900           PERFORM DB2-FETCH-T01DOTY-CRS-5                                
085000         END-PERFORM                                                      
085100         IF NOT LINES-MISSING                                             
085200           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
085300         ELSE                                                             
085400           MOVE SPACE                 TO RESP-IDMSG-INFO                  
085500         END-IF                                                           
085600         PERFORM DB2-CLOSE-T01DOTY-CRS-5                                  
085700       END-IF                                                             
085800     END-IF                                                               
085900     .                                                                    
086000     EJECT                                                                
086100                                                                          
086200*** - HANDLE KEY KDPARTGR-4                                               
086300 FAJ-HANDLE-KDPARTGR-KEY-4 SECTION.                                       
086400     PERFORM DB2-COUNT-CRS-FAJ                                            
086500     IF WS-COUNTER-FAJ = ZERO                                             
086600       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
086700     ELSE                                                                 
086800       IF WS-COUNTER-FAJ > WS-MAX-LINES                                   
086900         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
087000       END-IF                                                             
087100     END-IF                                                               
087200                                                                          
087300     IF RESP-IDMSG-ERROR = SPACE                                          
087400                                                                          
087500       PERFORM DB2-DCL-OPN-T01PAIN-CRS-AA                                 
087600       PERFORM DB2-FETCH-T01PAIN-CRS-AA                                   
087700       MOVE ZERO TO WS-IX                                                 
087800       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
087900         PERFORM S03-MOVE-TO-RESPOND                                      
088000         PERFORM DB2-FETCH-T01PAIN-CRS-AA                                 
088100       END-PERFORM                                                        
088200       IF NOT LINES-MISSING                                               
088300         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
088400       ELSE                                                               
088500         MOVE SPACE                 TO RESP-IDMSG-INFO                    
088600       END-IF                                                             
088700       PERFORM DB2-CLOSE-T01PAIN-CRS-AA                                   
088800                                                                          
088900       IF WS-IX = ZERO                                                    
089000         PERFORM DB2-DCL-OPN-T01DOTY-CRS-6                                
089100         PERFORM DB2-FETCH-T01DOTY-CRS-6                                  
089200         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
089300           PERFORM S03-MOVE-TO-RESPOND                                    
089400           PERFORM DB2-FETCH-T01DOTY-CRS-6                                
089500         END-PERFORM                                                      
089600         IF NOT LINES-MISSING                                             
089700           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
089800         ELSE                                                             
089900           MOVE SPACE                 TO RESP-IDMSG-INFO                  
090000         END-IF                                                           
090100         PERFORM DB2-CLOSE-T01DOTY-CRS-6                                  
090200       END-IF                                                             
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600                                                                          
090700*** - HANDLE KEY KDVALISO                                                 
090800 FAK-HANDLE-KDVALISO-KEY   SECTION.                                       
090900     PERFORM DB2-COUNT-CRS-FAK                                            
091000     IF WS-COUNTER-FAK = ZERO                                             
091100       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
091200     ELSE                                                                 
091300       IF WS-COUNTER-FAK > WS-MAX-LINES                                   
091400         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
091500       END-IF                                                             
091600     END-IF                                                               
091700                                                                          
091800     IF RESP-IDMSG-ERROR = SPACE                                          
091900                                                                          
092000       PERFORM DB2-DCL-OPN-T01PAIN-CRS-AB                                 
092100       PERFORM DB2-FETCH-T01PAIN-CRS-AB                                   
092200       MOVE ZERO TO WS-IX                                                 
092300       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
092400         PERFORM S03-MOVE-TO-RESPOND                                      
092500         PERFORM DB2-FETCH-T01PAIN-CRS-AB                                 
092600       END-PERFORM                                                        
092700       IF NOT LINES-MISSING                                               
092800         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
092900       ELSE                                                               
093000         MOVE SPACE                 TO RESP-IDMSG-INFO                    
093100       END-IF                                                             
093200       PERFORM DB2-CLOSE-T01PAIN-CRS-AB                                   
093300                                                                          
093400       IF WS-IX = ZERO                                                    
093500         PERFORM DB2-DCL-OPN-T01DOTY-CRS-7                                
093600         PERFORM DB2-FETCH-T01DOTY-CRS-7                                  
093700         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
093800           PERFORM S03-MOVE-TO-RESPOND                                    
093900           PERFORM DB2-FETCH-T01DOTY-CRS-7                                
094000         END-PERFORM                                                      
094100         IF NOT LINES-MISSING                                             
094200           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
094300         ELSE                                                             
094400           MOVE SPACE                 TO RESP-IDMSG-INFO                  
094500         END-IF                                                           
094600         PERFORM DB2-CLOSE-T01DOTY-CRS-7                                  
094700       END-IF                                                             
094800     END-IF                                                               
094900     .                                                                    
095000     EJECT                                                                
095100                                                                          
095200*** - HANDLE WHEN KEYS ARE SPACES                                         
095300 FAL-HANDLE-BLANK-KEYS SECTION.                                           
095400     PERFORM DB2-COUNT-CRS-FAL                                            
095500     IF WS-COUNTER-FAL = ZERO                                             
095600       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
095700     ELSE                                                                 
095800       IF WS-COUNTER-FAL > WS-MAX-LINES                                   
095900         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
096000       END-IF                                                             
096100     END-IF                                                               
096200                                                                          
096300     IF RESP-IDMSG-ERROR = SPACE                                          
096400       PERFORM DB2-DCL-OPN-T01PAIN-CRS-10A                                
096500       PERFORM DB2-FETCH-T01PAIN-CRS-10A                                  
096600       MOVE ZERO TO WS-IX                                                 
096700       PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)              
096800         PERFORM S03-MOVE-TO-RESPOND                                      
096900         PERFORM DB2-FETCH-T01PAIN-CRS-10A                                
097000       END-PERFORM                                                        
097100       IF NOT LINES-MISSING                                               
097200         MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                    
097300       ELSE                                                               
097400         MOVE SPACE                 TO RESP-IDMSG-INFO                    
097500       END-IF                                                             
097600       PERFORM DB2-CLOSE-T01PAIN-CRS-10A                                  
097700                                                                          
097800       IF WS-IX = ZERO                                                    
097900         PERFORM DB2-DCL-OPN-T01DOTY-CRS-10B                              
098000         PERFORM DB2-FETCH-T01DOTY-CRS-10B                                
098100         PERFORM UNTIL LINES-MISSING OR (WS-IX = WS-MAX-LINES)            
098200           PERFORM S03-MOVE-TO-RESPOND                                    
098300           PERFORM DB2-FETCH-T01DOTY-CRS-10B                              
098400         END-PERFORM                                                      
098500         IF NOT LINES-MISSING                                             
098600           MOVE  INF-MORE-LINES-EXIST TO RESP-IDMSG-INFO                  
098700         ELSE                                                             
098800           MOVE SPACE                 TO RESP-IDMSG-INFO                  
098900         END-IF                                                           
099000         PERFORM DB2-CLOSE-T01DOTY-CRS-10B                                
099100       END-IF                                                             
099200     END-IF                                                               
099300     .                                                                    
099400     EJECT                                                                
099500                                                                          
099600*   --- DISPATCHER SECTION START                                          
099700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
099800     MOVE 'GETARG'             TO SUB-KDFUNC                              
099900     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
100000     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
100100                                                                          
100200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
100300                                                                          
100400     IF SUB-KDRC > 0                                                      
100500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
100600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
100700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
100800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
100900     END-IF                                                               
101000     .                                                                    
101100     EJECT                                                                
101200                                                                          
101300 S02-RETURN-RESPONSE SECTION.                                             
101400     MOVE 'RETURN'             TO SUB-KDFUNC                              
101500                                                                          
101600     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
101700                              - ((WS-MAX-LINES - WS-IX)                   
101800                              * LENGTH OF RESP-TABELLRAD)                 
101900                                                                          
102000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
102100                                                                          
102200     IF SUB-KDRC > 0                                                      
102300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
102400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
102500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
102600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
102700     END-IF                                                               
102800     .                                                                    
102900     EJECT                                                                
103000                                                                          
103100*   --- MOVE TO OUTPUT SECTION START                                      
103200*** - MOVE DATA TO RESPOND WHEN CURRENT LINE,                             
103300***   WHEN COMING LINE MODIFY CURRENT LINE.                               
103400 S03-MOVE-TO-RESPOND SECTION.                                             
103500     PERFORM S04-SELECT-SEARCH-RESPOND                                    
103600     IF LINES-FOUND                                                       
103700       ADD  1 TO WS-IX                                                    
103800       IF MAP-KDSTATUS-LINE    = WS-CURRENT                               
103900       AND MAP-KDSTATUS-LINE-2 = WS-CURRENT                               
104000         IF  MAP-KDFINDOC-LINE = RESP-KDFINDOC-LINE(WS-IX)                
104100         AND MAP-KDPARTTY-LINE = RESP-KDPARTTY-LINE(WS-IX)                
104200         AND MAP-KDPARTGR-LINE = RESP-KDPARTGR-LINE(WS-IX)                
104300         AND MAP-KDVALISO-LINE = RESP-KDVALISO-LINE(WS-IX)                
104400           CONTINUE                                                       
104500         ELSE                                                             
104600           MOVE MAP-KDFINDOC-LINE TO RESP-KDFINDOC-LINE(WS-IX)            
104700           MOVE MAP-KDPARTTY-LINE TO RESP-KDPARTTY-LINE(WS-IX)            
104800           MOVE MAP-KDPARTGR-LINE TO RESP-KDPARTGR-LINE(WS-IX)            
104900           MOVE MAP-KDVALISO-LINE TO RESP-KDVALISO-LINE(WS-IX)            
105000           MOVE MAP-DAREGDAT-LINE TO RESP-DAREGDAT-LINE(WS-IX)            
105100           MOVE MAP-DAUPPDAT-LINE TO RESP-DAUPPDAT-LINE(WS-IX)            
105200           MOVE MAP-IDUSER-LINE   TO RESP-IDUSER-LINE(WS-IX)              
105300           IF WS-KEY1-MARKED = 'Y'                                        
105400             MOVE YES             TO RESP-FLCOMING-LINE(WS-IX)            
105500             MOVE 'N'             TO WS-KEY1-MARKED                       
105600           ELSE                                                           
105700             MOVE NOO             TO RESP-FLCOMING-LINE(WS-IX)            
105800           END-IF                                                         
105900         END-IF                                                           
106000       ELSE                                                               
106100         IF  MAP-KDFINDOC-LINE = RESP-KDFINDOC-LINE(WS-IX)                
106200         AND MAP-KDPARTTY-LINE = RESP-KDPARTTY-LINE(WS-IX)                
106300         AND MAP-KDPARTGR-LINE = RESP-KDPARTGR-LINE(WS-IX)                
106400         AND MAP-KDVALISO-LINE = RESP-KDVALISO-LINE(WS-IX)                
106500           CONTINUE                                                       
106600         ELSE                                                             
106700           IF MAP-KDSTATUS-LINE = WS-COMING                               
106800             MOVE 'Y' TO WS-KEY1-MARKED                                   
106900           END-IF                                                         
107000           IF MAP-KDSTATUS-LINE-2 = WS-COMING                             
107100             MOVE 'Y' TO WS-KEY1-MARKED                                   
107200           END-IF                                                         
107300         END-IF                                                           
107400       END-IF                                                             
107500     ELSE                                                                 
107600       IF MAP-KDSTATUS-LINE-2 = WS-CURRENT                                
107700         MOVE SPACE      TO MAP-DAREGDAT-LINE-2                           
107800                            MAP-DAUPPDAT-LINE-2                           
107900         MOVE WS-NOT-REG TO MAP-IDUSER-LINE-2                             
108000         ADD 1 TO WS-IX                                                   
108100         IF  MAP-KDFINDOC-LINE = RESP-KDFINDOC-LINE(WS-IX)                
108200         AND MAP-KDPARTTY-LINE = RESP-KDPARTTY-LINE(WS-IX)                
108300         AND MAP-KDPARTGR-LINE = RESP-KDPARTGR-LINE(WS-IX)                
108400         AND MAP-KDVALISO-LINE = RESP-KDVALISO-LINE(WS-IX)                
108500           CONTINUE                                                       
108600         ELSE                                                             
108700           MOVE MAP-KDFINDOC-LINE-2 TO RESP-KDFINDOC-LINE(WS-IX)          
108800           MOVE MAP-KDPARTTY-LINE-2 TO RESP-KDPARTTY-LINE(WS-IX)          
108900           MOVE MAP-KDPARTGR-LINE-2 TO RESP-KDPARTGR-LINE(WS-IX)          
109000           MOVE MAP-KDVALISO-LINE-2 TO RESP-KDVALISO-LINE(WS-IX)          
109100           MOVE MAP-DAREGDAT-LINE-2 TO RESP-DAREGDAT-LINE(WS-IX)          
109200           MOVE MAP-DAUPPDAT-LINE-2 TO RESP-DAUPPDAT-LINE(WS-IX)          
109300           MOVE MAP-IDUSER-LINE-2   TO RESP-IDUSER-LINE(WS-IX)            
109400           MOVE NOO                 TO RESP-FLCOMING-LINE(WS-IX)          
109500         END-IF                                                           
109600       END-IF                                                             
109700     END-IF                                                               
109800     .                                                                    
109900     EJECT                                                                
110000                                                                          
110100 S04-SELECT-SEARCH-RESPOND SECTION.                                       
110200     IF WS-COUNTER-T01PAIN > ZERO                                         
110300       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
110400         PERFORM DB2-SELECT-T01PAIN-FAA-2                                 
110500         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
110600         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
110700         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
110800         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
110900       ELSE                                                               
111000         PERFORM DB2-SELECT-T01PAIN-FAA                                   
111100         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
111200         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
111300         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
111400         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
111500       END-IF                                                             
111600     END-IF                                                               
111700                                                                          
111800     IF WS-COUNTER-FAB > ZERO                                             
111900       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
112000         PERFORM DB2-SELECT-T01PAIN-FAB-2                                 
112100         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
112200         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
112300         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
112400         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
112500       ELSE                                                               
112600         PERFORM DB2-SELECT-T01PAIN-FAB                                   
112700         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
112800         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
112900         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
113000         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
113100       END-IF                                                             
113200     END-IF                                                               
113300                                                                          
113400     IF WS-COUNTER-FAC > ZERO                                             
113500       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
113600         PERFORM DB2-SELECT-T01PAIN-FAC-2                                 
113700         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
113800         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
113900         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
114000         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
114100       ELSE                                                               
114200         PERFORM DB2-SELECT-T01PAIN-FAC                                   
114300         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
114400         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
114500         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
114600         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
114700       END-IF                                                             
114800     END-IF                                                               
114900                                                                          
115000     IF WS-COUNTER-FAD > ZERO                                             
115100       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
115200         PERFORM DB2-SELECT-T01PAIN-FAD-2                                 
115300         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
115400         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
115500         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
115600         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
115700       ELSE                                                               
115800         PERFORM DB2-SELECT-T01PAIN-FAD                                   
115900         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
116000         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
116100         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
116200         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
116300       END-IF                                                             
116400     END-IF                                                               
116500                                                                          
116600     IF WS-COUNTER-FAE > ZERO                                             
116700       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
116800         PERFORM DB2-SELECT-T01PAIN-FAE-2                                 
116900         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
117000         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
117100         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
117200         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
117300       ELSE                                                               
117400         PERFORM DB2-SELECT-T01PAIN-FAE                                   
117500         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
117600         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
117700         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
117800         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
117900       END-IF                                                             
118000     END-IF                                                               
118100                                                                          
118200     IF WS-COUNTER-FAF > ZERO                                             
118300       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
118400         PERFORM DB2-SELECT-T01PAIN-FAF-2                                 
118500         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
118600         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
118700         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
118800         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
118900       ELSE                                                               
119000         PERFORM DB2-SELECT-T01PAIN-FAF                                   
119100         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
119200         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
119300         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
119400         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
119500       END-IF                                                             
119600     END-IF                                                               
119700                                                                          
119800     IF WS-COUNTER-FAG > ZERO                                             
119900       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
120000         PERFORM DB2-SELECT-T01PAIN-FAG-2                                 
120100         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
120200         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
120300         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
120400         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
120500       ELSE                                                               
120600         PERFORM DB2-SELECT-T01PAIN-FAG                                   
120700         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
120800         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
120900         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
121000         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
121100       END-IF                                                             
121200     END-IF                                                               
121300                                                                          
121400     IF WS-COUNTER-FAH > ZERO                                             
121500       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
121600         PERFORM DB2-SELECT-T01PAIN-FAH-2                                 
121700         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
121800         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
121900         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
122000         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
122100       ELSE                                                               
122200         PERFORM DB2-SELECT-T01PAIN-FAH                                   
122300         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
122400         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
122500         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
122600         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
122700       END-IF                                                             
122800     END-IF                                                               
122900                                                                          
123000     IF WS-COUNTER-FAI > ZERO                                             
123100       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
123200         PERFORM DB2-SELECT-T01PAIN-FAI-2                                 
123300         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
123400         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
123500         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
123600         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
123700       ELSE                                                               
123800         PERFORM DB2-SELECT-T01PAIN-FAI                                   
123900         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
124000         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
124100         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
124200         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
124300       END-IF                                                             
124400     END-IF                                                               
124500                                                                          
124600     IF WS-COUNTER-FAJ > ZERO                                             
124700       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
124800         PERFORM DB2-SELECT-T01PAIN-FAJ-2                                 
124900         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
125000         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
125100         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
125200         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
125300       ELSE                                                               
125400         PERFORM DB2-SELECT-T01PAIN-FAJ                                   
125500         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
125600         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
125700         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
125800         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
125900       END-IF                                                             
126000     END-IF                                                               
126100                                                                          
126200     IF WS-COUNTER-FAK > ZERO                                             
126300       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
126400         PERFORM DB2-SELECT-T01PAIN-FAK-2                                 
126500         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
126600         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
126700         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
126800         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
126900       ELSE                                                               
127000         PERFORM DB2-SELECT-T01PAIN-FAK                                   
127100         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
127200         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
127300         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
127400         MOVE MAP-KDVALISO-LINE   TO MAP-KDVALISO-LINE-2                  
127500       END-IF                                                             
127600     END-IF                                                               
127700                                                                          
127800     IF WS-COUNTER-FAL > ZERO                                             
127900       IF MAP-KDSTATUS-LINE < MAP-KDSTATUS-LINE-2                         
128000         PERFORM DB2-SELECT-T01PAIN-FAL-2                                 
128100         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
128200         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
128300         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
128400         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
128500       ELSE                                                               
128600         PERFORM DB2-SELECT-T01PAIN-FAL                                   
128700         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
128800         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
128900         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
129000         MOVE MAP-KDVALISO-LINE-2 TO MAP-KDVALISO-LINE                    
129100       END-IF                                                             
129200     END-IF                                                               
129300     .                                                                    
129400     EJECT                                                                
129500                                                                          
129600*   --- DB2 SECTIONS                                                      
129700*** - CHECK THAT THE REQUESTED LEGAL SELLER EXIST                         
129800 DB2-SELECT-T01LSEL-TAB SECTION.                                          
129900     MOVE 000100 TO GOOD-SQLCODECODES                                     
130000                                                                          
130100     EXEC SQL                                                             
130200           SELECT  IDLEGSEL                                               
130300                 , BELEGRAD_1                                             
130400                                                                          
130500           INTO   :WS-IDLEGSEL                                            
130600                , :WS-BELEGRAD-1                                          
130700                                                                          
130800           FROM    T01LSEL                                                
130900                                                                          
131000           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
131100               AND KDSTATUS = :WS-CURRENT                                 
131200     END-EXEC                                                             
131300                                                                          
131400     MOVE SQLCODE TO SQLCODE-WS                                           
131500     PERFORM DB2-STATUS-CHECK                                             
131600     .                                                                    
131700     EJECT                                                                
131800                                                                          
131900 DB2-SELECT-T01CURR-TAB SECTION.                                          
132000     MOVE 000100 TO GOOD-SQLCODECODES                                     
132100                                                                          
132200     EXEC SQL                                                             
132300           SELECT  DISTINCT KDVALISO                                      
132400                 , IDLEGSEL                                               
132500                                                                          
132600           INTO   :WS-KDVALISO                                            
132700                , :WS-IDLEGSEL                                            
132800                                                                          
132900           FROM    T01CURR                                                
133000                                                                          
133100           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
133200               AND KDVALISO = :REQU-KDVALISO-KEY                          
133300     END-EXEC                                                             
133400                                                                          
133500     MOVE SQLCODE TO SQLCODE-WS                                           
133600     PERFORM DB2-STATUS-CHECK                                             
133700     .                                                                    
133800     EJECT                                                                
133900                                                                          
134000 DB2-SELECT-T01PAIN-FAA SECTION.                                          
134100     MOVE 000100 TO GOOD-SQLCODECODES                                     
134200                                                                          
134300     EXEC SQL                                                             
134400           SELECT  KDFINDOC                                               
134500                 , DAREGDAT                                               
134600                 , DAUPPDAT                                               
134700                 , IDUSER                                                 
134800                                                                          
134900           INTO    :WS-KDFINDOC                                           
135000                 , :MAP-DAREGDAT-LINE                                     
135100                 , :MAP-DAUPPDAT-LINE                                     
135200                 , :MAP-IDUSER-LINE                                       
135300                                                                          
135400           FROM    T01PAIN                                                
135500                                                                          
135600           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
135700                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
135800                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
135900                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
136000                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
136100                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
136200     END-EXEC                                                             
136300                                                                          
136400     MOVE SQLCODE TO SQLCODE-WS                                           
136500     PERFORM DB2-STATUS-CHECK                                             
136600     .                                                                    
136700     EJECT                                                                
136800                                                                          
136900 DB2-SELECT-T01PAIN-FAA-2 SECTION.                                        
137000     MOVE 000100 TO GOOD-SQLCODECODES                                     
137100                                                                          
137200     EXEC SQL                                                             
137300           SELECT  KDFINDOC                                               
137400                 , DAREGDAT                                               
137500                 , DAUPPDAT                                               
137600                 , IDUSER                                                 
137700                                                                          
137800           INTO    :WS-KDFINDOC                                           
137900                 , :MAP-DAREGDAT-LINE                                     
138000                 , :MAP-DAUPPDAT-LINE                                     
138100                 , :MAP-IDUSER-LINE                                       
138200                                                                          
138300           FROM    T01PAIN                                                
138400                                                                          
138500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
138600                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
138700                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
138800                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
138900                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
139000                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
139100     END-EXEC                                                             
139200                                                                          
139300     MOVE SQLCODE TO SQLCODE-WS                                           
139400     PERFORM DB2-STATUS-CHECK                                             
139500     .                                                                    
139600     EJECT                                                                
139700                                                                          
139800 DB2-SELECT-T01PAIN-FAB SECTION.                                          
139900     MOVE 000100 TO GOOD-SQLCODECODES                                     
140000                                                                          
140100     EXEC SQL                                                             
140200           SELECT  KDFINDOC                                               
140300                 , DAREGDAT                                               
140400                 , DAUPPDAT                                               
140500                 , IDUSER                                                 
140600                                                                          
140700           INTO    :WS-KDFINDOC                                           
140800                 , :MAP-DAREGDAT-LINE                                     
140900                 , :MAP-DAUPPDAT-LINE                                     
141000                 , :MAP-IDUSER-LINE                                       
141100                                                                          
141200           FROM    T01PAIN                                                
141300                                                                          
141400           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
141500                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
141600                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
141700                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
141800                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
141900                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
142000     END-EXEC                                                             
142100                                                                          
142200     MOVE SQLCODE TO SQLCODE-WS                                           
142300     PERFORM DB2-STATUS-CHECK                                             
142400     .                                                                    
142500     EJECT                                                                
142600                                                                          
142700 DB2-SELECT-T01PAIN-FAB-2 SECTION.                                        
142800     MOVE 000100 TO GOOD-SQLCODECODES                                     
142900                                                                          
143000     EXEC SQL                                                             
143100           SELECT  KDFINDOC                                               
143200                 , DAREGDAT                                               
143300                 , DAUPPDAT                                               
143400                 , IDUSER                                                 
143500                                                                          
143600           INTO    :WS-KDFINDOC                                           
143700                 , :MAP-DAREGDAT-LINE                                     
143800                 , :MAP-DAUPPDAT-LINE                                     
143900                 , :MAP-IDUSER-LINE                                       
144000                                                                          
144100           FROM    T01PAIN                                                
144200                                                                          
144300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
144400                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
144500                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
144600                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
144700                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
144800                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
144900     END-EXEC                                                             
145000                                                                          
145100     MOVE SQLCODE TO SQLCODE-WS                                           
145200     PERFORM DB2-STATUS-CHECK                                             
145300     .                                                                    
145400     EJECT                                                                
145500                                                                          
145600 DB2-SELECT-T01PAIN-FAC SECTION.                                          
145700     MOVE 000100 TO GOOD-SQLCODECODES                                     
145800                                                                          
145900     EXEC SQL                                                             
146000           SELECT  KDFINDOC                                               
146100                 , DAREGDAT                                               
146200                 , DAUPPDAT                                               
146300                 , IDUSER                                                 
146400                                                                          
146500           INTO    :WS-KDFINDOC                                           
146600                 , :MAP-DAREGDAT-LINE                                     
146700                 , :MAP-DAUPPDAT-LINE                                     
146800                 , :MAP-IDUSER-LINE                                       
146900                                                                          
147000           FROM    T01PAIN                                                
147100                                                                          
147200           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
147300                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
147400                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
147500                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
147600                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
147700                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
147800     END-EXEC                                                             
147900                                                                          
148000     MOVE SQLCODE TO SQLCODE-WS                                           
148100     PERFORM DB2-STATUS-CHECK                                             
148200     .                                                                    
148300     EJECT                                                                
148400                                                                          
148500 DB2-SELECT-T01PAIN-FAC-2 SECTION.                                        
148600     MOVE 000100 TO GOOD-SQLCODECODES                                     
148700                                                                          
148800     EXEC SQL                                                             
148900           SELECT  KDFINDOC                                               
149000                 , DAREGDAT                                               
149100                 , DAUPPDAT                                               
149200                 , IDUSER                                                 
149300                                                                          
149400           INTO    :WS-KDFINDOC                                           
149500                 , :MAP-DAREGDAT-LINE                                     
149600                 , :MAP-DAUPPDAT-LINE                                     
149700                 , :MAP-IDUSER-LINE                                       
149800                                                                          
149900           FROM    T01PAIN                                                
150000                                                                          
150100           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
150200                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
150300                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
150400                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
150500                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
150600                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
150700     END-EXEC                                                             
150800                                                                          
150900     MOVE SQLCODE TO SQLCODE-WS                                           
151000     PERFORM DB2-STATUS-CHECK                                             
151100     .                                                                    
151200     EJECT                                                                
151300                                                                          
151400 DB2-SELECT-T01PAIN-FAD SECTION.                                          
151500     MOVE 000100 TO GOOD-SQLCODECODES                                     
151600                                                                          
151700     EXEC SQL                                                             
151800           SELECT  KDFINDOC                                               
151900                 , DAREGDAT                                               
152000                 , DAUPPDAT                                               
152100                 , IDUSER                                                 
152200                                                                          
152300           INTO    :WS-KDFINDOC                                           
152400*                , :MAP-DAREGDAT-LINE                                     
152500*                , :MAP-DAUPPDAT-LINE                                     
152600*                , :MAP-IDUSER-LINE                                       
152700                                                                          
152800           FROM    T01PAIN                                                
152900                                                                          
153000           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
153100                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
153200                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
153300                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
153400                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
153500                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
153600     END-EXEC                                                             
153700                                                                          
153800     MOVE SQLCODE TO SQLCODE-WS                                           
153900     PERFORM DB2-STATUS-CHECK                                             
154000     .                                                                    
154100     EJECT                                                                
154200                                                                          
154300 DB2-SELECT-T01PAIN-FAD-2 SECTION.                                        
154400     MOVE 000100 TO GOOD-SQLCODECODES                                     
154500                                                                          
154600     EXEC SQL                                                             
154700           SELECT  KDFINDOC                                               
154800                 , DAREGDAT                                               
154900                 , DAUPPDAT                                               
155000                 , IDUSER                                                 
155100                                                                          
155200           INTO    :WS-KDFINDOC                                           
155300*                , :MAP-DAREGDAT-LINE                                     
155400*                , :MAP-DAUPPDAT-LINE                                     
155500*                , :MAP-IDUSER-LINE                                       
155600                                                                          
155700           FROM    T01PAIN                                                
155800                                                                          
155900           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
156000                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
156100                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
156200                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
156300                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
156400                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
156500     END-EXEC                                                             
156600                                                                          
156700     MOVE SQLCODE TO SQLCODE-WS                                           
156800     PERFORM DB2-STATUS-CHECK                                             
156900     .                                                                    
157000     EJECT                                                                
157100                                                                          
157200 DB2-SELECT-T01PAIN-FAE SECTION.                                          
157300     MOVE 000100 TO GOOD-SQLCODECODES                                     
157400                                                                          
157500     EXEC SQL                                                             
157600           SELECT  KDFINDOC                                               
157700                 , DAREGDAT                                               
157800                 , DAUPPDAT                                               
157900                 , IDUSER                                                 
158000                                                                          
158100           INTO    :WS-KDFINDOC                                           
158200                 , :MAP-DAREGDAT-LINE                                     
158300                 , :MAP-DAUPPDAT-LINE                                     
158400                 , :MAP-IDUSER-LINE                                       
158500                                                                          
158600           FROM    T01PAIN                                                
158700                                                                          
158800           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
158900                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
159000                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
159100                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
159200                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
159300                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
159400     END-EXEC                                                             
159500                                                                          
159600     MOVE SQLCODE TO SQLCODE-WS                                           
159700     PERFORM DB2-STATUS-CHECK                                             
159800     .                                                                    
159900     EJECT                                                                
160000                                                                          
160100 DB2-SELECT-T01PAIN-FAE-2 SECTION.                                        
160200     MOVE 000100 TO GOOD-SQLCODECODES                                     
160300                                                                          
160400     EXEC SQL                                                             
160500           SELECT  KDFINDOC                                               
160600                 , DAREGDAT                                               
160700                 , DAUPPDAT                                               
160800                 , IDUSER                                                 
160900                                                                          
161000           INTO    :WS-KDFINDOC                                           
161100                 , :MAP-DAREGDAT-LINE                                     
161200                 , :MAP-DAUPPDAT-LINE                                     
161300                 , :MAP-IDUSER-LINE                                       
161400                                                                          
161500           FROM    T01PAIN                                                
161600                                                                          
161700           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
161800                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
161900                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
162000                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
162100                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
162200                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
162300     END-EXEC                                                             
162400                                                                          
162500     MOVE SQLCODE TO SQLCODE-WS                                           
162600     PERFORM DB2-STATUS-CHECK                                             
162700     .                                                                    
162800     EJECT                                                                
162900                                                                          
163000 DB2-SELECT-T01PAIN-FAF SECTION.                                          
163100     MOVE 000100 TO GOOD-SQLCODECODES                                     
163200                                                                          
163300     EXEC SQL                                                             
163400           SELECT  KDFINDOC                                               
163500                 , DAREGDAT                                               
163600                 , DAUPPDAT                                               
163700                 , IDUSER                                                 
163800                                                                          
163900           INTO    :WS-KDFINDOC                                           
164000                 , :MAP-DAREGDAT-LINE                                     
164100                 , :MAP-DAUPPDAT-LINE                                     
164200                 , :MAP-IDUSER-LINE                                       
164300                                                                          
164400           FROM    T01PAIN                                                
164500                                                                          
164600           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
164700                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
164800                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
164900                AND KDVALISO = :MAP-KDVALISO-LINE                         
165000                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
165100                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
165200     END-EXEC                                                             
165300                                                                          
165400     MOVE SQLCODE TO SQLCODE-WS                                           
165500     PERFORM DB2-STATUS-CHECK                                             
165600     .                                                                    
165700     EJECT                                                                
165800                                                                          
165900 DB2-SELECT-T01PAIN-FAF-2 SECTION.                                        
166000     MOVE 000100 TO GOOD-SQLCODECODES                                     
166100                                                                          
166200     EXEC SQL                                                             
166300           SELECT  KDFINDOC                                               
166400                 , DAREGDAT                                               
166500                 , DAUPPDAT                                               
166600                 , IDUSER                                                 
166700                                                                          
166800           INTO    :WS-KDFINDOC                                           
166900                 , :MAP-DAREGDAT-LINE                                     
167000                 , :MAP-DAUPPDAT-LINE                                     
167100                 , :MAP-IDUSER-LINE                                       
167200                                                                          
167300           FROM    T01PAIN                                                
167400                                                                          
167500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
167600                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
167700                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
167800                AND KDVALISO = :MAP-KDVALISO-LINE                         
167900                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
168000                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
168100     END-EXEC                                                             
168200                                                                          
168300     MOVE SQLCODE TO SQLCODE-WS                                           
168400     PERFORM DB2-STATUS-CHECK                                             
168500     .                                                                    
168600     EJECT                                                                
168700                                                                          
168800 DB2-SELECT-T01PAIN-FAG SECTION.                                          
168900     MOVE 000100 TO GOOD-SQLCODECODES                                     
169000                                                                          
169100     EXEC SQL                                                             
169200           SELECT  KDFINDOC                                               
169300                 , DAREGDAT                                               
169400                 , DAUPPDAT                                               
169500                 , IDUSER                                                 
169600                                                                          
169700           INTO    :WS-KDFINDOC                                           
169800                 , :MAP-DAREGDAT-LINE                                     
169900                 , :MAP-DAUPPDAT-LINE                                     
170000                 , :MAP-IDUSER-LINE                                       
170100                                                                          
170200           FROM    T01PAIN                                                
170300                                                                          
170400           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
170500                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
170600                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
170700                AND KDVALISO = :MAP-KDVALISO-LINE                         
170800                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
170900                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
171000     END-EXEC                                                             
171100                                                                          
171200     MOVE SQLCODE TO SQLCODE-WS                                           
171300     PERFORM DB2-STATUS-CHECK                                             
171400     .                                                                    
171500     EJECT                                                                
171600                                                                          
171700 DB2-SELECT-T01PAIN-FAG-2 SECTION.                                        
171800     MOVE 000100 TO GOOD-SQLCODECODES                                     
171900                                                                          
172000     EXEC SQL                                                             
172100           SELECT  KDFINDOC                                               
172200                 , DAREGDAT                                               
172300                 , DAUPPDAT                                               
172400                 , IDUSER                                                 
172500                                                                          
172600           INTO    :WS-KDFINDOC                                           
172700                 , :MAP-DAREGDAT-LINE                                     
172800                 , :MAP-DAUPPDAT-LINE                                     
172900                 , :MAP-IDUSER-LINE                                       
173000                                                                          
173100           FROM    T01PAIN                                                
173200                                                                          
173300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
173400                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
173500                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
173600                AND KDVALISO = :MAP-KDVALISO-LINE                         
173700                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
173800                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
173900     END-EXEC                                                             
174000                                                                          
174100     MOVE SQLCODE TO SQLCODE-WS                                           
174200     PERFORM DB2-STATUS-CHECK                                             
174300     .                                                                    
174400     EJECT                                                                
174500                                                                          
174600 DB2-SELECT-T01PAIN-FAH SECTION.                                          
174700     MOVE 000100 TO GOOD-SQLCODECODES                                     
174800                                                                          
174900     EXEC SQL                                                             
175000           SELECT  KDFINDOC                                               
175100                 , DAREGDAT                                               
175200                 , DAUPPDAT                                               
175300                 , IDUSER                                                 
175400                                                                          
175500           INTO    :WS-KDFINDOC                                           
175600                 , :MAP-DAREGDAT-LINE                                     
175700                 , :MAP-DAUPPDAT-LINE                                     
175800                 , :MAP-IDUSER-LINE                                       
175900                                                                          
176000           FROM    T01PAIN                                                
176100                                                                          
176200           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
176300                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
176400                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
176500                AND KDVALISO = :MAP-KDVALISO-LINE                         
176600                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
176700                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
176800     END-EXEC                                                             
176900                                                                          
177000     MOVE SQLCODE TO SQLCODE-WS                                           
177100     PERFORM DB2-STATUS-CHECK                                             
177200     .                                                                    
177300     EJECT                                                                
177400                                                                          
177500 DB2-SELECT-T01PAIN-FAH-2 SECTION.                                        
177600     MOVE 000100 TO GOOD-SQLCODECODES                                     
177700                                                                          
177800     EXEC SQL                                                             
177900           SELECT  KDFINDOC                                               
178000                 , DAREGDAT                                               
178100                 , DAUPPDAT                                               
178200                 , IDUSER                                                 
178300                                                                          
178400           INTO    :WS-KDFINDOC                                           
178500                 , :MAP-DAREGDAT-LINE                                     
178600                 , :MAP-DAUPPDAT-LINE                                     
178700                 , :MAP-IDUSER-LINE                                       
178800                                                                          
178900           FROM    T01PAIN                                                
179000                                                                          
179100           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
179200                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
179300                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
179400                AND KDVALISO = :MAP-KDVALISO-LINE                         
179500                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
179600                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
179700     END-EXEC                                                             
179800                                                                          
179900     MOVE SQLCODE TO SQLCODE-WS                                           
180000     PERFORM DB2-STATUS-CHECK                                             
180100     .                                                                    
180200     EJECT                                                                
180300                                                                          
180400 DB2-SELECT-T01PAIN-FAI SECTION.                                          
180500     MOVE 000100 TO GOOD-SQLCODECODES                                     
180600                                                                          
180700     EXEC SQL                                                             
180800           SELECT  KDFINDOC                                               
180900                 , DAREGDAT                                               
181000                 , DAUPPDAT                                               
181100                 , IDUSER                                                 
181200                                                                          
181300           INTO    :WS-KDFINDOC                                           
181400                 , :MAP-DAREGDAT-LINE                                     
181500                 , :MAP-DAUPPDAT-LINE                                     
181600                 , :MAP-IDUSER-LINE                                       
181700                                                                          
181800           FROM    T01PAIN                                                
181900                                                                          
182000           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
182100                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
182200                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
182300                AND KDVALISO = :MAP-KDVALISO-LINE                         
182400                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
182500                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
182600     END-EXEC                                                             
182700                                                                          
182800     MOVE SQLCODE TO SQLCODE-WS                                           
182900     PERFORM DB2-STATUS-CHECK                                             
183000     .                                                                    
183100     EJECT                                                                
183200                                                                          
183300 DB2-SELECT-T01PAIN-FAI-2 SECTION.                                        
183400     MOVE 000100 TO GOOD-SQLCODECODES                                     
183500                                                                          
183600     EXEC SQL                                                             
183700           SELECT  KDFINDOC                                               
183800                 , DAREGDAT                                               
183900                 , DAUPPDAT                                               
184000                 , IDUSER                                                 
184100                                                                          
184200           INTO    :WS-KDFINDOC                                           
184300                 , :MAP-DAREGDAT-LINE                                     
184400                 , :MAP-DAUPPDAT-LINE                                     
184500                 , :MAP-IDUSER-LINE                                       
184600                                                                          
184700           FROM    T01PAIN                                                
184800                                                                          
184900           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
185000                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
185100                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
185200                AND KDVALISO = :MAP-KDVALISO-LINE                         
185300                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
185400                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
185500     END-EXEC                                                             
185600                                                                          
185700     MOVE SQLCODE TO SQLCODE-WS                                           
185800     PERFORM DB2-STATUS-CHECK                                             
185900     .                                                                    
186000     EJECT                                                                
186100                                                                          
186200 DB2-SELECT-T01PAIN-FAJ SECTION.                                          
186300     MOVE 000100 TO GOOD-SQLCODECODES                                     
186400                                                                          
186500     EXEC SQL                                                             
186600           SELECT  KDFINDOC                                               
186700                 , DAREGDAT                                               
186800                 , DAUPPDAT                                               
186900                 , IDUSER                                                 
187000                                                                          
187100           INTO    :WS-KDFINDOC                                           
187200                 , :MAP-DAREGDAT-LINE                                     
187300                 , :MAP-DAUPPDAT-LINE                                     
187400                 , :MAP-IDUSER-LINE                                       
187500                                                                          
187600           FROM    T01PAIN                                                
187700                                                                          
187800           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
187900                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
188000                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
188100                AND KDVALISO = :MAP-KDVALISO-LINE                         
188200                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
188300                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
188400     END-EXEC                                                             
188500                                                                          
188600     MOVE SQLCODE TO SQLCODE-WS                                           
188700     PERFORM DB2-STATUS-CHECK                                             
188800     .                                                                    
188900     EJECT                                                                
189000                                                                          
189100 DB2-SELECT-T01PAIN-FAJ-2 SECTION.                                        
189200     MOVE 000100 TO GOOD-SQLCODECODES                                     
189300                                                                          
189400     EXEC SQL                                                             
189500           SELECT  KDFINDOC                                               
189600                 , DAREGDAT                                               
189700                 , DAUPPDAT                                               
189800                 , IDUSER                                                 
189900                                                                          
190000           INTO    :WS-KDFINDOC                                           
190100                 , :MAP-DAREGDAT-LINE                                     
190200                 , :MAP-DAUPPDAT-LINE                                     
190300                 , :MAP-IDUSER-LINE                                       
190400                                                                          
190500           FROM    T01PAIN                                                
190600                                                                          
190700           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
190800                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
190900                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
191000                AND KDVALISO = :MAP-KDVALISO-LINE                         
191100                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
191200                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
191300     END-EXEC                                                             
191400                                                                          
191500     MOVE SQLCODE TO SQLCODE-WS                                           
191600     PERFORM DB2-STATUS-CHECK                                             
191700     .                                                                    
191800     EJECT                                                                
191900                                                                          
192000 DB2-SELECT-T01PAIN-FAK SECTION.                                          
192100     MOVE 000100 TO GOOD-SQLCODECODES                                     
192200                                                                          
192300     EXEC SQL                                                             
192400           SELECT  KDFINDOC                                               
192500                 , DAREGDAT                                               
192600                 , DAUPPDAT                                               
192700                 , IDUSER                                                 
192800                                                                          
192900           INTO    :WS-KDFINDOC                                           
193000                 , :MAP-DAREGDAT-LINE                                     
193100                 , :MAP-DAUPPDAT-LINE                                     
193200                 , :MAP-IDUSER-LINE                                       
193300                                                                          
193400           FROM    T01PAIN                                                
193500                                                                          
193600           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
193700                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
193800                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
193900                AND KDVALISO = :MAP-KDVALISO-LINE                         
194000                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
194100                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
194200     END-EXEC                                                             
194300                                                                          
194400     MOVE SQLCODE TO SQLCODE-WS                                           
194500     PERFORM DB2-STATUS-CHECK                                             
194600     .                                                                    
194700     EJECT                                                                
194800                                                                          
194900 DB2-SELECT-T01PAIN-FAK-2 SECTION.                                        
195000     MOVE 000100 TO GOOD-SQLCODECODES                                     
195100                                                                          
195200     EXEC SQL                                                             
195300           SELECT  KDFINDOC                                               
195400                 , DAREGDAT                                               
195500                 , DAUPPDAT                                               
195600                 , IDUSER                                                 
195700                                                                          
195800           INTO    :WS-KDFINDOC                                           
195900                 , :MAP-DAREGDAT-LINE                                     
196000                 , :MAP-DAUPPDAT-LINE                                     
196100                 , :MAP-IDUSER-LINE                                       
196200                                                                          
196300           FROM    T01PAIN                                                
196400                                                                          
196500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
196600                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
196700                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
196800                AND KDVALISO = :MAP-KDVALISO-LINE                         
196900                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
197000                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
197100     END-EXEC                                                             
197200                                                                          
197300     MOVE SQLCODE TO SQLCODE-WS                                           
197400     PERFORM DB2-STATUS-CHECK                                             
197500     .                                                                    
197600     EJECT                                                                
197700                                                                          
197800 DB2-SELECT-T01PAIN-FAL SECTION.                                          
197900     MOVE 000100 TO GOOD-SQLCODECODES                                     
198000                                                                          
198100     EXEC SQL                                                             
198200           SELECT  KDFINDOC                                               
198300                 , DAREGDAT                                               
198400                 , DAUPPDAT                                               
198500                 , IDUSER                                                 
198600                                                                          
198700           INTO    :WS-KDFINDOC                                           
198800                                                                          
198900           FROM    T01PAIN                                                
199000                                                                          
199100           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
199200                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
199300                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
199400                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
199500                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
199600                AND KDSTATUS = :MAP-KDSTATUS-LINE                         
199700     END-EXEC                                                             
199800                                                                          
199900     MOVE SQLCODE TO SQLCODE-WS                                           
200000     PERFORM DB2-STATUS-CHECK                                             
200100     .                                                                    
200200     EJECT                                                                
200300                                                                          
200400 DB2-SELECT-T01PAIN-FAL-2 SECTION.                                        
200500     MOVE 000100 TO GOOD-SQLCODECODES                                     
200600                                                                          
200700     EXEC SQL                                                             
200800           SELECT  KDFINDOC                                               
200900                 , DAREGDAT                                               
201000                 , DAUPPDAT                                               
201100                 , IDUSER                                                 
201200                                                                          
201300           INTO    :WS-KDFINDOC                                           
201400                                                                          
201500           FROM    T01PAIN                                                
201600                                                                          
201700           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
201800                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
201900                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
202000                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
202100                AND KDVALISO = :MAP-KDVALISO-LINE-2                       
202200                AND KDSTATUS = :MAP-KDSTATUS-LINE-2                       
202300     END-EXEC                                                             
202400                                                                          
202500     MOVE SQLCODE TO SQLCODE-WS                                           
202600     PERFORM DB2-STATUS-CHECK                                             
202700     .                                                                    
202800     EJECT                                                                
202900                                                                          
203000* * * * * * * * * *   - CURSOR-FAA- * * * * * * * * * * * * * *           
203100 DB2-COUNT-CRS-FAA SECTION.                                               
203200     EXEC SQL                                                             
203300                                                                          
203400           SELECT COUNT(*)                                                
203500                                                                          
203600           INTO  :WS-COUNTER-T01PAIN                                      
203700                                                                          
203800           FROM   T01CUGR A                                               
203900                , T01DOTY B                                               
204000                                                                          
204100           WHERE    A.IDLEGSEL   = :REQU-IDLEGSEL-KEY                     
204200                AND A.IDLEGSEL   = B.IDLEGSEL                             
204300                AND ( B.KDFINDOC = :REQU-KDFINDOC-KEY                     
204400                OR  A.KDPARTTY   = :REQU-KDPARTTY-KEY                     
204500                OR  A.KDPARTGR   = :REQU-KDPARTGR-KEY )                   
204600                AND A.DADELDAT   = '00000000'                             
204700                AND A.KDSTATUS   = :WS-CURRENT                            
204800                                                                          
204900     END-EXEC                                                             
205000                                                                          
205100     MOVE 000100  TO GOOD-SQLCODECODES                                    
205200                                                                          
205300     MOVE SQLCODE TO SQLCODE-WS                                           
205400     PERFORM DB2-STATUS-CHECK                                             
205500     .                                                                    
205600     EJECT                                                                
205700                                                                          
205800 DB2-DCL-OPN-T01DOTY-CRS-3 SECTION.                                       
205900     MOVE 000100 TO GOOD-SQLCODECODES                                     
206000                                                                          
206100     EXEC SQL                                                             
206200         DECLARE T01DOTY-CRS-3 CURSOR WITH HOLD FOR                       
206300                                                                          
206400           SELECT  A.KDFINDOC                                             
206500                 , B.KDPARTTY                                             
206600                 , B.KDPARTGR                                             
206700                 , A.KDSTATUS                                             
206800                 , A.DAREGDAT                                             
206900                 , A.DAUPPDAT                                             
207000                 , A.IDUSER                                               
207100                                                                          
207200           FROM    T01DOTY A                                              
207300                 , T01CUGR B                                              
207400                                                                          
207500           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
207600                AND A.IDLEGSEL = B.IDLEGSEL                               
207700                AND A.DADELDAT = '00000000'                               
207800                AND B.DADELDAT = '00000000'                               
207900                AND A.KDSTATUS = :WS-CURRENT                              
208000                AND B.KDSTATUS = :WS-CURRENT                              
208100                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
208200                AND B.KDPARTTY = :REQU-KDPARTTY-KEY                       
208300                AND B.KDPARTGR = :REQU-KDPARTGR-KEY                       
208400                                                                          
208500           ORDER BY A.IDLEGSEL                                            
208600                  , A.KDFINDOC                                            
208700                  , B.KDPARTTY                                            
208800                  , B.KDPARTGR                                            
208900     END-EXEC                                                             
209000                                                                          
209100     MOVE 000100  TO GOOD-SQLCODECODES                                    
209200                                                                          
209300     EXEC SQL                                                             
209400        OPEN T01DOTY-CRS-3                                                
209500     END-EXEC                                                             
209600                                                                          
209700     MOVE SQLCODE TO SQLCODE-WS                                           
209800     PERFORM DB2-STATUS-CHECK                                             
209900     .                                                                    
210000     EJECT                                                                
210100                                                                          
210200 DB2-FETCH-T01DOTY-CRS-3 SECTION.                                         
210300     MOVE 000100  TO GOOD-SQLCODECODES                                    
210400                                                                          
210500     EXEC SQL                                                             
210600                                                                          
210700         FETCH T01DOTY-CRS-3                                              
210800                                                                          
210900         INTO :MAP-KDFINDOC-LINE                                          
211000            , :MAP-KDPARTTY-LINE                                          
211100            , :MAP-KDPARTGR-LINE                                          
211200            , :MAP-KDSTATUS-LINE-2                                        
211300            , :MAP-DAREGDAT-LINE-2                                        
211400            , :MAP-DAUPPDAT-LINE-2                                        
211500            , :MAP-IDUSER-LINE-2                                          
211600     END-EXEC                                                             
211700                                                                          
211800     MOVE SQLCODE TO SQLCODE-WS                                           
211900     PERFORM DB2-STATUS-CHECK                                             
212000     .                                                                    
212100     EJECT                                                                
212200                                                                          
212300 DB2-CLOSE-T01DOTY-CRS-3 SECTION.                                         
212400     EXEC SQL                                                             
212500        CLOSE T01DOTY-CRS-3                                               
212600     END-EXEC                                                             
212700     .                                                                    
212800     EJECT                                                                
212900                                                                          
213000                                                                          
213100 DB2-DCL-OPN-T01PAIN-CRS-1 SECTION.                                       
213200     MOVE 000100 TO GOOD-SQLCODECODES                                     
213300                                                                          
213400     EXEC SQL                                                             
213500         DECLARE T01PAIN-CRS-1 CURSOR WITH HOLD FOR                       
213600                                                                          
213700           SELECT  DISTINCT KDFINDOC                                      
213800                 , IDLEGSEL                                               
213900                 , KDPARTTY                                               
214000                 , KDPARTGR                                               
214100                 , KDVALISO                                               
214200                 , KDSTATUS                                               
214300                 , KDSTATUS                                               
214400                 , DAREGDAT                                               
214500                 , DAUPPDAT                                               
214600                 , IDUSER                                                 
214700                                                                          
214800           FROM    T01PAIN                                                
214900                                                                          
215000           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
215100                AND KDFINDOC = :REQU-KDFINDOC-KEY                         
215200                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
215300                AND KDPARTGR = :REQU-KDPARTGR-KEY                         
215400                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
215500                                                                          
215600           ORDER BY IDLEGSEL                                              
215700                  , KDVALISO                                              
215800                  , KDSTATUS DESC                                         
215900     END-EXEC                                                             
216000                                                                          
216100     MOVE 000100  TO GOOD-SQLCODECODES                                    
216200                                                                          
216300     EXEC SQL                                                             
216400        OPEN T01PAIN-CRS-1                                                
216500     END-EXEC                                                             
216600                                                                          
216700     MOVE SQLCODE TO SQLCODE-WS                                           
216800     PERFORM DB2-STATUS-CHECK                                             
216900     .                                                                    
217000     EJECT                                                                
217100                                                                          
217200 DB2-FETCH-T01PAIN-CRS-1 SECTION.                                         
217300     MOVE 000100  TO GOOD-SQLCODECODES                                    
217400                                                                          
217500     EXEC SQL                                                             
217600                                                                          
217700         FETCH T01PAIN-CRS-1                                              
217800                                                                          
217900         INTO :MAP-KDFINDOC-LINE                                          
218000            , :MAP-IDLEGSEL-LINE                                          
218100            , :MAP-KDPARTTY-LINE                                          
218200            , :MAP-KDPARTGR-LINE                                          
218300            , :MAP-KDVALISO-LINE-2                                        
218400            , :MAP-KDSTATUS-LINE                                          
218500            , :MAP-KDSTATUS-LINE-2                                        
218600            , :MAP-DAREGDAT-LINE                                          
218700            , :MAP-DAUPPDAT-LINE                                          
218800            , :MAP-IDUSER-LINE                                            
218900     END-EXEC                                                             
219000                                                                          
219100     MOVE SQLCODE TO SQLCODE-WS                                           
219200     PERFORM DB2-STATUS-CHECK                                             
219300     .                                                                    
219400     EJECT                                                                
219500                                                                          
219600 DB2-CLOSE-T01PAIN-CRS-1 SECTION.                                         
219700     EXEC SQL                                                             
219800        CLOSE T01PAIN-CRS-1                                               
219900     END-EXEC                                                             
220000     .                                                                    
220100     EJECT                                                                
220200                                                                          
220300* * * * * * * * * *   - CURSOR-FAB-  * * * * * * * * * * * * *            
220400 DB2-COUNT-CRS-FAB SECTION.                                               
220500     EXEC SQL                                                             
220600                                                                          
220700          SELECT COUNT(*)                                                 
220800                                                                          
220900          INTO  :WS-COUNTER-FAB                                           
221000                                                                          
221100          FROM   T01DOTY A                                                
221200               , T01CUGR B                                                
221300                                                                          
221400          WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
221500             AND   A.IDLEGSEL = B.IDLEGSEL                                
221600             AND ( A.KDFINDOC = :REQU-KDFINDOC-KEY                        
221700             OR    B.KDPARTTY = :REQU-KDPARTTY-KEY )                      
221800             AND   A.KDSTATUS = :WS-CURRENT                               
221900             AND   A.DADELDAT = '00000000'                                
222000                                                                          
222100     END-EXEC                                                             
222200                                                                          
222300     MOVE 000100  TO GOOD-SQLCODECODES                                    
222400                                                                          
222500     MOVE SQLCODE TO SQLCODE-WS                                           
222600     PERFORM DB2-STATUS-CHECK                                             
222700     .                                                                    
222800     EJECT                                                                
222900                                                                          
223000 DB2-DCL-OPN-T01DOTY-CRS-2 SECTION.                                       
223100     MOVE 000100 TO GOOD-SQLCODECODES                                     
223200                                                                          
223300     EXEC SQL                                                             
223400         DECLARE T01DOTY-CRS-2 CURSOR WITH HOLD FOR                       
223500                                                                          
223600           SELECT  A.KDFINDOC                                             
223700                 , B.KDPARTTY                                             
223800                 , B.KDPARTGR                                             
223900                 , A.KDSTATUS                                             
224000                 , A.DAREGDAT                                             
224100                 , A.DAUPPDAT                                             
224200                 , A.IDUSER                                               
224300                                                                          
224400           FROM    T01DOTY A                                              
224500                 , T01CUGR B                                              
224600                                                                          
224700           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
224800                AND A.IDLEGSEL = B.IDLEGSEL                               
224900                AND A.DADELDAT = '00000000'                               
225000                AND B.DADELDAT = '00000000'                               
225100                AND A.KDSTATUS = :WS-CURRENT                              
225200                AND B.KDSTATUS = :WS-CURRENT                              
225300                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
225400                AND B.KDPARTTY = :REQU-KDPARTTY-KEY                       
225500                                                                          
225600           ORDER BY A.IDLEGSEL                                            
225700                  , B.KDPARTGR                                            
225800     END-EXEC                                                             
225900                                                                          
226000     MOVE 000100  TO GOOD-SQLCODECODES                                    
226100                                                                          
226200     EXEC SQL                                                             
226300        OPEN T01DOTY-CRS-2                                                
226400     END-EXEC                                                             
226500                                                                          
226600     MOVE SQLCODE TO SQLCODE-WS                                           
226700     PERFORM DB2-STATUS-CHECK                                             
226800     .                                                                    
226900     EJECT                                                                
227000                                                                          
227100 DB2-FETCH-T01DOTY-CRS-2 SECTION.                                         
227200     MOVE 000100  TO GOOD-SQLCODECODES                                    
227300                                                                          
227400     EXEC SQL                                                             
227500                                                                          
227600         FETCH T01DOTY-CRS-2                                              
227700                                                                          
227800         INTO :MAP-KDFINDOC-LINE                                          
227900            , :MAP-KDPARTTY-LINE                                          
228000            , :MAP-KDPARTGR-LINE-2                                        
228100            , :MAP-KDSTATUS-LINE-2                                        
228200            , :MAP-DAREGDAT-LINE-2                                        
228300            , :MAP-DAUPPDAT-LINE-2                                        
228400            , :MAP-IDUSER-LINE-2                                          
228500     END-EXEC                                                             
228600                                                                          
228700     MOVE SQLCODE TO SQLCODE-WS                                           
228800     PERFORM DB2-STATUS-CHECK                                             
228900     .                                                                    
229000     EJECT                                                                
229100                                                                          
229200 DB2-CLOSE-T01DOTY-CRS-2 SECTION.                                         
229300     EXEC SQL                                                             
229400        CLOSE T01DOTY-CRS-2                                               
229500     END-EXEC                                                             
229600     .                                                                    
229700     EJECT                                                                
229800                                                                          
229900 DB2-DCL-OPN-T01PAIN-CRS-2A SECTION.                                      
230000     MOVE 000100 TO GOOD-SQLCODECODES                                     
230100                                                                          
230200     EXEC SQL                                                             
230300         DECLARE T01PAIN-CRS-2A CURSOR WITH HOLD FOR                      
230400                                                                          
230500          SELECT   A.KDFINDOC                                             
230600                 , B.KDPARTTY                                             
230700                 , B.KDPARTGR                                             
230800                 , A.KDVALISO                                             
230900                 , A.KDSTATUS                                             
231000                 , B.KDSTATUS                                             
231100                 , A.DAREGDAT                                             
231200                 , B.DAREGDAT                                             
231300                 , A.DAUPPDAT                                             
231400                 , B.DAUPPDAT                                             
231500                 , A.IDUSER                                               
231600                 , B.IDUSER                                               
231700                                                                          
231800          FROM     T01PAIN A                                              
231900                 , T01CUGR B                                              
232000                                                                          
232100          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
232200             AND   A.IDLEGSEL = B.IDLEGSEL                                
232300             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
232400             AND   B.KDPARTTY = :REQU-KDPARTTY-KEY                        
232500             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
232600             AND   B.KDSTATUS = :WS-CURRENT                               
232700             AND   B.DADELDAT = '00000000'                                
232800             AND   A.KDPARTGR =  B.KDPARTGR                               
232900             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
233000             AND   A.IDLEGSEL = B.IDLEGSEL                                
233100             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
233200             AND   B.KDPARTTY = :REQU-KDPARTTY-KEY                        
233300             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
233400             AND   B.KDSTATUS = :WS-CURRENT                               
233500             AND   B.DADELDAT = '00000000'                                
233600             AND   A.KDPARTGR <> B.KDPARTGR )                             
233700                                                                          
233800          ORDER BY A.IDLEGSEL                                             
233900                 , A.KDFINDOC                                             
234000                 , A.KDPARTTY                                             
234100                 , A.KDSTATUS DESC                                        
234200     END-EXEC                                                             
234300                                                                          
234400     MOVE 000100  TO GOOD-SQLCODECODES                                    
234500                                                                          
234600     EXEC SQL                                                             
234700        OPEN T01PAIN-CRS-2A                                               
234800     END-EXEC                                                             
234900                                                                          
235000     MOVE SQLCODE TO SQLCODE-WS                                           
235100     PERFORM DB2-STATUS-CHECK                                             
235200     .                                                                    
235300     EJECT                                                                
235400                                                                          
235500 DB2-FETCH-T01PAIN-CRS-2A SECTION.                                        
235600     MOVE 000100  TO GOOD-SQLCODECODES                                    
235700                                                                          
235800     EXEC SQL                                                             
235900                                                                          
236000         FETCH T01PAIN-CRS-2A                                             
236100                                                                          
236200         INTO :MAP-KDFINDOC-LINE                                          
236300            , :MAP-KDPARTTY-LINE                                          
236400            , :MAP-KDPARTGR-LINE-2                                        
236500            , :MAP-KDVALISO-LINE-2                                        
236600            , :MAP-KDSTATUS-LINE                                          
236700            , :MAP-KDSTATUS-LINE-2                                        
236800            , :MAP-DAREGDAT-LINE                                          
236900            , :MAP-DAREGDAT-LINE-2                                        
237000            , :MAP-DAUPPDAT-LINE                                          
237100            , :MAP-DAUPPDAT-LINE-2                                        
237200            , :MAP-IDUSER-LINE                                            
237300            , :MAP-IDUSER-LINE-2                                          
237400     END-EXEC                                                             
237500                                                                          
237600     MOVE SQLCODE TO SQLCODE-WS                                           
237700     PERFORM DB2-STATUS-CHECK                                             
237800     .                                                                    
237900     EJECT                                                                
238000                                                                          
238100 DB2-CLOSE-T01PAIN-CRS-2A SECTION.                                        
238200     EXEC SQL                                                             
238300        CLOSE T01PAIN-CRS-2A                                              
238400     END-EXEC                                                             
238500     .                                                                    
238600     EJECT                                                                
238700                                                                          
238800* * * * * * * * * *   - CURSOR-FAC-  * * * * * * * * * * * * *            
238900 DB2-COUNT-CRS-FAC SECTION.                                               
239000     EXEC SQL                                                             
239100                                                                          
239200          SELECT COUNT(*)                                                 
239300                                                                          
239400          INTO  :WS-COUNTER-FAC                                           
239500                                                                          
239600          FROM   T01DOTY                                                  
239700                                                                          
239800          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
239900             AND   KDSTATUS = :WS-CURRENT                                 
240000             AND   DADELDAT = '00000000'                                  
240100             AND   KDFINDOC = :REQU-KDFINDOC-KEY                          
240200                                                                          
240300     END-EXEC                                                             
240400                                                                          
240500     MOVE 000100  TO GOOD-SQLCODECODES                                    
240600                                                                          
240700     MOVE SQLCODE TO SQLCODE-WS                                           
240800     PERFORM DB2-STATUS-CHECK                                             
240900     .                                                                    
241000     EJECT                                                                
241100                                                                          
241200 DB2-DCL-OPN-T01DOTY-CRS-1 SECTION.                                       
241300     MOVE 000100 TO GOOD-SQLCODECODES                                     
241400                                                                          
241500     EXEC SQL                                                             
241600         DECLARE T01DOTY-CRS-1 CURSOR WITH HOLD FOR                       
241700                                                                          
241800           SELECT  A.KDFINDOC                                             
241900                 , B.KDPARTTY                                             
242000                 , B.KDPARTGR                                             
242100                 , A.KDSTATUS                                             
242200                 , A.DAREGDAT                                             
242300                 , A.DAUPPDAT                                             
242400                 , A.IDUSER                                               
242500                                                                          
242600           FROM    T01DOTY A                                              
242700                 , T01CUGR B                                              
242800                                                                          
242900           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
243000                AND A.IDLEGSEL = B.IDLEGSEL                               
243100                AND A.DADELDAT = '00000000'                               
243200                AND B.DADELDAT = '00000000'                               
243300                AND A.KDSTATUS = :WS-CURRENT                              
243400                AND B.KDSTATUS = :WS-CURRENT                              
243500                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
243600                                                                          
243700           ORDER BY A.IDLEGSEL                                            
243800                  , A.KDFINDOC                                            
243900                  , B.KDPARTTY                                            
244000                  , B.KDPARTGR                                            
244100     END-EXEC                                                             
244200                                                                          
244300     MOVE 000100  TO GOOD-SQLCODECODES                                    
244400                                                                          
244500     EXEC SQL                                                             
244600        OPEN T01DOTY-CRS-1                                                
244700     END-EXEC                                                             
244800                                                                          
244900     MOVE SQLCODE TO SQLCODE-WS                                           
245000     PERFORM DB2-STATUS-CHECK                                             
245100     .                                                                    
245200     EJECT                                                                
245300                                                                          
245400 DB2-FETCH-T01DOTY-CRS-1 SECTION.                                         
245500     MOVE 000100  TO GOOD-SQLCODECODES                                    
245600                                                                          
245700     EXEC SQL                                                             
245800                                                                          
245900         FETCH T01DOTY-CRS-1                                              
246000                                                                          
246100         INTO :MAP-KDFINDOC-LINE                                          
246200            , :MAP-KDPARTTY-LINE-2                                        
246300            , :MAP-KDPARTGR-LINE-2                                        
246400            , :MAP-KDSTATUS-LINE-2                                        
246500            , :MAP-DAREGDAT-LINE-2                                        
246600            , :MAP-DAUPPDAT-LINE-2                                        
246700            , :MAP-IDUSER-LINE-2                                          
246800     END-EXEC                                                             
246900                                                                          
247000     MOVE SQLCODE TO SQLCODE-WS                                           
247100     PERFORM DB2-STATUS-CHECK                                             
247200     .                                                                    
247300     EJECT                                                                
247400                                                                          
247500 DB2-CLOSE-T01DOTY-CRS-1 SECTION.                                         
247600     EXEC SQL                                                             
247700        CLOSE T01DOTY-CRS-1                                               
247800     END-EXEC                                                             
247900     .                                                                    
248000     EJECT                                                                
248100                                                                          
248200 DB2-DCL-OPN-T01PAIN-CRS-3A SECTION.                                      
248300     MOVE 000100 TO GOOD-SQLCODECODES                                     
248400                                                                          
248500     EXEC SQL                                                             
248600         DECLARE T01PAIN-CRS-3A CURSOR WITH HOLD FOR                      
248700                                                                          
248800          SELECT   A.KDFINDOC                                             
248900                 , B.KDPARTTY                                             
249000                 , B.KDPARTGR                                             
249100                 , A.KDVALISO                                             
249200                 , A.KDSTATUS                                             
249300                 , B.KDSTATUS                                             
249400                 , A.DAREGDAT                                             
249500                 , B.DAREGDAT                                             
249600                 , A.DAUPPDAT                                             
249700                 , B.DAUPPDAT                                             
249800                 , A.IDUSER                                               
249900                 , B.IDUSER                                               
250000                                                                          
250100          FROM     T01PAIN A                                              
250200                 , T01CUGR B                                              
250300                                                                          
250400          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
250500             AND   A.IDLEGSEL = B.IDLEGSEL                                
250600             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
250700             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
250800             AND   B.KDSTATUS = :WS-CURRENT                               
250900             AND   B.DADELDAT = '00000000'                                
251000             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
251100             AND   A.IDLEGSEL = B.IDLEGSEL                                
251200             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
251300             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
251400             AND   B.KDSTATUS = :WS-CURRENT                               
251500             AND   B.DADELDAT = '00000000'                                
251600             AND ( B.KDPARTTY <> A.KDPARTTY                               
251700             AND   B.KDPARTGR <> A.KDPARTGR )                             
251800             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
251900             AND   A.IDLEGSEL = B.IDLEGSEL                                
252000             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
252100             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
252200             AND   B.KDSTATUS = :WS-CURRENT                               
252300             AND   B.DADELDAT = '00000000'                                
252400             AND   B.KDPARTTY =  A.KDPARTTY                               
252500             AND   B.KDPARTGR =  A.KDPARTGR ))                            
252600                                                                          
252700          ORDER BY A.IDLEGSEL                                             
252800                 , B.KDPARTTY                                             
252900                 , B.KDPARTGR                                             
253000                 , A.KDSTATUS DESC                                        
253100     END-EXEC                                                             
253200                                                                          
253300     MOVE 000100  TO GOOD-SQLCODECODES                                    
253400                                                                          
253500     EXEC SQL                                                             
253600        OPEN T01PAIN-CRS-3A                                               
253700     END-EXEC                                                             
253800                                                                          
253900     MOVE SQLCODE TO SQLCODE-WS                                           
254000     PERFORM DB2-STATUS-CHECK                                             
254100     .                                                                    
254200     EJECT                                                                
254300                                                                          
254400 DB2-FETCH-T01PAIN-CRS-3A SECTION.                                        
254500     MOVE 000100  TO GOOD-SQLCODECODES                                    
254600                                                                          
254700     EXEC SQL                                                             
254800                                                                          
254900         FETCH T01PAIN-CRS-3A                                             
255000                                                                          
255100         INTO :MAP-KDFINDOC-LINE                                          
255200            , :MAP-KDPARTTY-LINE-2                                        
255300            , :MAP-KDPARTGR-LINE-2                                        
255400            , :MAP-KDVALISO-LINE-2                                        
255500            , :MAP-KDSTATUS-LINE                                          
255600            , :MAP-KDSTATUS-LINE-2                                        
255700            , :MAP-DAREGDAT-LINE                                          
255800            , :MAP-DAREGDAT-LINE-2                                        
255900            , :MAP-DAUPPDAT-LINE                                          
256000            , :MAP-DAUPPDAT-LINE-2                                        
256100            , :MAP-IDUSER-LINE                                            
256200            , :MAP-IDUSER-LINE-2                                          
256300     END-EXEC                                                             
256400                                                                          
256500     MOVE SQLCODE TO SQLCODE-WS                                           
256600     PERFORM DB2-STATUS-CHECK                                             
256700     .                                                                    
256800     EJECT                                                                
256900                                                                          
257000 DB2-CLOSE-T01PAIN-CRS-3A SECTION.                                        
257100     EXEC SQL                                                             
257200        CLOSE T01PAIN-CRS-3A                                              
257300     END-EXEC                                                             
257400     .                                                                    
257500     EJECT                                                                
257600                                                                          
257700* * * * * * * * * *   - CURSOR-FAD-  * * * * * * * * * * * * *            
257800 DB2-COUNT-CRS-FAD SECTION.                                               
257900     EXEC SQL                                                             
258000                                                                          
258100          SELECT COUNT(*)                                                 
258200                                                                          
258300          INTO  :WS-COUNTER-FAD                                           
258400                                                                          
258500          FROM   T01CUGR A                                                
258600                                                                          
258700          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
258800             AND   KDPARTTY = :REQU-KDPARTTY-KEY                          
258900             AND   KDPARTGR = :REQU-KDPARTGR-KEY                          
259000             AND   KDSTATUS = :WS-CURRENT                                 
259100             AND   DADELDAT = '00000000'                                  
259200                                                                          
259300     END-EXEC                                                             
259400                                                                          
259500     MOVE 000100  TO GOOD-SQLCODECODES                                    
259600                                                                          
259700     MOVE SQLCODE TO SQLCODE-WS                                           
259800     PERFORM DB2-STATUS-CHECK                                             
259900     .                                                                    
260000     EJECT                                                                
260100                                                                          
260200 DB2-DCL-OPN-T01CUGR-CRS-2 SECTION.                                       
260300     MOVE 000100 TO GOOD-SQLCODECODES                                     
260400                                                                          
260500     EXEC SQL                                                             
260600         DECLARE T01CUGR-CRS-2 CURSOR WITH HOLD FOR                       
260700                                                                          
260800           SELECT  B.KDFINDOC                                             
260900                 , A.KDPARTTY                                             
261000                 , A.KDPARTGR                                             
261100                 , A.KDSTATUS                                             
261200                 , A.DAREGDAT                                             
261300                 , A.DAUPPDAT                                             
261400                 , A.IDUSER                                               
261500                                                                          
261600           FROM    T01CUGR A                                              
261700                 , T01DOTY B                                              
261800                                                                          
261900           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
262000                AND A.IDLEGSEL = B.IDLEGSEL                               
262100                AND A.DADELDAT = '00000000'                               
262200                AND B.DADELDAT = '00000000'                               
262300                AND A.KDSTATUS = :WS-CURRENT                              
262400                AND B.KDSTATUS = :WS-CURRENT                              
262500                AND ( A.KDPARTTY = :REQU-KDPARTTY-KEY                     
262600                OR  A.KDPARTGR = :REQU-KDPARTTY-KEY )                     
262700                                                                          
262800           ORDER BY A.IDLEGSEL                                            
262900                  , B.KDFINDOC                                            
263000                  , A.KDPARTTY                                            
263100                  , A.KDPARTGR                                            
263200     END-EXEC                                                             
263300                                                                          
263400     MOVE 000100  TO GOOD-SQLCODECODES                                    
263500                                                                          
263600     EXEC SQL                                                             
263700        OPEN T01CUGR-CRS-2                                                
263800     END-EXEC                                                             
263900                                                                          
264000     MOVE SQLCODE TO SQLCODE-WS                                           
264100     PERFORM DB2-STATUS-CHECK                                             
264200     .                                                                    
264300     EJECT                                                                
264400                                                                          
264500 DB2-FETCH-T01CUGR-CRS-2 SECTION.                                         
264600     MOVE 000100  TO GOOD-SQLCODECODES                                    
264700                                                                          
264800     EXEC SQL                                                             
264900                                                                          
265000         FETCH T01CUGR-CRS-2                                              
265100                                                                          
265200         INTO :MAP-KDFINDOC-LINE-2                                        
265300            , :MAP-KDPARTTY-LINE                                          
265400            , :MAP-KDPARTGR-LINE                                          
265500            , :MAP-KDSTATUS-LINE-2                                        
265600            , :MAP-DAREGDAT-LINE-2                                        
265700            , :MAP-DAUPPDAT-LINE-2                                        
265800            , :MAP-IDUSER-LINE-2                                          
265900     END-EXEC                                                             
266000                                                                          
266100     MOVE SQLCODE TO SQLCODE-WS                                           
266200     PERFORM DB2-STATUS-CHECK                                             
266300     .                                                                    
266400     EJECT                                                                
266500                                                                          
266600 DB2-CLOSE-T01CUGR-CRS-2 SECTION.                                         
266700     EXEC SQL                                                             
266800        CLOSE T01CUGR-CRS-2                                               
266900     END-EXEC                                                             
267000     .                                                                    
267100     EJECT                                                                
267200                                                                          
267300                                                                          
267400 DB2-DCL-OPN-T01PAIN-CRS-4A SECTION.                                      
267500     MOVE 000100 TO GOOD-SQLCODECODES                                     
267600                                                                          
267700     EXEC SQL                                                             
267800         DECLARE T01PAIN-CRS-4A CURSOR WITH HOLD FOR                      
267900                                                                          
268000          SELECT   B.KDFINDOC                                             
268100                 , A.KDPARTTY                                             
268200                 , A.KDPARTGR                                             
268300                 , A.KDVALISO                                             
268400                 , A.KDSTATUS                                             
268500                 , B.KDSTATUS                                             
268600                 , A.DAREGDAT                                             
268700                 , B.DAREGDAT                                             
268800                 , A.DAUPPDAT                                             
268900                 , B.DAUPPDAT                                             
269000                 , A.IDUSER                                               
269100                 , B.IDUSER                                               
269200                                                                          
269300          FROM     T01PAIN A                                              
269400                 , T01DOTY B                                              
269500                                                                          
269600          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
269700             AND   A.IDLEGSEL = B.IDLEGSEL                                
269800             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
269900             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
270000             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
270100             AND   B.DADELDAT = '00000000'                                
270200             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
270300             AND   A.IDLEGSEL = B.IDLEGSEL                                
270400             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
270500             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
270600             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
270700             AND   B.DADELDAT = '00000000'                                
270800             AND ( A.KDFINDOC <> B.KDFINDOC                               
270900             OR    A.KDFINDOC =  B.KDFINDOC ))                            
271000             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
271100             AND   A.IDLEGSEL = B.IDLEGSEL                                
271200             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
271300             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
271400             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
271500             AND   B.DADELDAT = '00000000'                                
271600             AND   A.KDFINDOC <> B.KDFINDOC                               
271700             AND   A.KDFINDOC =  B.KDFINDOC )                             
271800                                                                          
271900          ORDER BY A.IDLEGSEL                                             
272000                 , A.KDFINDOC                                             
272100                 , A.KDPARTTY                                             
272200                 , A.KDPARTGR                                             
272300                 , A.KDVALISO                                             
272400                 , A.KDSTATUS DESC                                        
272500     END-EXEC                                                             
272600                                                                          
272700     MOVE 000100  TO GOOD-SQLCODECODES                                    
272800                                                                          
272900     EXEC SQL                                                             
273000        OPEN T01PAIN-CRS-4A                                               
273100     END-EXEC                                                             
273200                                                                          
273300     MOVE SQLCODE TO SQLCODE-WS                                           
273400     PERFORM DB2-STATUS-CHECK                                             
273500     .                                                                    
273600     EJECT                                                                
273700                                                                          
273800 DB2-FETCH-T01PAIN-CRS-4A SECTION.                                        
273900     MOVE 000100  TO GOOD-SQLCODECODES                                    
274000                                                                          
274100     EXEC SQL                                                             
274200                                                                          
274300         FETCH T01PAIN-CRS-4A                                             
274400                                                                          
274500         INTO :MAP-KDFINDOC-LINE-2                                        
274600            , :MAP-KDPARTTY-LINE                                          
274700            , :MAP-KDPARTGR-LINE                                          
274800            , :MAP-KDVALISO-LINE-2                                        
274900            , :MAP-KDSTATUS-LINE                                          
275000            , :MAP-KDSTATUS-LINE-2                                        
275100            , :MAP-DAREGDAT-LINE                                          
275200            , :MAP-DAREGDAT-LINE-2                                        
275300            , :MAP-DAUPPDAT-LINE                                          
275400            , :MAP-DAUPPDAT-LINE-2                                        
275500            , :MAP-IDUSER-LINE                                            
275600            , :MAP-IDUSER-LINE-2                                          
275700     END-EXEC                                                             
275800                                                                          
275900     MOVE SQLCODE TO SQLCODE-WS                                           
276000     PERFORM DB2-STATUS-CHECK                                             
276100     .                                                                    
276200     EJECT                                                                
276300                                                                          
276400 DB2-CLOSE-T01PAIN-CRS-4A SECTION.                                        
276500     EXEC SQL                                                             
276600        CLOSE T01PAIN-CRS-4A                                              
276700     END-EXEC                                                             
276800     .                                                                    
276900     EJECT                                                                
277000                                                                          
277100* * * * * * * * * *   - CURSOR-FAE-  * * * * * * * * * * * * *            
277200 DB2-COUNT-CRS-FAE SECTION.                                               
277300     EXEC SQL                                                             
277400                                                                          
277500          SELECT COUNT(*)                                                 
277600                                                                          
277700          INTO  :WS-COUNTER-FAE                                           
277800                                                                          
277900          FROM   T01CUGR A                                                
278000                                                                          
278100          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
278200             AND   KDPARTTY = :REQU-KDPARTTY-KEY                          
278300             AND   KDSTATUS = :WS-CURRENT                                 
278400             AND   DADELDAT = '00000000'                                  
278500                                                                          
278600     END-EXEC                                                             
278700                                                                          
278800     MOVE 000100  TO GOOD-SQLCODECODES                                    
278900                                                                          
279000     MOVE SQLCODE TO SQLCODE-WS                                           
279100     PERFORM DB2-STATUS-CHECK                                             
279200     .                                                                    
279300     EJECT                                                                
279400                                                                          
279500 DB2-DCL-OPN-T01CUGR-CRS-1 SECTION.                                       
279600     MOVE 000100 TO GOOD-SQLCODECODES                                     
279700                                                                          
279800     EXEC SQL                                                             
279900         DECLARE T01CUGR-CRS-1 CURSOR WITH HOLD FOR                       
280000                                                                          
280100           SELECT  B.KDFINDOC                                             
280200                 , A.KDPARTTY                                             
280300                 , A.KDPARTGR                                             
280400                 , A.KDSTATUS                                             
280500                 , A.DAREGDAT                                             
280600                 , A.DAUPPDAT                                             
280700                 , A.IDUSER                                               
280800                                                                          
280900           FROM    T01CUGR A                                              
281000                 , T01DOTY B                                              
281100                                                                          
281200           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
281300                AND A.IDLEGSEL = B.IDLEGSEL                               
281400                AND A.KDPARTTY = :REQU-KDPARTTY-KEY                       
281500                AND A.DADELDAT = '00000000'                               
281600                AND B.DADELDAT = '00000000'                               
281700                AND A.KDSTATUS = :WS-CURRENT                              
281800                AND B.KDSTATUS = :WS-CURRENT                              
281900                                                                          
282000           ORDER BY A.IDLEGSEL                                            
282100                  , B.KDFINDOC                                            
282200                  , A.KDPARTTY                                            
282300                  , A.KDPARTGR                                            
282400     END-EXEC                                                             
282500                                                                          
282600     MOVE 000100  TO GOOD-SQLCODECODES                                    
282700                                                                          
282800     EXEC SQL                                                             
282900        OPEN T01CUGR-CRS-1                                                
283000     END-EXEC                                                             
283100                                                                          
283200     MOVE SQLCODE TO SQLCODE-WS                                           
283300     PERFORM DB2-STATUS-CHECK                                             
283400     .                                                                    
283500     EJECT                                                                
283600                                                                          
283700 DB2-FETCH-T01CUGR-CRS-1 SECTION.                                         
283800     MOVE 000100  TO GOOD-SQLCODECODES                                    
283900                                                                          
284000     EXEC SQL                                                             
284100                                                                          
284200         FETCH T01CUGR-CRS-1                                              
284300                                                                          
284400         INTO :MAP-KDFINDOC-LINE-2                                        
284500            , :MAP-KDPARTTY-LINE                                          
284600            , :MAP-KDPARTGR-LINE-2                                        
284700            , :MAP-KDSTATUS-LINE-2                                        
284800            , :MAP-DAREGDAT-LINE-2                                        
284900            , :MAP-DAUPPDAT-LINE-2                                        
285000            , :MAP-IDUSER-LINE-2                                          
285100     END-EXEC                                                             
285200                                                                          
285300     MOVE SQLCODE TO SQLCODE-WS                                           
285400     PERFORM DB2-STATUS-CHECK                                             
285500     .                                                                    
285600     EJECT                                                                
285700                                                                          
285800 DB2-CLOSE-T01CUGR-CRS-1 SECTION.                                         
285900     EXEC SQL                                                             
286000        CLOSE T01CUGR-CRS-1                                               
286100     END-EXEC                                                             
286200     .                                                                    
286300     EJECT                                                                
286400                                                                          
286500 DB2-DCL-OPN-T01PAIN-CRS-5A SECTION.                                      
286600     MOVE 000100 TO GOOD-SQLCODECODES                                     
286700                                                                          
286800     EXEC SQL                                                             
286900         DECLARE T01PAIN-CRS-5A CURSOR WITH HOLD FOR                      
287000                                                                          
287100          SELECT   B.KDFINDOC                                             
287200                 , A.KDPARTTY                                             
287300                 , A.KDPARTGR                                             
287400                 , A.KDVALISO                                             
287500                 , A.KDSTATUS                                             
287600                 , B.KDSTATUS                                             
287700                 , A.DAREGDAT                                             
287800                 , B.DAREGDAT                                             
287900                 , A.DAUPPDAT                                             
288000                 , B.DAUPPDAT                                             
288100                 , A.IDUSER                                               
288200                 , B.IDUSER                                               
288300                                                                          
288400          FROM     T01PAIN A                                              
288500                 , T01DOTY B                                              
288600                                                                          
288700          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
288800             AND   A.IDLEGSEL = B.IDLEGSEL                                
288900             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
289000             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
289100             AND   B.DADELDAT = '00000000'                                
289200             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
289300             AND   A.IDLEGSEL = B.IDLEGSEL                                
289400             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
289500             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
289600             AND   B.DADELDAT = '00000000'                                
289700             AND ( A.KDFINDOC <> B.KDFINDOC                               
289800             OR    A.KDFINDOC =  B.KDFINDOC ))                            
289900             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
290000             AND   A.IDLEGSEL = B.IDLEGSEL                                
290100             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
290200             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
290300             AND   B.DADELDAT = '00000000'                                
290400             AND   A.KDFINDOC <> B.KDFINDOC                               
290500             AND   A.KDFINDOC =  B.KDFINDOC )                             
290600                                                                          
290700          ORDER BY A.IDLEGSEL                                             
290800                 , A.KDFINDOC                                             
290900                 , A.KDPARTTY                                             
291000                 , A.KDPARTGR                                             
291100                 , A.KDVALISO                                             
291200                 , A.KDSTATUS DESC                                        
291300     END-EXEC                                                             
291400                                                                          
291500     MOVE 000100  TO GOOD-SQLCODECODES                                    
291600                                                                          
291700     EXEC SQL                                                             
291800        OPEN T01PAIN-CRS-5A                                               
291900     END-EXEC                                                             
292000                                                                          
292100     MOVE SQLCODE TO SQLCODE-WS                                           
292200     PERFORM DB2-STATUS-CHECK                                             
292300     .                                                                    
292400     EJECT                                                                
292500                                                                          
292600 DB2-FETCH-T01PAIN-CRS-5A SECTION.                                        
292700     MOVE 000100  TO GOOD-SQLCODECODES                                    
292800                                                                          
292900     EXEC SQL                                                             
293000                                                                          
293100         FETCH T01PAIN-CRS-5A                                             
293200                                                                          
293300         INTO :MAP-KDFINDOC-LINE-2                                        
293400            , :MAP-KDPARTTY-LINE                                          
293500            , :MAP-KDPARTGR-LINE-2                                        
293600            , :MAP-KDVALISO-LINE-2                                        
293700            , :MAP-KDSTATUS-LINE                                          
293800            , :MAP-KDSTATUS-LINE-2                                        
293900            , :MAP-DAREGDAT-LINE                                          
294000            , :MAP-DAREGDAT-LINE-2                                        
294100            , :MAP-DAUPPDAT-LINE                                          
294200            , :MAP-DAUPPDAT-LINE-2                                        
294300            , :MAP-IDUSER-LINE                                            
294400            , :MAP-IDUSER-LINE-2                                          
294500     END-EXEC                                                             
294600                                                                          
294700     MOVE SQLCODE TO SQLCODE-WS                                           
294800     PERFORM DB2-STATUS-CHECK                                             
294900     .                                                                    
295000     EJECT                                                                
295100                                                                          
295200 DB2-CLOSE-T01PAIN-CRS-5A SECTION.                                        
295300     EXEC SQL                                                             
295400        CLOSE T01PAIN-CRS-5A                                              
295500     END-EXEC                                                             
295600     .                                                                    
295700     EJECT                                                                
295800                                                                          
295900* * * * * * * * * *   - CURSOR-FAF-  * * * * * * * * * * * * *            
296000 DB2-COUNT-CRS-FAF SECTION.                                               
296100     EXEC SQL                                                             
296200                                                                          
296300          SELECT COUNT(*)                                                 
296400                                                                          
296500          INTO  :WS-COUNTER-FAF                                           
296600                                                                          
296700          FROM   T01CUGR A                                                
296800                ,T01CURR B                                                
296900                                                                          
297000          WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
297100             AND A.KDPARTTY = :REQU-KDPARTTY-KEY                          
297200             AND B.KDVALISO = :REQU-KDVALISO-KEY                          
297300             AND A.KDSTATUS = :WS-CURRENT                                 
297400             AND A.DADELDAT = '00000000'                                  
297500             AND A.IDLEGSEL = B.IDLEGSEL                                  
297600                                                                          
297700     END-EXEC                                                             
297800                                                                          
297900     MOVE 000100  TO GOOD-SQLCODECODES                                    
298000                                                                          
298100     MOVE SQLCODE TO SQLCODE-WS                                           
298200     PERFORM DB2-STATUS-CHECK                                             
298300     .                                                                    
298400     EJECT                                                                
298500                                                                          
298600 DB2-DCL-OPN-T01CUGR-CRS-3 SECTION.                                       
298700     MOVE 000100 TO GOOD-SQLCODECODES                                     
298800                                                                          
298900     EXEC SQL                                                             
299000         DECLARE T01CUGR-CRS-3 CURSOR WITH HOLD FOR                       
299100                                                                          
299200           SELECT  B.KDFINDOC                                             
299300                 , A.KDPARTTY                                             
299400                 , A.KDPARTGR                                             
299500                 , C.KDVALISO                                             
299600                 , A.KDSTATUS                                             
299700                 , A.DAREGDAT                                             
299800                 , A.DAUPPDAT                                             
299900                 , A.IDUSER                                               
300000                                                                          
300100           FROM    T01CUGR A                                              
300200                 , T01DOTY B                                              
300300                 , T01CURR C                                              
300400                                                                          
300500           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
300600                AND A.IDLEGSEL = B.IDLEGSEL                               
300700                AND A.IDLEGSEL = C.IDLEGSEL                               
300800                AND A.KDPARTTY = :REQU-KDPARTTY-KEY                       
300900                AND C.KDVALISO = :REQU-KDVALISO-KEY                       
301000                AND A.DADELDAT = '00000000'                               
301100                AND B.DADELDAT = '00000000'                               
301200                AND A.KDSTATUS = :WS-CURRENT                              
301300                AND B.KDSTATUS = :WS-CURRENT                              
301400                                                                          
301500           ORDER BY A.IDLEGSEL                                            
301600                  , B.KDFINDOC                                            
301700                  , A.KDPARTTY                                            
301800                  , A.KDPARTGR                                            
301900     END-EXEC                                                             
302000                                                                          
302100     MOVE 000100  TO GOOD-SQLCODECODES                                    
302200                                                                          
302300     EXEC SQL                                                             
302400        OPEN T01CUGR-CRS-3                                                
302500     END-EXEC                                                             
302600                                                                          
302700     MOVE SQLCODE TO SQLCODE-WS                                           
302800     PERFORM DB2-STATUS-CHECK                                             
302900     .                                                                    
303000     EJECT                                                                
303100                                                                          
303200 DB2-FETCH-T01CUGR-CRS-3 SECTION.                                         
303300     MOVE 000100  TO GOOD-SQLCODECODES                                    
303400                                                                          
303500     EXEC SQL                                                             
303600                                                                          
303700         FETCH T01CUGR-CRS-3                                              
303800                                                                          
303900         INTO :MAP-KDFINDOC-LINE-2                                        
304000            , :MAP-KDPARTTY-LINE                                          
304100            , :MAP-KDPARTGR-LINE-2                                        
304200            , :MAP-KDVALISO-LINE                                          
304300            , :MAP-KDSTATUS-LINE-2                                        
304400            , :MAP-DAREGDAT-LINE-2                                        
304500            , :MAP-DAUPPDAT-LINE-2                                        
304600            , :MAP-IDUSER-LINE-2                                          
304700     END-EXEC                                                             
304800                                                                          
304900     MOVE SQLCODE TO SQLCODE-WS                                           
305000     PERFORM DB2-STATUS-CHECK                                             
305100     .                                                                    
305200     EJECT                                                                
305300                                                                          
305400 DB2-CLOSE-T01CUGR-CRS-3 SECTION.                                         
305500     EXEC SQL                                                             
305600        CLOSE T01CUGR-CRS-3                                               
305700     END-EXEC                                                             
305800     .                                                                    
305900     EJECT                                                                
306000                                                                          
306100 DB2-DCL-OPN-T01PAIN-CRS-6A SECTION.                                      
306200     MOVE 000100 TO GOOD-SQLCODECODES                                     
306300                                                                          
306400     EXEC SQL                                                             
306500         DECLARE T01PAIN-CRS-6A CURSOR WITH HOLD FOR                      
306600                                                                          
306700          SELECT   B.KDFINDOC                                             
306800                 , A.KDPARTTY                                             
306900                 , A.KDPARTGR                                             
307000                 , A.KDVALISO                                             
307100                 , A.KDSTATUS                                             
307200                 , B.KDSTATUS                                             
307300                 , A.DAREGDAT                                             
307400                 , B.DAREGDAT                                             
307500                 , A.DAUPPDAT                                             
307600                 , B.DAUPPDAT                                             
307700                 , A.IDUSER                                               
307800                 , B.IDUSER                                               
307900                                                                          
308000          FROM     T01PAIN A                                              
308100                 , T01DOTY B                                              
308200                                                                          
308300          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
308400             AND   A.IDLEGSEL = B.IDLEGSEL                                
308500             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
308600             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
308700             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
308800             AND   B.DADELDAT = '00000000'                                
308900             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
309000             AND   A.IDLEGSEL = B.IDLEGSEL                                
309100             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
309200             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
309300             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
309400             AND   B.DADELDAT = '00000000'                                
309500             AND ( A.KDFINDOC <> B.KDFINDOC                               
309600             OR    A.KDFINDOC =  B.KDFINDOC ))                            
309700             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
309800             AND   A.IDLEGSEL = B.IDLEGSEL                                
309900             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
310000             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
310100             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
310200             AND   B.DADELDAT = '00000000'                                
310300             AND   A.KDFINDOC <> B.KDFINDOC                               
310400             AND   A.KDFINDOC =  B.KDFINDOC )                             
310500                                                                          
310600          ORDER BY A.IDLEGSEL                                             
310700                 , A.KDFINDOC                                             
310800                 , A.KDPARTTY                                             
310900                 , A.KDPARTGR                                             
311000                 , A.KDVALISO                                             
311100                 , A.KDSTATUS DESC                                        
311200     END-EXEC                                                             
311300                                                                          
311400     MOVE 000100  TO GOOD-SQLCODECODES                                    
311500                                                                          
311600     EXEC SQL                                                             
311700        OPEN T01PAIN-CRS-6A                                               
311800     END-EXEC                                                             
311900                                                                          
312000     MOVE SQLCODE TO SQLCODE-WS                                           
312100     PERFORM DB2-STATUS-CHECK                                             
312200     .                                                                    
312300     EJECT                                                                
312400                                                                          
312500 DB2-FETCH-T01PAIN-CRS-6A SECTION.                                        
312600     MOVE 000100  TO GOOD-SQLCODECODES                                    
312700                                                                          
312800     EXEC SQL                                                             
312900                                                                          
313000         FETCH T01PAIN-CRS-6A                                             
313100                                                                          
313200         INTO :MAP-KDFINDOC-LINE-2                                        
313300            , :MAP-KDPARTTY-LINE                                          
313400            , :MAP-KDPARTGR-LINE-2                                        
313500            , :MAP-KDVALISO-LINE                                          
313600            , :MAP-KDSTATUS-LINE                                          
313700            , :MAP-KDSTATUS-LINE-2                                        
313800            , :MAP-DAREGDAT-LINE                                          
313900            , :MAP-DAREGDAT-LINE-2                                        
314000            , :MAP-DAUPPDAT-LINE                                          
314100            , :MAP-DAUPPDAT-LINE-2                                        
314200            , :MAP-IDUSER-LINE                                            
314300            , :MAP-IDUSER-LINE-2                                          
314400     END-EXEC                                                             
314500                                                                          
314600     MOVE SQLCODE TO SQLCODE-WS                                           
314700     PERFORM DB2-STATUS-CHECK                                             
314800     .                                                                    
314900     EJECT                                                                
315000                                                                          
315100 DB2-CLOSE-T01PAIN-CRS-6A SECTION.                                        
315200     EXEC SQL                                                             
315300        CLOSE T01PAIN-CRS-6A                                              
315400     END-EXEC                                                             
315500     .                                                                    
315600     EJECT                                                                
315700                                                                          
315800* * * * * * * * * *   - CURSOR-FAG-  * * * * * * * * * * * * *            
315900 DB2-COUNT-CRS-FAG SECTION.                                               
316000     EXEC SQL                                                             
316100                                                                          
316200          SELECT COUNT(*)                                                 
316300                                                                          
316400          INTO  :WS-COUNTER-FAG                                           
316500                                                                          
316600          FROM   T01CUGR A                                                
316700                ,T01CURR B                                                
316800                                                                          
316900          WHERE   A.IDLEGSEL = :REQU-IDLEGSEL-KEY                         
317000             AND  A.KDPARTTY = :REQU-KDPARTTY-KEY                         
317100             AND  A.KDPARTGR = :REQU-KDPARTGR-KEY                         
317200             AND  B.KDVALISO = :REQU-KDVALISO-KEY                         
317300             AND  A.KDSTATUS = :WS-CURRENT                                
317400             AND  A.IDLEGSEL = B.IDLEGSEL                                 
317500             AND  A.DADELDAT = '00000000'                                 
317600                                                                          
317700     END-EXEC                                                             
317800                                                                          
317900     MOVE 000100  TO GOOD-SQLCODECODES                                    
318000                                                                          
318100     MOVE SQLCODE TO SQLCODE-WS                                           
318200     PERFORM DB2-STATUS-CHECK                                             
318300     .                                                                    
318400     EJECT                                                                
318500                                                                          
318600 DB2-DCL-OPN-T01CUGR-CRS-4 SECTION.                                       
318700     MOVE 000100 TO GOOD-SQLCODECODES                                     
318800                                                                          
318900     EXEC SQL                                                             
319000         DECLARE T01CUGR-CRS-4 CURSOR WITH HOLD FOR                       
319100                                                                          
319200           SELECT  B.KDFINDOC                                             
319300                 , A.KDPARTTY                                             
319400                 , A.KDPARTGR                                             
319500                 , C.KDVALISO                                             
319600                 , A.KDSTATUS                                             
319700                 , A.DAREGDAT                                             
319800                 , A.DAUPPDAT                                             
319900                 , A.IDUSER                                               
320000                                                                          
320100           FROM    T01CUGR A                                              
320200                 , T01DOTY B                                              
320300                 , T01CURR C                                              
320400                                                                          
320500           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
320600                AND A.IDLEGSEL = B.IDLEGSEL                               
320700                AND A.IDLEGSEL = C.IDLEGSEL                               
320800                AND A.DADELDAT = '00000000'                               
320900                AND B.DADELDAT = '00000000'                               
321000                AND A.KDSTATUS = :WS-CURRENT                              
321100                AND B.KDSTATUS = :WS-CURRENT                              
321200                AND C.KDVALISO = :REQU-KDVALISO-KEY                       
321300                AND ( A.KDPARTTY = :REQU-KDPARTTY-KEY                     
321400                OR  A.KDPARTGR = :REQU-KDPARTTY-KEY )                     
321500                                                                          
321600           ORDER BY A.IDLEGSEL                                            
321700                  , B.KDFINDOC                                            
321800                  , A.KDPARTTY                                            
321900                  , A.KDPARTGR                                            
322000                  , C.KDVALISO                                            
322100     END-EXEC                                                             
322200                                                                          
322300     MOVE 000100  TO GOOD-SQLCODECODES                                    
322400                                                                          
322500     EXEC SQL                                                             
322600        OPEN T01CUGR-CRS-4                                                
322700     END-EXEC                                                             
322800                                                                          
322900     MOVE SQLCODE TO SQLCODE-WS                                           
323000     PERFORM DB2-STATUS-CHECK                                             
323100     .                                                                    
323200     EJECT                                                                
323300                                                                          
323400 DB2-FETCH-T01CUGR-CRS-4 SECTION.                                         
323500     MOVE 000100  TO GOOD-SQLCODECODES                                    
323600                                                                          
323700     EXEC SQL                                                             
323800                                                                          
323900         FETCH T01CUGR-CRS-4                                              
324000                                                                          
324100         INTO :MAP-KDFINDOC-LINE-2                                        
324200            , :MAP-KDPARTTY-LINE                                          
324300            , :MAP-KDPARTGR-LINE                                          
324400            , :MAP-KDVALISO-LINE                                          
324500            , :MAP-KDSTATUS-LINE-2                                        
324600            , :MAP-DAREGDAT-LINE-2                                        
324700            , :MAP-DAUPPDAT-LINE-2                                        
324800            , :MAP-IDUSER-LINE-2                                          
324900     END-EXEC                                                             
325000                                                                          
325100     MOVE SQLCODE TO SQLCODE-WS                                           
325200     PERFORM DB2-STATUS-CHECK                                             
325300     .                                                                    
325400     EJECT                                                                
325500                                                                          
325600 DB2-CLOSE-T01CUGR-CRS-4 SECTION.                                         
325700     EXEC SQL                                                             
325800        CLOSE T01CUGR-CRS-4                                               
325900     END-EXEC                                                             
326000     .                                                                    
326100     EJECT                                                                
326200                                                                          
326300                                                                          
326400 DB2-DCL-OPN-T01PAIN-CRS-7A SECTION.                                      
326500     MOVE 000100 TO GOOD-SQLCODECODES                                     
326600                                                                          
326700     EXEC SQL                                                             
326800         DECLARE T01PAIN-CRS-7A CURSOR WITH HOLD FOR                      
326900                                                                          
327000          SELECT   B.KDFINDOC                                             
327100                 , A.KDPARTTY                                             
327200                 , A.KDPARTGR                                             
327300                 , A.KDVALISO                                             
327400                 , A.KDSTATUS                                             
327500                 , B.KDSTATUS                                             
327600                 , A.DAREGDAT                                             
327700                 , B.DAREGDAT                                             
327800                 , A.DAUPPDAT                                             
327900                 , B.DAUPPDAT                                             
328000                 , A.IDUSER                                               
328100                 , B.IDUSER                                               
328200                                                                          
328300          FROM     T01PAIN A                                              
328400                 , T01DOTY B                                              
328500                                                                          
328600          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
328700             AND   A.IDLEGSEL = B.IDLEGSEL                                
328800             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
328900             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
329000             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
329100             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
329200             AND   B.DADELDAT = '00000000'                                
329300             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
329400             AND   A.IDLEGSEL = B.IDLEGSEL                                
329500             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
329600             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
329700             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
329800             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
329900             AND   B.DADELDAT = '00000000'                                
330000             AND ( A.KDFINDOC <> B.KDFINDOC                               
330100             OR    A.KDFINDOC =  B.KDFINDOC ))                            
330200             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
330300             AND   A.IDLEGSEL = B.IDLEGSEL                                
330400             AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                        
330500             AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                        
330600             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
330700             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
330800             AND   B.DADELDAT = '00000000'                                
330900             AND   A.KDFINDOC <> B.KDFINDOC                               
331000             AND   A.KDFINDOC =  B.KDFINDOC )                             
331100                                                                          
331200          ORDER BY A.IDLEGSEL                                             
331300                 , A.KDFINDOC                                             
331400                 , A.KDPARTTY                                             
331500                 , A.KDPARTGR                                             
331600                 , A.KDVALISO                                             
331700                 , A.KDSTATUS DESC                                        
331800     END-EXEC                                                             
331900                                                                          
332000     MOVE 000100  TO GOOD-SQLCODECODES                                    
332100                                                                          
332200     EXEC SQL                                                             
332300        OPEN T01PAIN-CRS-7A                                               
332400     END-EXEC                                                             
332500                                                                          
332600     MOVE SQLCODE TO SQLCODE-WS                                           
332700     PERFORM DB2-STATUS-CHECK                                             
332800     .                                                                    
332900     EJECT                                                                
333000                                                                          
333100 DB2-FETCH-T01PAIN-CRS-7A SECTION.                                        
333200     MOVE 000100  TO GOOD-SQLCODECODES                                    
333300                                                                          
333400     EXEC SQL                                                             
333500                                                                          
333600         FETCH T01PAIN-CRS-7A                                             
333700                                                                          
333800         INTO :MAP-KDFINDOC-LINE-2                                        
333900            , :MAP-KDPARTTY-LINE                                          
334000            , :MAP-KDPARTGR-LINE                                          
334100            , :MAP-KDVALISO-LINE                                          
334200            , :MAP-KDSTATUS-LINE                                          
334300            , :MAP-KDSTATUS-LINE-2                                        
334400            , :MAP-DAREGDAT-LINE                                          
334500            , :MAP-DAREGDAT-LINE-2                                        
334600            , :MAP-DAUPPDAT-LINE                                          
334700            , :MAP-DAUPPDAT-LINE-2                                        
334800            , :MAP-IDUSER-LINE                                            
334900            , :MAP-IDUSER-LINE-2                                          
335000     END-EXEC                                                             
335100                                                                          
335200     MOVE SQLCODE TO SQLCODE-WS                                           
335300     PERFORM DB2-STATUS-CHECK                                             
335400     .                                                                    
335500     EJECT                                                                
335600                                                                          
335700 DB2-CLOSE-T01PAIN-CRS-7A SECTION.                                        
335800     EXEC SQL                                                             
335900        CLOSE T01PAIN-CRS-7A                                              
336000     END-EXEC                                                             
336100     .                                                                    
336200     EJECT                                                                
336300                                                                          
336400* * * * * * * * * *   - CURSOR-FAH-  * * * * * * * * * * * * *            
336500 DB2-COUNT-CRS-FAH SECTION.                                               
336600     EXEC SQL                                                             
336700                                                                          
336800          SELECT COUNT(*)                                                 
336900                                                                          
337000          INTO  :WS-COUNTER-FAH                                           
337100                                                                          
337200          FROM   T01DOTY A                                                
337300                ,T01CURR B                                                
337400                                                                          
337500          WHERE  A.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
337600             AND A.IDLEGSEL = B.IDLEGSEL                                  
337700             AND A.KDSTATUS = :WS-CURRENT                                 
337800             AND A.DADELDAT = '00000000'                                  
337900             AND A.KDFINDOC = :REQU-KDFINDOC-KEY                          
338000             AND B.KDVALISO = :REQU-KDVALISO-KEY                          
338100                                                                          
338200     END-EXEC                                                             
338300                                                                          
338400     MOVE 000100  TO GOOD-SQLCODECODES                                    
338500                                                                          
338600     MOVE SQLCODE TO SQLCODE-WS                                           
338700     PERFORM DB2-STATUS-CHECK                                             
338800     .                                                                    
338900     EJECT                                                                
339000                                                                          
339100 DB2-DCL-OPN-T01DOTY-CRS-4 SECTION.                                       
339200     MOVE 000100 TO GOOD-SQLCODECODES                                     
339300                                                                          
339400     EXEC SQL                                                             
339500         DECLARE T01DOTY-CRS-4 CURSOR WITH HOLD FOR                       
339600                                                                          
339700           SELECT  DISTINCT A.KDFINDOC                                    
339800                 , B.KDPARTTY                                             
339900                 , B.KDPARTGR                                             
340000                 , C.KDVALISO                                             
340100                 , A.KDSTATUS                                             
340200                 , A.DAREGDAT                                             
340300                 , A.DAUPPDAT                                             
340400                 , A.IDUSER                                               
340500                                                                          
340600           FROM    T01DOTY A                                              
340700                 , T01CUGR B                                              
340800                 , T01CURR C                                              
340900                                                                          
341000           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
341100                AND A.IDLEGSEL = B.IDLEGSEL                               
341200                AND A.IDLEGSEL = C.IDLEGSEL                               
341300                AND A.DADELDAT = '00000000'                               
341400                AND B.DADELDAT = '00000000'                               
341500                AND A.KDSTATUS = :WS-CURRENT                              
341600                AND B.KDSTATUS = :WS-CURRENT                              
341700                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
341800                AND C.KDVALISO = :REQU-KDVALISO-KEY                       
341900                                                                          
342000     END-EXEC                                                             
342100                                                                          
342200     MOVE 000100  TO GOOD-SQLCODECODES                                    
342300                                                                          
342400     EXEC SQL                                                             
342500        OPEN T01DOTY-CRS-4                                                
342600     END-EXEC                                                             
342700                                                                          
342800     MOVE SQLCODE TO SQLCODE-WS                                           
342900     PERFORM DB2-STATUS-CHECK                                             
343000     .                                                                    
343100     EJECT                                                                
343200                                                                          
343300 DB2-FETCH-T01DOTY-CRS-4 SECTION.                                         
343400     MOVE 000100  TO GOOD-SQLCODECODES                                    
343500                                                                          
343600     EXEC SQL                                                             
343700                                                                          
343800         FETCH T01DOTY-CRS-4                                              
343900                                                                          
344000         INTO :MAP-KDFINDOC-LINE                                          
344100            , :MAP-KDPARTTY-LINE-2                                        
344200            , :MAP-KDPARTGR-LINE-2                                        
344300            , :MAP-KDVALISO-LINE                                          
344400            , :MAP-KDSTATUS-LINE-2                                        
344500            , :MAP-DAREGDAT-LINE-2                                        
344600            , :MAP-DAUPPDAT-LINE-2                                        
344700            , :MAP-IDUSER-LINE-2                                          
344800     END-EXEC                                                             
344900                                                                          
345000     MOVE SQLCODE TO SQLCODE-WS                                           
345100     PERFORM DB2-STATUS-CHECK                                             
345200     .                                                                    
345300     EJECT                                                                
345400                                                                          
345500 DB2-CLOSE-T01DOTY-CRS-4 SECTION.                                         
345600     EXEC SQL                                                             
345700        CLOSE T01DOTY-CRS-4                                               
345800     END-EXEC                                                             
345900     .                                                                    
346000     EJECT                                                                
346100                                                                          
346200 DB2-DCL-OPN-T01PAIN-CRS-8A SECTION.                                      
346300     MOVE 000100 TO GOOD-SQLCODECODES                                     
346400                                                                          
346500     EXEC SQL                                                             
346600         DECLARE T01PAIN-CRS-8A CURSOR WITH HOLD FOR                      
346700                                                                          
346800          SELECT  DISTINCT  A.KDFINDOC                                    
346900                 , B.KDPARTTY                                             
347000                 , B.KDPARTGR                                             
347100                 , A.KDVALISO                                             
347200                 , A.KDSTATUS                                             
347300                 , B.KDSTATUS                                             
347400                 , B.DAREGDAT                                             
347500                 , B.DAUPPDAT                                             
347600                 , B.IDUSER                                               
347700                                                                          
347800          FROM     T01PAIN A                                              
347900                 , T01CUGR B                                              
348000                                                                          
348100          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
348200             AND   A.IDLEGSEL = B.IDLEGSEL                                
348300             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
348400             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
348500             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
348600             AND   B.KDSTATUS = :WS-CURRENT                               
348700             AND   B.DADELDAT = '00000000'                                
348800             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
348900             AND   A.IDLEGSEL = B.IDLEGSEL                                
349000             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
349100             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
349200             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
349300             AND   B.KDSTATUS = :WS-CURRENT                               
349400             AND   B.DADELDAT = '00000000'                                
349500             AND ( B.KDPARTTY <> A.KDPARTTY                               
349600             OR    B.KDPARTGR <> A.KDPARTGR ))                            
349700             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
349800             AND   A.IDLEGSEL = B.IDLEGSEL                                
349900             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
350000             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
350100             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
350200             AND   B.KDSTATUS = :WS-CURRENT                               
350300             AND   B.DADELDAT = '00000000'                                
350400             AND   B.KDPARTTY <> A.KDPARTTY                               
350500             AND   B.KDPARTGR <> A.KDPARTGR )                             
350600                                                                          
350700     END-EXEC                                                             
350800                                                                          
350900     MOVE 000100  TO GOOD-SQLCODECODES                                    
351000                                                                          
351100     EXEC SQL                                                             
351200        OPEN T01PAIN-CRS-8A                                               
351300     END-EXEC                                                             
351400                                                                          
351500     MOVE SQLCODE TO SQLCODE-WS                                           
351600     PERFORM DB2-STATUS-CHECK                                             
351700     .                                                                    
351800     EJECT                                                                
351900                                                                          
352000 DB2-FETCH-T01PAIN-CRS-8A SECTION.                                        
352100     MOVE 000100  TO GOOD-SQLCODECODES                                    
352200                                                                          
352300     EXEC SQL                                                             
352400                                                                          
352500         FETCH T01PAIN-CRS-8A                                             
352600                                                                          
352700         INTO :MAP-KDFINDOC-LINE                                          
352800            , :MAP-KDPARTTY-LINE-2                                        
352900            , :MAP-KDPARTGR-LINE-2                                        
353000            , :MAP-KDVALISO-LINE                                          
353100            , :MAP-KDSTATUS-LINE                                          
353200            , :MAP-KDSTATUS-LINE-2                                        
353300            , :MAP-DAREGDAT-LINE                                          
353400            , :MAP-DAREGDAT-LINE-2                                        
353500            , :MAP-DAUPPDAT-LINE                                          
353600            , :MAP-DAUPPDAT-LINE-2                                        
353700            , :MAP-IDUSER-LINE                                            
353800            , :MAP-IDUSER-LINE-2                                          
353900     END-EXEC                                                             
354000                                                                          
354100     MOVE SQLCODE TO SQLCODE-WS                                           
354200     PERFORM DB2-STATUS-CHECK                                             
354300     .                                                                    
354400     EJECT                                                                
354500                                                                          
354600 DB2-CLOSE-T01PAIN-CRS-8A SECTION.                                        
354700     EXEC SQL                                                             
354800        CLOSE T01PAIN-CRS-8A                                              
354900     END-EXEC                                                             
355000     .                                                                    
355100     EJECT                                                                
355200                                                                          
355300* * * * * * * * * *   - CURSOR-FAI-  * * * * * * * * * * * * *            
355400 DB2-COUNT-CRS-FAI SECTION.                                               
355500     EXEC SQL                                                             
355600                                                                          
355700          SELECT COUNT(*)                                                 
355800                                                                          
355900          INTO  :WS-COUNTER-FAI                                           
356000                                                                          
356100          FROM   T01DOTY A                                                
356200               , T01CUGR B                                                
356300               , T01CURR C                                                
356400                                                                          
356500          WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
356600             AND   A.IDLEGSEL = B.IDLEGSEL                                
356700             AND   A.IDLEGSEL = C.IDLEGSEL                                
356800             AND   C.KDVALISO = :REQU-KDVALISO-KEY                        
356900             AND ( A.KDFINDOC = :REQU-KDFINDOC-KEY                        
357000             OR    B.KDPARTTY = :REQU-KDPARTTY-KEY )                      
357100             AND   A.KDSTATUS = :WS-CURRENT                               
357200             AND   A.DADELDAT = '00000000'                                
357300                                                                          
357400     END-EXEC                                                             
357500                                                                          
357600     MOVE 000100  TO GOOD-SQLCODECODES                                    
357700                                                                          
357800     MOVE SQLCODE TO SQLCODE-WS                                           
357900     PERFORM DB2-STATUS-CHECK                                             
358000     .                                                                    
358100     EJECT                                                                
358200                                                                          
358300 DB2-DCL-OPN-T01DOTY-CRS-5 SECTION.                                       
358400     MOVE 000100 TO GOOD-SQLCODECODES                                     
358500                                                                          
358600     EXEC SQL                                                             
358700         DECLARE T01DOTY-CRS-5 CURSOR WITH HOLD FOR                       
358800                                                                          
358900           SELECT  A.KDFINDOC                                             
359000                 , B.KDPARTTY                                             
359100                 , B.KDPARTGR                                             
359200                 , C.KDVALISO                                             
359300                 , A.KDSTATUS                                             
359400                 , A.DAREGDAT                                             
359500                 , A.DAUPPDAT                                             
359600                 , A.IDUSER                                               
359700                                                                          
359800           FROM    T01DOTY A                                              
359900                 , T01CUGR B                                              
360000                 , T01CURR C                                              
360100                                                                          
360200           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
360300                AND A.IDLEGSEL = B.IDLEGSEL                               
360400                AND A.IDLEGSEL = C.IDLEGSEL                               
360500                AND A.DADELDAT = '00000000'                               
360600                AND B.DADELDAT = '00000000'                               
360700                AND A.KDSTATUS = :WS-CURRENT                              
360800                AND B.KDSTATUS = :WS-CURRENT                              
360900                AND C.KDVALISO = :REQU-KDVALISO-KEY                       
361000                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
361100                AND B.KDPARTTY = :REQU-KDPARTTY-KEY                       
361200                                                                          
361300           ORDER BY A.IDLEGSEL                                            
361400                  , A.KDFINDOC                                            
361500                  , B.KDPARTTY                                            
361600                  , B.KDPARTGR                                            
361700                  , C.KDVALISO                                            
361800     END-EXEC                                                             
361900                                                                          
362000     MOVE 000100  TO GOOD-SQLCODECODES                                    
362100                                                                          
362200     EXEC SQL                                                             
362300        OPEN T01DOTY-CRS-5                                                
362400     END-EXEC                                                             
362500                                                                          
362600     MOVE SQLCODE TO SQLCODE-WS                                           
362700     PERFORM DB2-STATUS-CHECK                                             
362800     .                                                                    
362900     EJECT                                                                
363000                                                                          
363100 DB2-FETCH-T01DOTY-CRS-5 SECTION.                                         
363200     MOVE 000100  TO GOOD-SQLCODECODES                                    
363300                                                                          
363400     EXEC SQL                                                             
363500                                                                          
363600         FETCH T01DOTY-CRS-5                                              
363700                                                                          
363800         INTO :MAP-KDFINDOC-LINE                                          
363900            , :MAP-KDPARTTY-LINE                                          
364000            , :MAP-KDPARTGR-LINE-2                                        
364100            , :MAP-KDVALISO-LINE                                          
364200            , :MAP-KDSTATUS-LINE-2                                        
364300            , :MAP-DAREGDAT-LINE-2                                        
364400            , :MAP-DAUPPDAT-LINE-2                                        
364500            , :MAP-IDUSER-LINE-2                                          
364600     END-EXEC                                                             
364700                                                                          
364800     MOVE SQLCODE TO SQLCODE-WS                                           
364900     PERFORM DB2-STATUS-CHECK                                             
365000     .                                                                    
365100     EJECT                                                                
365200                                                                          
365300 DB2-CLOSE-T01DOTY-CRS-5 SECTION.                                         
365400     EXEC SQL                                                             
365500        CLOSE T01DOTY-CRS-5                                               
365600     END-EXEC                                                             
365700     .                                                                    
365800     EJECT                                                                
365900                                                                          
366000 DB2-DCL-OPN-T01PAIN-CRS-9A SECTION.                                      
366100     MOVE 000100 TO GOOD-SQLCODECODES                                     
366200                                                                          
366300     EXEC SQL                                                             
366400         DECLARE T01PAIN-CRS-9A CURSOR WITH HOLD FOR                      
366500                                                                          
366600          SELECT   A.KDFINDOC                                             
366700                 , B.KDPARTTY                                             
366800                 , B.KDPARTGR                                             
366900                 , A.KDVALISO                                             
367000                 , A.KDSTATUS                                             
367100                 , B.KDSTATUS                                             
367200                 , A.DAREGDAT                                             
367300                 , B.DAREGDAT                                             
367400                 , A.DAUPPDAT                                             
367500                 , B.DAUPPDAT                                             
367600                 , A.IDUSER                                               
367700                 , B.IDUSER                                               
367800                                                                          
367900          FROM     T01PAIN A                                              
368000                 , T01CUGR B                                              
368100                                                                          
368200          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
368300             AND   A.IDLEGSEL = B.IDLEGSEL                                
368400             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
368500             AND   B.KDPARTTY = :REQU-KDPARTTY-KEY                        
368600             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
368700             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
368800             AND   B.KDSTATUS = :WS-CURRENT                               
368900             AND   B.DADELDAT = '00000000'                                
369000             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
369100             AND   A.IDLEGSEL = B.IDLEGSEL                                
369200             AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                        
369300             AND   B.KDPARTTY = :REQU-KDPARTTY-KEY                        
369400             AND   A.KDVALISO = :REQU-KDVALISO-KEY                        
369500             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
369600             AND   B.KDSTATUS = :WS-CURRENT                               
369700             AND   B.DADELDAT = '00000000'                                
369800             AND   A.KDPARTGR <> B.KDPARTGR )                             
369900                                                                          
370000          ORDER BY A.IDLEGSEL                                             
370100                 , A.KDFINDOC                                             
370200                 , A.KDPARTTY                                             
370300                 , A.KDPARTGR                                             
370400                 , A.KDVALISO                                             
370500                 , A.KDSTATUS DESC                                        
370600     END-EXEC                                                             
370700                                                                          
370800     MOVE 000100  TO GOOD-SQLCODECODES                                    
370900                                                                          
371000     EXEC SQL                                                             
371100        OPEN T01PAIN-CRS-9A                                               
371200     END-EXEC                                                             
371300                                                                          
371400     MOVE SQLCODE TO SQLCODE-WS                                           
371500     PERFORM DB2-STATUS-CHECK                                             
371600     .                                                                    
371700     EJECT                                                                
371800                                                                          
371900 DB2-FETCH-T01PAIN-CRS-9A SECTION.                                        
372000     MOVE 000100  TO GOOD-SQLCODECODES                                    
372100                                                                          
372200     EXEC SQL                                                             
372300                                                                          
372400         FETCH T01PAIN-CRS-9A                                             
372500                                                                          
372600         INTO :MAP-KDFINDOC-LINE                                          
372700            , :MAP-KDPARTTY-LINE                                          
372800            , :MAP-KDPARTGR-LINE-2                                        
372900            , :MAP-KDVALISO-LINE                                          
373000            , :MAP-KDSTATUS-LINE                                          
373100            , :MAP-KDSTATUS-LINE-2                                        
373200            , :MAP-DAREGDAT-LINE                                          
373300            , :MAP-DAREGDAT-LINE-2                                        
373400            , :MAP-DAUPPDAT-LINE                                          
373500            , :MAP-DAUPPDAT-LINE-2                                        
373600            , :MAP-IDUSER-LINE                                            
373700            , :MAP-IDUSER-LINE-2                                          
373800     END-EXEC                                                             
373900                                                                          
374000     MOVE SQLCODE TO SQLCODE-WS                                           
374100     PERFORM DB2-STATUS-CHECK                                             
374200     .                                                                    
374300     EJECT                                                                
374400                                                                          
374500 DB2-CLOSE-T01PAIN-CRS-9A SECTION.                                        
374600     EXEC SQL                                                             
374700        CLOSE T01PAIN-CRS-9A                                              
374800     END-EXEC                                                             
374900     .                                                                    
375000     EJECT                                                                
375100                                                                          
375200* * * * * * * * * *   - CURSOR-FAJ- * * * * * * * * * * * * * *           
375300 DB2-COUNT-CRS-FAJ SECTION.                                               
375400     EXEC SQL                                                             
375500                                                                          
375600           SELECT COUNT(*)                                                
375700                                                                          
375800           INTO  :WS-COUNTER-FAJ                                          
375900                                                                          
376000           FROM   T01CUGR A                                               
376100                , T01DOTY B                                               
376200                , T01CURR C                                               
376300                                                                          
376400           WHERE    A.IDLEGSEL   = :REQU-IDLEGSEL-KEY                     
376500                AND A.IDLEGSEL   = B.IDLEGSEL                             
376600                AND A.IDLEGSEL   = C.IDLEGSEL                             
376700                AND C.KDVALISO   = :REQU-KDVALISO-KEY                     
376800                AND ( B.KDFINDOC = :REQU-KDFINDOC-KEY                     
376900                OR  A.KDPARTTY   = :REQU-KDPARTTY-KEY                     
377000                OR  A.KDPARTGR   = :REQU-KDPARTGR-KEY )                   
377100                AND A.DADELDAT   = '00000000'                             
377200                AND A.KDSTATUS   = :WS-CURRENT                            
377300                                                                          
377400     END-EXEC                                                             
377500                                                                          
377600     MOVE 000100  TO GOOD-SQLCODECODES                                    
377700                                                                          
377800     MOVE SQLCODE TO SQLCODE-WS                                           
377900     PERFORM DB2-STATUS-CHECK                                             
378000     .                                                                    
378100     EJECT                                                                
378200                                                                          
378300 DB2-DCL-OPN-T01DOTY-CRS-6 SECTION.                                       
378400     MOVE 000100 TO GOOD-SQLCODECODES                                     
378500                                                                          
378600     EXEC SQL                                                             
378700         DECLARE T01DOTY-CRS-6 CURSOR WITH HOLD FOR                       
378800                                                                          
378900           SELECT  A.KDFINDOC                                             
379000                 , B.KDPARTTY                                             
379100                 , B.KDPARTGR                                             
379200                 , C.KDVALISO                                             
379300                 , A.KDSTATUS                                             
379400                 , A.DAREGDAT                                             
379500                 , A.DAUPPDAT                                             
379600                 , A.IDUSER                                               
379700                                                                          
379800           FROM    T01DOTY A                                              
379900                 , T01CUGR B                                              
380000                 , T01CURR C                                              
380100                                                                          
380200           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
380300                AND A.IDLEGSEL = B.IDLEGSEL                               
380400                AND A.IDLEGSEL = C.IDLEGSEL                               
380500                AND A.DADELDAT = '00000000'                               
380600                AND B.DADELDAT = '00000000'                               
380700                AND A.KDSTATUS = :WS-CURRENT                              
380800                AND B.KDSTATUS = :WS-CURRENT                              
380900                AND C.KDVALISO = :REQU-KDVALISO-KEY                       
381000                AND A.KDFINDOC = :REQU-KDFINDOC-KEY                       
381100                AND B.KDPARTTY = :REQU-KDPARTTY-KEY                       
381200                AND B.KDPARTGR = :REQU-KDPARTGR-KEY                       
381300                                                                          
381400           ORDER BY A.IDLEGSEL                                            
381500                  , A.KDFINDOC                                            
381600                  , B.KDPARTTY                                            
381700                  , B.KDPARTGR                                            
381800                  , C.KDVALISO                                            
381900     END-EXEC                                                             
382000                                                                          
382100     MOVE 000100  TO GOOD-SQLCODECODES                                    
382200                                                                          
382300     EXEC SQL                                                             
382400        OPEN T01DOTY-CRS-6                                                
382500     END-EXEC                                                             
382600                                                                          
382700     MOVE SQLCODE TO SQLCODE-WS                                           
382800     PERFORM DB2-STATUS-CHECK                                             
382900     .                                                                    
383000     EJECT                                                                
383100                                                                          
383200 DB2-FETCH-T01DOTY-CRS-6 SECTION.                                         
383300     MOVE 000100  TO GOOD-SQLCODECODES                                    
383400                                                                          
383500     EXEC SQL                                                             
383600                                                                          
383700         FETCH T01DOTY-CRS-6                                              
383800                                                                          
383900         INTO :MAP-KDFINDOC-LINE                                          
384000            , :MAP-KDPARTTY-LINE                                          
384100            , :MAP-KDPARTGR-LINE                                          
384200            , :MAP-KDVALISO-LINE                                          
384300            , :MAP-KDSTATUS-LINE-2                                        
384400            , :MAP-DAREGDAT-LINE-2                                        
384500            , :MAP-DAUPPDAT-LINE-2                                        
384600            , :MAP-IDUSER-LINE-2                                          
384700     END-EXEC                                                             
384800                                                                          
384900     MOVE SQLCODE TO SQLCODE-WS                                           
385000     PERFORM DB2-STATUS-CHECK                                             
385100     .                                                                    
385200     EJECT                                                                
385300                                                                          
385400 DB2-CLOSE-T01DOTY-CRS-6 SECTION.                                         
385500     EXEC SQL                                                             
385600        CLOSE T01DOTY-CRS-6                                               
385700     END-EXEC                                                             
385800     .                                                                    
385900     EJECT                                                                
386000                                                                          
386100                                                                          
386200 DB2-DCL-OPN-T01PAIN-CRS-AA SECTION.                                      
386300     MOVE 000100 TO GOOD-SQLCODECODES                                     
386400                                                                          
386500     EXEC SQL                                                             
386600         DECLARE T01PAIN-CRS-AA CURSOR WITH HOLD FOR                      
386700                                                                          
386800           SELECT  KDFINDOC                                               
386900                 , KDPARTTY                                               
387000                 , KDPARTGR                                               
387100                 , KDVALISO                                               
387200                 , KDSTATUS                                               
387300                 , KDSTATUS                                               
387400                 , DAREGDAT                                               
387500                 , DAUPPDAT                                               
387600                 , IDUSER                                                 
387700                                                                          
387800           FROM    T01PAIN                                                
387900                                                                          
388000           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
388100                AND KDFINDOC = :REQU-KDFINDOC-KEY                         
388200                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
388300                AND KDPARTGR = :REQU-KDPARTGR-KEY                         
388400                AND KDVALISO = :REQU-KDVALISO-KEY                         
388500                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
388600                                                                          
388700           ORDER BY IDLEGSEL                                              
388800                  , KDFINDOC                                              
388900                  , KDPARTTY                                              
389000                  , KDPARTGR                                              
389100                  , KDVALISO                                              
389200     END-EXEC                                                             
389300                                                                          
389400     MOVE 000100  TO GOOD-SQLCODECODES                                    
389500                                                                          
389600     EXEC SQL                                                             
389700        OPEN T01PAIN-CRS-AA                                               
389800     END-EXEC                                                             
389900                                                                          
390000     MOVE SQLCODE TO SQLCODE-WS                                           
390100     PERFORM DB2-STATUS-CHECK                                             
390200     .                                                                    
390300     EJECT                                                                
390400                                                                          
390500 DB2-FETCH-T01PAIN-CRS-AA SECTION.                                        
390600     MOVE 000100  TO GOOD-SQLCODECODES                                    
390700                                                                          
390800     EXEC SQL                                                             
390900                                                                          
391000         FETCH T01PAIN-CRS-AA                                             
391100                                                                          
391200         INTO :MAP-KDFINDOC-LINE                                          
391300            , :MAP-KDPARTTY-LINE                                          
391400            , :MAP-KDPARTGR-LINE                                          
391500            , :MAP-KDVALISO-LINE                                          
391600            , :MAP-KDSTATUS-LINE                                          
391700            , :MAP-KDSTATUS-LINE-2                                        
391800            , :MAP-DAREGDAT-LINE                                          
391900            , :MAP-DAUPPDAT-LINE                                          
392000            , :MAP-IDUSER-LINE                                            
392100     END-EXEC                                                             
392200                                                                          
392300     MOVE SQLCODE TO SQLCODE-WS                                           
392400     PERFORM DB2-STATUS-CHECK                                             
392500     .                                                                    
392600     EJECT                                                                
392700                                                                          
392800 DB2-CLOSE-T01PAIN-CRS-AA SECTION.                                        
392900     EXEC SQL                                                             
393000        CLOSE T01PAIN-CRS-AA                                              
393100     END-EXEC                                                             
393200     .                                                                    
393300     EJECT                                                                
393400                                                                          
393500* * * * * * * * * *   - CURSOR-FAK- * * * * * * * * * * * * * *           
393600 DB2-COUNT-CRS-FAK SECTION.                                               
393700     EXEC SQL                                                             
393800                                                                          
393900           SELECT COUNT(*)                                                
394000                                                                          
394100           INTO  :WS-COUNTER-FAK                                          
394200                                                                          
394300           FROM   T01CUGR A                                               
394400                , T01DOTY B                                               
394500                , T01CURR C                                               
394600                                                                          
394700           WHERE    A.IDLEGSEL   = :REQU-IDLEGSEL-KEY                     
394800                AND A.IDLEGSEL   = B.IDLEGSEL                             
394900                AND A.IDLEGSEL   = C.IDLEGSEL                             
395000                AND A.DADELDAT   = '00000000'                             
395100                AND B.DADELDAT   = '00000000'                             
395200                AND A.KDSTATUS   = :WS-CURRENT                            
395300                AND B.KDSTATUS   = :WS-CURRENT                            
395400                AND C.KDVALISO   = :REQU-KDVALISO-KEY                     
395500                AND C.DASTADAT   = (SELECT MAX(DASTADAT)                  
395600                                    FROM T01CURR C                        
395700                                    WHERE C.KDVALISO =                    
395800                                    :REQU-KDVALISO-KEY)                   
395900                                                                          
396000     END-EXEC                                                             
396100                                                                          
396200     MOVE 000100  TO GOOD-SQLCODECODES                                    
396300                                                                          
396400     MOVE SQLCODE TO SQLCODE-WS                                           
396500     PERFORM DB2-STATUS-CHECK                                             
396600     .                                                                    
396700     EJECT                                                                
396800                                                                          
396900 DB2-DCL-OPN-T01DOTY-CRS-7 SECTION.                                       
397000     MOVE 000100 TO GOOD-SQLCODECODES                                     
397100                                                                          
397200     EXEC SQL                                                             
397300         DECLARE T01DOTY-CRS-7 CURSOR WITH HOLD FOR                       
397400                                                                          
397500           SELECT  DISTINCT A.KDFINDOC                                    
397600                 , B.KDPARTTY                                             
397700                 , B.KDPARTGR                                             
397800                 , C.KDVALISO                                             
397900                 , A.KDSTATUS                                             
398000                 , A.DAREGDAT                                             
398100                 , A.DAUPPDAT                                             
398200                 , A.IDUSER                                               
398300                                                                          
398400           FROM    T01DOTY A                                              
398500                 , T01CUGR B                                              
398600                 , T01CURR C                                              
398700                                                                          
398800           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
398900                AND A.IDLEGSEL = B.IDLEGSEL                               
399000                AND A.IDLEGSEL = C.IDLEGSEL                               
399100                AND C.KDVALISO = :REQU-KDVALISO-KEY                       
399200                AND A.DADELDAT = '00000000'                               
399300                AND B.DADELDAT = '00000000'                               
399400                AND A.KDSTATUS = :WS-CURRENT                              
399500                AND B.KDSTATUS = :WS-CURRENT                              
399600                                                                          
399700     END-EXEC                                                             
399800                                                                          
399900     MOVE 000100  TO GOOD-SQLCODECODES                                    
400000                                                                          
400100     EXEC SQL                                                             
400200        OPEN T01DOTY-CRS-7                                                
400300     END-EXEC                                                             
400400                                                                          
400500     MOVE SQLCODE TO SQLCODE-WS                                           
400600     PERFORM DB2-STATUS-CHECK                                             
400700     .                                                                    
400800     EJECT                                                                
400900                                                                          
401000 DB2-FETCH-T01DOTY-CRS-7 SECTION.                                         
401100     MOVE 000100  TO GOOD-SQLCODECODES                                    
401200                                                                          
401300     EXEC SQL                                                             
401400                                                                          
401500         FETCH T01DOTY-CRS-7                                              
401600                                                                          
401700         INTO :MAP-KDFINDOC-LINE-2                                        
401800            , :MAP-KDPARTTY-LINE-2                                        
401900            , :MAP-KDPARTGR-LINE-2                                        
402000            , :MAP-KDVALISO-LINE                                          
402100            , :MAP-KDSTATUS-LINE-2                                        
402200            , :MAP-DAREGDAT-LINE-2                                        
402300            , :MAP-DAUPPDAT-LINE-2                                        
402400            , :MAP-IDUSER-LINE-2                                          
402500     END-EXEC                                                             
402600                                                                          
402700     MOVE SQLCODE TO SQLCODE-WS                                           
402800     PERFORM DB2-STATUS-CHECK                                             
402900     .                                                                    
403000     EJECT                                                                
403100                                                                          
403200 DB2-CLOSE-T01DOTY-CRS-7 SECTION.                                         
403300     EXEC SQL                                                             
403400        CLOSE T01DOTY-CRS-7                                               
403500     END-EXEC                                                             
403600     .                                                                    
403700     EJECT                                                                
403800                                                                          
403900                                                                          
404000 DB2-DCL-OPN-T01PAIN-CRS-AB SECTION.                                      
404100     MOVE 000100 TO GOOD-SQLCODECODES                                     
404200                                                                          
404300     EXEC SQL                                                             
404400         DECLARE T01PAIN-CRS-AB CURSOR WITH HOLD FOR                      
404500                                                                          
404600           SELECT  KDFINDOC                                               
404700                 , KDPARTTY                                               
404800                 , KDPARTGR                                               
404900                 , KDVALISO                                               
405000                 , KDSTATUS                                               
405100                 , KDSTATUS                                               
405200                 , DAREGDAT                                               
405300                 , DAUPPDAT                                               
405400                 , IDUSER                                                 
405500                                                                          
405600           FROM    T01PAIN                                                
405700                                                                          
405800           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
405900                AND KDVALISO = :REQU-KDVALISO-KEY                         
406000                AND KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING           
406100                                                                          
406200           ORDER BY IDLEGSEL                                              
406300                  , KDFINDOC                                              
406400                  , KDPARTTY                                              
406500                  , KDPARTGR                                              
406600                  , KDVALISO                                              
406700     END-EXEC                                                             
406800                                                                          
406900     MOVE 000100  TO GOOD-SQLCODECODES                                    
407000                                                                          
407100     EXEC SQL                                                             
407200        OPEN T01PAIN-CRS-AB                                               
407300     END-EXEC                                                             
407400                                                                          
407500     MOVE SQLCODE TO SQLCODE-WS                                           
407600     PERFORM DB2-STATUS-CHECK                                             
407700     .                                                                    
407800     EJECT                                                                
407900                                                                          
408000 DB2-FETCH-T01PAIN-CRS-AB SECTION.                                        
408100     MOVE 000100  TO GOOD-SQLCODECODES                                    
408200                                                                          
408300     EXEC SQL                                                             
408400                                                                          
408500         FETCH T01PAIN-CRS-AB                                             
408600                                                                          
408700         INTO :MAP-KDFINDOC-LINE-2                                        
408800            , :MAP-KDPARTTY-LINE-2                                        
408900            , :MAP-KDPARTGR-LINE-2                                        
409000            , :MAP-KDVALISO-LINE                                          
409100            , :MAP-KDSTATUS-LINE                                          
409200            , :MAP-KDSTATUS-LINE-2                                        
409300            , :MAP-DAREGDAT-LINE                                          
409400            , :MAP-DAUPPDAT-LINE                                          
409500            , :MAP-IDUSER-LINE                                            
409600     END-EXEC                                                             
409700                                                                          
409800     MOVE SQLCODE TO SQLCODE-WS                                           
409900     PERFORM DB2-STATUS-CHECK                                             
410000     .                                                                    
410100     EJECT                                                                
410200                                                                          
410300 DB2-CLOSE-T01PAIN-CRS-AB SECTION.                                        
410400     EXEC SQL                                                             
410500        CLOSE T01PAIN-CRS-AB                                              
410600     END-EXEC                                                             
410700     .                                                                    
410800     EJECT                                                                
410900                                                                          
411000* * * * * * * * * *   - CURSOR-FAL-  * * * * * * * * * * * * *            
411100 DB2-COUNT-CRS-FAL SECTION.                                               
411200     EXEC SQL                                                             
411300                                                                          
411400          SELECT COUNT(*)                                                 
411500                                                                          
411600          INTO  :WS-COUNTER-FAL                                           
411700                                                                          
411800          FROM   T01DOTY                                                  
411900                                                                          
412000          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
412100             AND   KDSTATUS = :WS-CURRENT                                 
412200             AND   DADELDAT = '00000000'                                  
412300                                                                          
412400     END-EXEC                                                             
412500                                                                          
412600     MOVE 000100  TO GOOD-SQLCODECODES                                    
412700                                                                          
412800     MOVE SQLCODE TO SQLCODE-WS                                           
412900     PERFORM DB2-STATUS-CHECK                                             
413000     .                                                                    
413100     EJECT                                                                
413200                                                                          
413300 DB2-DCL-OPN-T01DOTY-CRS-10B SECTION.                                     
413400     MOVE 000100 TO GOOD-SQLCODECODES                                     
413500                                                                          
413600     EXEC SQL                                                             
413700         DECLARE T01DOTY-CRS-10B CURSOR WITH HOLD FOR                     
413800                                                                          
413900           SELECT  A.KDFINDOC                                             
414000                 , B.KDPARTTY                                             
414100                 , B.KDPARTGR                                             
414200                 , A.KDSTATUS                                             
414300                 , A.DAREGDAT                                             
414400                 , A.DAUPPDAT                                             
414500                 , A.IDUSER                                               
414600                                                                          
414700           FROM    T01DOTY A                                              
414800                 , T01CUGR B                                              
414900                                                                          
415000           WHERE    A.IDLEGSEL = :REQU-IDLEGSEL-KEY                       
415100                AND A.IDLEGSEL = B.IDLEGSEL                               
415200                AND A.DADELDAT = '00000000'                               
415300                AND B.DADELDAT = '00000000'                               
415400                AND A.KDSTATUS = :WS-CURRENT                              
415500                AND B.KDSTATUS = :WS-CURRENT                              
415600                                                                          
415700           ORDER BY A.IDLEGSEL                                            
415800                  , A.KDFINDOC                                            
415900                  , B.KDPARTTY                                            
416000                  , B.KDPARTGR                                            
416100     END-EXEC                                                             
416200                                                                          
416300     MOVE 000100  TO GOOD-SQLCODECODES                                    
416400                                                                          
416500     EXEC SQL                                                             
416600        OPEN T01DOTY-CRS-10B                                              
416700     END-EXEC                                                             
416800                                                                          
416900     MOVE SQLCODE TO SQLCODE-WS                                           
417000     PERFORM DB2-STATUS-CHECK                                             
417100     .                                                                    
417200     EJECT                                                                
417300                                                                          
417400 DB2-FETCH-T01DOTY-CRS-10B SECTION.                                       
417500     MOVE 000100  TO GOOD-SQLCODECODES                                    
417600                                                                          
417700     EXEC SQL                                                             
417800                                                                          
417900         FETCH T01DOTY-CRS-10B                                            
418000                                                                          
418100         INTO :MAP-KDFINDOC-LINE                                          
418200            , :MAP-KDPARTTY-LINE-2                                        
418300            , :MAP-KDPARTGR-LINE-2                                        
418400            , :MAP-KDSTATUS-LINE-2                                        
418500            , :MAP-DAREGDAT-LINE-2                                        
418600            , :MAP-DAUPPDAT-LINE-2                                        
418700            , :MAP-IDUSER-LINE-2                                          
418800     END-EXEC                                                             
418900                                                                          
419000     MOVE SQLCODE TO SQLCODE-WS                                           
419100     PERFORM DB2-STATUS-CHECK                                             
419200     .                                                                    
419300     EJECT                                                                
419400                                                                          
419500 DB2-CLOSE-T01DOTY-CRS-10B SECTION.                                       
419600     EXEC SQL                                                             
419700        CLOSE T01DOTY-CRS-10B                                             
419800     END-EXEC                                                             
419900     .                                                                    
420000     EJECT                                                                
420100                                                                          
420200 DB2-DCL-OPN-T01PAIN-CRS-10A SECTION.                                     
420300     MOVE 000100 TO GOOD-SQLCODECODES                                     
420400                                                                          
420500     EXEC SQL                                                             
420600         DECLARE T01PAIN-CRS-10A CURSOR WITH HOLD FOR                     
420700                                                                          
420800          SELECT  DISTINCT A.KDFINDOC                                     
420900                 , B.KDPARTTY                                             
421000                 , B.KDPARTGR                                             
421100                 , A.KDVALISO                                             
421200                 , A.KDSTATUS                                             
421300                 , B.KDSTATUS                                             
421400                 , A.DAREGDAT                                             
421500                 , B.DAREGDAT                                             
421600                 , A.DAUPPDAT                                             
421700                 , B.DAUPPDAT                                             
421800                 , A.IDUSER                                               
421900                 , B.IDUSER                                               
422000                                                                          
422100          FROM     T01PAIN A                                              
422200                 , T01CUGR B                                              
422300                 , T01DOTY C                                              
422400                                                                          
422500          WHERE    B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
422600             AND   A.IDLEGSEL = B.IDLEGSEL                                
422700             AND   A.IDLEGSEL = C.IDLEGSEL                                
422800             AND   A.KDFINDOC = C.KDFINDOC                                
422900             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
423000             AND   B.KDSTATUS = :WS-CURRENT                               
423100             AND   B.DADELDAT = '00000000'                                
423200             AND   C.KDSTATUS = :WS-CURRENT                               
423300             AND   C.DADELDAT = '00000000'                                
423400             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
423500             AND   A.IDLEGSEL = B.IDLEGSEL                                
423600             AND   A.KDFINDOC = C.KDFINDOC                                
423700             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
423800             AND   B.KDSTATUS = :WS-CURRENT                               
423900             AND   B.DADELDAT = '00000000'                                
424000             AND   C.KDSTATUS = :WS-CURRENT                               
424100             AND   C.DADELDAT = '00000000'                                
424200             AND ( B.KDPARTTY <> A.KDPARTTY                               
424300             AND   B.KDPARTGR <> A.KDPARTGR )                             
424400             OR (  B.IDLEGSEL = :REQU-IDLEGSEL-KEY                        
424500             AND   A.IDLEGSEL = B.IDLEGSEL                                
424600             AND   A.KDFINDOC = C.KDFINDOC                                
424700             AND   A.KDSTATUS BETWEEN :WS-CURRENT AND :WS-COMING          
424800             AND   B.KDSTATUS = :WS-CURRENT                               
424900             AND   B.DADELDAT = '00000000'                                
425000             AND   C.KDSTATUS = :WS-CURRENT                               
425100             AND   C.DADELDAT = '00000000'                                
425200             AND   B.KDPARTTY =  A.KDPARTTY                               
425300             AND   B.KDPARTGR =  A.KDPARTGR ))                            
425400                                                                          
425500*         ORDER BY A.IDLEGSEL                                             
425600*                , C.KDFINDOC                                             
425700*                , B.KDPARTTY                                             
425800*                , B.KDPARTGR                                             
425900*                , A.KDVALISO                                             
426000*                , A.KDSTATUS DESC                                        
426100     END-EXEC                                                             
426200                                                                          
426300     MOVE 000100  TO GOOD-SQLCODECODES                                    
426400                                                                          
426500     EXEC SQL                                                             
426600        OPEN T01PAIN-CRS-10A                                              
426700     END-EXEC                                                             
426800                                                                          
426900     MOVE SQLCODE TO SQLCODE-WS                                           
427000     PERFORM DB2-STATUS-CHECK                                             
427100     .                                                                    
427200     EJECT                                                                
427300                                                                          
427400 DB2-FETCH-T01PAIN-CRS-10A SECTION.                                       
427500     MOVE 000100  TO GOOD-SQLCODECODES                                    
427600                                                                          
427700     EXEC SQL                                                             
427800                                                                          
427900         FETCH T01PAIN-CRS-10A                                            
428000                                                                          
428100         INTO :MAP-KDFINDOC-LINE                                          
428200            , :MAP-KDPARTTY-LINE-2                                        
428300            , :MAP-KDPARTGR-LINE-2                                        
428400            , :MAP-KDVALISO-LINE-2                                        
428500            , :MAP-KDSTATUS-LINE                                          
428600            , :MAP-KDSTATUS-LINE-2                                        
428700            , :MAP-DAREGDAT-LINE                                          
428800            , :MAP-DAREGDAT-LINE-2                                        
428900            , :MAP-DAUPPDAT-LINE                                          
429000            , :MAP-DAUPPDAT-LINE-2                                        
429100            , :MAP-IDUSER-LINE                                            
429200            , :MAP-IDUSER-LINE-2                                          
429300     END-EXEC                                                             
429400                                                                          
429500     MOVE SQLCODE TO SQLCODE-WS                                           
429600     PERFORM DB2-STATUS-CHECK                                             
429700     .                                                                    
429800     EJECT                                                                
429900                                                                          
430000 DB2-CLOSE-T01PAIN-CRS-10A SECTION.                                       
430100     EXEC SQL                                                             
430200        CLOSE T01PAIN-CRS-3A                                              
430300     END-EXEC                                                             
430400     .                                                                    
430500     EJECT                                                                
430600                                                                          
430700 DB2-STATUS-CHECK  SECTION.                                               
430800     SET SQLCODE-IX TO 1                                                  
430900     SEARCH GOOD-SQLCODE                                                  
431000       AT END                                                             
431100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
431200          DELIMITED BY SIZE INTO ERROR-TEXT                               
431300          CALL ABEND USING RKOD-ABEND-DB2                                 
431400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
431500          CONTINUE                                                        
431600     END-SEARCH                                                           
431700     .                                                                    
