000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF028600.                                                
000400 AUTHOR.         BARSHARANI BISHOYE                                       
000500 DATE-WRITTEN.   22/06/2020.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.ROLLINGDOCNUMSERASSLOCATE                        
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE T01LSEL                                
001500*        THE PROGRAM READS   TABLE T01ASNS                                
001600*        THE PROGRAM READS   TABLE T01BURE                                
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: WF0286T                                             
002000*                                                                         
002100*        REQUEST:     WZ01REQU                                            
002200*                     WF0286I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    WZ01RESP                                            
002600*                     WF0286O1                                            
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)  VALUE 'WF028600'.             
003400                                                                          
003500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700 77  KDRC-DISPLAY                PIC Z(5).                                
003800                                                                          
003900*    --- CONSTANTS                                                        
004000 77  YES                         PIC X      VALUE 'Y'.                    
004100 77  NOO                         PIC X      VALUE 'N'.                    
004200                                                                          
004300 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
004400 77  WS-CURRENT                  PIC S9(3)  VALUE +001    COMP-3.         
004500 77  WS-ACTIVE                   PIC X(8)   VALUE '00000000'.             
004600 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004700 77  WS-NOT-REG                  PIC X(8)   VALUE 'NOT REG.'.             
004800 77  WS-ADRESS                   PIC X(50)                                
004900           VALUE 'CARPARTS.BILLIT.ROLLINGDOCNUMSERASSLOCATE'.             
005000                                                                          
005100 77  KEYS-SW                     PIC X      VALUE SPACE.                  
005200     88  KEYS-OK                            VALUE 'Y'.                    
005300     88  KEYS-WRONG                         VALUE 'N'.                    
005400                                                                          
005500*    --- WORK FIELDS                                                      
005600 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
005610 01  WS-IX2                      PIC S9(9)  VALUE ZERO    BINARY.         
005700 01  WS-COUNTER-FAA              PIC S9(7)  VALUE ZERO    COMP-3.         
005800 01  WS-COUNTER-FAA-2            PIC S9(7)  VALUE ZERO    COMP-3.         
005900 01  WS-COUNTER-FAB              PIC S9(7)  VALUE ZERO    COMP-3.         
006000 01  WS-COUNTER-FAB-2            PIC S9(7)  VALUE ZERO    COMP-3.         
006100 01  WS-COUNT-FAB-2A             PIC S9(7)  VALUE ZERO    COMP-3.         
006200 01  WS-COUNTER-FAC              PIC S9(7)  VALUE ZERO    COMP-3.         
006300 01  WS-COUNTER-FAC-2            PIC S9(7)  VALUE ZERO    COMP-3.         
006400 01  WS-COUNT-FAC-2A             PIC S9(7)  VALUE ZERO    COMP-3.         
006500 01  WS-COUNTER-FAD              PIC S9(7)  VALUE ZERO    COMP-3.         
006600 01  WS-COUNTER-FAD-2            PIC S9(7)  VALUE ZERO    COMP-3.         
006700 01  WS-COUNT-FAD-2A             PIC S9(7)  VALUE ZERO    COMP-3.         
006800 01  WS-COUNTER-FAE              PIC S9(7)  VALUE ZERO    COMP-3.         
006900 01  WS-COUNTER-FAE-2            PIC S9(7)  VALUE ZERO    COMP-3.         
007000 01  WS-COUNT-FAE-2A             PIC S9(7)  VALUE ZERO    COMP-3.         
007010 01  WS-COUNTER-FAF              PIC S9(7)  VALUE ZERO    COMP-3.         
007020 01  WS-COUNTER-FAF-2            PIC S9(7)  VALUE ZERO    COMP-3.         
007030 01  WS-COUNT-FAF-2A             PIC S9(7)  VALUE ZERO    COMP-3.         
007100 01  WS-KEY1-MARKED              PIC X      VALUE SPACE.                  
007200                                                                          
007300*    --- MAPPING FIELDS                                                   
007400 01  MAP-KDFINDOC-LINE           PIC X(4)   VALUE SPACE.                  
007500 01  MAP-KDFINDOC-LINE-2         PIC X(4)   VALUE SPACE.                  
007600 01  MAP-KDPARTTY-LINE           PIC X(3)   VALUE SPACE.                  
007700 01  MAP-KDPARTTY-LINE-2         PIC X(3)   VALUE SPACE.                  
007800 01  MAP-KDPARTGR-LINE           PIC X(15)  VALUE SPACE.                  
007900 01  MAP-KDPARTGR-LINE-2         PIC X(15)  VALUE SPACE.                  
008000 01  MAP-IDLANDX3-LINE           PIC X(3)   VALUE SPACE.                  
008300 01  MAP-IDLOPNR-LINE            PIC S9(3)  VALUE ZERO COMP-3.            
008400 01  MAP-IDLOPNR-LINE-2          PIC 9(3)   VALUE ZERO.                   
008500 01  MAP-DAREGDAT-LINE           PIC X(8)   VALUE SPACE.                  
008600 01  MAP-IDUSER-LINE             PIC X(8)   VALUE SPACE.                  
008700                                                                          
008800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008900 01  GENERAL-SUBPROGRAMS.                                                 
009000     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
009100     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
009200                                                                          
009300*    --- PARAMETERS TO ABEND                                              
009400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
009800                                                                          
009900 01  MESSAGE-CODES.                                                       
010000     03  ERROR-CODES.                                                     
010100         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
010200         05  ERR-INVALID-FIELD       PIC X(3)   VALUE '023'.              
010300         05  ERR-MUST-BE-NUMERIC     PIC X(3)   VALUE '024'.              
010400         05  NOT-FOUND               PIC X(3)   VALUE '025'.              
010500         05  ERR-MUST-BE-ENTERED     PIC X(3)   VALUE '026'.              
010600         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
010700         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
010800         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
010900                                                                          
011000*01  -COPY WZ01SUB                                                        
011100     EJECT                                                                
011200*                                                                         
011300 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
011400 01  REQU-AREA.                                                           
011500*    03 -COPY WZ01REQU                                                    
011600*    03 -COPY WF0286I1                                                    
011700     EJECT                                                                
011800                                                                          
011900 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
012000 01  RESP-AREA.                                                           
012100*    03 -COPY WZ01RESP                                                    
012200*    03 -COPY WF0286O1                                                    
012300     EJECT                                                                
012400                                                                          
012500 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
012600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012700                                                                          
012800 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
012900 01  DB2-WS.                                                              
013000     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
013100         88  CURSOR-OK                      VALUE 000.                    
013200         88  LINES-FOUND                    VALUE 000.                    
013300         88  LINES-MISSING                  VALUE 100.                    
013400         88  RESOURCE-WRONG                 VALUE 904.                    
013500     03  GOOD-SQLCODECODES.                                               
013600         05  GOOD-SQLCODE OCCURS 5                                        
013700             INDEXED BY SQLCODE-IX PIC 9(3).                              
013800                                                                          
013900 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
014000*01  -COPY T01LSEL -PRE T01LSEL-                                          
014100     EJECT                                                                
014200                                                                          
014300 01  FILLER                      PIC X(16)   VALUE 'T01ASNS-AREA'.        
014400*01  -COPY T01ASNS -PRE T01ASNS-                                          
014500     EJECT                                                                
014600                                                                          
014700 01  FILLER                      PIC X(16)   VALUE 'T01BURE-AREA'.        
014800*01  -COPY T01BURE -PRE T01BURE-                                          
014900     EJECT                                                                
015000                                                                          
015100       EXEC SQL INCLUDE T01LSEL END-EXEC.                                 
015200     EJECT                                                                
015300       EXEC SQL INCLUDE T01ASNS END-EXEC.                                 
015400     EJECT                                                                
015500       EXEC SQL INCLUDE T01BURE END-EXEC.                                 
015600     EJECT                                                                
015700 LINKAGE SECTION.                                                         
015800                                                                          
015900 PROCEDURE DIVISION.                                                      
016000 MAIN SECTION.                                                            
016100                                                                          
016200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
016300     IF SUB-KDRC = ZERO                                                   
016400       PERFORM A-INIT                                                     
016500       PERFORM B-CHECK-KEYS                                               
016600       IF KEYS-OK                                                         
016700         PERFORM BA-CHECK-KEY-RELATION                                    
016800       END-IF                                                             
016900       IF KEYS-OK                                                         
017000         PERFORM F-READ-SHOW-INFO                                         
017100       END-IF                                                             
017200*        CALL ABEND USING RKOD-ABEND-DB2                                  
017300       PERFORM S02-RETURN-RESPONSE                                        
017400     END-IF                                                               
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017810                                                                          
017900 A-INIT SECTION.                                                          
018100     INITIALIZE GOOD-SQLCODECODES                                         
018200     MOVE ALL '+' TO RESP-AREA                                            
018300     MOVE SPACE TO RESP-IDMSG-ERROR                                       
018400     MOVE SPACE TO RESP-IDMSG-INFO                                        
018500     MOVE SPACE TO RESP-IDELMT-ERROR                                      
018600     MOVE ZERO TO RESP-KVRADER                                            
018700     .                                                                    
018710                                                                          
018800*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
018900 B-CHECK-KEYS SECTION.                                                    
019000     MOVE YES TO KEYS-SW                                                  
019100     IF REQU-IDMSGVER NUMERIC                                             
019200       IF REQU-IDLEGSEL-KEY > SPACE                                       
019300       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
019400       AND REQU-KDPGMACT = WS-SEARCH                                      
019500         CONTINUE                                                         
019600       ELSE                                                               
019700         MOVE NOO TO KEYS-SW                                              
019800       END-IF                                                             
019900     ELSE                                                                 
020000       MOVE NOO TO KEYS-SW                                                
020100     END-IF                                                               
020200                                                                          
020510     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
020520       MOVE NOO TO KEYS-SW                                                
020530     END-IF                                                               
020600                                                                          
020700     IF KEYS-WRONG                                                        
020800       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
020900       IF REQU-IDMSGVER NUMERIC                                           
021000         CONTINUE                                                         
021100       ELSE                                                               
021200         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
021300         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
021400       END-IF                                                             
021500       IF REQU-KDPGMACT = WS-SEARCH                                       
021600         CONTINUE                                                         
021700       ELSE                                                               
021800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
021900         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
022000       END-IF                                                             
022100       IF REQU-IDUSER = SPACE OR = ALL '+'                                
022200         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
022300         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
022400       END-IF                                                             
022500     END-IF                                                               
022600     IF KEYS-OK                                                           
022700       PERFORM DB2-SELECT-T01LSEL-TAB                                     
022800       IF LINES-FOUND                                                     
022900         CONTINUE                                                         
023000       ELSE                                                               
023100         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
023200         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
023300         MOVE NOO TO KEYS-SW                                              
023400       END-IF                                                             
023500     END-IF                                                               
023600     .                                                                    
023610                                                                          
023700*** - CHECK REQUESTED SEARCHING KEYS                                      
023800 BA-CHECK-KEY-RELATION SECTION.                                           
024000     IF (REQU-KDFINDOC-KEY = SPACE OR = ALL '+')                          
024100       IF (REQU-KDPARTTY-KEY = SPACE OR = ALL '+')                        
024200         IF (REQU-KDPARTGR-KEY = SPACE OR = ALL '+')                      
024510             CONTINUE                                                     
024600         ELSE                                                             
024700           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
024800           MOVE 'KDPARTTY'          TO RESP-IDELMT-ERROR                  
024900           MOVE NOO TO KEYS-SW                                            
025000         END-IF                                                           
025100       ELSE                                                               
025200         CONTINUE                                                         
025300       END-IF                                                             
025400     ELSE                                                                 
025500       IF REQU-KDPARTTY-KEY = SPACE OR = ALL '+'                          
025600         IF (REQU-KDPARTGR-KEY = SPACE OR = ALL '+')                      
025700           CONTINUE                                                       
025800         ELSE                                                             
025900           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
026000           MOVE 'KDPARTTY'          TO RESP-IDELMT-ERROR                  
026100           MOVE NOO TO KEYS-SW                                            
026200         END-IF                                                           
026300       ELSE                                                               
026400         CONTINUE                                                         
026500       END-IF                                                             
026600     END-IF                                                               
026700     .                                                                    
026710                                                                          
026800*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
026900 F-READ-SHOW-INFO SECTION.                                                
027100     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
027200     MOVE REQU-KDFINDOC-KEY  TO RESP-KDFINDOC-KEY                         
027300     MOVE REQU-KDPARTTY-KEY  TO RESP-KDPARTTY-KEY                         
027400     MOVE REQU-KDPARTGR-KEY  TO RESP-KDPARTGR-KEY                         
027500     MOVE REQU-IDMSGVER      TO RESP-IDMSGVER                             
027600     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
027700                                                                          
027800     PERFORM FA-READ-BASICDATA                                            
027900     .                                                                    
027910                                                                          
028000*** - CHECK WHICH REQUESTED KEY                                           
028100 FA-READ-BASICDATA SECTION.                                               
028300     MOVE ZERO TO WS-COUNTER-FAA                                          
028400     MOVE ZERO TO WS-COUNTER-FAA-2                                        
028500     MOVE ZERO TO WS-COUNTER-FAB                                          
028600     MOVE ZERO TO WS-COUNTER-FAB-2                                        
028700     MOVE ZERO TO WS-COUNT-FAB-2A                                         
028800     MOVE ZERO TO WS-COUNTER-FAC                                          
028900     MOVE ZERO TO WS-COUNTER-FAC-2                                        
029000     MOVE ZERO TO WS-COUNTER-FAD                                          
029100     MOVE ZERO TO WS-COUNTER-FAD-2                                        
029200     MOVE ZERO TO WS-COUNTER-FAE                                          
029210     MOVE ZERO TO WS-COUNTER-FAE-2                                        
029300     MOVE ZERO TO WS-COUNTER-FAF                                          
029310     MOVE ZERO TO WS-COUNTER-FAF-2                                        
029320     MOVE ZERO TO WS-COUNT-FAF-2A                                         
029400     MOVE SPACE TO WS-KEY1-MARKED                                         
029500                                                                          
029600     IF REQU-IDLEGSEL-KEY > SPACE                                         
029610     AND REQU-KDFINDOC-KEY = ALL '+'                                      
029700     AND REQU-KDPARTTY-KEY = ALL '+'                                      
029800     AND REQU-KDPARTGR-KEY = ALL '+'                                      
029814       PERFORM FAF-SEARCH-EMPTY                                           
029900     ELSE                                                                 
030000       IF REQU-KDFINDOC-KEY = ALL '+'                                     
030100         IF REQU-KDPARTTY-KEY = ALL '+'                                   
030102           CONTINUE                                                       
030110         ELSE                                                             
030112           IF REQU-KDPARTGR-KEY = ALL '+'                                 
030113             PERFORM FAE-SEARCH-TYPE                                      
030114           ELSE                                                           
030115            PERFORM FAC-SEARCH-TYPE-GROUP                                 
030116           END-IF                                                         
030120         END-IF                                                           
030200       ELSE                                                               
030210         IF REQU-KDPARTTY-KEY = ALL '+'                                   
030220           PERFORM FAD-SEARCH-DOC                                         
030230         ELSE                                                             
030240           IF REQU-KDPARTGR-KEY = ALL '+'                                 
030250             PERFORM FAB-SEARCH-DOC-TYPE                                  
030251           ELSE                                                           
030252             PERFORM FAA-SEARCH-ALL                                       
030260           END-IF                                                         
030270         END-IF                                                           
030300       END-IF                                                             
030400     END-IF                                                               
031800     MOVE WS-IX2 TO RESP-KVRADER                                          
031900     .                                                                    
031910                                                                          
032000*** - HANDLE ALL KEYS                                                     
032100 FAA-SEARCH-ALL SECTION.                                                  
032200     PERFORM DB2-COUNT-CRS-FAA                                            
032300     IF WS-COUNTER-FAA = ZERO                                             
032400       PERFORM DB2-COUNT-CRS-FAA-2                                        
032500       IF WS-COUNTER-FAA-2 = ZERO                                         
032600         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
032700       ELSE                                                               
032800         IF WS-COUNTER-FAA-2 > WS-MAX-LINES                               
032900           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
033000         END-IF                                                           
033100       END-IF                                                             
033200     ELSE                                                                 
033300       IF WS-COUNTER-FAA > WS-MAX-LINES                                   
033400         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
033500       END-IF                                                             
033600     END-IF                                                               
033700                                                                          
033800     IF RESP-IDMSG-ERROR = SPACE                                          
033900       PERFORM DB2-DCL-OPN-T01ASNS-CRS-1                                  
034000       PERFORM DB2-FETCH-T01ASNS-CRS-1                                    
034100       MOVE 1    TO WS-IX                                                 
034110       MOVE ZERO TO WS-IX2                                                
034200       PERFORM UNTIL LINES-MISSING                                        
034300         PERFORM S03-MOVE-TO-RESPOND                                      
034400         PERFORM DB2-FETCH-T01ASNS-CRS-1                                  
034500       END-PERFORM                                                        
034600       PERFORM DB2-CLOSE-T01ASNS-CRS-1                                    
034700                                                                          
034800       IF WS-IX2 = ZERO                                                   
034900         PERFORM DB2-DCL-OPN-T01BURE-CRS-1                                
035000         PERFORM DB2-FETCH-T01BURE-CRS-1                                  
035100         PERFORM UNTIL LINES-MISSING                                      
035200           PERFORM S03-MOVE-TO-RESPOND                                    
035300           PERFORM DB2-FETCH-T01BURE-CRS-1                                
035400         END-PERFORM                                                      
035500         PERFORM DB2-CLOSE-T01BURE-CRS-1                                  
035600       END-IF                                                             
035700     END-IF                                                               
035800     .                                                                    
035810                                                                          
035900*** - HANDLE KEY FINDOC AND PARTNER TYPE                                  
036000 FAB-SEARCH-DOC-TYPE SECTION.                                             
036100     PERFORM DB2-COUNT-CRS-FAB                                            
036200     IF WS-COUNTER-FAB = ZERO                                             
036300       PERFORM DB2-COUNT-CRS-FAB-2                                        
036400       IF WS-COUNTER-FAB-2 = ZERO                                         
036500         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
036600       ELSE                                                               
036700         IF WS-COUNTER-FAB-2 > WS-MAX-LINES                               
036800           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
036900         END-IF                                                           
037000       END-IF                                                             
037100     ELSE                                                                 
037200       IF WS-COUNTER-FAB > WS-MAX-LINES                                   
037300         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
037400       END-IF                                                             
037500     END-IF                                                               
037600                                                                          
037700     IF RESP-IDMSG-ERROR = SPACE                                          
037800       PERFORM DB2-DCL-OPN-T01ASNS-CRS-2                                  
037900       PERFORM DB2-FETCH-T01ASNS-CRS-2                                    
038000       MOVE 1    TO WS-IX                                                 
038010       MOVE ZERO TO WS-IX2                                                
038100       PERFORM UNTIL LINES-MISSING                                        
038200         PERFORM DB2-COUNT-T01ASNS-2A                                     
038300         IF WS-COUNT-FAB-2A > ZERO                                        
038400           PERFORM DB2-DCL-OPN-T01ASNS-CRS-2A                             
038500           PERFORM DB2-FETCH-T01ASNS-CRS-2A                               
038600           PERFORM UNTIL LINES-MISSING                                    
038700             PERFORM S03-MOVE-TO-RESPOND                                  
038800             PERFORM DB2-FETCH-T01ASNS-CRS-2A                             
038900           END-PERFORM                                                    
039000           PERFORM DB2-CLOSE-T01ASNS-CRS-2A                               
039100         ELSE                                                             
039200           PERFORM DB2-COUNT-T01BURE-2A                                   
039300           IF WS-COUNT-FAB-2A > ZERO                                      
039400             PERFORM DB2-DCL-OPN-T01BURE-CRS-2A                           
039500             PERFORM DB2-FETCH-T01BURE-CRS-2A                             
039600             PERFORM UNTIL LINES-MISSING                                  
039700               PERFORM S03-MOVE-TO-RESPOND                                
039800               PERFORM DB2-FETCH-T01BURE-CRS-2A                           
039900             END-PERFORM                                                  
040000             PERFORM DB2-CLOSE-T01BURE-CRS-2A                             
040100           END-IF                                                         
040200         END-IF                                                           
040300         PERFORM DB2-FETCH-T01ASNS-CRS-2                                  
040400       END-PERFORM                                                        
040500       PERFORM DB2-CLOSE-T01ASNS-CRS-2                                    
040600                                                                          
040700       IF WS-IX2 = ZERO                                                   
040800         PERFORM DB2-DCL-OPN-T01BURE-CRS-2                                
040900         PERFORM DB2-FETCH-T01BURE-CRS-2                                  
041000         PERFORM UNTIL LINES-MISSING                                      
041100           PERFORM S03-MOVE-TO-RESPOND                                    
041200           PERFORM DB2-FETCH-T01BURE-CRS-2                                
041300         END-PERFORM                                                      
041400         PERFORM DB2-CLOSE-T01BURE-CRS-2                                  
041500       END-IF                                                             
041600                                                                          
041700     END-IF                                                               
041800     .                                                                    
041810                                                                          
041900*** - HANDLE KEY PARTNER TYPE AND PARTNER GROUP                           
042000 FAC-SEARCH-TYPE-GROUP SECTION.                                           
042100     PERFORM DB2-COUNT-CRS-FAC                                            
042200     IF WS-COUNTER-FAC = ZERO                                             
042300       PERFORM DB2-COUNT-CRS-FAC-2                                        
042400       IF WS-COUNTER-FAC-2 = ZERO                                         
042500         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
042600       ELSE                                                               
042700         IF WS-COUNTER-FAC-2 > WS-MAX-LINES                               
042800           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
042900         END-IF                                                           
043000       END-IF                                                             
043100     ELSE                                                                 
043200       IF WS-COUNTER-FAC > WS-MAX-LINES                                   
043300         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
043400       END-IF                                                             
043500     END-IF                                                               
043600                                                                          
043700     IF RESP-IDMSG-ERROR = SPACE                                          
043800       PERFORM DB2-DCL-OPN-T01ASNS-CRS-3                                  
043900       PERFORM DB2-FETCH-T01ASNS-CRS-3                                    
044000       MOVE 1    TO WS-IX                                                 
044010       MOVE ZERO TO WS-IX2                                                
044100       PERFORM UNTIL LINES-MISSING                                        
044200         PERFORM DB2-COUNT-T01ASNS-3A                                     
044300         IF WS-COUNT-FAC-2A > ZERO                                        
044400           PERFORM DB2-DCL-OPN-T01ASNS-CRS-3A                             
044500           PERFORM DB2-FETCH-T01ASNS-CRS-3A                               
044600           PERFORM UNTIL LINES-MISSING                                    
044700             PERFORM S03-MOVE-TO-RESPOND                                  
044800             PERFORM DB2-FETCH-T01ASNS-CRS-3A                             
044900           END-PERFORM                                                    
045000           PERFORM DB2-CLOSE-T01ASNS-CRS-3A                               
045100         ELSE                                                             
045200           PERFORM DB2-COUNT-T01BURE-3A                                   
045300           IF WS-COUNT-FAC-2A > ZERO                                      
045400             PERFORM DB2-DCL-OPN-T01BURE-CRS-3A                           
045500             PERFORM DB2-FETCH-T01BURE-CRS-3A                             
045600             PERFORM UNTIL LINES-MISSING                                  
045700               PERFORM S03-MOVE-TO-RESPOND                                
045800               PERFORM DB2-FETCH-T01BURE-CRS-3A                           
045900             END-PERFORM                                                  
046000             PERFORM DB2-CLOSE-T01BURE-CRS-3A                             
046100           END-IF                                                         
046200         END-IF                                                           
046300         PERFORM DB2-FETCH-T01ASNS-CRS-3                                  
046400       END-PERFORM                                                        
046500       PERFORM DB2-CLOSE-T01ASNS-CRS-3                                    
046600                                                                          
046700       IF WS-IX2 = ZERO                                                   
046800         PERFORM DB2-DCL-OPN-T01BURE-CRS-3                                
046900         PERFORM DB2-FETCH-T01BURE-CRS-3                                  
047000         PERFORM UNTIL LINES-MISSING                                      
047100           PERFORM S03-MOVE-TO-RESPOND                                    
047200           PERFORM DB2-FETCH-T01BURE-CRS-3                                
047300         END-PERFORM                                                      
047400         PERFORM DB2-CLOSE-T01BURE-CRS-3                                  
047500       END-IF                                                             
047600     END-IF                                                               
047700     .                                                                    
047710                                                                          
047800*** - HANDLE KEY KDFINDOC                                                 
047900 FAD-SEARCH-DOC SECTION.                                                  
048000     PERFORM DB2-COUNT-CRS-FAD                                            
048100     IF WS-COUNTER-FAD = ZERO                                             
048200       PERFORM DB2-COUNT-CRS-FAD-2                                        
048300       IF WS-COUNTER-FAD-2 = ZERO                                         
048400         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
048500       ELSE                                                               
048600         IF WS-COUNTER-FAD-2 > WS-MAX-LINES                               
048700           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
048800         END-IF                                                           
048900       END-IF                                                             
049000     ELSE                                                                 
049100       IF WS-COUNTER-FAD > WS-MAX-LINES                                   
049200         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
049300       END-IF                                                             
049400     END-IF                                                               
049500                                                                          
049600     IF RESP-IDMSG-ERROR = SPACE                                          
049700       PERFORM DB2-DCL-OPN-T01ASNS-CRS-4                                  
049800       PERFORM DB2-FETCH-T01ASNS-CRS-4                                    
049900       MOVE 1    TO WS-IX                                                 
049910       MOVE ZERO TO WS-IX2                                                
050000       PERFORM UNTIL LINES-MISSING                                        
050100         PERFORM DB2-COUNT-T01ASNS-4A                                     
050200         IF WS-COUNT-FAD-2A > ZERO                                        
050300           PERFORM DB2-DCL-OPN-T01ASNS-CRS-4A                             
050400           PERFORM DB2-FETCH-T01ASNS-CRS-4A                               
050500           PERFORM UNTIL LINES-MISSING                                    
050600             PERFORM S03-MOVE-TO-RESPOND                                  
050700             PERFORM DB2-FETCH-T01ASNS-CRS-4A                             
050800           END-PERFORM                                                    
050900           PERFORM DB2-CLOSE-T01ASNS-CRS-4A                               
051000         ELSE                                                             
051100           PERFORM DB2-COUNT-T01BURE-4A                                   
051200           IF WS-COUNT-FAD-2A > ZERO                                      
051300             PERFORM DB2-DCL-OPN-T01BURE-CRS-4A                           
051400             PERFORM DB2-FETCH-T01BURE-CRS-4A                             
051500             PERFORM UNTIL LINES-MISSING                                  
051600               PERFORM S03-MOVE-TO-RESPOND                                
051700               PERFORM DB2-FETCH-T01BURE-CRS-4A                           
051800             END-PERFORM                                                  
051900             PERFORM DB2-CLOSE-T01BURE-CRS-4A                             
052000           END-IF                                                         
052100         END-IF                                                           
052200         PERFORM DB2-FETCH-T01ASNS-CRS-4                                  
052300       END-PERFORM                                                        
052400       PERFORM DB2-CLOSE-T01ASNS-CRS-4                                    
052500                                                                          
052600       IF WS-IX2 = ZERO                                                   
052700         PERFORM DB2-DCL-OPN-T01BURE-CRS-4                                
052800         PERFORM DB2-FETCH-T01BURE-CRS-4                                  
052900         PERFORM UNTIL LINES-MISSING                                      
053000           PERFORM S03-MOVE-TO-RESPOND                                    
053100           PERFORM DB2-FETCH-T01BURE-CRS-4                                
053200         END-PERFORM                                                      
053300         PERFORM DB2-CLOSE-T01BURE-CRS-4                                  
053400       END-IF                                                             
053500     END-IF                                                               
053600     .                                                                    
053610                                                                          
053700*** - HANDLE KEY PARTNER TYPE                                             
053800 FAE-SEARCH-TYPE SECTION.                                                 
053900     PERFORM DB2-COUNT-CRS-FAE                                            
054000     IF WS-COUNTER-FAE = ZERO                                             
054100       PERFORM DB2-COUNT-CRS-FAE-2                                        
054200       IF WS-COUNTER-FAE-2 = ZERO                                         
054300         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
054400       ELSE                                                               
054500         IF WS-COUNTER-FAE-2 > WS-MAX-LINES                               
054600           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
054700         END-IF                                                           
054800       END-IF                                                             
054900     ELSE                                                                 
055000       IF WS-COUNTER-FAE > WS-MAX-LINES                                   
055100         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
055200       END-IF                                                             
055300     END-IF                                                               
055400                                                                          
055500     IF RESP-IDMSG-ERROR = SPACE                                          
055600       PERFORM DB2-DCL-OPN-T01ASNS-CRS-5                                  
055700       PERFORM DB2-FETCH-T01ASNS-CRS-5                                    
055800       MOVE 1    TO WS-IX                                                 
055810       MOVE ZERO TO WS-IX2                                                
055900       PERFORM UNTIL LINES-MISSING                                        
056000         PERFORM DB2-COUNT-T01ASNS-5A                                     
056100         IF WS-COUNT-FAE-2A > ZERO                                        
056200           PERFORM DB2-DCL-OPN-T01ASNS-CRS-5A                             
056300           PERFORM DB2-FETCH-T01ASNS-CRS-5A                               
056400           PERFORM UNTIL LINES-MISSING                                    
056500             PERFORM S03-MOVE-TO-RESPOND                                  
056600             PERFORM DB2-FETCH-T01ASNS-CRS-5A                             
056700           END-PERFORM                                                    
056800           PERFORM DB2-CLOSE-T01ASNS-CRS-5A                               
056900         ELSE                                                             
057000           PERFORM DB2-COUNT-T01BURE-5A                                   
057100           IF WS-COUNT-FAE-2A > ZERO                                      
057200             PERFORM DB2-DCL-OPN-T01BURE-CRS-5A                           
057300             PERFORM DB2-FETCH-T01BURE-CRS-5A                             
057400             PERFORM UNTIL LINES-MISSING                                  
057500               PERFORM S03-MOVE-TO-RESPOND                                
057600               PERFORM DB2-FETCH-T01BURE-CRS-5A                           
057700             END-PERFORM                                                  
057800             PERFORM DB2-CLOSE-T01BURE-CRS-5A                             
057900           END-IF                                                         
058000         END-IF                                                           
058100         PERFORM DB2-FETCH-T01ASNS-CRS-5                                  
058200       END-PERFORM                                                        
058300       PERFORM DB2-CLOSE-T01ASNS-CRS-5                                    
058400                                                                          
058500       IF WS-IX2 = ZERO                                                   
058600         PERFORM DB2-DCL-OPN-T01BURE-CRS-5                                
058700         PERFORM DB2-FETCH-T01BURE-CRS-5                                  
058800         PERFORM UNTIL LINES-MISSING                                      
058900           PERFORM S03-MOVE-TO-RESPOND                                    
059000           PERFORM DB2-FETCH-T01BURE-CRS-5                                
059100         END-PERFORM                                                      
059200         PERFORM DB2-CLOSE-T01BURE-CRS-5                                  
059300       END-IF                                                             
059400     END-IF                                                               
059500     .                                                                    
059510                                                                          
059520*** - HANDLE KEY PARTNER TYPE                                             
059530 FAF-SEARCH-EMPTY SECTION.                                                
059540     PERFORM DB2-COUNT-CRS-FAF                                            
059550     IF WS-COUNTER-FAF = ZERO                                             
059560       PERFORM DB2-COUNT-CRS-FAF-2                                        
059570       IF WS-COUNTER-FAF-2 = ZERO                                         
059580         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
059590       ELSE                                                               
059600         IF WS-COUNTER-FAF-2 > WS-MAX-LINES                               
059700           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
059800         END-IF                                                           
059900       END-IF                                                             
059910     ELSE                                                                 
059920       IF WS-COUNTER-FAF > WS-MAX-LINES                                   
059921         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                      
059922       END-IF                                                             
059923     END-IF                                                               
059924                                                                          
059925     IF RESP-IDMSG-ERROR = SPACE                                          
059927       PERFORM DB2-DCL-OPN-T01ASNS-CRS-6A                                 
059928       PERFORM DB2-FETCH-T01ASNS-CRS-6A                                   
059929       MOVE 1    TO WS-IX                                                 
059930       MOVE ZERO TO WS-IX2                                                
059934       PERFORM UNTIL LINES-MISSING                                        
059935         PERFORM S03-MOVE-TO-RESPOND                                      
059936         PERFORM DB2-FETCH-T01ASNS-CRS-6A                                 
059937       END-PERFORM                                                        
059938       PERFORM DB2-CLOSE-T01ASNS-CRS-6A                                   
059940                                                                          
059942       IF WS-IX2 = ZERO                                                   
059943         PERFORM DB2-DCL-OPN-T01BURE-CRS-6A                               
059944         PERFORM DB2-FETCH-T01BURE-CRS-6A                                 
059945         PERFORM UNTIL LINES-MISSING                                      
059946           PERFORM S03-MOVE-TO-RESPOND                                    
059947           PERFORM DB2-FETCH-T01BURE-CRS-6A                               
059948         END-PERFORM                                                      
059949         PERFORM DB2-CLOSE-T01BURE-CRS-6A                                 
059950       END-IF                                                             
059960     END-IF                                                               
059991     .                                                                    
059992                                                                          
059993*   --- DISPATCHER SECTION START                                          
059994 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
059995     MOVE 'GETARG'             TO SUB-KDFUNC                              
060000     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
060100     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
060200                                                                          
060300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
060400                                                                          
060500     IF SUB-KDRC > 0                                                      
060600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
060700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
060800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
060900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
061000     END-IF                                                               
061100     .                                                                    
061110                                                                          
061200 S02-RETURN-RESPONSE SECTION.                                             
061400     MOVE 'RETURN'             TO SUB-KDFUNC                              
061500                                                                          
061600     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
061700                              - ((WS-MAX-LINES - WS-IX2)                  
061800                              * LENGTH OF RESP-TABELLRAD)                 
061900                                                                          
062000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
062100                                                                          
062200     IF SUB-KDRC > 0                                                      
062300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
062400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
062500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
062600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
062700     END-IF                                                               
062800     .                                                                    
062810                                                                          
062900*** - MOVE TO RESPOND AND COUNT NUMBER OF HITS.                           
063000 S03-MOVE-TO-RESPOND SECTION.                                             
063100     PERFORM S04-SELECT-SEARCH-RESPOND                                    
063200     IF LINES-FOUND                                                       
063300       PERFORM DB2-SELECT-T01ASNS-TAB                                     
063400       IF LINES-MISSING                                                   
063500         MOVE SPACE      TO MAP-DAREGDAT-LINE                             
063600         MOVE SPACE      TO MAP-IDLANDX3-LINE                             
063700         MOVE ZERO       TO MAP-IDLOPNR-LINE                              
063900         MOVE WS-NOT-REG TO MAP-IDUSER-LINE                               
064000       ELSE                                                               
064100         CONTINUE                                                         
064200       END-IF                                                             
064400       MOVE MAP-IDLOPNR-LINE  TO MAP-IDLOPNR-LINE-2                       
064500       IF  MAP-KDFINDOC-LINE  = RESP-KDFINDOC-LINE(WS-IX)                 
064600       AND MAP-KDPARTTY-LINE  = RESP-KDPARTTY-LINE(WS-IX)                 
064700       AND MAP-KDPARTGR-LINE  = RESP-KDPARTGR-LINE(WS-IX)                 
064800       AND MAP-IDLANDX3-LINE  = RESP-IDLANDX3-LINE(WS-IX)                 
064810       AND MAP-IDLOPNR-LINE-2 = RESP-IDLOPNR-LINE(WS-IX)                  
065100         CONTINUE                                                         
065200       ELSE                                                               
065300         ADD 1 TO WS-IX2                                                  
065500         MOVE MAP-IDLOPNR-LINE  TO MAP-IDLOPNR-LINE-2                     
065600         MOVE MAP-KDFINDOC-LINE-2 TO RESP-KDFINDOC-LINE(WS-IX)            
065700         MOVE MAP-KDPARTTY-LINE-2 TO RESP-KDPARTTY-LINE(WS-IX)            
065800         MOVE MAP-KDPARTGR-LINE-2 TO RESP-KDPARTGR-LINE(WS-IX)            
065900         MOVE MAP-IDLANDX3-LINE   TO RESP-IDLANDX3-LINE(WS-IX)            
066100         MOVE MAP-IDLOPNR-LINE-2  TO RESP-IDLOPNR-LINE(WS-IX)             
066200         MOVE MAP-DAREGDAT-LINE   TO RESP-DAREGDAT-LINE(WS-IX)            
066300         MOVE MAP-IDUSER-LINE     TO RESP-IDUSER-LINE(WS-IX)              
066310         ADD 1 TO WS-IX                                                   
066400       END-IF                                                             
066500     ELSE                                                                 
066600       CONTINUE                                                           
066700     END-IF                                                               
066800     IF WS-IX2 = ZERO                                                     
066900       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300                                                                          
067400 S04-SELECT-SEARCH-RESPOND SECTION.                                       
067500     IF WS-COUNTER-FAA > ZERO OR WS-COUNTER-FAA-2 > 0                     
067600       PERFORM DB2-SELECT-T01ASNS-FAA                                     
067700       IF LINES-FOUND                                                     
067800         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
067900         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
068000         MOVE MAP-KDPARTGR-LINE TO MAP-KDPARTGR-LINE-2                    
068100       ELSE                                                               
068200         PERFORM DB2-SELECT-T01BURE-FAA                                   
068300         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
068400         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
068500         MOVE MAP-KDPARTGR-LINE TO MAP-KDPARTGR-LINE-2                    
068600         MOVE ZERO              TO MAP-IDLOPNR-LINE                       
068700         MOVE SPACE             TO MAP-DAREGDAT-LINE                      
068800         MOVE SPACE             TO MAP-IDUSER-LINE                        
068900       END-IF                                                             
069000     END-IF                                                               
069100                                                                          
069200     IF WS-COUNTER-FAB > ZERO OR WS-COUNTER-FAB-2 > 0                     
069300       PERFORM DB2-DCL-OPN-T01ASNS-FAB-1                                  
069400       PERFORM DB2-FETCH-T01ASNS-FAB-1                                    
069500       IF LINES-FOUND                                                     
069600         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
069700         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
069800         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
069900       ELSE                                                               
070000         PERFORM DB2-SELECT-T01BURE-FAB                                   
070100         MOVE MAP-KDFINDOC-LINE TO MAP-KDFINDOC-LINE-2                    
070200         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
070300         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
070400         MOVE ZERO              TO MAP-IDLOPNR-LINE                       
070500         MOVE SPACE             TO MAP-DAREGDAT-LINE                      
070600         MOVE SPACE             TO MAP-IDUSER-LINE                        
070700       END-IF                                                             
070800       PERFORM DB2-CLOSE-T01ASNS-FAB-1                                    
070900     END-IF                                                               
071000                                                                          
071100     IF WS-COUNTER-FAC > ZERO OR WS-COUNTER-FAC-2 > 0                     
071200       PERFORM DB2-DCL-OPN-T01ASNS-FAC-1                                  
071300       PERFORM DB2-FETCH-T01ASNS-FAC-1                                    
071400       IF LINES-FOUND                                                     
071500         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
071600         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
071700         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
071800       ELSE                                                               
071900         PERFORM DB2-SELECT-T01BURE-FAC                                   
072000         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
072100         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
072200         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
072300         MOVE ZERO              TO MAP-IDLOPNR-LINE                       
072400         MOVE SPACE             TO MAP-DAREGDAT-LINE                      
072500         MOVE SPACE             TO MAP-IDUSER-LINE                        
072600       END-IF                                                             
072700       PERFORM DB2-CLOSE-T01ASNS-FAC-1                                    
072800     END-IF                                                               
072900                                                                          
073000     IF WS-COUNTER-FAD > ZERO OR WS-COUNTER-FAD-2 > 0                     
073100       PERFORM DB2-DCL-OPN-T01ASNS-FAD-1                                  
073200       PERFORM DB2-FETCH-T01ASNS-FAD-1                                    
073300       IF LINES-FOUND                                                     
073400         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
073500         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
073600         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
073700       ELSE                                                               
073800         PERFORM DB2-SELECT-T01BURE-FAD                                   
073900         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
074000         MOVE MAP-KDPARTTY-LINE-2 TO MAP-KDPARTTY-LINE                    
074100         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
074200         MOVE ZERO              TO MAP-IDLOPNR-LINE                       
074300         MOVE SPACE             TO MAP-DAREGDAT-LINE                      
074400         MOVE SPACE             TO MAP-IDUSER-LINE                        
074500       END-IF                                                             
074600       PERFORM DB2-CLOSE-T01ASNS-FAD-1                                    
074700     END-IF                                                               
074800                                                                          
074900     IF WS-COUNTER-FAE > ZERO OR WS-COUNTER-FAE-2 > 0                     
075000       PERFORM DB2-DCL-OPN-T01ASNS-FAE-1                                  
075100       PERFORM DB2-FETCH-T01ASNS-FAE-1                                    
075200       IF LINES-FOUND                                                     
075300         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
075400         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
075500         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
075600       ELSE                                                               
075700         PERFORM DB2-SELECT-T01BURE-FAE                                   
075800         MOVE MAP-KDFINDOC-LINE-2 TO MAP-KDFINDOC-LINE                    
075900         MOVE MAP-KDPARTTY-LINE TO MAP-KDPARTTY-LINE-2                    
076000         MOVE MAP-KDPARTGR-LINE-2 TO MAP-KDPARTGR-LINE                    
076100         MOVE ZERO              TO MAP-IDLOPNR-LINE                       
076200         MOVE SPACE             TO MAP-DAREGDAT-LINE                      
076300         MOVE SPACE             TO MAP-IDUSER-LINE                        
076400       END-IF                                                             
076500       PERFORM DB2-CLOSE-T01ASNS-FAE-1                                    
076600     END-IF                                                               
076610                                                                          
076700     IF WS-COUNTER-FAF > ZERO OR WS-COUNTER-FAF-2 > 0                     
076710       PERFORM DB2-DCL-OPN-T01ASNS-FAF-1                                  
076720       PERFORM DB2-FETCH-T01ASNS-FAF-1                                    
076730       IF LINES-FOUND                                                     
076740         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
076750         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
076760         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
076770       ELSE                                                               
076780         PERFORM DB2-SELECT-T01BURE-FAF                                   
076790         MOVE MAP-KDFINDOC-LINE   TO MAP-KDFINDOC-LINE-2                  
076791         MOVE MAP-KDPARTTY-LINE   TO MAP-KDPARTTY-LINE-2                  
076792         MOVE MAP-KDPARTGR-LINE   TO MAP-KDPARTGR-LINE-2                  
076797         MOVE ZERO              TO MAP-IDLOPNR-LINE                       
076798         MOVE SPACE             TO MAP-DAREGDAT-LINE                      
076799         MOVE SPACE             TO MAP-IDUSER-LINE                        
076800       END-IF                                                             
076801       PERFORM DB2-CLOSE-T01ASNS-FAF-1                                    
076802     END-IF                                                               
076803     .                                                                    
076804     EJECT                                                                
076810                                                                          
076900*   --- DB2 SECTIONS                                                      
077000 DB2-SELECT-T01LSEL-TAB SECTION.                                          
077200     MOVE 000100 TO GOOD-SQLCODECODES                                     
077300                                                                          
077400     EXEC SQL                                                             
077500           SELECT BELEGRAD_1                                              
077600                                                                          
077700           INTO  :T01LSEL-BELEGRAD-1                                      
077800                                                                          
077900           FROM   T01LSEL                                                 
078000                                                                          
078100           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
078200           AND    KDSTATUS = :WS-CURRENT                                  
078300     END-EXEC                                                             
078400                                                                          
078500     MOVE SQLCODE TO SQLCODE-WS                                           
078600     PERFORM DB2-STATUS-CHECK                                             
078700     .                                                                    
078800                                                                          
078900 DB2-SELECT-T01ASNS-TAB SECTION.                                          
079000     MOVE 000100 TO GOOD-SQLCODECODES                                     
079100                                                                          
079200     EXEC SQL                                                             
079300           SELECT KDFINDOC                                                
079400                                                                          
079500           INTO  :T01ASNS-KDFINDOC                                        
079600                                                                          
079700           FROM   T01ASNS                                                 
079800                                                                          
079900           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
080000           AND    KDFINDOC = :MAP-KDFINDOC-LINE-2                         
080100           AND    KDPARTTY = :MAP-KDPARTTY-LINE-2                         
080200           AND    KDPARTGR = :MAP-KDPARTGR-LINE-2                         
080300           AND    IDLANDX3 = :MAP-IDLANDX3-LINE                           
080500           AND    IDLOPNR  = :MAP-IDLOPNR-LINE                            
080600     END-EXEC                                                             
080700                                                                          
080800     MOVE SQLCODE TO SQLCODE-WS                                           
080900     PERFORM DB2-STATUS-CHECK                                             
081000     .                                                                    
081010                                                                          
081100 DB2-SELECT-T01ASNS-FAA SECTION.                                          
081200     MOVE 000100 TO GOOD-SQLCODECODES                                     
081300                                                                          
081400     EXEC SQL                                                             
081500           SELECT KDFINDOC                                                
081600                                                                          
081700           INTO  :T01ASNS-KDFINDOC                                        
081800                                                                          
081900           FROM   T01ASNS                                                 
082000                                                                          
082100           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
082200           AND    KDFINDOC = :MAP-KDFINDOC-LINE                           
082300           AND    KDPARTTY = :MAP-KDPARTTY-LINE                           
082400           AND    KDPARTGR = :MAP-KDPARTGR-LINE                           
082500           AND    IDLANDX3 = :MAP-IDLANDX3-LINE                           
082700           AND    IDLOPNR  = :MAP-IDLOPNR-LINE                            
082800     END-EXEC                                                             
082900                                                                          
083000     MOVE SQLCODE TO SQLCODE-WS                                           
083100     PERFORM DB2-STATUS-CHECK                                             
083200     .                                                                    
083210                                                                          
083300 DB2-SELECT-T01BURE-FAA SECTION.                                          
083400     MOVE 000100 TO GOOD-SQLCODECODES                                     
083500                                                                          
083600     EXEC SQL                                                             
083700           SELECT KDFINDOC                                                
083800                                                                          
083900           INTO  :T01ASNS-KDFINDOC                                        
084000                                                                          
084100           FROM    T01BURE                                                
084200                                                                          
084300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
084400                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
084500                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
084600                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
084700                AND DADELDAT = '00000000'                                 
084800                AND KDSTATUS = :WS-CURRENT                                
084900     END-EXEC                                                             
085000                                                                          
085100     MOVE SQLCODE TO SQLCODE-WS                                           
085200     PERFORM DB2-STATUS-CHECK                                             
085300     .                                                                    
085400     EJECT                                                                
085500                                                                          
085600 DB2-DCL-OPN-T01ASNS-FAB-1 SECTION.                                       
085800     MOVE 000100 TO GOOD-SQLCODECODES                                     
085900                                                                          
086000     EXEC SQL                                                             
086100         DECLARE T01ASNS-FAB-1 CURSOR WITH HOLD FOR                       
086200                                                                          
086300           SELECT  KDFINDOC                                               
086400                                                                          
086500           FROM    T01ASNS                                                
086600                                                                          
086700           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
086800                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
086900                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
087000                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
087100                AND IDLOPNR  = :MAP-IDLOPNR-LINE                          
087200                AND DAREGDAT = :MAP-DAREGDAT-LINE                         
087300                AND IDUSER   = :MAP-IDUSER-LINE                           
087400                                                                          
087500     END-EXEC                                                             
087600                                                                          
087700     MOVE 000100  TO GOOD-SQLCODECODES                                    
087800                                                                          
087900     EXEC SQL                                                             
088000       OPEN T01ASNS-FAB-1                                                 
088100     END-EXEC                                                             
088200                                                                          
088300     MOVE SQLCODE TO SQLCODE-WS                                           
088400     PERFORM DB2-STATUS-CHECK                                             
088500     .                                                                    
088510                                                                          
088600 DB2-FETCH-T01ASNS-FAB-1 SECTION.                                         
088800     MOVE 000100  TO GOOD-SQLCODECODES                                    
088900                                                                          
089000     EXEC SQL                                                             
089100         FETCH T01ASNS-FAB-1                                              
089200                                                                          
089300         INTO :T01ASNS-KDFINDOC                                           
089400     END-EXEC                                                             
089500                                                                          
089600     MOVE SQLCODE TO SQLCODE-WS                                           
089700     PERFORM DB2-STATUS-CHECK                                             
089800     .                                                                    
089810                                                                          
089900 DB2-CLOSE-T01ASNS-FAB-1 SECTION.                                         
090100     EXEC SQL                                                             
090200       CLOSE T01ASNS-FAB-1                                                
090300     END-EXEC                                                             
090400     .                                                                    
090410                                                                          
090500 DB2-SELECT-T01BURE-FAB SECTION.                                          
090600     MOVE 000100 TO GOOD-SQLCODECODES                                     
090700                                                                          
090800     EXEC SQL                                                             
090900           SELECT  KDFINDOC                                               
091000                                                                          
091100           INTO    :T01ASNS-KDFINDOC                                      
091200                                                                          
091300           FROM    T01BURE                                                
091400                                                                          
091500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
091600                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
091700                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
091800                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
091900                AND DADELDAT = '00000000'                                 
092000                AND KDSTATUS = :WS-CURRENT                                
092100     END-EXEC                                                             
092200                                                                          
092300     MOVE SQLCODE TO SQLCODE-WS                                           
092400     PERFORM DB2-STATUS-CHECK                                             
092500     .                                                                    
092600     EJECT                                                                
092700                                                                          
092800 DB2-DCL-OPN-T01ASNS-FAC-1 SECTION.                                       
093000     MOVE 000100 TO GOOD-SQLCODECODES                                     
093100                                                                          
093200     EXEC SQL                                                             
093300         DECLARE T01ASNS-FAC-1 CURSOR WITH HOLD FOR                       
093400                                                                          
093500           SELECT  KDFINDOC                                               
093600                                                                          
093700           FROM    T01ASNS                                                
093800                                                                          
093900           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
094000                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
094100                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
094200                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
094300                AND IDLOPNR  = :MAP-IDLOPNR-LINE                          
094400                AND DAREGDAT = :MAP-DAREGDAT-LINE                         
094500                AND IDUSER   = :MAP-IDUSER-LINE                           
094600                                                                          
094700     END-EXEC                                                             
094800                                                                          
094900     MOVE 000100  TO GOOD-SQLCODECODES                                    
095000                                                                          
095100     EXEC SQL                                                             
095200       OPEN T01ASNS-FAC-1                                                 
095300     END-EXEC                                                             
095400                                                                          
095500     MOVE SQLCODE TO SQLCODE-WS                                           
095600     PERFORM DB2-STATUS-CHECK                                             
095700     .                                                                    
095710                                                                          
095800 DB2-FETCH-T01ASNS-FAC-1 SECTION.                                         
096000     MOVE 000100  TO GOOD-SQLCODECODES                                    
096100                                                                          
096200     EXEC SQL                                                             
096300         FETCH T01ASNS-FAC-1                                              
096400                                                                          
096500         INTO :T01ASNS-KDFINDOC                                           
096600     END-EXEC                                                             
096700                                                                          
096800     MOVE SQLCODE TO SQLCODE-WS                                           
096900     PERFORM DB2-STATUS-CHECK                                             
097000     .                                                                    
097010                                                                          
097100 DB2-CLOSE-T01ASNS-FAC-1 SECTION.                                         
097300     EXEC SQL                                                             
097400       CLOSE T01ASNS-FAC-1                                                
097500     END-EXEC                                                             
097600     .                                                                    
097700                                                                          
097800 DB2-SELECT-T01BURE-FAC SECTION.                                          
097900     MOVE 000100 TO GOOD-SQLCODECODES                                     
098000                                                                          
098100     EXEC SQL                                                             
098200           SELECT  KDFINDOC                                               
098300                                                                          
098400           INTO    :T01ASNS-KDFINDOC                                      
098500                                                                          
098600           FROM    T01BURE                                                
098700                                                                          
098800           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
098900                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
099000                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
099100                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
099200                AND DADELDAT = '00000000'                                 
099300                AND KDSTATUS = :WS-CURRENT                                
099400     END-EXEC                                                             
099500                                                                          
099600     MOVE SQLCODE TO SQLCODE-WS                                           
099700     PERFORM DB2-STATUS-CHECK                                             
099800     .                                                                    
099900     EJECT                                                                
100000                                                                          
100100 DB2-DCL-OPN-T01ASNS-FAD-1 SECTION.                                       
100300     MOVE 000100 TO GOOD-SQLCODECODES                                     
100400                                                                          
100500     EXEC SQL                                                             
100600         DECLARE T01ASNS-FAD-1 CURSOR WITH HOLD FOR                       
100700                                                                          
100800           SELECT  KDFINDOC                                               
100900                                                                          
101000           FROM    T01ASNS                                                
101100                                                                          
101200           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
101300                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
101400                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
101500                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
101600                AND IDLOPNR  = :MAP-IDLOPNR-LINE                          
101700                AND DAREGDAT = :MAP-DAREGDAT-LINE                         
101800                AND IDUSER   = :MAP-IDUSER-LINE                           
101900                                                                          
102000     END-EXEC                                                             
102100                                                                          
102200     MOVE 000100  TO GOOD-SQLCODECODES                                    
102300                                                                          
102400     EXEC SQL                                                             
102500       OPEN T01ASNS-FAD-1                                                 
102600     END-EXEC                                                             
102700                                                                          
102800     MOVE SQLCODE TO SQLCODE-WS                                           
102900     PERFORM DB2-STATUS-CHECK                                             
103000     .                                                                    
103010                                                                          
103100 DB2-FETCH-T01ASNS-FAD-1 SECTION.                                         
103300     MOVE 000100  TO GOOD-SQLCODECODES                                    
103400                                                                          
103500     EXEC SQL                                                             
103600         FETCH T01ASNS-FAD-1                                              
103700                                                                          
103800         INTO :T01ASNS-KDFINDOC                                           
103900     END-EXEC                                                             
104000                                                                          
104100     MOVE SQLCODE TO SQLCODE-WS                                           
104200     PERFORM DB2-STATUS-CHECK                                             
104300     .                                                                    
104310                                                                          
104400 DB2-CLOSE-T01ASNS-FAD-1 SECTION.                                         
104600     EXEC SQL                                                             
104700       CLOSE T01ASNS-FAD-1                                                
104800     END-EXEC                                                             
104900     .                                                                    
105000                                                                          
105100 DB2-SELECT-T01BURE-FAD SECTION.                                          
105200     MOVE 000100 TO GOOD-SQLCODECODES                                     
105300                                                                          
105400     EXEC SQL                                                             
105500           SELECT  KDFINDOC                                               
105600                                                                          
105700           INTO    :T01ASNS-KDFINDOC                                      
105800                                                                          
105900           FROM    T01BURE                                                
106000                                                                          
106100           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
106200                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
106300                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
106400                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
106500                AND DADELDAT = '00000000'                                 
106600                AND KDSTATUS = :WS-CURRENT                                
106700     END-EXEC                                                             
106800                                                                          
106900     MOVE SQLCODE TO SQLCODE-WS                                           
107000     PERFORM DB2-STATUS-CHECK                                             
107100     .                                                                    
107200     EJECT                                                                
107300                                                                          
107400 DB2-DCL-OPN-T01ASNS-FAE-1 SECTION.                                       
107600     MOVE 000100 TO GOOD-SQLCODECODES                                     
107700                                                                          
107800     EXEC SQL                                                             
107900         DECLARE T01ASNS-FAE-1 CURSOR WITH HOLD FOR                       
108000                                                                          
108100           SELECT  KDFINDOC                                               
108200                                                                          
108300           FROM    T01ASNS                                                
108400                                                                          
108500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
108600                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
108700                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
108800                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
108900                AND IDLOPNR  = :MAP-IDLOPNR-LINE                          
109000                AND DAREGDAT = :MAP-DAREGDAT-LINE                         
109100                AND IDUSER   = :MAP-IDUSER-LINE                           
109200                                                                          
109300     END-EXEC                                                             
109400                                                                          
109500     MOVE 000100  TO GOOD-SQLCODECODES                                    
109600                                                                          
109700     EXEC SQL                                                             
109800       OPEN T01ASNS-FAE-1                                                 
109900     END-EXEC                                                             
110000                                                                          
110100     MOVE SQLCODE TO SQLCODE-WS                                           
110200     PERFORM DB2-STATUS-CHECK                                             
110300     .                                                                    
110310                                                                          
110400 DB2-FETCH-T01ASNS-FAE-1 SECTION.                                         
110600     MOVE 000100  TO GOOD-SQLCODECODES                                    
110700                                                                          
110800     EXEC SQL                                                             
110900         FETCH T01ASNS-FAE-1                                              
111000                                                                          
111100         INTO :T01ASNS-KDFINDOC                                           
111200     END-EXEC                                                             
111300                                                                          
111400     MOVE SQLCODE TO SQLCODE-WS                                           
111500     PERFORM DB2-STATUS-CHECK                                             
111600     .                                                                    
111610                                                                          
111700 DB2-CLOSE-T01ASNS-FAE-1 SECTION.                                         
111900     EXEC SQL                                                             
112000       CLOSE T01ASNS-FAE-1                                                
112100     END-EXEC                                                             
112200     .                                                                    
112210                                                                          
112300 DB2-SELECT-T01BURE-FAE SECTION.                                          
112400     MOVE 000100 TO GOOD-SQLCODECODES                                     
112500                                                                          
112600     EXEC SQL                                                             
112700           SELECT  KDFINDOC                                               
112800                                                                          
112900           INTO    :T01ASNS-KDFINDOC                                      
113000                                                                          
113100           FROM    T01BURE                                                
113200                                                                          
113300           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
113400                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
113500                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
113600                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
113700                AND DADELDAT = '00000000'                                 
113800                AND KDSTATUS = :WS-CURRENT                                
113900     END-EXEC                                                             
114000                                                                          
114100     MOVE SQLCODE TO SQLCODE-WS                                           
114200     PERFORM DB2-STATUS-CHECK                                             
114300     .                                                                    
114400     EJECT                                                                
114500                                                                          
114510 DB2-DCL-OPN-T01ASNS-FAF-1 SECTION.                                       
114520     MOVE 000100 TO GOOD-SQLCODECODES                                     
114530                                                                          
114540     EXEC SQL                                                             
114550         DECLARE T01ASNS-FAF-1 CURSOR WITH HOLD FOR                       
114560                                                                          
114570           SELECT  KDFINDOC                                               
114580                                                                          
114590           FROM    T01ASNS                                                
114591                                                                          
114592           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
114593                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
114594                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
114595                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
114596                AND IDLOPNR  = :MAP-IDLOPNR-LINE                          
114597                AND DAREGDAT = :MAP-DAREGDAT-LINE                         
114598                AND IDUSER   = :MAP-IDUSER-LINE                           
114599                                                                          
114600     END-EXEC                                                             
114601                                                                          
114602     MOVE 000100  TO GOOD-SQLCODECODES                                    
114603                                                                          
114604     EXEC SQL                                                             
114605       OPEN T01ASNS-FAF-1                                                 
114606     END-EXEC                                                             
114607                                                                          
114608     MOVE SQLCODE TO SQLCODE-WS                                           
114609     PERFORM DB2-STATUS-CHECK                                             
114610     .                                                                    
114611                                                                          
114612 DB2-FETCH-T01ASNS-FAF-1 SECTION.                                         
114613     MOVE 000100  TO GOOD-SQLCODECODES                                    
114614                                                                          
114615     EXEC SQL                                                             
114616         FETCH T01ASNS-FAF-1                                              
114617                                                                          
114618         INTO :T01ASNS-KDFINDOC                                           
114619     END-EXEC                                                             
114620                                                                          
114621     MOVE SQLCODE TO SQLCODE-WS                                           
114622     PERFORM DB2-STATUS-CHECK                                             
114623     .                                                                    
114624                                                                          
114625 DB2-CLOSE-T01ASNS-FAF-1 SECTION.                                         
114626     EXEC SQL                                                             
114627       CLOSE T01ASNS-FAF-1                                                
114628     END-EXEC                                                             
114629     .                                                                    
114630                                                                          
114631 DB2-SELECT-T01BURE-FAF SECTION.                                          
114632     MOVE 000100 TO GOOD-SQLCODECODES                                     
114633                                                                          
114634     EXEC SQL                                                             
114635           SELECT  KDFINDOC                                               
114636                                                                          
114637           INTO    :T01ASNS-KDFINDOC                                      
114638                                                                          
114639           FROM    T01BURE                                                
114640                                                                          
114641           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
114642                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
114643                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
114644                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
114645                AND DADELDAT = '00000000'                                 
114646                AND KDSTATUS = :WS-CURRENT                                
114647     END-EXEC                                                             
114648                                                                          
114649     MOVE SQLCODE TO SQLCODE-WS                                           
114650     PERFORM DB2-STATUS-CHECK                                             
114651     .                                                                    
114652     EJECT                                                                
114653                                                                          
118900 DB2-DCL-OPN-T01ASNS-CRS-1 SECTION.                                       
119100     MOVE 000100 TO GOOD-SQLCODECODES                                     
119200                                                                          
119300     EXEC SQL                                                             
119400         DECLARE T01ASNS-CRS-1 CURSOR WITH HOLD FOR                       
119500                                                                          
119600           SELECT  KDFINDOC                                               
119700                 , KDPARTTY                                               
119800                 , KDPARTGR                                               
119900                 , IDLANDX3                                               
120100                 , IDLOPNR                                                
120200                 , DAREGDAT                                               
120300                 , IDUSER                                                 
120400                                                                          
120500           FROM    T01ASNS                                                
120600                                                                          
120700           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
120800           AND     KDFINDOC = :REQU-KDFINDOC-KEY                          
120900           AND     KDPARTTY = :REQU-KDPARTTY-KEY                          
121000           AND     KDPARTGR = :REQU-KDPARTGR-KEY                          
121100                                                                          
121200           ORDER BY KDFINDOC    DESC                                      
121400                  , KDPARTTY                                              
121500                  , KDPARTGR                                              
121600                  , IDLANDX3                                              
121700                  , IDLOPNR                                               
121800     END-EXEC                                                             
121900                                                                          
122000     MOVE 000100  TO GOOD-SQLCODECODES                                    
122100                                                                          
122200     EXEC SQL                                                             
122300       OPEN T01ASNS-CRS-1                                                 
122400     END-EXEC                                                             
122500                                                                          
122600     MOVE SQLCODE TO SQLCODE-WS                                           
122700     PERFORM DB2-STATUS-CHECK                                             
122800     .                                                                    
122810                                                                          
122900 DB2-FETCH-T01ASNS-CRS-1 SECTION.                                         
123100     MOVE 000100  TO GOOD-SQLCODECODES                                    
123200                                                                          
123300     EXEC SQL                                                             
123400         FETCH T01ASNS-CRS-1                                              
123500                                                                          
123600         INTO :MAP-KDFINDOC-LINE                                          
123700            , :MAP-KDPARTTY-LINE                                          
123800            , :MAP-KDPARTGR-LINE                                          
123900            , :MAP-IDLANDX3-LINE                                          
124100            , :MAP-IDLOPNR-LINE                                           
124200            , :MAP-DAREGDAT-LINE                                          
124300            , :MAP-IDUSER-LINE                                            
124400     END-EXEC                                                             
124500                                                                          
124600     MOVE SQLCODE TO SQLCODE-WS                                           
124700     PERFORM DB2-STATUS-CHECK                                             
124800     .                                                                    
124810                                                                          
124900 DB2-CLOSE-T01ASNS-CRS-1 SECTION.                                         
125100     EXEC SQL                                                             
125200       CLOSE T01ASNS-CRS-1                                                
125300     END-EXEC                                                             
125400     .                                                                    
125410                                                                          
125500 DB2-DCL-OPN-T01BURE-CRS-1 SECTION.                                       
125600     MOVE 000100 TO GOOD-SQLCODECODES                                     
125700                                                                          
125800     EXEC SQL                                                             
125900         DECLARE T01BURE-CRS-1 CURSOR WITH HOLD FOR                       
126000                                                                          
126100           SELECT  KDFINDOC                                               
126200                 , KDPARTTY                                               
126300                 , KDPARTGR                                               
126400                                                                          
126500           FROM    T01BURE                                                
126600                                                                          
126700           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
126800                AND DADELDAT = '00000000'                                 
126900                AND KDSTATUS = :WS-CURRENT                                
127000                AND KDFINDOC = :REQU-KDFINDOC-KEY                         
127100                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
127200                AND KDPARTGR = :REQU-KDPARTGR-KEY                         
127300                                                                          
127400           ORDER BY IDLEGSEL                                              
127500                  , KDFINDOC                                              
127600                  , KDPARTTY                                              
127700                  , KDPARTGR                                              
127800     END-EXEC                                                             
127900                                                                          
128000     MOVE 000100  TO GOOD-SQLCODECODES                                    
128100                                                                          
128200     EXEC SQL                                                             
128300        OPEN T01BURE-CRS-1                                                
128400     END-EXEC                                                             
128500                                                                          
128600     MOVE SQLCODE TO SQLCODE-WS                                           
128700     PERFORM DB2-STATUS-CHECK                                             
128800     .                                                                    
128900     EJECT                                                                
129000                                                                          
129100 DB2-FETCH-T01BURE-CRS-1 SECTION.                                         
129200     MOVE 000100  TO GOOD-SQLCODECODES                                    
129300                                                                          
129400     EXEC SQL                                                             
129500                                                                          
129600         FETCH T01BURE-CRS-1                                              
129700                                                                          
129800         INTO :MAP-KDFINDOC-LINE                                          
129900            , :MAP-KDPARTTY-LINE                                          
130000            , :MAP-KDPARTGR-LINE                                          
130100     END-EXEC                                                             
130200                                                                          
130300     MOVE SQLCODE TO SQLCODE-WS                                           
130400     PERFORM DB2-STATUS-CHECK                                             
130500     .                                                                    
130600     EJECT                                                                
130700                                                                          
130800 DB2-CLOSE-T01BURE-CRS-1 SECTION.                                         
130900     EXEC SQL                                                             
131000        CLOSE T01BURE-CRS-1                                               
131100     END-EXEC                                                             
131200     .                                                                    
131300     EJECT                                                                
131400                                                                          
131500* * * * * * * * * *   - CURSOR-FAB   * * * * * * * * * * * * * * *        
131600 DB2-COUNT-CRS-FAB SECTION.                                               
131800     MOVE 000100  TO GOOD-SQLCODECODES                                    
131900                                                                          
132000     EXEC SQL                                                             
132100          SELECT COUNT(*)                                                 
132200                                                                          
132300          INTO  :WS-COUNTER-FAB                                           
132400                                                                          
132500          FROM   T01BURE                                                  
132600                                                                          
132700          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
132800          AND    KDSTATUS = :WS-CURRENT                                   
132900          AND    DADELDAT = :WS-ACTIVE                                    
133000          AND    KDFINDOC = :REQU-KDFINDOC-KEY                            
133100          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
133200     END-EXEC                                                             
133300                                                                          
133400     MOVE SQLCODE TO SQLCODE-WS                                           
133500     PERFORM DB2-STATUS-CHECK                                             
133600     .                                                                    
133610                                                                          
133700 DB2-COUNT-CRS-FAB-2 SECTION.                                             
133900     MOVE 000100  TO GOOD-SQLCODECODES                                    
134000                                                                          
134100     EXEC SQL                                                             
134200          SELECT COUNT(*)                                                 
134300                                                                          
134400          INTO  :WS-COUNTER-FAB-2                                         
134500                                                                          
134600          FROM   T01ASNS                                                  
134700                                                                          
134800          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
134900          AND    KDFINDOC = :REQU-KDFINDOC-KEY                            
135000          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
135100     END-EXEC                                                             
135200                                                                          
135300     MOVE SQLCODE TO SQLCODE-WS                                           
135400     PERFORM DB2-STATUS-CHECK                                             
135500     .                                                                    
135510                                                                          
135600 DB2-COUNT-T01ASNS-2A SECTION.                                            
135800     MOVE 000100  TO GOOD-SQLCODECODES                                    
135900                                                                          
136000     EXEC SQL                                                             
136100          SELECT COUNT(*)                                                 
136200                                                                          
136300          INTO  :WS-COUNT-FAB-2A                                          
136400                                                                          
136500          FROM   T01ASNS                                                  
136600                                                                          
136700          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
136800          AND    KDFINDOC = :MAP-KDFINDOC-LINE                            
136900          AND    KDPARTTY = :MAP-KDPARTTY-LINE                            
137000          AND    KDPARTGR = :MAP-KDPARTGR-LINE-2                          
137100     END-EXEC                                                             
137200                                                                          
137300     MOVE SQLCODE TO SQLCODE-WS                                           
137400     PERFORM DB2-STATUS-CHECK                                             
137500     .                                                                    
137510                                                                          
137600 DB2-COUNT-T01BURE-2A SECTION.                                            
137800     MOVE 000100  TO GOOD-SQLCODECODES                                    
137900                                                                          
138000     EXEC SQL                                                             
138100          SELECT COUNT(*)                                                 
138200                                                                          
138300          INTO  :WS-COUNT-FAB-2A                                          
138400                                                                          
138500          FROM   T01BURE                                                  
138600                                                                          
138700          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
138800          AND    KDSTATUS = :WS-CURRENT                                   
138900          AND    DADELDAT = :WS-ACTIVE                                    
139000          AND    KDFINDOC = :REQU-KDFINDOC-KEY                            
139100          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
139200     END-EXEC                                                             
139300                                                                          
139400     MOVE SQLCODE TO SQLCODE-WS                                           
139500     PERFORM DB2-STATUS-CHECK                                             
139600     .                                                                    
139610                                                                          
139700 DB2-DCL-OPN-T01ASNS-CRS-2 SECTION.                                       
139900     MOVE 000100 TO GOOD-SQLCODECODES                                     
140000                                                                          
140100     EXEC SQL                                                             
140200         DECLARE T01ASNS-CRS-2 CURSOR WITH HOLD FOR                       
140300         SELECT  A.KDFINDOC                                               
140400              , A.KDPARTTY                                                
140500              , A.KDPARTGR                                                
140600                                                                          
140700         FROM   T01ASNS A                                                 
140800                                                                          
140900         WHERE   A.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
141000           AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                          
141100           AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                          
141200                                                                          
141300         UNION                                                            
141400                                                                          
141500         SELECT B.KDFINDOC                                                
141600              , B.KDPARTTY                                                
141700              , B.KDPARTGR                                                
141800                                                                          
141900         FROM   T01BURE B                                                 
142000                                                                          
142100         WHERE   B.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
142200            AND  B.KDSTATUS = :WS-CURRENT                                 
142300            AND  B.DADELDAT = :WS-ACTIVE                                  
142400            AND  B.KDFINDOC = :REQU-KDFINDOC-KEY                          
142500            AND  B.KDPARTTY = :REQU-KDPARTTY-KEY                          
142600            AND  NOT EXISTS                                               
142700                                                                          
142800             (SELECT *                                                    
142900              FROM   T01ASNS                                              
143000                    ,T01BURE                                              
143100              WHERE  T01ASNS.IDLEGSEL = T01BURE.IDLEGSEL AND              
143200                     T01ASNS.KDFINDOC = T01BURE.KDFINDOC AND              
143300                     T01ASNS.KDPARTTY = T01BURE.KDPARTTY AND              
143400                     T01ASNS.KDPARTGR = T01BURE.KDPARTGR)                 
143500     END-EXEC                                                             
143600                                                                          
143700     MOVE 000100  TO GOOD-SQLCODECODES                                    
143800                                                                          
143900     EXEC SQL                                                             
144000       OPEN T01ASNS-CRS-2                                                 
144100     END-EXEC                                                             
144200                                                                          
144300     MOVE SQLCODE TO SQLCODE-WS                                           
144400     PERFORM DB2-STATUS-CHECK                                             
144500     .                                                                    
144510                                                                          
144600 DB2-FETCH-T01ASNS-CRS-2 SECTION.                                         
144800     MOVE 000100  TO GOOD-SQLCODECODES                                    
144900                                                                          
145000     EXEC SQL                                                             
145100         FETCH T01ASNS-CRS-2                                              
145200                                                                          
145300         INTO :MAP-KDFINDOC-LINE                                          
145400            , :MAP-KDPARTTY-LINE                                          
145500            , :MAP-KDPARTGR-LINE-2                                        
145600     END-EXEC                                                             
145700                                                                          
145800     MOVE SQLCODE TO SQLCODE-WS                                           
145900     PERFORM DB2-STATUS-CHECK                                             
146000     .                                                                    
146010                                                                          
146100 DB2-CLOSE-T01ASNS-CRS-2 SECTION.                                         
146300     EXEC SQL                                                             
146400       CLOSE T01ASNS-CRS-2                                                
146500     END-EXEC                                                             
146600     .                                                                    
146610                                                                          
146700 DB2-DCL-OPN-T01ASNS-CRS-2A SECTION.                                      
146900     MOVE 000100 TO GOOD-SQLCODECODES                                     
147000                                                                          
147100     EXEC SQL                                                             
147200         DECLARE T01ASNS-CRS-2A CURSOR WITH HOLD FOR                      
147300                                                                          
147400           SELECT  KDFINDOC                                               
147500                 , KDPARTTY                                               
147600                 , KDPARTGR                                               
147700                 , IDLANDX3                                               
147900                 , IDLOPNR                                                
148000                 , DAREGDAT                                               
148100                 , IDUSER                                                 
148200                                                                          
148300           FROM    T01ASNS                                                
148400                                                                          
148500           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
148600           AND     KDFINDOC = :MAP-KDFINDOC-LINE                          
148700           AND     KDPARTTY = :MAP-KDPARTTY-LINE                          
148800           AND     KDPARTGR = :MAP-KDPARTGR-LINE-2                        
148900                                                                          
149000           ORDER BY KDFINDOC DESC                                         
149200                  , KDPARTTY                                              
149300                  , KDPARTGR                                              
149400                  , IDLANDX3                                              
149500                  , IDLOPNR                                               
149600     END-EXEC                                                             
149700                                                                          
149800     MOVE 000100  TO GOOD-SQLCODECODES                                    
149900                                                                          
150000     EXEC SQL                                                             
150100       OPEN T01ASNS-CRS-2A                                                
150200     END-EXEC                                                             
150300                                                                          
150400     MOVE SQLCODE TO SQLCODE-WS                                           
150500     PERFORM DB2-STATUS-CHECK                                             
150600     .                                                                    
150610                                                                          
150700 DB2-FETCH-T01ASNS-CRS-2A SECTION.                                        
150900     MOVE 000100  TO GOOD-SQLCODECODES                                    
151000                                                                          
151100     EXEC SQL                                                             
151200         FETCH T01ASNS-CRS-2A                                             
151300                                                                          
151400         INTO :MAP-KDFINDOC-LINE                                          
151500            , :MAP-KDPARTTY-LINE                                          
151600            , :MAP-KDPARTGR-LINE-2                                        
151700            , :MAP-IDLANDX3-LINE                                          
151900            , :MAP-IDLOPNR-LINE                                           
152000            , :MAP-DAREGDAT-LINE                                          
152100            , :MAP-IDUSER-LINE                                            
152200     END-EXEC                                                             
152300                                                                          
152400     MOVE SQLCODE TO SQLCODE-WS                                           
152500     PERFORM DB2-STATUS-CHECK                                             
152600     .                                                                    
152610                                                                          
152700 DB2-CLOSE-T01ASNS-CRS-2A SECTION.                                        
152900     EXEC SQL                                                             
153000       CLOSE T01ASNS-CRS-2A                                               
153100     END-EXEC                                                             
153200     .                                                                    
153210                                                                          
153300 DB2-DCL-OPN-T01BURE-CRS-2A SECTION.                                      
153400     MOVE 000100 TO GOOD-SQLCODECODES                                     
153500                                                                          
153600     EXEC SQL                                                             
153700         DECLARE T01BURE-CRS-2A CURSOR WITH HOLD FOR                      
153800                                                                          
153900           SELECT  KDFINDOC                                               
154000                 , KDPARTTY                                               
154100                 , KDPARTGR                                               
154200                                                                          
154300           FROM    T01BURE                                                
154400                                                                          
154500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
154600                AND DADELDAT = '00000000'                                 
154700                AND KDSTATUS = :WS-CURRENT                                
154800                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
154900                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
155000                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
155100                                                                          
155200           ORDER BY IDLEGSEL                                              
155300                  , KDFINDOC                                              
155400                  , KDPARTTY                                              
155500                  , KDPARTGR                                              
155600     END-EXEC                                                             
155700                                                                          
155800     MOVE 000100  TO GOOD-SQLCODECODES                                    
155900                                                                          
156000     EXEC SQL                                                             
156100        OPEN T01BURE-CRS-2A                                               
156200     END-EXEC                                                             
156300                                                                          
156400     MOVE SQLCODE TO SQLCODE-WS                                           
156500     PERFORM DB2-STATUS-CHECK                                             
156600     .                                                                    
156700     EJECT                                                                
156800                                                                          
156900 DB2-FETCH-T01BURE-CRS-2A SECTION.                                        
157000     MOVE 000100  TO GOOD-SQLCODECODES                                    
157100                                                                          
157200     EXEC SQL                                                             
157300                                                                          
157400         FETCH T01BURE-CRS-2A                                             
157500                                                                          
157600         INTO :MAP-KDFINDOC-LINE                                          
157700            , :MAP-KDPARTTY-LINE                                          
157800            , :MAP-KDPARTGR-LINE-2                                        
157900     END-EXEC                                                             
158000                                                                          
158100     MOVE SQLCODE TO SQLCODE-WS                                           
158200     PERFORM DB2-STATUS-CHECK                                             
158300     .                                                                    
158400     EJECT                                                                
158500                                                                          
158600 DB2-CLOSE-T01BURE-CRS-2A SECTION.                                        
158700     EXEC SQL                                                             
158800        CLOSE T01BURE-CRS-2A                                              
158900     END-EXEC                                                             
159000     .                                                                    
159100     EJECT                                                                
159200                                                                          
159300 DB2-DCL-OPN-T01BURE-CRS-2 SECTION.                                       
159400     MOVE 000100 TO GOOD-SQLCODECODES                                     
159500                                                                          
159600     EXEC SQL                                                             
159700         DECLARE T01BURE-CRS-2 CURSOR WITH HOLD FOR                       
159800                                                                          
159900           SELECT  KDFINDOC                                               
160000                 , KDPARTTY                                               
160100                 , KDPARTGR                                               
160200                                                                          
160300           FROM    T01BURE                                                
160400                                                                          
160500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
160600                AND DADELDAT = '00000000'                                 
160700                AND KDSTATUS = :WS-CURRENT                                
160800                AND KDFINDOC = :REQU-KDFINDOC-KEY                         
160900                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
161000                                                                          
161100           ORDER BY IDLEGSEL                                              
161200                  , KDFINDOC                                              
161300                  , KDPARTTY                                              
161400                  , KDPARTGR                                              
161500     END-EXEC                                                             
161600                                                                          
161700     MOVE 000100  TO GOOD-SQLCODECODES                                    
161800                                                                          
161900     EXEC SQL                                                             
162000        OPEN T01BURE-CRS-2                                                
162100     END-EXEC                                                             
162200                                                                          
162300     MOVE SQLCODE TO SQLCODE-WS                                           
162400     PERFORM DB2-STATUS-CHECK                                             
162500     .                                                                    
162600     EJECT                                                                
162700                                                                          
162800 DB2-FETCH-T01BURE-CRS-2 SECTION.                                         
162900     MOVE 000100  TO GOOD-SQLCODECODES                                    
163000                                                                          
163100     EXEC SQL                                                             
163200                                                                          
163300         FETCH T01BURE-CRS-2                                              
163400                                                                          
163500         INTO :MAP-KDFINDOC-LINE                                          
163600            , :MAP-KDPARTTY-LINE                                          
163700            , :MAP-KDPARTGR-LINE-2                                        
163800     END-EXEC                                                             
163900                                                                          
164000     MOVE SQLCODE TO SQLCODE-WS                                           
164100     PERFORM DB2-STATUS-CHECK                                             
164200     .                                                                    
164300     EJECT                                                                
164400                                                                          
164500 DB2-CLOSE-T01BURE-CRS-2 SECTION.                                         
164600     EXEC SQL                                                             
164700        CLOSE T01BURE-CRS-2                                               
164800     END-EXEC                                                             
164900     .                                                                    
165000     EJECT                                                                
165100                                                                          
165200* * * * * * * * * *   - CURSOR-FAC   * * * * * * * * * * * * * * *        
165300 DB2-COUNT-CRS-FAC SECTION.                                               
165500     MOVE 000100  TO GOOD-SQLCODECODES                                    
165600                                                                          
165700     EXEC SQL                                                             
165800          SELECT COUNT(*)                                                 
165900                                                                          
166000          INTO  :WS-COUNTER-FAC                                           
166100                                                                          
166200          FROM   T01BURE                                                  
166300                                                                          
166400          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
166500          AND    KDSTATUS = :WS-CURRENT                                   
166600          AND    DADELDAT = :WS-ACTIVE                                    
166700          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
166800          AND    KDPARTGR = :REQU-KDPARTGR-KEY                            
166900     END-EXEC                                                             
167000                                                                          
167100     MOVE SQLCODE TO SQLCODE-WS                                           
167200     PERFORM DB2-STATUS-CHECK                                             
167300     .                                                                    
167310                                                                          
167400 DB2-COUNT-CRS-FAC-2 SECTION.                                             
167600     MOVE 000100  TO GOOD-SQLCODECODES                                    
167700                                                                          
167800     EXEC SQL                                                             
167900          SELECT COUNT(*)                                                 
168000                                                                          
168100          INTO  :WS-COUNTER-FAC-2                                         
168200                                                                          
168300          FROM   T01ASNS                                                  
168400                                                                          
168500          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
168600          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
168700          AND    KDPARTGR = :REQU-KDPARTGR-KEY                            
168800     END-EXEC                                                             
168900                                                                          
169000     MOVE SQLCODE TO SQLCODE-WS                                           
169100     PERFORM DB2-STATUS-CHECK                                             
169200     .                                                                    
169210                                                                          
169300 DB2-COUNT-T01ASNS-3A SECTION.                                            
169500     MOVE 000100  TO GOOD-SQLCODECODES                                    
169600                                                                          
169700     EXEC SQL                                                             
169800          SELECT COUNT(*)                                                 
169900                                                                          
170000          INTO  :WS-COUNT-FAC-2A                                          
170100                                                                          
170200          FROM   T01ASNS                                                  
170300                                                                          
170400          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
170500          AND    KDFINDOC = :MAP-KDFINDOC-LINE-2                          
170600          AND    KDPARTTY = :MAP-KDPARTTY-LINE                            
170700          AND    KDPARTGR = :MAP-KDPARTGR-LINE                            
170800     END-EXEC                                                             
170900                                                                          
171000     MOVE SQLCODE TO SQLCODE-WS                                           
171100     PERFORM DB2-STATUS-CHECK                                             
171200     .                                                                    
171210                                                                          
171300 DB2-COUNT-T01BURE-3A SECTION.                                            
171500     MOVE 000100  TO GOOD-SQLCODECODES                                    
171600                                                                          
171700     EXEC SQL                                                             
171800          SELECT COUNT(*)                                                 
171900                                                                          
172000          INTO  :WS-COUNT-FAC-2A                                          
172100                                                                          
172200          FROM   T01BURE                                                  
172300                                                                          
172400          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
172500          AND    KDSTATUS = :WS-CURRENT                                   
172600          AND    DADELDAT = :WS-ACTIVE                                    
172700          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
172800          AND    KDPARTGR = :REQU-KDPARTGR-KEY                            
172900     END-EXEC                                                             
173000                                                                          
173100     MOVE SQLCODE TO SQLCODE-WS                                           
173200     PERFORM DB2-STATUS-CHECK                                             
173300     .                                                                    
173310                                                                          
173400 DB2-DCL-OPN-T01ASNS-CRS-3 SECTION.                                       
173600     MOVE 000100 TO GOOD-SQLCODECODES                                     
173700                                                                          
173800     EXEC SQL                                                             
173900         DECLARE T01ASNS-CRS-3 CURSOR WITH HOLD FOR                       
174000         SELECT  A.KDFINDOC                                               
174100              , A.KDPARTTY                                                
174200              , A.KDPARTGR                                                
174300                                                                          
174400         FROM   T01ASNS A                                                 
174500                                                                          
174600         WHERE   A.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
174700           AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                          
174800           AND   A.KDPARTGR = :REQU-KDPARTGR-KEY                          
174900                                                                          
175000         UNION                                                            
175100                                                                          
175200         SELECT B.KDFINDOC                                                
175300              , B.KDPARTTY                                                
175400              , B.KDPARTGR                                                
175500                                                                          
175600         FROM   T01BURE B                                                 
175700                                                                          
175800         WHERE   B.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
175900            AND  B.KDSTATUS = :WS-CURRENT                                 
176000            AND  B.DADELDAT = :WS-ACTIVE                                  
176100            AND  B.KDPARTTY = :REQU-KDPARTTY-KEY                          
176200            AND  B.KDPARTGR = :REQU-KDPARTGR-KEY                          
176300            AND  NOT EXISTS                                               
176400                                                                          
176500             (SELECT *                                                    
176600              FROM   T01ASNS                                              
176700                    ,T01BURE                                              
176800              WHERE  T01ASNS.IDLEGSEL = T01BURE.IDLEGSEL AND              
176900                     T01ASNS.KDFINDOC = T01BURE.KDFINDOC AND              
177000                     T01ASNS.KDPARTTY = T01BURE.KDPARTTY AND              
177100                     T01ASNS.KDPARTGR = T01BURE.KDPARTGR)                 
177200     END-EXEC                                                             
177300                                                                          
177400     MOVE 000100  TO GOOD-SQLCODECODES                                    
177500                                                                          
177600     EXEC SQL                                                             
177700       OPEN T01ASNS-CRS-3                                                 
177800     END-EXEC                                                             
177900                                                                          
178000     MOVE SQLCODE TO SQLCODE-WS                                           
178100     PERFORM DB2-STATUS-CHECK                                             
178200     .                                                                    
178210                                                                          
178300 DB2-FETCH-T01ASNS-CRS-3 SECTION.                                         
178500     MOVE 000100  TO GOOD-SQLCODECODES                                    
178600                                                                          
178700     EXEC SQL                                                             
178800         FETCH T01ASNS-CRS-3                                              
178900                                                                          
179000         INTO :MAP-KDFINDOC-LINE-2                                        
179100            , :MAP-KDPARTTY-LINE                                          
179200            , :MAP-KDPARTGR-LINE                                          
179300     END-EXEC                                                             
179400                                                                          
179500     MOVE SQLCODE TO SQLCODE-WS                                           
179600     PERFORM DB2-STATUS-CHECK                                             
179700     .                                                                    
179710                                                                          
179800 DB2-CLOSE-T01ASNS-CRS-3 SECTION.                                         
180000     EXEC SQL                                                             
180100       CLOSE T01ASNS-CRS-3                                                
180200     END-EXEC                                                             
180300     .                                                                    
180310                                                                          
180400 DB2-DCL-OPN-T01ASNS-CRS-3A SECTION.                                      
180600     MOVE 000100 TO GOOD-SQLCODECODES                                     
180700                                                                          
180800     EXEC SQL                                                             
180900         DECLARE T01ASNS-CRS-3A CURSOR WITH HOLD FOR                      
181000                                                                          
181100           SELECT  KDFINDOC                                               
181200                 , KDPARTTY                                               
181300                 , KDPARTGR                                               
181400                 , IDLANDX3                                               
181600                 , IDLOPNR                                                
181700                 , DAREGDAT                                               
181800                 , IDUSER                                                 
181900                                                                          
182000           FROM    T01ASNS                                                
182100                                                                          
182200           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
182300           AND     KDFINDOC = :MAP-KDFINDOC-LINE-2                        
182400           AND     KDPARTTY = :MAP-KDPARTTY-LINE                          
182500           AND     KDPARTGR = :MAP-KDPARTGR-LINE                          
182600                                                                          
182700           ORDER BY KDFINDOC DESC                                         
182900                  , KDPARTTY                                              
183000                  , KDPARTGR                                              
183100                  , IDLANDX3                                              
183200                  , IDLOPNR                                               
183300     END-EXEC                                                             
183400                                                                          
183500     MOVE 000100  TO GOOD-SQLCODECODES                                    
183600                                                                          
183700     EXEC SQL                                                             
183800       OPEN T01ASNS-CRS-3A                                                
183900     END-EXEC                                                             
184000                                                                          
184100     MOVE SQLCODE TO SQLCODE-WS                                           
184200     PERFORM DB2-STATUS-CHECK                                             
184300     .                                                                    
184310                                                                          
184400 DB2-FETCH-T01ASNS-CRS-3A SECTION.                                        
184600     MOVE 000100  TO GOOD-SQLCODECODES                                    
184700                                                                          
184800     EXEC SQL                                                             
184900         FETCH T01ASNS-CRS-3A                                             
185000                                                                          
185100         INTO :MAP-KDFINDOC-LINE-2                                        
185200            , :MAP-KDPARTTY-LINE                                          
185300            , :MAP-KDPARTGR-LINE                                          
185400            , :MAP-IDLANDX3-LINE                                          
185600            , :MAP-IDLOPNR-LINE                                           
185700            , :MAP-DAREGDAT-LINE                                          
185800            , :MAP-IDUSER-LINE                                            
185900     END-EXEC                                                             
186000                                                                          
186100     MOVE SQLCODE TO SQLCODE-WS                                           
186200     PERFORM DB2-STATUS-CHECK                                             
186300     .                                                                    
186310                                                                          
186400 DB2-CLOSE-T01ASNS-CRS-3A SECTION.                                        
186600     EXEC SQL                                                             
186700       CLOSE T01ASNS-CRS-3A                                               
186800     END-EXEC                                                             
186900     .                                                                    
186910                                                                          
187000 DB2-DCL-OPN-T01BURE-CRS-3A SECTION.                                      
187100     MOVE 000100 TO GOOD-SQLCODECODES                                     
187200                                                                          
187300     EXEC SQL                                                             
187400         DECLARE T01BURE-CRS-3A CURSOR WITH HOLD FOR                      
187500                                                                          
187600           SELECT  KDFINDOC                                               
187700                 , KDPARTTY                                               
187800                 , KDPARTGR                                               
187900                                                                          
188000           FROM    T01BURE                                                
188100                                                                          
188200           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
188300                AND DADELDAT = '00000000'                                 
188400                AND KDSTATUS = :WS-CURRENT                                
188500                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
188600                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
188700                AND KDPARTGR = :MAP-KDPARTGR-LINE                         
188800                                                                          
188900           ORDER BY IDLEGSEL                                              
189000                  , KDFINDOC                                              
189100                  , KDPARTTY                                              
189200                  , KDPARTGR                                              
189300     END-EXEC                                                             
189400                                                                          
189500     MOVE 000100  TO GOOD-SQLCODECODES                                    
189600                                                                          
189700     EXEC SQL                                                             
189800        OPEN T01BURE-CRS-3A                                               
189900     END-EXEC                                                             
190000                                                                          
190100     MOVE SQLCODE TO SQLCODE-WS                                           
190200     PERFORM DB2-STATUS-CHECK                                             
190300     .                                                                    
190400     EJECT                                                                
190500                                                                          
190600 DB2-FETCH-T01BURE-CRS-3A SECTION.                                        
190700     MOVE 000100  TO GOOD-SQLCODECODES                                    
190800                                                                          
190900     EXEC SQL                                                             
191000                                                                          
191100         FETCH T01BURE-CRS-3A                                             
191200                                                                          
191300         INTO :MAP-KDFINDOC-LINE-2                                        
191400            , :MAP-KDPARTTY-LINE                                          
191500            , :MAP-KDPARTGR-LINE                                          
191600     END-EXEC                                                             
191700                                                                          
191800     MOVE SQLCODE TO SQLCODE-WS                                           
191900     PERFORM DB2-STATUS-CHECK                                             
192000     .                                                                    
192100     EJECT                                                                
192200                                                                          
192300 DB2-CLOSE-T01BURE-CRS-3A SECTION.                                        
192400     EXEC SQL                                                             
192500        CLOSE T01BURE-CRS-3A                                              
192600     END-EXEC                                                             
192700     .                                                                    
192800     EJECT                                                                
192900                                                                          
193000 DB2-DCL-OPN-T01BURE-CRS-3 SECTION.                                       
193100     MOVE 000100 TO GOOD-SQLCODECODES                                     
193200                                                                          
193300     EXEC SQL                                                             
193400         DECLARE T01BURE-CRS-3 CURSOR WITH HOLD FOR                       
193500                                                                          
193600           SELECT  KDFINDOC                                               
193700                 , KDPARTTY                                               
193800                 , KDPARTGR                                               
193900                                                                          
194000           FROM    T01BURE                                                
194100                                                                          
194200           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
194300                AND DADELDAT = '00000000'                                 
194400                AND KDSTATUS = :WS-CURRENT                                
194500                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
194600                AND KDPARTGR = :REQU-KDPARTGR-KEY                         
194700                                                                          
194800           ORDER BY IDLEGSEL                                              
194900                  , KDFINDOC                                              
195000                  , KDPARTTY                                              
195100                  , KDPARTGR                                              
195200     END-EXEC                                                             
195300                                                                          
195400     MOVE 000100  TO GOOD-SQLCODECODES                                    
195500                                                                          
195600     EXEC SQL                                                             
195700        OPEN T01BURE-CRS-3                                                
195800     END-EXEC                                                             
195900                                                                          
196000     MOVE SQLCODE TO SQLCODE-WS                                           
196100     PERFORM DB2-STATUS-CHECK                                             
196200     .                                                                    
196300     EJECT                                                                
196400                                                                          
196500 DB2-FETCH-T01BURE-CRS-3 SECTION.                                         
196600     MOVE 000100  TO GOOD-SQLCODECODES                                    
196700                                                                          
196800     EXEC SQL                                                             
196900                                                                          
197000         FETCH T01BURE-CRS-3                                              
197100                                                                          
197200         INTO :MAP-KDFINDOC-LINE-2                                        
197300            , :MAP-KDPARTTY-LINE                                          
197400            , :MAP-KDPARTGR-LINE                                          
197500     END-EXEC                                                             
197600                                                                          
197700     MOVE SQLCODE TO SQLCODE-WS                                           
197800     PERFORM DB2-STATUS-CHECK                                             
197900     .                                                                    
198000     EJECT                                                                
198100                                                                          
198200 DB2-CLOSE-T01BURE-CRS-3 SECTION.                                         
198300     EXEC SQL                                                             
198400        CLOSE T01BURE-CRS-3                                               
198500     END-EXEC                                                             
198600     .                                                                    
198700     EJECT                                                                
198800                                                                          
198810* * * * * * * * * *   - CURSOR-FAA-  * * * * * * * * * * * * * * *        
198820 DB2-COUNT-CRS-FAA SECTION.                                               
198830     EXEC SQL                                                             
198840           SELECT COUNT(*)                                                
198850                                                                          
198860           INTO  :WS-COUNTER-FAA                                          
198870                                                                          
198880           FROM   T01BURE                                                 
198890                                                                          
198891           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
198892           AND    KDFINDOC = :REQU-KDFINDOC-KEY                           
198893           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
198894           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
198895           AND    DADELDAT = '00000000'                                   
198896           AND    KDSTATUS = :WS-CURRENT                                  
198897     END-EXEC                                                             
198898                                                                          
198899     MOVE 000100  TO GOOD-SQLCODECODES                                    
198900                                                                          
198901     MOVE SQLCODE TO SQLCODE-WS                                           
198902     PERFORM DB2-STATUS-CHECK                                             
198903     .                                                                    
198904                                                                          
198905 DB2-COUNT-CRS-FAA-2 SECTION.                                             
198906     EXEC SQL                                                             
198907           SELECT COUNT(*)                                                
198908                                                                          
198909           INTO  :WS-COUNTER-FAA-2                                        
198910                                                                          
198911           FROM   T01ASNS                                                 
198912                                                                          
198913           WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                           
198914           AND    KDFINDOC = :REQU-KDFINDOC-KEY                           
198915           AND    KDPARTTY = :REQU-KDPARTTY-KEY                           
198916           AND    KDPARTGR = :REQU-KDPARTGR-KEY                           
198917     END-EXEC                                                             
198918                                                                          
198919     MOVE 000100  TO GOOD-SQLCODECODES                                    
198920                                                                          
198921     MOVE SQLCODE TO SQLCODE-WS                                           
198922     PERFORM DB2-STATUS-CHECK                                             
198923     .                                                                    
198924                                                                          
198930* * * * * * * * * *   - CURSOR-FAD   * * * * * * * * * * * * * * *        
199000 DB2-COUNT-CRS-FAD SECTION.                                               
199200     MOVE 000100  TO GOOD-SQLCODECODES                                    
199300                                                                          
199400     EXEC SQL                                                             
199500          SELECT COUNT(*)                                                 
199600                                                                          
199700          INTO  :WS-COUNTER-FAD                                           
199800                                                                          
199900          FROM   T01BURE                                                  
200000                                                                          
200100          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
200200          AND    KDSTATUS = :WS-CURRENT                                   
200300          AND    DADELDAT = :WS-ACTIVE                                    
200400          AND    KDFINDOC = :REQU-KDFINDOC-KEY                            
200500     END-EXEC                                                             
200600                                                                          
200700     MOVE SQLCODE TO SQLCODE-WS                                           
200800     PERFORM DB2-STATUS-CHECK                                             
200900     .                                                                    
200910                                                                          
201000 DB2-COUNT-CRS-FAD-2 SECTION.                                             
201200     MOVE 000100  TO GOOD-SQLCODECODES                                    
201300                                                                          
201400     EXEC SQL                                                             
201500          SELECT COUNT(*)                                                 
201600                                                                          
201700          INTO  :WS-COUNTER-FAD-2                                         
201800                                                                          
201900          FROM   T01BURE                                                  
202000                                                                          
202100          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
202200          AND    KDFINDOC = :REQU-KDFINDOC-KEY                            
202300     END-EXEC                                                             
202400                                                                          
202500     MOVE SQLCODE TO SQLCODE-WS                                           
202600     PERFORM DB2-STATUS-CHECK                                             
202700     .                                                                    
202710                                                                          
202800 DB2-COUNT-T01ASNS-4A SECTION.                                            
203000     MOVE 000100  TO GOOD-SQLCODECODES                                    
203100                                                                          
203200     EXEC SQL                                                             
203300          SELECT COUNT(*)                                                 
203400                                                                          
203500          INTO  :WS-COUNT-FAD-2A                                          
203600                                                                          
203700          FROM   T01ASNS                                                  
203800                                                                          
203900          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
204000          AND    KDFINDOC = :MAP-KDFINDOC-LINE                            
204100          AND    KDPARTTY = :MAP-KDPARTTY-LINE-2                          
204200          AND    KDPARTGR = :MAP-KDPARTGR-LINE-2                          
204300     END-EXEC                                                             
204400                                                                          
204500     MOVE SQLCODE TO SQLCODE-WS                                           
204600     PERFORM DB2-STATUS-CHECK                                             
204700     .                                                                    
204710                                                                          
204800 DB2-COUNT-T01BURE-4A SECTION.                                            
205000     MOVE 000100  TO GOOD-SQLCODECODES                                    
205100                                                                          
205200     EXEC SQL                                                             
205300          SELECT COUNT(*)                                                 
205400                                                                          
205500          INTO  :WS-COUNT-FAD-2A                                          
205600                                                                          
205700          FROM   T01BURE                                                  
205800                                                                          
205900          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
206000          AND    KDSTATUS = :WS-CURRENT                                   
206100          AND    DADELDAT = :WS-ACTIVE                                    
206200          AND    KDFINDOC = :REQU-KDFINDOC-KEY                            
206300     END-EXEC                                                             
206400                                                                          
206500     MOVE SQLCODE TO SQLCODE-WS                                           
206600     PERFORM DB2-STATUS-CHECK                                             
206700     .                                                                    
206710                                                                          
206800 DB2-DCL-OPN-T01ASNS-CRS-4 SECTION.                                       
207000     MOVE 000100 TO GOOD-SQLCODECODES                                     
207100                                                                          
207200     EXEC SQL                                                             
207300         DECLARE T01ASNS-CRS-4 CURSOR WITH HOLD FOR                       
207400         SELECT  A.KDFINDOC                                               
207500              , A.KDPARTTY                                                
207600              , A.KDPARTGR                                                
207700                                                                          
207800         FROM   T01ASNS A                                                 
207900                                                                          
208000         WHERE   A.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
208100           AND   A.KDFINDOC = :REQU-KDFINDOC-KEY                          
208200                                                                          
208300         UNION                                                            
208400                                                                          
208500         SELECT B.KDFINDOC                                                
208600              , B.KDPARTTY                                                
208700              , B.KDPARTGR                                                
208800                                                                          
208900         FROM   T01BURE B                                                 
209000                                                                          
209100         WHERE   B.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
209200            AND  B.KDSTATUS = :WS-CURRENT                                 
209300            AND  B.DADELDAT = :WS-ACTIVE                                  
209400            AND  B.KDFINDOC = :REQU-KDFINDOC-KEY                          
209500            AND  NOT EXISTS                                               
209600                                                                          
209700             (SELECT *                                                    
209800              FROM   T01ASNS                                              
209900                    ,T01BURE                                              
210000              WHERE  T01ASNS.IDLEGSEL = T01BURE.IDLEGSEL AND              
210100                     T01ASNS.KDFINDOC = T01BURE.KDFINDOC AND              
210200                     T01ASNS.KDPARTTY = T01BURE.KDPARTTY AND              
210300                     T01ASNS.KDPARTGR = T01BURE.KDPARTGR)                 
210400     END-EXEC                                                             
210500                                                                          
210600     MOVE 000100  TO GOOD-SQLCODECODES                                    
210700                                                                          
210800     EXEC SQL                                                             
210900       OPEN T01ASNS-CRS-4                                                 
211000     END-EXEC                                                             
211100                                                                          
211200     MOVE SQLCODE TO SQLCODE-WS                                           
211300     PERFORM DB2-STATUS-CHECK                                             
211400     .                                                                    
211410                                                                          
211500 DB2-FETCH-T01ASNS-CRS-4 SECTION.                                         
211700     MOVE 000100  TO GOOD-SQLCODECODES                                    
211800                                                                          
211900     EXEC SQL                                                             
212000         FETCH T01ASNS-CRS-4                                              
212100                                                                          
212200         INTO :MAP-KDFINDOC-LINE                                          
212300            , :MAP-KDPARTTY-LINE-2                                        
212400            , :MAP-KDPARTGR-LINE-2                                        
212500     END-EXEC                                                             
212600                                                                          
212700     MOVE SQLCODE TO SQLCODE-WS                                           
212800     PERFORM DB2-STATUS-CHECK                                             
212900     .                                                                    
212910                                                                          
213000 DB2-CLOSE-T01ASNS-CRS-4 SECTION.                                         
213200     EXEC SQL                                                             
213300       CLOSE T01ASNS-CRS-4                                                
213400     END-EXEC                                                             
213500     .                                                                    
213510                                                                          
213600 DB2-DCL-OPN-T01ASNS-CRS-4A SECTION.                                      
213800     MOVE 000100 TO GOOD-SQLCODECODES                                     
213900                                                                          
214000     EXEC SQL                                                             
214100         DECLARE T01ASNS-CRS-4A CURSOR WITH HOLD FOR                      
214200                                                                          
214300           SELECT  KDFINDOC                                               
214400                 , KDPARTTY                                               
214500                 , KDPARTGR                                               
214600                 , IDLANDX3                                               
214800                 , IDLOPNR                                                
214900                 , DAREGDAT                                               
215000                 , IDUSER                                                 
215100                                                                          
215200           FROM    T01ASNS                                                
215300                                                                          
215400           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
215500           AND     KDFINDOC = :MAP-KDFINDOC-LINE                          
215600           AND     KDPARTTY = :MAP-KDPARTTY-LINE-2                        
215700           AND     KDPARTGR = :MAP-KDPARTGR-LINE-2                        
215800                                                                          
215900           ORDER BY KDFINDOC DESC                                         
216100                  , KDPARTTY                                              
216200                  , KDPARTGR                                              
216300                  , IDLANDX3                                              
216400                  , IDLOPNR                                               
216500     END-EXEC                                                             
216600                                                                          
216700     MOVE 000100  TO GOOD-SQLCODECODES                                    
216800                                                                          
216900     EXEC SQL                                                             
217000       OPEN T01ASNS-CRS-4A                                                
217100     END-EXEC                                                             
217200                                                                          
217300     MOVE SQLCODE TO SQLCODE-WS                                           
217400     PERFORM DB2-STATUS-CHECK                                             
217500     .                                                                    
217510                                                                          
217600 DB2-FETCH-T01ASNS-CRS-4A SECTION.                                        
217800     MOVE 000100  TO GOOD-SQLCODECODES                                    
217900                                                                          
218000     EXEC SQL                                                             
218100         FETCH T01ASNS-CRS-4A                                             
218200                                                                          
218300         INTO :MAP-KDFINDOC-LINE                                          
218400            , :MAP-KDPARTTY-LINE-2                                        
218500            , :MAP-KDPARTGR-LINE-2                                        
218600            , :MAP-IDLANDX3-LINE                                          
218800            , :MAP-IDLOPNR-LINE                                           
218900            , :MAP-DAREGDAT-LINE                                          
219000            , :MAP-IDUSER-LINE                                            
219100     END-EXEC                                                             
219200                                                                          
219300     MOVE SQLCODE TO SQLCODE-WS                                           
219400     PERFORM DB2-STATUS-CHECK                                             
219500     .                                                                    
219510                                                                          
219600 DB2-CLOSE-T01ASNS-CRS-4A SECTION.                                        
219800     EXEC SQL                                                             
219900       CLOSE T01ASNS-CRS-4A                                               
220000     END-EXEC                                                             
220100     .                                                                    
220110                                                                          
220200 DB2-DCL-OPN-T01BURE-CRS-4A SECTION.                                      
220300     MOVE 000100 TO GOOD-SQLCODECODES                                     
220400                                                                          
220500     EXEC SQL                                                             
220600         DECLARE T01BURE-CRS-4A CURSOR WITH HOLD FOR                      
220700                                                                          
220800           SELECT  KDFINDOC                                               
220900                 , KDPARTTY                                               
221000                 , KDPARTGR                                               
221100                                                                          
221200           FROM    T01BURE                                                
221300                                                                          
221400           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
221500                AND DADELDAT = '00000000'                                 
221600                AND KDSTATUS = :WS-CURRENT                                
221700                AND KDFINDOC = :MAP-KDFINDOC-LINE                         
221800                AND KDPARTTY = :MAP-KDPARTTY-LINE-2                       
221900                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
222000                                                                          
222100           ORDER BY IDLEGSEL                                              
222200                  , KDFINDOC                                              
222300                  , KDPARTTY                                              
222400                  , KDPARTGR                                              
222500     END-EXEC                                                             
222600                                                                          
222700     MOVE 000100  TO GOOD-SQLCODECODES                                    
222800                                                                          
222900     EXEC SQL                                                             
223000        OPEN T01BURE-CRS-4A                                               
223100     END-EXEC                                                             
223200                                                                          
223300     MOVE SQLCODE TO SQLCODE-WS                                           
223400     PERFORM DB2-STATUS-CHECK                                             
223500     .                                                                    
223600     EJECT                                                                
223700                                                                          
223800 DB2-FETCH-T01BURE-CRS-4A SECTION.                                        
223900     MOVE 000100  TO GOOD-SQLCODECODES                                    
224000                                                                          
224100     EXEC SQL                                                             
224200                                                                          
224300         FETCH T01BURE-CRS-4A                                             
224400                                                                          
224500         INTO :MAP-KDFINDOC-LINE                                          
224600            , :MAP-KDPARTTY-LINE-2                                        
224700            , :MAP-KDPARTGR-LINE-2                                        
224800     END-EXEC                                                             
224900                                                                          
225000     MOVE SQLCODE TO SQLCODE-WS                                           
225100     PERFORM DB2-STATUS-CHECK                                             
225200     .                                                                    
225300     EJECT                                                                
225400                                                                          
225500 DB2-CLOSE-T01BURE-CRS-4A SECTION.                                        
225600     EXEC SQL                                                             
225700        CLOSE T01BURE-CRS-4A                                              
225800     END-EXEC                                                             
225900     .                                                                    
226000     EJECT                                                                
226100                                                                          
226200 DB2-DCL-OPN-T01BURE-CRS-4 SECTION.                                       
226300     MOVE 000100 TO GOOD-SQLCODECODES                                     
226400                                                                          
226500     EXEC SQL                                                             
226600         DECLARE T01BURE-CRS-4 CURSOR WITH HOLD FOR                       
226700                                                                          
226800           SELECT  KDFINDOC                                               
226900                 , KDPARTTY                                               
227000                 , KDPARTGR                                               
227100                                                                          
227200           FROM    T01BURE                                                
227300                                                                          
227400           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
227500                AND DADELDAT = '00000000'                                 
227600                AND KDSTATUS = :WS-CURRENT                                
227700                AND KDFINDOC = :REQU-KDFINDOC-KEY                         
227800                                                                          
227900           ORDER BY IDLEGSEL                                              
228000                  , KDFINDOC                                              
228100                  , KDPARTTY                                              
228200                  , KDPARTGR                                              
228300     END-EXEC                                                             
228400                                                                          
228500     MOVE 000100  TO GOOD-SQLCODECODES                                    
228600                                                                          
228700     EXEC SQL                                                             
228800        OPEN T01BURE-CRS-4                                                
228900     END-EXEC                                                             
229000                                                                          
229100     MOVE SQLCODE TO SQLCODE-WS                                           
229200     PERFORM DB2-STATUS-CHECK                                             
229300     .                                                                    
229400     EJECT                                                                
229500                                                                          
229600 DB2-FETCH-T01BURE-CRS-4 SECTION.                                         
229700     MOVE 000100  TO GOOD-SQLCODECODES                                    
229800                                                                          
229900     EXEC SQL                                                             
230000                                                                          
230100         FETCH T01BURE-CRS-4                                              
230200                                                                          
230300         INTO :MAP-KDFINDOC-LINE                                          
230400            , :MAP-KDPARTTY-LINE-2                                        
230500            , :MAP-KDPARTGR-LINE-2                                        
230600     END-EXEC                                                             
230700                                                                          
230800     MOVE SQLCODE TO SQLCODE-WS                                           
230900     PERFORM DB2-STATUS-CHECK                                             
231000     .                                                                    
231100     EJECT                                                                
231200                                                                          
231300 DB2-CLOSE-T01BURE-CRS-4 SECTION.                                         
231400     EXEC SQL                                                             
231500        CLOSE T01BURE-CRS-4                                               
231600     END-EXEC                                                             
231700     .                                                                    
231800     EJECT                                                                
231900                                                                          
232000* * * * * * * * * *   - CURSOR-FAE   * * * * * * * * * * * * * * *        
232100 DB2-COUNT-CRS-FAE SECTION.                                               
232200                                                                          
232300     MOVE 000100  TO GOOD-SQLCODECODES                                    
232400                                                                          
232500     EXEC SQL                                                             
232600          SELECT COUNT(*)                                                 
232700                                                                          
232800          INTO  :WS-COUNTER-FAE                                           
232900                                                                          
233000          FROM   T01BURE                                                  
233100                                                                          
233200          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
233300          AND    KDSTATUS = :WS-CURRENT                                   
233400          AND    DADELDAT = :WS-ACTIVE                                    
233500          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
233600     END-EXEC                                                             
233700                                                                          
233800     MOVE SQLCODE TO SQLCODE-WS                                           
233900     PERFORM DB2-STATUS-CHECK                                             
234000     .                                                                    
234010                                                                          
234100 DB2-COUNT-CRS-FAE-2 SECTION.                                             
234300     MOVE 000100  TO GOOD-SQLCODECODES                                    
234400                                                                          
234500     EXEC SQL                                                             
234600          SELECT COUNT(*)                                                 
234700                                                                          
234800          INTO  :WS-COUNTER-FAE-2                                         
234900                                                                          
235000          FROM   T01BURE                                                  
235100                                                                          
235200          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
235300          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
235400     END-EXEC                                                             
235500                                                                          
235600     MOVE SQLCODE TO SQLCODE-WS                                           
235700     PERFORM DB2-STATUS-CHECK                                             
235800     .                                                                    
235810                                                                          
235900 DB2-COUNT-T01ASNS-5A SECTION.                                            
236100     MOVE 000100  TO GOOD-SQLCODECODES                                    
236200                                                                          
236300     EXEC SQL                                                             
236400          SELECT COUNT(*)                                                 
236500                                                                          
236600          INTO  :WS-COUNT-FAE-2A                                          
236700                                                                          
236800          FROM   T01ASNS                                                  
236900                                                                          
237000          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
237100          AND    KDFINDOC = :MAP-KDFINDOC-LINE-2                          
237200          AND    KDPARTTY = :MAP-KDPARTTY-LINE                            
237300          AND    KDPARTGR = :MAP-KDPARTGR-LINE-2                          
237400     END-EXEC                                                             
237500                                                                          
237600     MOVE SQLCODE TO SQLCODE-WS                                           
237700     PERFORM DB2-STATUS-CHECK                                             
237800     .                                                                    
237810                                                                          
237900 DB2-COUNT-T01BURE-5A SECTION.                                            
238100     MOVE 000100  TO GOOD-SQLCODECODES                                    
238200                                                                          
238300     EXEC SQL                                                             
238400          SELECT COUNT(*)                                                 
238500                                                                          
238600          INTO  :WS-COUNT-FAE-2A                                          
238700                                                                          
238800          FROM   T01BURE                                                  
238900                                                                          
239000          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
239100          AND    KDSTATUS = :WS-CURRENT                                   
239200          AND    DADELDAT = :WS-ACTIVE                                    
239300          AND    KDPARTTY = :REQU-KDPARTTY-KEY                            
239400     END-EXEC                                                             
239500                                                                          
239600     MOVE SQLCODE TO SQLCODE-WS                                           
239700     PERFORM DB2-STATUS-CHECK                                             
239800     .                                                                    
239810                                                                          
239900 DB2-DCL-OPN-T01ASNS-CRS-5 SECTION.                                       
240100     MOVE 000100 TO GOOD-SQLCODECODES                                     
240200                                                                          
240300     EXEC SQL                                                             
240400         DECLARE T01ASNS-CRS-5 CURSOR WITH HOLD FOR                       
240500         SELECT  A.KDFINDOC                                               
240600              , A.KDPARTTY                                                
240700              , A.KDPARTGR                                                
240800                                                                          
240900         FROM   T01ASNS A                                                 
241000                                                                          
241100         WHERE   A.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
241200           AND   A.KDPARTTY = :REQU-KDPARTTY-KEY                          
241300                                                                          
241400         UNION                                                            
241500                                                                          
241600         SELECT B.KDFINDOC                                                
241700              , B.KDPARTTY                                                
241800              , B.KDPARTGR                                                
241900                                                                          
242000         FROM   T01BURE B                                                 
242100                                                                          
242200         WHERE   B.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
242300            AND  B.KDSTATUS = :WS-CURRENT                                 
242400            AND  B.DADELDAT = :WS-ACTIVE                                  
242500            AND  B.KDPARTTY = :REQU-KDPARTTY-KEY                          
242600            AND  NOT EXISTS                                               
242700                                                                          
242800             (SELECT *                                                    
242900              FROM   T01ASNS                                              
243000                    ,T01BURE                                              
243100              WHERE  T01ASNS.IDLEGSEL = T01BURE.IDLEGSEL AND              
243200                     T01ASNS.KDFINDOC = T01BURE.KDFINDOC AND              
243300                     T01ASNS.KDPARTTY = T01BURE.KDPARTTY AND              
243400                     T01ASNS.KDPARTGR = T01BURE.KDPARTGR)                 
243500     END-EXEC                                                             
243600                                                                          
243700     MOVE 000100  TO GOOD-SQLCODECODES                                    
243800                                                                          
243900     EXEC SQL                                                             
244000       OPEN T01ASNS-CRS-5                                                 
244100     END-EXEC                                                             
244200                                                                          
244300     MOVE SQLCODE TO SQLCODE-WS                                           
244400     PERFORM DB2-STATUS-CHECK                                             
244500     .                                                                    
244510                                                                          
244600 DB2-FETCH-T01ASNS-CRS-5 SECTION.                                         
244800     MOVE 000100  TO GOOD-SQLCODECODES                                    
244900                                                                          
245000     EXEC SQL                                                             
245100         FETCH T01ASNS-CRS-5                                              
245200                                                                          
245300         INTO :MAP-KDFINDOC-LINE-2                                        
245400            , :MAP-KDPARTTY-LINE                                          
245500            , :MAP-KDPARTGR-LINE-2                                        
245600     END-EXEC                                                             
245700                                                                          
245800     MOVE SQLCODE TO SQLCODE-WS                                           
245900     PERFORM DB2-STATUS-CHECK                                             
246000     .                                                                    
246010                                                                          
246100 DB2-CLOSE-T01ASNS-CRS-5 SECTION.                                         
246300     EXEC SQL                                                             
246400       CLOSE T01ASNS-CRS-5                                                
246500     END-EXEC                                                             
246600     .                                                                    
246610                                                                          
246700 DB2-DCL-OPN-T01ASNS-CRS-5A SECTION.                                      
246900     MOVE 000100 TO GOOD-SQLCODECODES                                     
247000                                                                          
247100     EXEC SQL                                                             
247200         DECLARE T01ASNS-CRS-5A CURSOR WITH HOLD FOR                      
247300                                                                          
247400           SELECT  KDFINDOC                                               
247500                 , KDPARTTY                                               
247600                 , KDPARTGR                                               
247700                 , IDLANDX3                                               
247900                 , IDLOPNR                                                
248000                 , DAREGDAT                                               
248100                 , IDUSER                                                 
248200                                                                          
248300           FROM    T01ASNS                                                
248400                                                                          
248500           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
248600           AND     KDFINDOC = :MAP-KDFINDOC-LINE-2                        
248700           AND     KDPARTTY = :MAP-KDPARTTY-LINE                          
248800           AND     KDPARTGR = :MAP-KDPARTGR-LINE-2                        
248900                                                                          
249000           ORDER BY KDFINDOC DESC                                         
249200                  , KDPARTTY                                              
249300                  , KDPARTGR                                              
249400                  , IDLANDX3                                              
249500                  , IDLOPNR                                               
249600     END-EXEC                                                             
249700                                                                          
249800     MOVE 000100  TO GOOD-SQLCODECODES                                    
249900                                                                          
250000     EXEC SQL                                                             
250100       OPEN T01ASNS-CRS-5A                                                
250200     END-EXEC                                                             
250300                                                                          
250400     MOVE SQLCODE TO SQLCODE-WS                                           
250500     PERFORM DB2-STATUS-CHECK                                             
250600     .                                                                    
250610                                                                          
250700 DB2-FETCH-T01ASNS-CRS-5A SECTION.                                        
250900     MOVE 000100  TO GOOD-SQLCODECODES                                    
251000                                                                          
251100     EXEC SQL                                                             
251200         FETCH T01ASNS-CRS-5A                                             
251300                                                                          
251400         INTO :MAP-KDFINDOC-LINE-2                                        
251500            , :MAP-KDPARTTY-LINE                                          
251600            , :MAP-KDPARTGR-LINE-2                                        
251700            , :MAP-IDLANDX3-LINE                                          
251900            , :MAP-IDLOPNR-LINE                                           
252000            , :MAP-DAREGDAT-LINE                                          
252100            , :MAP-IDUSER-LINE                                            
252200     END-EXEC                                                             
252300                                                                          
252400     MOVE SQLCODE TO SQLCODE-WS                                           
252500     PERFORM DB2-STATUS-CHECK                                             
252600     .                                                                    
252610                                                                          
252700 DB2-CLOSE-T01ASNS-CRS-5A SECTION.                                        
252900     EXEC SQL                                                             
253000       CLOSE T01ASNS-CRS-5A                                               
253100     END-EXEC                                                             
253200     .                                                                    
253210                                                                          
253300 DB2-DCL-OPN-T01BURE-CRS-5A SECTION.                                      
253400     MOVE 000100 TO GOOD-SQLCODECODES                                     
253500                                                                          
253600     EXEC SQL                                                             
253700         DECLARE T01BURE-CRS-5A CURSOR WITH HOLD FOR                      
253800                                                                          
253900           SELECT  KDFINDOC                                               
254000                 , KDPARTTY                                               
254100                 , KDPARTGR                                               
254200                                                                          
254300           FROM    T01BURE                                                
254400                                                                          
254500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
254600                AND DADELDAT = '00000000'                                 
254700                AND KDSTATUS = :WS-CURRENT                                
254800                AND KDFINDOC = :MAP-KDFINDOC-LINE-2                       
254900                AND KDPARTTY = :MAP-KDPARTTY-LINE                         
255000                AND KDPARTGR = :MAP-KDPARTGR-LINE-2                       
255100                                                                          
255200           ORDER BY IDLEGSEL                                              
255300                  , KDFINDOC                                              
255400                  , KDPARTTY                                              
255500                  , KDPARTGR                                              
255600     END-EXEC                                                             
255700                                                                          
255800     MOVE 000100  TO GOOD-SQLCODECODES                                    
255900                                                                          
256000     EXEC SQL                                                             
256100        OPEN T01BURE-CRS-5A                                               
256200     END-EXEC                                                             
256300                                                                          
256400     MOVE SQLCODE TO SQLCODE-WS                                           
256500     PERFORM DB2-STATUS-CHECK                                             
256600     .                                                                    
256700     EJECT                                                                
256800                                                                          
256900 DB2-FETCH-T01BURE-CRS-5A SECTION.                                        
257000     MOVE 000100  TO GOOD-SQLCODECODES                                    
257100                                                                          
257200     EXEC SQL                                                             
257300                                                                          
257400         FETCH T01BURE-CRS-5A                                             
257500                                                                          
257600         INTO :MAP-KDFINDOC-LINE-2                                        
257700            , :MAP-KDPARTTY-LINE                                          
257800            , :MAP-KDPARTGR-LINE-2                                        
257900     END-EXEC                                                             
258000                                                                          
258100     MOVE SQLCODE TO SQLCODE-WS                                           
258200     PERFORM DB2-STATUS-CHECK                                             
258300     .                                                                    
258400     EJECT                                                                
258500                                                                          
258600 DB2-CLOSE-T01BURE-CRS-5A SECTION.                                        
258700     EXEC SQL                                                             
258800        CLOSE T01BURE-CRS-5A                                              
258900     END-EXEC                                                             
259000     .                                                                    
259100     EJECT                                                                
259200                                                                          
259300 DB2-DCL-OPN-T01BURE-CRS-5 SECTION.                                       
259400     MOVE 000100 TO GOOD-SQLCODECODES                                     
259500                                                                          
259600     EXEC SQL                                                             
259700         DECLARE T01BURE-CRS-5 CURSOR WITH HOLD FOR                       
259800                                                                          
259900           SELECT  KDFINDOC                                               
260000                 , KDPARTTY                                               
260100                 , KDPARTGR                                               
260200                                                                          
260300           FROM    T01BURE                                                
260400                                                                          
260500           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
260600                AND DADELDAT = '00000000'                                 
260700                AND KDSTATUS = :WS-CURRENT                                
260800                AND KDPARTTY = :REQU-KDPARTTY-KEY                         
260900                                                                          
261000           ORDER BY IDLEGSEL                                              
261100                  , KDFINDOC                                              
261200                  , KDPARTTY                                              
261300                  , KDPARTGR                                              
261400     END-EXEC                                                             
261500                                                                          
261600     MOVE 000100  TO GOOD-SQLCODECODES                                    
261700                                                                          
261800     EXEC SQL                                                             
261900        OPEN T01BURE-CRS-5                                                
262000     END-EXEC                                                             
262100                                                                          
262200     MOVE SQLCODE TO SQLCODE-WS                                           
262300     PERFORM DB2-STATUS-CHECK                                             
262400     .                                                                    
262500     EJECT                                                                
262600                                                                          
262700 DB2-FETCH-T01BURE-CRS-5 SECTION.                                         
262800     MOVE 000100  TO GOOD-SQLCODECODES                                    
262900                                                                          
263000     EXEC SQL                                                             
263100                                                                          
263200         FETCH T01BURE-CRS-5                                              
263300                                                                          
263400         INTO :MAP-KDFINDOC-LINE-2                                        
263500            , :MAP-KDPARTTY-LINE                                          
263600            , :MAP-KDPARTGR-LINE-2                                        
263700     END-EXEC                                                             
263800                                                                          
263900     MOVE SQLCODE TO SQLCODE-WS                                           
264000     PERFORM DB2-STATUS-CHECK                                             
264100     .                                                                    
264200     EJECT                                                                
264300                                                                          
264400 DB2-CLOSE-T01BURE-CRS-5 SECTION.                                         
264500     EXEC SQL                                                             
264600        CLOSE T01BURE-CRS-5                                               
264700     END-EXEC                                                             
264800     .                                                                    
264900     EJECT                                                                
266816                                                                          
266817* * * * * * * * * *   - CURSOR-FAF   * * * * * * * * * * * * * * *        
266818 DB2-COUNT-CRS-FAF SECTION.                                               
266819                                                                          
266820     MOVE 000100  TO GOOD-SQLCODECODES                                    
266830                                                                          
266840     EXEC SQL                                                             
266850          SELECT COUNT(*)                                                 
266860                                                                          
266870          INTO  :WS-COUNTER-FAF                                           
266880                                                                          
266890          FROM   T01BURE                                                  
266900                                                                          
267000          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
267100          AND    KDSTATUS = :WS-CURRENT                                   
267200          AND    DADELDAT = :WS-ACTIVE                                    
267220     END-EXEC                                                             
267230                                                                          
267240     MOVE SQLCODE TO SQLCODE-WS                                           
267241     PERFORM DB2-STATUS-CHECK                                             
267242     .                                                                    
267243                                                                          
267244 DB2-COUNT-CRS-FAF-2 SECTION.                                             
267245     MOVE 000100  TO GOOD-SQLCODECODES                                    
267246                                                                          
267247     EXEC SQL                                                             
267248          SELECT COUNT(*)                                                 
267249                                                                          
267250          INTO  :WS-COUNTER-FAF-2                                         
267251                                                                          
267252          FROM   T01ASNS                                                  
267253                                                                          
267254          WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                            
267256     END-EXEC                                                             
267257                                                                          
267258     MOVE SQLCODE TO SQLCODE-WS                                           
267259     PERFORM DB2-STATUS-CHECK                                             
267260     .                                                                    
267261                                                                          
267281                                                                          
267301                                                                          
267302 DB2-DCL-OPN-T01ASNS-CRS-6 SECTION.                                       
267303     MOVE 000100 TO GOOD-SQLCODECODES                                     
267304                                                                          
267305     EXEC SQL                                                             
267306         DECLARE T01ASNS-CRS-6 CURSOR WITH HOLD FOR                       
267307         SELECT  A.KDFINDOC                                               
267308              , A.KDPARTTY                                                
267309              , A.KDPARTGR                                                
267310                                                                          
267311         FROM   T01ASNS A                                                 
267312                                                                          
267313         WHERE   A.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
267315                                                                          
267316         UNION                                                            
267317                                                                          
267318         SELECT B.KDFINDOC                                                
267319              , B.KDPARTTY                                                
267320              , B.KDPARTGR                                                
267321                                                                          
267322         FROM   T01BURE B                                                 
267323                                                                          
267324         WHERE   B.IDLEGSEL = :REQU-IDLEGSEL-KEY                          
267325            AND  B.KDSTATUS = :WS-CURRENT                                 
267326            AND  B.DADELDAT = :WS-ACTIVE                                  
267328            AND  NOT EXISTS                                               
267329                                                                          
267330             (SELECT *                                                    
267331              FROM   T01ASNS                                              
267332                    ,T01BURE                                              
267333              WHERE  T01ASNS.IDLEGSEL = T01BURE.IDLEGSEL AND              
267334                     T01ASNS.KDFINDOC = T01BURE.KDFINDOC AND              
267335                     T01ASNS.KDPARTTY = T01BURE.KDPARTTY AND              
267336                     T01ASNS.KDPARTGR = T01BURE.KDPARTGR)                 
267337     END-EXEC                                                             
267338                                                                          
267339     MOVE 000100  TO GOOD-SQLCODECODES                                    
267340                                                                          
267341     EXEC SQL                                                             
267342       OPEN T01ASNS-CRS-6                                                 
267343     END-EXEC                                                             
267344                                                                          
267345     MOVE SQLCODE TO SQLCODE-WS                                           
267346     PERFORM DB2-STATUS-CHECK                                             
267347     .                                                                    
267348                                                                          
267349 DB2-FETCH-T01ASNS-CRS-6 SECTION.                                         
267350     MOVE 000100  TO GOOD-SQLCODECODES                                    
267351                                                                          
267352     EXEC SQL                                                             
267353         FETCH T01ASNS-CRS-6                                              
267354                                                                          
267355         INTO :MAP-KDFINDOC-LINE-2                                        
267356            , :MAP-KDPARTTY-LINE-2                                        
267357            , :MAP-KDPARTGR-LINE-2                                        
267358     END-EXEC                                                             
267359                                                                          
267360     MOVE SQLCODE TO SQLCODE-WS                                           
267361     PERFORM DB2-STATUS-CHECK                                             
267362     .                                                                    
267363                                                                          
267364 DB2-CLOSE-T01ASNS-CRS-6 SECTION.                                         
267365     EXEC SQL                                                             
267366       CLOSE T01ASNS-CRS-6                                                
267367     END-EXEC                                                             
267368     .                                                                    
267369                                                                          
267370 DB2-DCL-OPN-T01ASNS-CRS-6A SECTION.                                      
267371     MOVE 000100 TO GOOD-SQLCODECODES                                     
267372                                                                          
267373     EXEC SQL                                                             
267374         DECLARE T01ASNS-CRS-6A CURSOR WITH HOLD FOR                      
267375                                                                          
267376           SELECT  KDFINDOC                                               
267377                 , KDPARTTY                                               
267378                 , KDPARTGR                                               
267379                 , IDLANDX3                                               
267380                 , IDLOPNR                                                
267381                 , DAREGDAT                                               
267382                 , IDUSER                                                 
267383                                                                          
267384           FROM    T01ASNS                                                
267385                                                                          
267386           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
267390                                                                          
267391           ORDER BY KDFINDOC DESC                                         
267392                  , KDPARTTY                                              
267393                  , KDPARTGR                                              
267394                  , IDLANDX3                                              
267395                  , IDLOPNR                                               
267396     END-EXEC                                                             
267397                                                                          
267398     MOVE 000100  TO GOOD-SQLCODECODES                                    
267399                                                                          
267400     EXEC SQL                                                             
267401       OPEN T01ASNS-CRS-6A                                                
267402     END-EXEC                                                             
267403                                                                          
267404     MOVE SQLCODE TO SQLCODE-WS                                           
267405     PERFORM DB2-STATUS-CHECK                                             
267406     .                                                                    
267407                                                                          
267408 DB2-FETCH-T01ASNS-CRS-6A SECTION.                                        
267409     MOVE 000100  TO GOOD-SQLCODECODES                                    
267410                                                                          
267411     EXEC SQL                                                             
267412         FETCH T01ASNS-CRS-6A                                             
267413                                                                          
267414         INTO :MAP-KDFINDOC-LINE                                          
267415            , :MAP-KDPARTTY-LINE                                          
267416            , :MAP-KDPARTGR-LINE                                          
267417            , :MAP-IDLANDX3-LINE                                          
267418            , :MAP-IDLOPNR-LINE                                           
267419            , :MAP-DAREGDAT-LINE                                          
267420            , :MAP-IDUSER-LINE                                            
267421     END-EXEC                                                             
267422                                                                          
267423     MOVE SQLCODE TO SQLCODE-WS                                           
267424     PERFORM DB2-STATUS-CHECK                                             
267426     .                                                                    
267427                                                                          
267428 DB2-CLOSE-T01ASNS-CRS-6A SECTION.                                        
267429     EXEC SQL                                                             
267430       CLOSE T01ASNS-CRS-6A                                               
267431     END-EXEC                                                             
267432     .                                                                    
267433                                                                          
267434 DB2-DCL-OPN-T01BURE-CRS-6A SECTION.                                      
267435     MOVE 000100 TO GOOD-SQLCODECODES                                     
267436                                                                          
267437     EXEC SQL                                                             
267438         DECLARE T01BURE-CRS-6A CURSOR WITH HOLD FOR                      
267439                                                                          
267440           SELECT  KDFINDOC                                               
267441                 , KDPARTTY                                               
267442                 , KDPARTGR                                               
267443                                                                          
267444           FROM    T01BURE                                                
267445                                                                          
267446           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
267447                AND DADELDAT = '00000000'                                 
267448                AND KDSTATUS = :WS-CURRENT                                
267449*               AND KDFINDOC = :MAP-KDFINDOC-LINE                         
267450*               AND KDPARTTY = :MAP-KDPARTTY-LINE                         
267451*               AND KDPARTGR = :MAP-KDPARTGR-LINE                         
267452                                                                          
267453           ORDER BY IDLEGSEL                                              
267454                  , KDFINDOC                                              
267455                  , KDPARTTY                                              
267456                  , KDPARTGR                                              
267457     END-EXEC                                                             
267458                                                                          
267459     MOVE 000100  TO GOOD-SQLCODECODES                                    
267460                                                                          
267461     EXEC SQL                                                             
267462        OPEN T01BURE-CRS-6A                                               
267463     END-EXEC                                                             
267464                                                                          
267465     MOVE SQLCODE TO SQLCODE-WS                                           
267466     PERFORM DB2-STATUS-CHECK                                             
267467     .                                                                    
267468     EJECT                                                                
267469                                                                          
267470 DB2-FETCH-T01BURE-CRS-6A SECTION.                                        
267471     MOVE 000100  TO GOOD-SQLCODECODES                                    
267472                                                                          
267473     EXEC SQL                                                             
267474                                                                          
267475         FETCH T01BURE-CRS-6A                                             
267476                                                                          
267477         INTO :MAP-KDFINDOC-LINE                                          
267478            , :MAP-KDPARTTY-LINE                                          
267479            , :MAP-KDPARTGR-LINE                                          
267480     END-EXEC                                                             
267481                                                                          
267482     MOVE SQLCODE TO SQLCODE-WS                                           
267483     PERFORM DB2-STATUS-CHECK                                             
267485     .                                                                    
267486     EJECT                                                                
267487                                                                          
267488 DB2-CLOSE-T01BURE-CRS-6A SECTION.                                        
267489     EXEC SQL                                                             
267490        CLOSE T01BURE-CRS-6A                                              
267491     END-EXEC                                                             
267492     .                                                                    
267493     EJECT                                                                
267551                                                                          
267552 DB2-STATUS-CHECK  SECTION.                                               
267553     SET SQLCODE-IX TO 1                                                  
267554     SEARCH GOOD-SQLCODE                                                  
267555       AT END                                                             
267556          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
267557          DELIMITED BY SIZE INTO ERROR-TEXT                               
267558          CALL ABEND USING RKOD-ABEND-DB2                                 
267559       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
267560          CONTINUE                                                        
267570     END-SEARCH                                                           
267600     .                                                                    
