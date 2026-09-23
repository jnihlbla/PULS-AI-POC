000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF028400.                                                
000400 AUTHOR.         BARSHARANI BISHOYE.                                      
000500 DATE-WRITTEN.   22/06/2020.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.ROLLINGDOCNUMSERLOCATE                           
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE T01LSEL                                
001500*        THE PROGRAM READS   TABLE T01NSDO                                
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: WF0284T                                             
001900*        REQUEST:     WZ01REQU                                            
002000*                     WF0284I1                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        RESPONSE:    WZ01RESP                                            
002400*                     WF0284O1                                            
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)  VALUE 'WF028400'.             
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
004300 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
004400 77  WS-ADRESS                   PIC X(50)                                
004500                  VALUE 'CARPARTS.BILLIT.ROLLINGDOCNUMSERLOCATE'.         
004600 77  WS-PERCENTAGE               PIC X      VALUE '%'.                    
004700                                                                          
004800 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004900     88  KEYS-OK                            VALUE 'Y'.                    
005000     88  KEYS-WRONG                         VALUE 'N'.                    
005100                                                                          
005200*    --- WORK FIELDS                                                      
005300 01  WS-IDLOPNR-KEY              PIC S9(3)  VALUE ZERO    COMP-3.         
005400 01  WS-IX-MOD                   PIC S9(4)  VALUE ZERO    BINARY.         
005500 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
005600 01  WS-COUNTER-T01NSDO          PIC S9(7)  VALUE ZERO    COMP-3.         
005700 01  WS-COUNTER-FAC              PIC S9(7)  VALUE ZERO    COMP-3.         
005800 01  WS-BETEXT-2                 PIC X(2)   VALUE SPACE.                  
005900 01  WS-BETEXT-3                 PIC X(3)   VALUE SPACE.                  
006000 01  WS-BETEXT-4                 PIC X(4)   VALUE SPACE.                  
006100 01  WS-BETEXT-5                 PIC X(5)   VALUE SPACE.                  
006200 01  WS-BETEXT-6                 PIC X(6)   VALUE SPACE.                  
006300 01  WS-BETEXT-7                 PIC X(7)   VALUE SPACE.                  
006400 01  WS-BETEXT-8                 PIC X(8)   VALUE SPACE.                  
006500 01  WS-BETEXT-9                 PIC X(9)   VALUE SPACE.                  
006600 01  WS-BETEXT-10                PIC X(10)  VALUE SPACE.                  
006700 01  WS-BETEXT-11                PIC X(11)  VALUE SPACE.                  
006800                                                                          
006900 01  WS-BETEXT-EDIT.                                                      
007000     03 WS-BETEXT-OCC11 OCCURS 11 PIC X.                                  
007100                                                                          
007200*    --- MAPPING FIELDS                                                   
007300 01  MAP-IDLOPNR-LINE            PIC S9(3)  VALUE ZERO  COMP-3.           
007400 01  MAP-BETEXT-LINE             PIC X(55)  VALUE SPACE.                  
007500 01  MAP-IDFINDOC-START-LINE     PIC S9(9)  VALUE ZERO  COMP-3.           
007600 01  MAP-IDFINDOC-NEXT-LINE      PIC S9(9)  VALUE ZERO  COMP-3.           
007700 01  MAP-IDFINDOC-STOP-LINE      PIC S9(9)  VALUE ZERO  COMP-3.           
007800 01  MAP-IDUSER-LINE             PIC X(8)   VALUE SPACE.                  
007900                                                                          
008000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008100 01  GENERAL-SUBPROGRAMS.                                                 
008200     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
008300     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
008400                                                                          
008500*    --- PARAMETERS TO ABEND                                              
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
009000                                                                          
009100 01  MESSAGE-CODES.                                                       
009200     03  ERROR-CODES.                                                     
009300         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
009400         05  ERR-INVALID-FIELD       PIC X(3)   VALUE '023'.              
009500         05  ERR-MUST-BE-NUMERIC     PIC X(3)   VALUE '024'.              
009600         05  NOT-FOUND               PIC X(3)   VALUE '025'.              
009700         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
009800         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
009900         05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.              
010000     EJECT                                                                
010100                                                                          
010200*01  -COPY WZ01SUB                                                        
010300     EJECT                                                                
010400*                                                                         
010500 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
010600 01  REQU-AREA.                                                           
010700*    03 -COPY WZ01REQU                                                    
010800*    03 -COPY WF0284I1                                                    
010900     EJECT                                                                
011000                                                                          
011100 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
011200 01  RESP-AREA.                                                           
011300*    03 -COPY WZ01RESP                                                    
011400*    03 -COPY WF0284O1                                                    
011500     EJECT                                                                
011600                                                                          
011700 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
011800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011900                                                                          
012000 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
012100 01  DB2-WS.                                                              
012200     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
012300         88  CURSOR-OK                      VALUE 000.                    
012400         88  LINES-FOUND                    VALUE 000.                    
012500         88  LINES-MISSING                  VALUE 100.                    
012600         88  RESOURCE-WRONG                 VALUE 904.                    
012700     03  GOOD-SQLCODECODES.                                               
012800         05  GOOD-SQLCODE OCCURS 5                                        
012900             INDEXED BY SQLCODE-IX PIC 9(3).                              
013000                                                                          
013100 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
013200                                                                          
013300*01  -COPY T01LSEL -PRE T01LSEL-                                          
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'T01NSDO-AREA'.        
013600                                                                          
013700*01  -COPY T01NSDO -PRE T01NSDO-                                          
013800     EJECT                                                                
013900       EXEC SQL INCLUDE T01LSEL END-EXEC.                                 
014000     EJECT                                                                
014100       EXEC SQL INCLUDE T01NSDO END-EXEC.                                 
014200     EJECT                                                                
014300 LINKAGE SECTION.                                                         
014400                                                                          
014500 PROCEDURE DIVISION.                                                      
014600 MAIN SECTION.                                                            
014700                                                                          
014800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014900     IF SUB-KDRC = ZERO                                                   
015000       PERFORM A-INIT                                                     
015100       PERFORM B-CHECK-KEYS                                               
015200       IF KEYS-OK                                                         
015300         PERFORM BA-CHECK-KEY-RELATION                                    
015400       END-IF                                                             
015500       IF KEYS-OK                                                         
015600         PERFORM F-READ-SHOW-INFO                                         
015700       END-IF                                                             
015800       PERFORM S02-RETURN-RESPONSE                                        
015900     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400 A-INIT SECTION.                                                          
016600     INITIALIZE GOOD-SQLCODECODES                                         
016700     MOVE ALL '+' TO RESP-AREA                                            
016800     MOVE SPACE TO RESP-IDMSG-ERROR                                       
016900     MOVE SPACE TO RESP-IDMSG-INFO                                        
017000     MOVE SPACE TO RESP-IDELMT-ERROR                                      
017100     MOVE ZERO TO RESP-KVRADER                                            
017200     .                                                                    
017300*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
017400 B-CHECK-KEYS SECTION.                                                    
017500                                                                          
017600     MOVE YES TO KEYS-SW                                                  
018600                                                                          
018700     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
018800       MOVE NOO TO KEYS-SW                                                
018900     END-IF                                                               
019000                                                                          
019100     IF KEYS-WRONG                                                        
019200       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
019300       IF REQU-IDMSGVER NUMERIC                                           
019400         CONTINUE                                                         
019500       ELSE                                                               
019600         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
019700         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
019800       END-IF                                                             
019900       IF REQU-KDPGMACT = WS-SEARCH                                       
020000         CONTINUE                                                         
020100       ELSE                                                               
020200         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020300         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
020400       END-IF                                                             
020500       IF REQU-IDUSER = SPACE OR = ALL '+'                                
020600         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020700         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
020800       END-IF                                                             
020900     END-IF                                                               
021000     IF KEYS-OK                                                           
021100       PERFORM DB2-SELECT-T01LSEL-TAB                                     
021200       IF LINES-FOUND                                                     
021300         CONTINUE                                                         
021400       ELSE                                                               
021500         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
021600         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
021700         MOVE NOO TO KEYS-SW                                              
021800       END-IF                                                             
021900     END-IF                                                               
022000     .                                                                    
022100*** - CHECK RELATION BETWEEN REQUSTED SEARCHING KEYS                      
022200 BA-CHECK-KEY-RELATION SECTION.                                           
022300                                                                          
022400     IF  REQU-IDLOPNR-KEY > ZERO                                          
022500     AND REQU-IDLOPNR-KEY NUMERIC                                         
022600       IF (REQU-BETEXT-KEY = SPACE OR = ALL '+')                          
022700         CONTINUE                                                         
022800       ELSE                                                               
022900         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
023000         MOVE 'BETEXT'          TO RESP-IDELMT-ERROR                      
023100         MOVE NOO TO KEYS-SW                                              
023200       END-IF                                                             
023300     ELSE                                                                 
023400       IF  REQU-IDLOPNR-KEY = ZERO    OR                                  
023500           REQU-IDLOPNR-KEY = ALL '+'                                     
023600           CONTINUE                                                       
024100       ELSE                                                               
024200         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
024300         MOVE 'IDLOPNR'         TO RESP-IDELMT-ERROR                      
024400         MOVE NOO TO KEYS-SW                                              
024500       END-IF                                                             
024600     END-IF                                                               
024700     .                                                                    
024800*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
024900 F-READ-SHOW-INFO SECTION.                                                
025000                                                                          
025100     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
025200     MOVE REQU-IDLOPNR-KEY   TO RESP-IDLOPNR-KEY                          
025300     MOVE REQU-BETEXT-KEY    TO RESP-BETEXT-KEY                           
025400     MOVE REQU-IDMSGVER      TO RESP-IDMSGVER                             
025500     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
025600                                                                          
025700     PERFORM FA-READ-BASICDATA                                            
025800     .                                                                    
025900*** - CHECK WHICH REQUESTED SEARCHING KEY.                                
026000***   MOVE NUMBERS OF FOUND LINES TO RESPONS.                             
026100 FA-READ-BASICDATA SECTION.                                               
026200                                                                          
026210       IF REQU-IDLEGSEL-KEY > SPACE                                       
026211       AND REQU-IDLOPNR-KEY  = ALL '+'                                    
026212       AND REQU-BETEXT-KEY   = ALL '+'                                    
026220          PERFORM FAC-HANDLE-ALL-KEY                                      
026230       ELSE                                                               
026240*      IF REQU-IDLOPNR-KEY  > ZERO                                        
026241       IF REQU-IDLOPNR-KEY  NOT = ALL '+'                                 
026250         PERFORM FAA-HANDLE-IDLOPNR-KEY                                   
026260       ELSE                                                               
026262         IF REQU-BETEXT-KEY  NOT = ALL '+'                                
026263         OR REQU-BETEXT-KEY  NOT = SPACE                                  
026270           MOVE ZERO TO WS-COUNTER-T01NSDO                                
026280           PERFORM FAB-HANDLE-BETEXT-KEY                                  
026281         ELSE                                                             
026283           CONTINUE                                                       
026284         END-IF                                                           
026290       END-IF                                                             
026291       END-IF                                                             
026294                                                                          
026296     MOVE WS-IX TO RESP-KVRADER                                           
026297     .                                                                    
027100*** - HANDLE KEY IDLOPNR                                                  
027200 FAA-HANDLE-IDLOPNR-KEY SECTION.                                          
027300                                                                          
027311     IF REQU-IDLOPNR-KEY   =  ALL '+'                                     
027320       MOVE ZERO      TO WS-IDLOPNR-KEY                                   
027330     ELSE                                                                 
027400       MOVE REQU-IDLOPNR-KEY TO WS-IDLOPNR-KEY                            
027401     END-IF                                                               
027500     PERFORM DB2-SELECT-T01NSDO-TAB                                       
027600                                                                          
027700     IF LINES-FOUND                                                       
027800        MOVE ZERO TO WS-IX                                                
027900        PERFORM S03-MOVE-TO-RESPOND                                       
028000     ELSE                                                                 
028010        IF  WS-COUNTER-FAC > ZERO                                         
028020          CONTINUE                                                        
028030        ELSE                                                              
028100          MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                    
028101        END-IF                                                            
028200     END-IF                                                               
028300     .                                                                    
028400*** - HANDLE KEY BETEXT                                                   
028500 FAB-HANDLE-BETEXT-KEY SECTION.                                           
028600                                                                          
028700     PERFORM FABA-EDIT-BETEXT-KEY                                         
028800     PERFORM DB2-COUNT-CRS-1                                              
028900                                                                          
029000     IF WS-COUNTER-T01NSDO = ZERO                                         
029100        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
029200     ELSE                                                                 
029300        IF WS-COUNTER-T01NSDO > WS-MAX-LINES                              
029400           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
029500        END-IF                                                            
029600     END-IF                                                               
029700                                                                          
029800     IF RESP-IDMSG-ERROR = SPACE                                          
029900        PERFORM DB2-DCL-OPN-T01NSDO-CRS-1                                 
030000        PERFORM DB2-FETCH-T01NSDO-CRS-1                                   
030100        MOVE ZERO TO WS-IX                                                
030200                                                                          
030300        PERFORM UNTIL LINES-MISSING                                       
030400           PERFORM S03-MOVE-TO-RESPOND                                    
030500           PERFORM DB2-FETCH-T01NSDO-CRS-1                                
030600        END-PERFORM                                                       
030700                                                                          
030800        PERFORM DB2-CLOSE-T01NSDO-CRS-1                                   
030900     END-IF                                                               
031000     .                                                                    
031201 FAC-HANDLE-ALL-KEY SECTION.                                              
031220     PERFORM DB2-COUNT-CRS-2                                              
031230                                                                          
031240     IF WS-COUNTER-FAC = ZERO                                             
031250        MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
031260     ELSE                                                                 
031270        IF WS-COUNTER-FAC > WS-MAX-LINES                                  
031280           MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-ERROR                    
031290        END-IF                                                            
031291     END-IF                                                               
031292                                                                          
031293     IF RESP-IDMSG-ERROR = SPACE                                          
031294        PERFORM DB2-DCL-OPN-T01NSDO-CRS-2                                 
031295        PERFORM DB2-FETCH-T01NSDO-CRS-2                                   
031296        MOVE ZERO TO WS-IX                                                
031297        PERFORM UNTIL LINES-MISSING                                       
031298           PERFORM S03-MOVE-TO-RESPOND                                    
031299           PERFORM DB2-FETCH-T01NSDO-CRS-2                                
031300        END-PERFORM                                                       
031301        PERFORM DB2-CLOSE-T01NSDO-CRS-2                                   
031302     END-IF                                                               
031304     .                                                                    
031305*** - CHECK NUMBERS OF ENTERED POSITION IN BETEXT-KEY AND                 
031306***   MOVE '%' AFTER ENTERED POSITION.                                    
031310 FABA-EDIT-BETEXT-KEY SECTION.                                            
031400                                                                          
031500     MOVE SPACE TO WS-BETEXT-2                                            
031600                   WS-BETEXT-3                                            
031700                   WS-BETEXT-4                                            
031800                   WS-BETEXT-5                                            
031900                   WS-BETEXT-6                                            
032000                   WS-BETEXT-7                                            
032100                   WS-BETEXT-8                                            
032200                   WS-BETEXT-9                                            
032300                   WS-BETEXT-10                                           
032400                   WS-BETEXT-11                                           
032500                   WS-BETEXT-EDIT                                         
032600     MOVE REQU-BETEXT-KEY TO WS-BETEXT-EDIT                               
032700     MOVE +10 TO WS-IX-MOD                                                
032800                                                                          
032900     PERFORM UNTIL WS-BETEXT-OCC11(WS-IX-MOD) > SPACE                     
033000     OR WS-IX-MOD < +1                                                    
033100        SUBTRACT 1 FROM WS-IX-MOD                                         
033200     END-PERFORM                                                          
033300                                                                          
033400     ADD 1 TO WS-IX-MOD                                                   
033500     MOVE WS-PERCENTAGE TO WS-BETEXT-OCC11(WS-IX-MOD)                     
033600                                                                          
033700     IF WS-IX-MOD = +2                                                    
033800       MOVE WS-BETEXT-EDIT TO WS-BETEXT-2                                 
033900     ELSE                                                                 
034000       IF WS-IX-MOD = +3                                                  
034100         MOVE WS-BETEXT-EDIT TO WS-BETEXT-3                               
034200       ELSE                                                               
034300         IF WS-IX-MOD = +4                                                
034400           MOVE WS-BETEXT-EDIT TO WS-BETEXT-4                             
034500         ELSE                                                             
034600           IF WS-IX-MOD = +5                                              
034700             MOVE WS-BETEXT-EDIT TO WS-BETEXT-5                           
034800           ELSE                                                           
034900             IF WS-IX-MOD = +6                                            
035000               MOVE WS-BETEXT-EDIT TO WS-BETEXT-6                         
035100             ELSE                                                         
035200               IF WS-IX-MOD = +7                                          
035300                 MOVE WS-BETEXT-EDIT TO WS-BETEXT-7                       
035400               ELSE                                                       
035500                 IF WS-IX-MOD = +8                                        
035600                   MOVE WS-BETEXT-EDIT TO WS-BETEXT-8                     
035700                 ELSE                                                     
035800                   IF WS-IX-MOD = +9                                      
035900                     MOVE WS-BETEXT-EDIT TO WS-BETEXT-9                   
036000                   ELSE                                                   
036100                     IF WS-IX-MOD = +10                                   
036200                       MOVE WS-BETEXT-EDIT TO WS-BETEXT-10                
036300                     ELSE                                                 
036400                       IF WS-IX-MOD = +11                                 
036500                         MOVE WS-BETEXT-EDIT TO WS-BETEXT-11              
036600                       END-IF                                             
036700                     END-IF                                               
036800                   END-IF                                                 
036900                 END-IF                                                   
037000               END-IF                                                     
037100             END-IF                                                       
037200           END-IF                                                         
037300         END-IF                                                           
037400       END-IF                                                             
037500     END-IF                                                               
037600     .                                                                    
037700*   --- DISPATCHER SECTION START                                          
037800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
037900                                                                          
038000     MOVE 'GETARG'             TO SUB-KDFUNC                              
038100     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
038200     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
038300                                                                          
038400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
038500                                                                          
038600     IF SUB-KDRC > 0                                                      
038700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
038800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
038900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039100     END-IF                                                               
039200     .                                                                    
039300 S02-RETURN-RESPONSE SECTION.                                             
039400                                                                          
039500     MOVE 'RETURN'             TO SUB-KDFUNC                              
039600                                                                          
039700     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
039800                              - ((WS-MAX-LINES - WS-IX)                   
039900                              * LENGTH OF RESP-TABELLRAD)                 
040000                                                                          
040100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
040200                                                                          
040300     IF SUB-KDRC > 0                                                      
040400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
040500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
040600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
040700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040800     END-IF                                                               
040900     .                                                                    
041000*** - MOVE DATA TO RESPOND AND COUNT NUMBERS OF FOUND LINES.              
041100 S03-MOVE-TO-RESPOND SECTION.                                             
041200                                                                          
041300     ADD 1 TO WS-IX                                                       
041400                                                                          
041500     MOVE MAP-IDLOPNR-LINE        TO RESP-IDLOPNR-LINE(WS-IX)             
041600     MOVE MAP-BETEXT-LINE         TO RESP-BETEXT-LINE(WS-IX)              
041700     MOVE MAP-IDFINDOC-START-LINE TO                                      
041800                                   RESP-IDFINDOC-START-LINE(WS-IX)        
041900     MOVE MAP-IDFINDOC-NEXT-LINE  TO                                      
042000                                   RESP-IDFINDOC-NEXT-LINE(WS-IX)         
042100     MOVE MAP-IDFINDOC-STOP-LINE  TO                                      
042200                                   RESP-IDFINDOC-STOP-LINE(WS-IX)         
042300     MOVE MAP-IDUSER-LINE         TO RESP-IDUSER-LINE(WS-IX)              
042400     .                                                                    
042500*** - CHECK THAT THE REQUESTED LEGAL SELLER EXIST                         
042600 DB2-SELECT-T01LSEL-TAB SECTION.                                          
042700                                                                          
042800     MOVE 000100 TO GOOD-SQLCODECODES                                     
042900                                                                          
043000     EXEC SQL                                                             
043100           SELECT  BELEGRAD_1                                             
043200                                                                          
043300           INTO   :T01LSEL-BELEGRAD-1                                     
043400                                                                          
043500           FROM    T01LSEL                                                
043600                                                                          
043700           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
043800               AND KDSTATUS = :WS-CURRENT                                 
043900     END-EXEC                                                             
044000                                                                          
044100     MOVE SQLCODE TO SQLCODE-WS                                           
044200     PERFORM DB2-STATUS-CHECK                                             
044300     .                                                                    
044400 DB2-SELECT-T01NSDO-TAB SECTION.                                          
044500                                                                          
044600     MOVE 000100 TO GOOD-SQLCODECODES                                     
044700                                                                          
044800     EXEC SQL                                                             
044900         SELECT  IDLOPNR                                                  
045000               , BETEXT                                                   
045100               , IDFINDOC_START                                           
045200               , IDFINDOC_NEXT                                            
045300               , IDFINDOC_STOP                                            
045400               , IDUSER                                                   
045500                                                                          
045600         INTO    :MAP-IDLOPNR-LINE                                        
045700               , :MAP-BETEXT-LINE                                         
045800               , :MAP-IDFINDOC-START-LINE                                 
045900               , :MAP-IDFINDOC-NEXT-LINE                                  
046000               , :MAP-IDFINDOC-STOP-LINE                                  
046100               , :MAP-IDUSER-LINE                                         
046200                                                                          
046300         FROM    T01NSDO                                                  
046400                                                                          
046500         WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                           
046600              AND IDLOPNR  = :WS-IDLOPNR-KEY                              
046700                                                                          
046800     END-EXEC                                                             
046900                                                                          
047000     MOVE SQLCODE TO SQLCODE-WS                                           
047100     PERFORM DB2-STATUS-CHECK                                             
047200     .                                                                    
047300* * * * * * * * * *   - CURSOR-1 -   * * * * * * * * * * * * * * *        
047400 DB2-COUNT-CRS-1 SECTION.                                                 
047500                                                                          
047600     EXEC SQL                                                             
047700                                                                          
047800          SELECT COUNT(*)                                                 
047900                                                                          
048000          INTO  :WS-COUNTER-T01NSDO                                       
048100                                                                          
048200          FROM   T01NSDO                                                  
048300                                                                          
048400          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
048500             AND (BETEXT LIKE :WS-BETEXT-2                                
048600             OR   BETEXT LIKE :WS-BETEXT-3                                
048700             OR   BETEXT LIKE :WS-BETEXT-4                                
048800             OR   BETEXT LIKE :WS-BETEXT-5                                
048900             OR   BETEXT LIKE :WS-BETEXT-6                                
049000             OR   BETEXT LIKE :WS-BETEXT-7                                
049100             OR   BETEXT LIKE :WS-BETEXT-8                                
049200             OR   BETEXT LIKE :WS-BETEXT-9                                
049300             OR   BETEXT LIKE :WS-BETEXT-10                               
049400             OR   BETEXT LIKE :WS-BETEXT-11)                              
049500     END-EXEC                                                             
049600                                                                          
049700     MOVE 000100  TO GOOD-SQLCODECODES                                    
049800                                                                          
049900     MOVE SQLCODE TO SQLCODE-WS                                           
050000     PERFORM DB2-STATUS-CHECK                                             
050100     .                                                                    
050200 DB2-DCL-OPN-T01NSDO-CRS-1 SECTION.                                       
050300                                                                          
050400     MOVE 000100 TO GOOD-SQLCODECODES                                     
050500                                                                          
050600     EXEC SQL                                                             
050700         DECLARE T01NSDO-CRS-1 CURSOR WITH HOLD FOR                       
050800                                                                          
050900          SELECT   IDLOPNR                                                
051000                 , BETEXT                                                 
051100                 , IDFINDOC_START                                         
051200                 , IDFINDOC_NEXT                                          
051300                 , IDFINDOC_STOP                                          
051400                 , IDUSER                                                 
051500                                                                          
051600          FROM     T01NSDO                                                
051700                                                                          
051800          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
051900             AND (BETEXT LIKE :WS-BETEXT-2                                
052000             OR   BETEXT LIKE :WS-BETEXT-3                                
052100             OR   BETEXT LIKE :WS-BETEXT-4                                
052200             OR   BETEXT LIKE :WS-BETEXT-5                                
052300             OR   BETEXT LIKE :WS-BETEXT-6                                
052400             OR   BETEXT LIKE :WS-BETEXT-7                                
052500             OR   BETEXT LIKE :WS-BETEXT-8                                
052600             OR   BETEXT LIKE :WS-BETEXT-9                                
052700             OR   BETEXT LIKE :WS-BETEXT-10                               
052800             OR   BETEXT LIKE :WS-BETEXT-11)                              
052900                                                                          
053000          ORDER BY IDLEGSEL                                               
053100                 , BETEXT                                                 
053200     END-EXEC                                                             
053300                                                                          
053400     MOVE 000100  TO GOOD-SQLCODECODES                                    
053500                                                                          
053600     EXEC SQL                                                             
053700        OPEN T01NSDO-CRS-1                                                
053800     END-EXEC                                                             
053900                                                                          
054000     MOVE SQLCODE TO SQLCODE-WS                                           
054100     PERFORM DB2-STATUS-CHECK                                             
054200     .                                                                    
054300 DB2-FETCH-T01NSDO-CRS-1 SECTION.                                         
054400                                                                          
054500     MOVE 000100  TO GOOD-SQLCODECODES                                    
054600                                                                          
054700     EXEC SQL                                                             
054800                                                                          
054900         FETCH T01NSDO-CRS-1                                              
055000                                                                          
055100         INTO :MAP-IDLOPNR-LINE                                           
055200            , :MAP-BETEXT-LINE                                            
055300            , :MAP-IDFINDOC-START-LINE                                    
055400            , :MAP-IDFINDOC-NEXT-LINE                                     
055500            , :MAP-IDFINDOC-STOP-LINE                                     
055600            , :MAP-IDUSER-LINE                                            
055700     END-EXEC                                                             
055800                                                                          
055900     MOVE SQLCODE TO SQLCODE-WS                                           
056000     PERFORM DB2-STATUS-CHECK                                             
056100     .                                                                    
056200                                                                          
056300 DB2-CLOSE-T01NSDO-CRS-1 SECTION.                                         
056400                                                                          
056500     EXEC SQL                                                             
056600        CLOSE T01NSDO-CRS-1                                               
056700     END-EXEC                                                             
056800     .                                                                    
056900 DB2-COUNT-CRS-2 SECTION.                                                 
057000                                                                          
057100     EXEC SQL                                                             
057200                                                                          
057300          SELECT COUNT(*)                                                 
057400                                                                          
057500          INTO  :WS-COUNTER-FAC                                           
057600                                                                          
057700          FROM   T01NSDO                                                  
057800                                                                          
057900          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
058100     END-EXEC                                                             
058200                                                                          
058300     MOVE 000100  TO GOOD-SQLCODECODES                                    
058400                                                                          
058500     MOVE SQLCODE TO SQLCODE-WS                                           
058600     PERFORM DB2-STATUS-CHECK                                             
058700     .                                                                    
058810 DB2-DCL-OPN-T01NSDO-CRS-2 SECTION.                                       
058900                                                                          
059000     MOVE 000100 TO GOOD-SQLCODECODES                                     
059100                                                                          
059200     EXEC SQL                                                             
059300         DECLARE T01NSDO-CRS-2 CURSOR WITH HOLD FOR                       
059400                                                                          
059500          SELECT   IDLOPNR                                                
059600                 , BETEXT                                                 
059700                 , IDFINDOC_START                                         
059800                 , IDFINDOC_NEXT                                          
059900                 , IDFINDOC_STOP                                          
060000                 , IDUSER                                                 
060100                                                                          
060200          FROM     T01NSDO                                                
060300                                                                          
060400          WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                          
060600                                                                          
060700          ORDER BY IDLEGSEL                                               
060800                 , IDLOPNR                                                
060900     END-EXEC                                                             
061000                                                                          
061100     MOVE 000100  TO GOOD-SQLCODECODES                                    
061200                                                                          
061300     EXEC SQL                                                             
061400        OPEN T01NSDO-CRS-2                                                
061500     END-EXEC                                                             
061600                                                                          
061700     MOVE SQLCODE TO SQLCODE-WS                                           
061800     PERFORM DB2-STATUS-CHECK                                             
061900     .                                                                    
062000 DB2-FETCH-T01NSDO-CRS-2 SECTION.                                         
062100                                                                          
062200     MOVE 000100  TO GOOD-SQLCODECODES                                    
062300                                                                          
062400     EXEC SQL                                                             
062500                                                                          
062600         FETCH T01NSDO-CRS-2                                              
062700                                                                          
062800         INTO :MAP-IDLOPNR-LINE                                           
062900            , :MAP-BETEXT-LINE                                            
063000            , :MAP-IDFINDOC-START-LINE                                    
063100            , :MAP-IDFINDOC-NEXT-LINE                                     
063200            , :MAP-IDFINDOC-STOP-LINE                                     
063300            , :MAP-IDUSER-LINE                                            
063400     END-EXEC                                                             
063500                                                                          
063600     MOVE SQLCODE TO SQLCODE-WS                                           
063700     PERFORM DB2-STATUS-CHECK                                             
063800     .                                                                    
063900                                                                          
064000 DB2-CLOSE-T01NSDO-CRS-2 SECTION.                                         
064100                                                                          
064200     EXEC SQL                                                             
064300        CLOSE T01NSDO-CRS-2                                               
064400     END-EXEC                                                             
064410     .                                                                    
064500 DB2-STATUS-CHECK  SECTION.                                               
064600                                                                          
064700     SET SQLCODE-IX TO 1                                                  
064800     SEARCH GOOD-SQLCODE                                                  
064900       AT END                                                             
065000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
065100          DELIMITED BY SIZE INTO ERROR-TEXT                               
065200          CALL ABEND USING RKOD-ABEND-DB2                                 
065300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
065400          CONTINUE                                                        
065500     END-SEARCH                                                           
065600     .                                                                    
