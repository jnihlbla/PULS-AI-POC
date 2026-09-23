000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ040100.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/06/26.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.DAP.DISTRRULESLOCATE                                    
001000*    FUNCTION:                                                            
001100*        SELECT DATA (DEPENDING ON REQUESTED KEY) AND RETURN              
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS   TABLE TZ4DIRU                                
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: WZ0401T                                             
001800*        REQUEST:     WZ01REQU                                            
001900*                     WZ0401I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    WZ01RESP                                            
002300*                     WZ0401O1                                            
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800                                                                          
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)  VALUE 'WZ040100'.             
003100                                                                          
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
003300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003400 77  KDRC-DISPLAY                PIC Z(5).                                
003500                                                                          
003600*    --- CONSTANTS                                                        
003700 77  YES                         PIC X      VALUE 'Y'.                    
003800 77  NOO                         PIC X      VALUE 'N'.                    
003900                                                                          
004000 77  WS-SEARCH                   PIC X      VALUE 'S'.                    
004100 77  WS-MAX-LINES                PIC S9(3)  VALUE +999    COMP-3.         
004200 77  WS-ADRESS                   PIC X(50)                                
004300                           VALUE 'CARPARTS.DAP.DISTRRULESLOCATE'.         
004400                                                                          
004500 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004600     88  KEYS-OK                            VALUE 'Y'.                    
004700     88  KEYS-WRONG                         VALUE 'N'.                    
004800                                                                          
004900*    --- SWITCH WHICH KEY                                                 
005000 01  WHICH-KEY-SW                PIC S9(3)  VALUE ZERO COMP-3.            
005100                                                                          
005200*    --- WORK FIELDS                                                      
005300 01  WS-COUNTER-TZ4DIRU          PIC S9(7)  VALUE ZERO    COMP-3.         
005400 01  WS-IX                       PIC S9(9)  VALUE ZERO    BINARY.         
005500 01  WS-TYPE                     PIC S9(3)  VALUE +001 COMP-3.            
005600 01  WS-TYPE-REC                 PIC S9(3)  VALUE +002 COMP-3.            
005700 01  WS-TYPE-DEST                PIC S9(3)  VALUE +003 COMP-3.            
005800 01  WS-TYPE-REC-DEST            PIC S9(3)  VALUE +004 COMP-3.            
005900 01  WS-REC                      PIC S9(3)  VALUE +005 COMP-3.            
006000 01  WS-REC-DEST                 PIC S9(3)  VALUE +006 COMP-3.            
006100 01  WS-DEST                     PIC S9(3)  VALUE +007 COMP-3.            
006200                                                                          
006300*    --- SEARCH KEY FIELDS                                                
006400 01  REQU-IDOUTTYPE-KEY-MIN      PIC X(15)  VALUE LOW-VALUE.              
006500 01  REQU-IDOUTTYPE-KEY-MAX      PIC X(15)  VALUE HIGH-VALUE.             
006600                                                                          
006700 01  REQU-IDOUTREC-KEY-MIN       PIC X(30)  VALUE LOW-VALUE.              
006800 01  REQU-IDOUTREC-KEY-MAX       PIC X(30)  VALUE HIGH-VALUE.             
006900                                                                          
007000 01  REQU-IDOUTDEST-KEY-MIN      PIC X(60)  VALUE LOW-VALUE.              
007100 01  REQU-IDOUTDEST-KEY-MAX      PIC X(60)  VALUE HIGH-VALUE.             
007200                                                                          
007300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007400 01  GENERAL-SUBPROGRAMS.                                                 
007500     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
007600     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
007700                                                                          
007800*    --- PARAMETERS TO ABEND                                              
007900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008200 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
008300                                                                          
008400 01  MESSAGE-CODES.                                                       
008500     03  ERROR-CODES.                                                     
008600         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
008700         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '027'.              
008800         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
008900         05  ERR-SYSTEM-ERROR        PIC X(3)   VALUE '099'.              
009000     EJECT                                                                
009100                                                                          
009200*01  -COPY WZ01SUB                                                        
009300     EJECT                                                                
009400*                                                                         
009500 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
009600 01  REQU-AREA.                                                           
009700*    03 -COPY WZ01REQU                                                    
009800*    03 -COPY WZ0401I1                                                    
009900     EJECT                                                                
010000                                                                          
010100 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
010200 01  RESP-AREA.                                                           
010300*    03 -COPY WZ01RESP                                                    
010400*    03 -COPY WZ0401O1                                                    
010500     EJECT                                                                
010600                                                                          
010700 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
010800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010900                                                                          
011000 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
011100 01  DB2-WS.                                                              
011200     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
011300         88  LINES-FOUND                    VALUE 000.                    
011400         88  LINES-MISSING                  VALUE 100.                    
011500         88  RESOURCE-WRONG                 VALUE 904.                    
011600     03  GOOD-SQLCODECODES.                                               
011700         05  GOOD-SQLCODE OCCURS 5                                        
011800             INDEXED BY SQLCODE-IX PIC 9(3).                              
011900                                                                          
012000 01  FILLER                      PIC X(16)   VALUE 'TZ4DIRU-AREA'.        
012100                                                                          
012200*01  -COPY TZ4DIRU -PRE DIRU-                                             
012300                                                                          
012400       EXEC SQL INCLUDE TZ4DIRU END-EXEC.                                 
012500     EJECT                                                                
012600 LINKAGE SECTION.                                                         
012700                                                                          
012800 PROCEDURE DIVISION.                                                      
012900 MAIN SECTION.                                                            
013000                                                                          
013100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
013200     IF SUB-KDRC = ZERO                                                   
013300       PERFORM A-INIT                                                     
013400       PERFORM B-CHECK-KEYS                                               
013500       IF KEYS-OK                                                         
013600         PERFORM F-READ-SHOW-INFO                                         
013700       END-IF                                                             
013800       PERFORM S02-RETURN-RESPONSE                                        
013900     END-IF                                                               
014000                                                                          
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK                                                               
014300     .                                                                    
014400 A-INIT SECTION.                                                          
014500                                                                          
014600     INITIALIZE GOOD-SQLCODECODES                                         
014700     MOVE ALL '+' TO RESP-AREA                                            
014800     MOVE ZERO TO RESP-KVRADER                                            
014900     MOVE SPACE TO RESP-IDMSG-ERROR                                       
015000     MOVE SPACE TO RESP-IDMSG-INFO                                        
015100     MOVE SPACE TO RESP-IDELMT-ERROR                                      
015200     .                                                                    
015300*** - CHECK REQUESTED SEARCHING KEYS AND COMPULSORY FIELDS                
015400 B-CHECK-KEYS SECTION.                                                    
015500                                                                          
015600     MOVE YES TO KEYS-SW                                                  
015700                                                                          
015800     IF REQU-IDMSGVER NUMERIC                                             
015900     AND REQU-KDPGMACT = WS-SEARCH                                        
016000       IF (REQU-IDOUTTYPE-KEY = SPACE OR = ALL '+')                       
016100       AND (REQU-IDOUTREC-KEY = SPACE OR = ALL '+')                       
016200       AND (REQU-IDOUTDEST-KEY = SPACE OR = ALL '+')                      
016300         MOVE NOO TO KEYS-SW                                              
016400       END-IF                                                             
016500     ELSE                                                                 
016600       MOVE NOO TO KEYS-SW                                                
016700     END-IF                                                               
016800                                                                          
016900     IF KEYS-WRONG                                                        
017000       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
017100                                                                          
017200       IF REQU-IDMSGVER NUMERIC                                           
017300         CONTINUE                                                         
017400       ELSE                                                               
017500         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
017600         MOVE 'IDMSGVER'       TO RESP-IDELMT-ERROR                       
017700       END-IF                                                             
017800                                                                          
017900       IF REQU-KDPGMACT = WS-SEARCH                                       
018000         CONTINUE                                                         
018100       ELSE                                                               
018200         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
018300         MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                       
018400       END-IF                                                             
018500     ELSE                                                                 
018600       IF (REQU-IDUSER > SPACE AND NOT = ALL '+')                         
018700         PERFORM BA-WHICH-REQU-SEARCH-KEY                                 
018800       ELSE                                                               
018900         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
019000         MOVE 'IDUSER'         TO RESP-IDELMT-ERROR                       
019100       END-IF                                                             
019200     END-IF                                                               
019300     .                                                                    
019400                                                                          
019500*** - CHECK WHICH REQUESTED SEARCH KEY                                    
019600 BA-WHICH-REQU-SEARCH-KEY SECTION.                                        
019700                                                                          
019800     IF (REQU-IDOUTTYPE-KEY > SPACE AND NOT = ALL '+')                    
019900       PERFORM BAA-IDOUTTYPE                                              
020000     ELSE                                                                 
020100       IF (REQU-IDOUTREC-KEY > SPACE AND NOT = ALL '+')                   
020200         PERFORM BAB-IDOUTREC                                             
020300       ELSE                                                               
020400         IF (REQU-IDOUTDEST-KEY > SPACE AND NOT = ALL '+')                
020500           PERFORM BAC-IDOUTDEST                                          
020600         END-IF                                                           
020700       END-IF                                                             
020800     END-IF                                                               
020900     .                                                                    
021000                                                                          
021100 BAA-IDOUTTYPE SECTION.                                                   
021200                                                                          
021300     IF (REQU-IDOUTREC-KEY > SPACE AND NOT = ALL '+')                     
021400       IF (REQU-IDOUTDEST-KEY > SPACE AND NOT = ALL '+')                  
021500         MOVE WS-TYPE-REC-DEST TO WHICH-KEY-SW                            
021600*        PERFORM S12-MAKE-IDOUTDEST-INTERVAL                              
021700       ELSE                                                               
021800         MOVE WS-TYPE-REC TO WHICH-KEY-SW                                 
021900       END-IF                                                             
022000       PERFORM S10-MAKE-IDOUTREC-INTERVAL                                 
022100     ELSE                                                                 
022200       IF (REQU-IDOUTDEST-KEY > SPACE AND NOT = ALL '+')                  
022300         MOVE WS-TYPE-DEST TO WHICH-KEY-SW                                
022400*        PERFORM S12-MAKE-IDOUTDEST-INTERVAL                              
022500       ELSE                                                               
022600         MOVE WS-TYPE TO WHICH-KEY-SW                                     
022700       END-IF                                                             
022800     END-IF                                                               
022900                                                                          
023000     PERFORM S11-MAKE-IDOUTTYPE-INTERVAL                                  
023100     .                                                                    
023200                                                                          
023300 BAB-IDOUTREC SECTION.                                                    
023400                                                                          
023500     IF (REQU-IDOUTDEST-KEY > SPACE AND NOT = ALL '+')                    
023600       MOVE WS-REC-DEST TO WHICH-KEY-SW                                   
023700*      PERFORM S12-MAKE-IDOUTDEST-INTERVAL                                
023800     ELSE                                                                 
023900       MOVE WS-REC TO WHICH-KEY-SW                                        
024000     END-IF                                                               
024100                                                                          
024200     PERFORM S10-MAKE-IDOUTREC-INTERVAL                                   
024300     .                                                                    
024400                                                                          
024500 BAC-IDOUTDEST SECTION.                                                   
024600                                                                          
024700     MOVE WS-DEST TO WHICH-KEY-SW                                         
024800                                                                          
024900*    -- NOTE: THE IDOUTDEST INTERVAL IS CURRENTLY ONLY USED FOR           
025000*    -- SIMPLE SEARCH ON DESTINATIONS (CURSOR TYPE 7)                     
025100     PERFORM S12-MAKE-IDOUTDEST-INTERVAL                                  
025200     .                                                                    
025300*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
025400 F-READ-SHOW-INFO SECTION.                                                
025500                                                                          
025600     MOVE REQU-IDOUTTYPE-KEY TO RESP-IDOUTTYPE-KEY                        
025700     MOVE REQU-IDOUTREC-KEY  TO RESP-IDOUTREC-KEY                         
025800     MOVE REQU-IDOUTDEST-KEY TO RESP-IDOUTDEST-KEY                        
025900                                                                          
026000     IF RESP-IDMSG-ERROR = SPACE                                          
026100       PERFORM FA-READ-BASICDATA                                          
026200     END-IF                                                               
026300     .                                                                    
026400 FA-READ-BASICDATA SECTION.                                               
026500                                                                          
026600     MOVE ZERO TO WS-COUNTER-TZ4DIRU                                      
026700                                                                          
026800     EVALUATE WHICH-KEY-SW                                                
026900       WHEN WS-TYPE                                                       
027000         PERFORM FAA-TYPE                                                 
027100       WHEN WS-TYPE-REC                                                   
027200         PERFORM FAB-TYPE-REC                                             
027300       WHEN WS-TYPE-DEST                                                  
027400         PERFORM FAC-TYPE-DEST                                            
027500       WHEN WS-TYPE-REC-DEST                                              
027600         PERFORM FAD-TYPE-REC-DEST                                        
027700       WHEN WS-REC                                                        
027800         PERFORM FAE-REC                                                  
027900       WHEN WS-REC-DEST                                                   
028000         PERFORM FAF-REC-DEST                                             
028100       WHEN WS-DEST                                                       
028200         PERFORM FAG-DEST                                                 
028300     END-EVALUATE                                                         
028400                                                                          
028500     MOVE WS-IX TO RESP-KVRADER                                           
028600     .                                                                    
028700*** - HANDLE SEARCH-KEY IDOUTTYPE                                         
028800 FAA-TYPE SECTION.                                                        
028900                                                                          
029000     PERFORM DB2-COUNT-CRS-1                                              
029100                                                                          
029200     IF WS-COUNTER-TZ4DIRU = ZERO                                         
029300       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
029400     ELSE                                                                 
029500       IF WS-COUNTER-TZ4DIRU > WS-MAX-LINES                               
029600         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-INFO                       
029700       END-IF                                                             
029800     END-IF                                                               
029900                                                                          
030000     IF RESP-IDMSG-ERROR = SPACE                                          
030100       PERFORM DB2-DCL-OPN-TZ4DIRU-CRS-1                                  
030200       PERFORM DB2-FETCH-TZ4DIRU-CRS-1                                    
030300       MOVE ZERO TO WS-IX                                                 
030400                                                                          
030500       PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES                
030600         PERFORM S03-MOVE-TO-RESPOND                                      
030700         PERFORM DB2-FETCH-TZ4DIRU-CRS-1                                  
030800       END-PERFORM                                                        
030900                                                                          
031000       PERFORM DB2-CLOSE-TZ4DIRU-CRS-1                                    
031100     END-IF                                                               
031200     .                                                                    
031300*** - HANDLE SEARCH-KEY IDOUTTYPE/IDOUTREC                                
031400 FAB-TYPE-REC SECTION.                                                    
031500                                                                          
031600     PERFORM DB2-COUNT-CRS-2                                              
031700                                                                          
031800     IF WS-COUNTER-TZ4DIRU = ZERO                                         
031900       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
032000     ELSE                                                                 
032100       IF WS-COUNTER-TZ4DIRU > WS-MAX-LINES                               
032200         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-INFO                       
032300       END-IF                                                             
032400     END-IF                                                               
032500                                                                          
032600     IF RESP-IDMSG-ERROR = SPACE                                          
032700       PERFORM DB2-DCL-OPN-TZ4DIRU-CRS-2                                  
032800       PERFORM DB2-FETCH-TZ4DIRU-CRS-2                                    
032900       MOVE ZERO TO WS-IX                                                 
033000                                                                          
033100       PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES                
033200         PERFORM S03-MOVE-TO-RESPOND                                      
033300         PERFORM DB2-FETCH-TZ4DIRU-CRS-2                                  
033400       END-PERFORM                                                        
033500                                                                          
033600       PERFORM DB2-CLOSE-TZ4DIRU-CRS-2                                    
033700     END-IF                                                               
033800     .                                                                    
033900*** - HANDLE SEARCH-KEY IDOUTTYPE/IDOUTDEST                               
034000 FAC-TYPE-DEST SECTION.                                                   
034100                                                                          
034200     PERFORM DB2-COUNT-CRS-3                                              
034300                                                                          
034400     IF WS-COUNTER-TZ4DIRU = ZERO                                         
034500       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
034600     ELSE                                                                 
034700       IF WS-COUNTER-TZ4DIRU > WS-MAX-LINES                               
034800         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-INFO                       
034900       END-IF                                                             
035000     END-IF                                                               
035100                                                                          
035200     IF RESP-IDMSG-ERROR = SPACE                                          
035300       PERFORM DB2-DCL-OPN-TZ4DIRU-CRS-3                                  
035400       PERFORM DB2-FETCH-TZ4DIRU-CRS-3                                    
035500       MOVE ZERO TO WS-IX                                                 
035600                                                                          
035700       PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES                
035800         PERFORM S03-MOVE-TO-RESPOND                                      
035900         PERFORM DB2-FETCH-TZ4DIRU-CRS-3                                  
036000       END-PERFORM                                                        
036100                                                                          
036200       PERFORM DB2-CLOSE-TZ4DIRU-CRS-3                                    
036300     END-IF                                                               
036400     .                                                                    
036500*** - HANDLE SEARCH-KEY IDOUTTYPE/IDOUTREC/IDOUTDEST                      
036600 FAD-TYPE-REC-DEST SECTION.                                               
036700                                                                          
036800     PERFORM DB2-COUNT-CRS-4                                              
036900                                                                          
037000     IF WS-COUNTER-TZ4DIRU = ZERO                                         
037100       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
037200     ELSE                                                                 
037300       IF WS-COUNTER-TZ4DIRU > WS-MAX-LINES                               
037400         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-INFO                       
037500       END-IF                                                             
037600     END-IF                                                               
037700                                                                          
037800     IF RESP-IDMSG-ERROR = SPACE                                          
037900       PERFORM DB2-DCL-OPN-TZ4DIRU-CRS-4                                  
038000       PERFORM DB2-FETCH-TZ4DIRU-CRS-4                                    
038100       MOVE ZERO TO WS-IX                                                 
038200                                                                          
038300       PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES                
038400         PERFORM S03-MOVE-TO-RESPOND                                      
038500         PERFORM DB2-FETCH-TZ4DIRU-CRS-4                                  
038600       END-PERFORM                                                        
038700                                                                          
038800       PERFORM DB2-CLOSE-TZ4DIRU-CRS-4                                    
038900     END-IF                                                               
039000     .                                                                    
039100*** - HANDLE SEARCH-KEY IDOUTREC                                          
039200 FAE-REC SECTION.                                                         
039300                                                                          
039400     PERFORM DB2-COUNT-CRS-5                                              
039500                                                                          
039600     IF WS-COUNTER-TZ4DIRU = ZERO                                         
039700       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
039800     ELSE                                                                 
039900       IF WS-COUNTER-TZ4DIRU > WS-MAX-LINES                               
040000         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-INFO                       
040100       END-IF                                                             
040200     END-IF                                                               
040300                                                                          
040400     IF RESP-IDMSG-ERROR = SPACE                                          
040500       PERFORM DB2-DCL-OPN-TZ4DIRU-CRS-5                                  
040600       PERFORM DB2-FETCH-TZ4DIRU-CRS-5                                    
040700       MOVE ZERO TO WS-IX                                                 
040800                                                                          
040900       PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES                
041000         PERFORM S03-MOVE-TO-RESPOND                                      
041100         PERFORM DB2-FETCH-TZ4DIRU-CRS-5                                  
041200       END-PERFORM                                                        
041300                                                                          
041400       PERFORM DB2-CLOSE-TZ4DIRU-CRS-5                                    
041500     END-IF                                                               
041600     .                                                                    
041700*** - HANDLE SEARCH-KEYS IDOUTREC/IDOUTDEST                               
041800 FAF-REC-DEST SECTION.                                                    
041900                                                                          
042000     PERFORM DB2-COUNT-CRS-6                                              
042100                                                                          
042200     IF WS-COUNTER-TZ4DIRU = ZERO                                         
042300       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
042400     ELSE                                                                 
042500       IF WS-COUNTER-TZ4DIRU > WS-MAX-LINES                               
042600         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-INFO                       
042700       END-IF                                                             
042800     END-IF                                                               
042900                                                                          
043000     IF RESP-IDMSG-ERROR = SPACE                                          
043100       PERFORM DB2-DCL-OPN-TZ4DIRU-CRS-6                                  
043200       PERFORM DB2-FETCH-TZ4DIRU-CRS-6                                    
043300       MOVE ZERO TO WS-IX                                                 
043400                                                                          
043500       PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES                
043600         PERFORM S03-MOVE-TO-RESPOND                                      
043700         PERFORM DB2-FETCH-TZ4DIRU-CRS-6                                  
043800       END-PERFORM                                                        
043900                                                                          
044000       PERFORM DB2-CLOSE-TZ4DIRU-CRS-6                                    
044100     END-IF                                                               
044200     .                                                                    
044300*** - HANDLE SEARCH-KEYS IDOUTDEST                                        
044400 FAG-DEST SECTION.                                                        
044500                                                                          
044600     PERFORM DB2-COUNT-CRS-7                                              
044700                                                                          
044800     IF WS-COUNTER-TZ4DIRU = ZERO                                         
044900       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
045000     ELSE                                                                 
045100       IF WS-COUNTER-TZ4DIRU > WS-MAX-LINES                               
045200         MOVE ERR-TOO-MANY-LINES TO RESP-IDMSG-INFO                       
045300       END-IF                                                             
045400     END-IF                                                               
045500                                                                          
045600     IF RESP-IDMSG-ERROR = SPACE                                          
045700       PERFORM DB2-DCL-OPN-TZ4DIRU-CRS-7                                  
045800       PERFORM DB2-FETCH-TZ4DIRU-CRS-7                                    
045900       MOVE ZERO TO WS-IX                                                 
046000                                                                          
046100       PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES                
046200         PERFORM S03-MOVE-TO-RESPOND                                      
046300         PERFORM DB2-FETCH-TZ4DIRU-CRS-7                                  
046400       END-PERFORM                                                        
046500                                                                          
046600       PERFORM DB2-CLOSE-TZ4DIRU-CRS-7                                    
046700     END-IF                                                               
046800     .                                                                    
046900                                                                          
047000*   --- DISPATCHER SECTION START                                          
047100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
047200                                                                          
047300     MOVE 'GETARG'             TO SUB-KDFUNC                              
047400     MOVE WS-ADRESS            TO SUB-ADDISPABS                           
047500     MOVE LENGTH OF REQU-AREA  TO SUB-KVDLEN                              
047600                                                                          
047700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
047800                                                                          
047900     IF SUB-KDRC > 0                                                      
048000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
048100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
048200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
048300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
048400     END-IF                                                               
048500     .                                                                    
048600 S02-RETURN-RESPONSE SECTION.                                             
048700                                                                          
048800     MOVE 'RETURN'             TO SUB-KDFUNC                              
048900                                                                          
049000     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
049100                              - ((WS-MAX-LINES - WS-IX)                   
049200                              * LENGTH OF RESP-TABELLRAD)                 
049300                                                                          
049400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
049500                                                                          
049600     IF SUB-KDRC > 0                                                      
049700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
049800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
049900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
050000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050100     END-IF                                                               
050200     .                                                                    
050300*** - MOVE DATA TO RESPOND.                                               
050400 S03-MOVE-TO-RESPOND SECTION.                                             
050500                                                                          
050600     ADD 1 TO WS-IX                                                       
050700                                                                          
050800     MOVE DIRU-IDOUTTYPE     TO RESP-IDOUTTYPE-LINE(WS-IX)                
050900     MOVE DIRU-IDOUTREC-FROM TO RESP-IDOUTREC-FROM-LINE(WS-IX)            
051000     MOVE DIRU-IDOUTREC-TO   TO RESP-IDOUTREC-TO-LINE(WS-IX)              
051100     MOVE DIRU-TIREGDAT      TO RESP-TIREGDAT-LINE(WS-IX)                 
051200     MOVE DIRU-TIUPPDAT      TO RESP-TIUPPDAT-LINE(WS-IX)                 
051300     MOVE DIRU-TIANVDAT      TO RESP-TIANVDAT-LINE(WS-IX)                 
051400     MOVE DIRU-IDUSER        TO RESP-IDUSER-LINE(WS-IX)                   
051500     .                                                                    
051600                                                                          
051700 S10-MAKE-IDOUTREC-INTERVAL SECTION.                                      
051800                                                                          
051900*    -- CREATE AN INTERVAL OUT OF THE SPECIFIED IDOUTDEST VALUE           
052000     MOVE REQU-IDOUTREC-KEY TO REQU-IDOUTREC-KEY-MIN                      
052100                               REQU-IDOUTREC-KEY-MAX                      
052200*    INSPECT REQU-IDOUTREC-KEY-MIN CONVERTING SPACE TO LOW-VALUE          
052300*    INSPECT REQU-IDOUTREC-KEY-MAX CONVERTING SPACE TO HIGH-VALUE         
052400     MOVE ZERO TO TALLY                                                   
052500     INSPECT FUNCTION REVERSE(REQU-IDOUTREC-KEY)                          
052600       TALLYING TALLY FOR LEADING SPACE                                   
052700     IF TALLY > 0                                                         
052800       MOVE ALL '9' TO REQU-IDOUTREC-KEY-MAX (31 - TALLY:)                
052900     END-IF                                                               
053000     .                                                                    
053100                                                                          
053200 S11-MAKE-IDOUTTYPE-INTERVAL SECTION.                                     
053300                                                                          
053400*    -- CREATE AN INTERVAL OUT OF THE SPECIFIED IDOUTDEST VALUE           
053500     MOVE REQU-IDOUTTYPE-KEY TO REQU-IDOUTTYPE-KEY-MIN                    
053600                                REQU-IDOUTTYPE-KEY-MAX                    
053700*    INSPECT REQU-IDOUTTYPE-KEY-MIN CONVERTING SPACE TO LOW-VALUE         
053800     INSPECT REQU-IDOUTTYPE-KEY-MAX CONVERTING SPACE TO HIGH-VALUE        
053900     .                                                                    
054000                                                                          
054100 S12-MAKE-IDOUTDEST-INTERVAL SECTION.                                     
054200                                                                          
054300*    -- CREATE AN INTERVAL OUT OF THE SPECIFIED IDOUTDEST VALUE           
054400*    -- NOTE: THE INTERVAL IS CURRENTLY ONLY USED FOR SIMPLE              
054500*    -- SEARCH ON DESTINATIONS                                            
054600     MOVE REQU-IDOUTDEST-KEY TO REQU-IDOUTDEST-KEY-MIN                    
054700                                REQU-IDOUTDEST-KEY-MAX                    
054800*    INSPECT REQU-IDOUTDEST-KEY-MIN CONVERTING SPACE TO LOW-VALUE         
054900     INSPECT REQU-IDOUTDEST-KEY-MAX CONVERTING SPACE TO HIGH-VALUE        
055000     .                                                                    
055100                                                                          
055200*   --- DB2 SECTIONS                                                      
055300* * * * * * * * * * *   CURSOR-1   * * * * * * * * * * * * * * * *        
055400 DB2-COUNT-CRS-1 SECTION.                                                 
055500                                                                          
055600     EXEC SQL                                                             
055700       SELECT COUNT(*)                                                    
055800                                                                          
055900       INTO  :WS-COUNTER-TZ4DIRU                                          
056000                                                                          
056100       FROM   TZ4DIRU                                                     
056200                                                                          
056300       WHERE   IDOUTTYPE     >= :REQU-IDOUTTYPE-KEY-MIN                   
056400       AND     IDOUTTYPE     <= :REQU-IDOUTTYPE-KEY-MAX                   
056500     END-EXEC                                                             
056600                                                                          
056700     MOVE 000100  TO GOOD-SQLCODECODES                                    
056800     MOVE SQLCODE TO SQLCODE-WS                                           
056900     PERFORM DB2-STATUS-CHECK                                             
057000     .                                                                    
057100 DB2-DCL-OPN-TZ4DIRU-CRS-1 SECTION.                                       
057200                                                                          
057300     MOVE 000100 TO GOOD-SQLCODECODES                                     
057400                                                                          
057500     EXEC SQL                                                             
057600       DECLARE TZ4DIRU-CRS-1 CURSOR WITH HOLD FOR                         
057700                                                                          
057800       SELECT  IDOUTTYPE                                                  
057900             , IDOUTREC_FROM                                              
058000             , IDOUTREC_TO                                                
058100             , TIREGDAT                                                   
058200             , TIUPPDAT                                                   
058300             , TIANVDAT                                                   
058400             , IDUSER                                                     
058500                                                                          
058600       FROM    TZ4DIRU                                                    
058700                                                                          
058800       WHERE   IDOUTTYPE     >= :REQU-IDOUTTYPE-KEY-MIN                   
058900       AND     IDOUTTYPE     <= :REQU-IDOUTTYPE-KEY-MAX                   
059000                                                                          
059100       ORDER BY IDOUTTYPE                                                 
059200              , IDOUTREC_FROM                                             
059300              , IDOUTREC_TO                                               
059400     END-EXEC                                                             
059500                                                                          
059600     MOVE 000100  TO GOOD-SQLCODECODES                                    
059700                                                                          
059800     EXEC SQL                                                             
059900       OPEN TZ4DIRU-CRS-1                                                 
060000     END-EXEC                                                             
060100                                                                          
060200     MOVE SQLCODE TO SQLCODE-WS                                           
060300     PERFORM DB2-STATUS-CHECK                                             
060400     .                                                                    
060500 DB2-FETCH-TZ4DIRU-CRS-1 SECTION.                                         
060600                                                                          
060700     MOVE 000100  TO GOOD-SQLCODECODES                                    
060800                                                                          
060900     EXEC SQL                                                             
061000       FETCH TZ4DIRU-CRS-1                                                
061100                                                                          
061200       INTO :DIRU-IDOUTTYPE                                               
061300          , :DIRU-IDOUTREC-FROM                                           
061400          , :DIRU-IDOUTREC-TO                                             
061500          , :DIRU-TIREGDAT                                                
061600          , :DIRU-TIUPPDAT                                                
061700          , :DIRU-TIANVDAT                                                
061800          , :DIRU-IDUSER                                                  
061900     END-EXEC                                                             
062000                                                                          
062100     MOVE SQLCODE TO SQLCODE-WS                                           
062200     PERFORM DB2-STATUS-CHECK                                             
062300     .                                                                    
062400 DB2-CLOSE-TZ4DIRU-CRS-1 SECTION.                                         
062500                                                                          
062600     EXEC SQL                                                             
062700       CLOSE TZ4DIRU-CRS-1                                                
062800     END-EXEC                                                             
062900     .                                                                    
063000* * * * * * * * * *   - CURSOR-2 -   * * * * * * * * * * * * * * *        
063100 DB2-COUNT-CRS-2 SECTION.                                                 
063200                                                                          
063300     EXEC SQL                                                             
063400       SELECT COUNT(*)                                                    
063500                                                                          
063600       INTO  :WS-COUNTER-TZ4DIRU                                          
063700                                                                          
063800       FROM   TZ4DIRU                                                     
063900                                                                          
064000       WHERE   IDOUTTYPE     >= :REQU-IDOUTTYPE-KEY-MIN                   
064100       AND     IDOUTTYPE     <= :REQU-IDOUTTYPE-KEY-MAX                   
064200       AND     IDOUTREC_FROM >= :REQU-IDOUTREC-KEY-MIN                    
064300       AND     IDOUTREC_FROM <= :REQU-IDOUTREC-KEY-MAX                    
064400     END-EXEC                                                             
064500                                                                          
064600     MOVE 000100  TO GOOD-SQLCODECODES                                    
064700     MOVE SQLCODE TO SQLCODE-WS                                           
064800     PERFORM DB2-STATUS-CHECK                                             
064900     .                                                                    
065000 DB2-DCL-OPN-TZ4DIRU-CRS-2 SECTION.                                       
065100                                                                          
065200     MOVE 000100 TO GOOD-SQLCODECODES                                     
065300                                                                          
065400     EXEC SQL                                                             
065500       DECLARE TZ4DIRU-CRS-2 CURSOR WITH HOLD FOR                         
065600                                                                          
065700        SELECT   IDOUTTYPE                                                
065800               , IDOUTREC_FROM                                            
065900               , IDOUTREC_TO                                              
066000               , TIREGDAT                                                 
066100               , TIUPPDAT                                                 
066200               , TIANVDAT                                                 
066300               , IDUSER                                                   
066400                                                                          
066500        FROM     TZ4DIRU                                                  
066600                                                                          
066700        WHERE   IDOUTTYPE     >= :REQU-IDOUTTYPE-KEY-MIN                  
066800        AND     IDOUTTYPE     <= :REQU-IDOUTTYPE-KEY-MAX                  
066900        AND     IDOUTREC_FROM >= :REQU-IDOUTREC-KEY-MIN                   
067000        AND     IDOUTREC_FROM <= :REQU-IDOUTREC-KEY-MAX                   
067100                                                                          
067200        ORDER BY IDOUTTYPE                                                
067300               , IDOUTREC_FROM                                            
067400               , IDOUTREC_TO                                              
067500     END-EXEC                                                             
067600                                                                          
067700     MOVE 000100  TO GOOD-SQLCODECODES                                    
067800                                                                          
067900     EXEC SQL                                                             
068000       OPEN TZ4DIRU-CRS-2                                                 
068100     END-EXEC                                                             
068200                                                                          
068300     MOVE SQLCODE TO SQLCODE-WS                                           
068400     PERFORM DB2-STATUS-CHECK                                             
068500     .                                                                    
068600 DB2-FETCH-TZ4DIRU-CRS-2 SECTION.                                         
068700                                                                          
068800     MOVE 000100  TO GOOD-SQLCODECODES                                    
068900                                                                          
069000     EXEC SQL                                                             
069100       FETCH TZ4DIRU-CRS-2                                                
069200                                                                          
069300       INTO :DIRU-IDOUTTYPE                                               
069400          , :DIRU-IDOUTREC-FROM                                           
069500          , :DIRU-IDOUTREC-TO                                             
069600          , :DIRU-TIREGDAT                                                
069700          , :DIRU-TIUPPDAT                                                
069800          , :DIRU-TIANVDAT                                                
069900          , :DIRU-IDUSER                                                  
070000     END-EXEC                                                             
070100                                                                          
070200     MOVE SQLCODE TO SQLCODE-WS                                           
070300     PERFORM DB2-STATUS-CHECK                                             
070400     .                                                                    
070500 DB2-CLOSE-TZ4DIRU-CRS-2 SECTION.                                         
070600                                                                          
070700     EXEC SQL                                                             
070800       CLOSE TZ4DIRU-CRS-2                                                
070900     END-EXEC                                                             
071000     .                                                                    
071100* * * * * * * * * * *   CURSOR-3   * * * * * * * * * * * * * * * *        
071200 DB2-COUNT-CRS-3 SECTION.                                                 
071300                                                                          
071400     EXEC SQL                                                             
071500         SELECT COUNT(*)                                                  
071600                                                                          
071700         INTO  :WS-COUNTER-TZ4DIRU                                        
071800                                                                          
071900         FROM   TZ4DIRU                                                   
072000                                                                          
072100         WHERE  IDOUTTYPE     >= :REQU-IDOUTTYPE-KEY-MIN                  
072200         AND    IDOUTTYPE     <= :REQU-IDOUTTYPE-KEY-MAX                  
072300          AND  (IDOUTDEST_1  = :REQU-IDOUTDEST-KEY                        
072400           OR   IDOUTDEST_2  = :REQU-IDOUTDEST-KEY                        
072500           OR   IDOUTDEST_3  = :REQU-IDOUTDEST-KEY                        
072600           OR   IDOUTDEST_4  = :REQU-IDOUTDEST-KEY                        
072610           OR   IDOUTDEST_5  = :REQU-IDOUTDEST-KEY                        
072620           OR   IDOUTDEST_6  = :REQU-IDOUTDEST-KEY                        
072630           OR   IDOUTDEST_7  = :REQU-IDOUTDEST-KEY                        
072640           OR   IDOUTDEST_8  = :REQU-IDOUTDEST-KEY                        
072650           OR   IDOUTDEST_9  = :REQU-IDOUTDEST-KEY                        
072660           OR   IDOUTDEST_10 = :REQU-IDOUTDEST-KEY                        
072670           OR   IDOUTDEST_11 = :REQU-IDOUTDEST-KEY                        
072680           OR   IDOUTDEST_12 = :REQU-IDOUTDEST-KEY                        
072690           OR   IDOUTDEST_13 = :REQU-IDOUTDEST-KEY                        
072691           OR   IDOUTDEST_14 = :REQU-IDOUTDEST-KEY                        
072700           OR   IDOUTDEST_15 = :REQU-IDOUTDEST-KEY)                       
072800     END-EXEC                                                             
072900                                                                          
073000     MOVE 000100  TO GOOD-SQLCODECODES                                    
073100     MOVE SQLCODE TO SQLCODE-WS                                           
073200     PERFORM DB2-STATUS-CHECK                                             
073300     .                                                                    
073400 DB2-DCL-OPN-TZ4DIRU-CRS-3 SECTION.                                       
073500                                                                          
073600     MOVE 000100 TO GOOD-SQLCODECODES                                     
073700                                                                          
073800     EXEC SQL                                                             
073900       DECLARE TZ4DIRU-CRS-3 CURSOR WITH HOLD FOR                         
074000                                                                          
074100       SELECT   IDOUTTYPE                                                 
074200              , IDOUTREC_FROM                                             
074300              , IDOUTREC_TO                                               
074400              , TIREGDAT                                                  
074500              , TIUPPDAT                                                  
074600              , TIANVDAT                                                  
074700              , IDUSER                                                    
074800                                                                          
074900       FROM     TZ4DIRU                                                   
075000                                                                          
075100       WHERE  IDOUTTYPE   >= :REQU-IDOUTTYPE-KEY-MIN                      
075200        AND   IDOUTTYPE   <= :REQU-IDOUTTYPE-KEY-MAX                      
075300        AND  (IDOUTDEST_1  = :REQU-IDOUTDEST-KEY                          
075400         OR   IDOUTDEST_2  = :REQU-IDOUTDEST-KEY                          
075500         OR   IDOUTDEST_3  = :REQU-IDOUTDEST-KEY                          
075600         OR   IDOUTDEST_4  = :REQU-IDOUTDEST-KEY                          
075610         OR   IDOUTDEST_5  = :REQU-IDOUTDEST-KEY                          
075620         OR   IDOUTDEST_6  = :REQU-IDOUTDEST-KEY                          
075630         OR   IDOUTDEST_7  = :REQU-IDOUTDEST-KEY                          
075640         OR   IDOUTDEST_8  = :REQU-IDOUTDEST-KEY                          
075650         OR   IDOUTDEST_9  = :REQU-IDOUTDEST-KEY                          
075660         OR   IDOUTDEST_10 = :REQU-IDOUTDEST-KEY                          
075670         OR   IDOUTDEST_11 = :REQU-IDOUTDEST-KEY                          
075680         OR   IDOUTDEST_12 = :REQU-IDOUTDEST-KEY                          
075690         OR   IDOUTDEST_13 = :REQU-IDOUTDEST-KEY                          
075691         OR   IDOUTDEST_14 = :REQU-IDOUTDEST-KEY                          
075700         OR   IDOUTDEST_15 = :REQU-IDOUTDEST-KEY)                         
075800                                                                          
075900       ORDER BY IDOUTTYPE                                                 
076000              , IDOUTREC_FROM                                             
076100              , IDOUTREC_TO                                               
076200     END-EXEC                                                             
076300                                                                          
076400     MOVE 000100  TO GOOD-SQLCODECODES                                    
076500                                                                          
076600     EXEC SQL                                                             
076700       OPEN TZ4DIRU-CRS-3                                                 
076800     END-EXEC                                                             
076900                                                                          
077000     MOVE SQLCODE TO SQLCODE-WS                                           
077100     PERFORM DB2-STATUS-CHECK                                             
077200     .                                                                    
077300 DB2-FETCH-TZ4DIRU-CRS-3 SECTION.                                         
077400                                                                          
077500     MOVE 000100  TO GOOD-SQLCODECODES                                    
077600                                                                          
077700     EXEC SQL                                                             
077800       FETCH TZ4DIRU-CRS-3                                                
077900                                                                          
078000       INTO :DIRU-IDOUTTYPE                                               
078100          , :DIRU-IDOUTREC-FROM                                           
078200          , :DIRU-IDOUTREC-TO                                             
078300          , :DIRU-TIREGDAT                                                
078400          , :DIRU-TIUPPDAT                                                
078500          , :DIRU-TIANVDAT                                                
078600          , :DIRU-IDUSER                                                  
078700     END-EXEC                                                             
078800                                                                          
078900     MOVE SQLCODE TO SQLCODE-WS                                           
079000     PERFORM DB2-STATUS-CHECK                                             
079100     .                                                                    
079200 DB2-CLOSE-TZ4DIRU-CRS-3 SECTION.                                         
079300                                                                          
079400     EXEC SQL                                                             
079500       CLOSE TZ4DIRU-CRS-3                                                
079600     END-EXEC                                                             
079700     .                                                                    
079800* * * * * * * * * * *   CURSOR-4   * * * * * * * * * * * * * * * *        
079900 DB2-COUNT-CRS-4 SECTION.                                                 
080000                                                                          
080100     EXEC SQL                                                             
080200       SELECT COUNT(*)                                                    
080300                                                                          
080400       INTO  :WS-COUNTER-TZ4DIRU                                          
080500                                                                          
080600       FROM   TZ4DIRU                                                     
080700                                                                          
080800        WHERE    IDOUTTYPE     >= :REQU-IDOUTTYPE-KEY-MIN                 
080900        AND      IDOUTTYPE     <= :REQU-IDOUTTYPE-KEY-MAX                 
081000        AND      IDOUTREC_FROM >= :REQU-IDOUTREC-KEY-MIN                  
081100        AND      IDOUTREC_FROM <= :REQU-IDOUTREC-KEY-MAX                  
081200        AND     (IDOUTDEST_1    = :REQU-IDOUTDEST-KEY                     
081300         OR      IDOUTDEST_2    = :REQU-IDOUTDEST-KEY                     
081400         OR      IDOUTDEST_3    = :REQU-IDOUTDEST-KEY                     
081500         OR      IDOUTDEST_4    = :REQU-IDOUTDEST-KEY                     
081510         OR      IDOUTDEST_5    = :REQU-IDOUTDEST-KEY                     
081520         OR      IDOUTDEST_6    = :REQU-IDOUTDEST-KEY                     
081530         OR      IDOUTDEST_7    = :REQU-IDOUTDEST-KEY                     
081540         OR      IDOUTDEST_8    = :REQU-IDOUTDEST-KEY                     
081550         OR      IDOUTDEST_9    = :REQU-IDOUTDEST-KEY                     
081560         OR      IDOUTDEST_10   = :REQU-IDOUTDEST-KEY                     
081570         OR      IDOUTDEST_11   = :REQU-IDOUTDEST-KEY                     
081580         OR      IDOUTDEST_12   = :REQU-IDOUTDEST-KEY                     
081590         OR      IDOUTDEST_13   = :REQU-IDOUTDEST-KEY                     
081591         OR      IDOUTDEST_14   = :REQU-IDOUTDEST-KEY                     
081600         OR      IDOUTDEST_15   = :REQU-IDOUTDEST-KEY)                    
081700                                                                          
081800     END-EXEC                                                             
081900                                                                          
082000     MOVE 000100  TO GOOD-SQLCODECODES                                    
082100     MOVE SQLCODE TO SQLCODE-WS                                           
082200     PERFORM DB2-STATUS-CHECK                                             
082300     .                                                                    
082400 DB2-DCL-OPN-TZ4DIRU-CRS-4 SECTION.                                       
082500                                                                          
082600     MOVE 000100 TO GOOD-SQLCODECODES                                     
082700                                                                          
082800     EXEC SQL                                                             
082900       DECLARE TZ4DIRU-CRS-4 CURSOR WITH HOLD FOR                         
083000                                                                          
083100       SELECT   IDOUTTYPE                                                 
083200              , IDOUTREC_FROM                                             
083300              , IDOUTREC_TO                                               
083400              , TIREGDAT                                                  
083500              , TIUPPDAT                                                  
083600              , TIANVDAT                                                  
083700              , IDUSER                                                    
083800                                                                          
083900       FROM     TZ4DIRU                                                   
084000                                                                          
084100        WHERE    IDOUTTYPE     >= :REQU-IDOUTTYPE-KEY-MIN                 
084200        AND      IDOUTTYPE     <= :REQU-IDOUTTYPE-KEY-MAX                 
084300        AND      IDOUTREC_FROM >= :REQU-IDOUTREC-KEY-MIN                  
084400        AND      IDOUTREC_FROM <= :REQU-IDOUTREC-KEY-MAX                  
084500        AND     (IDOUTDEST_1    = :REQU-IDOUTDEST-KEY                     
084600         OR      IDOUTDEST_2    = :REQU-IDOUTDEST-KEY                     
084700         OR      IDOUTDEST_3    = :REQU-IDOUTDEST-KEY                     
084800         OR      IDOUTDEST_4    = :REQU-IDOUTDEST-KEY                     
084810         OR      IDOUTDEST_5    = :REQU-IDOUTDEST-KEY                     
084820         OR      IDOUTDEST_6    = :REQU-IDOUTDEST-KEY                     
084830         OR      IDOUTDEST_7    = :REQU-IDOUTDEST-KEY                     
084840         OR      IDOUTDEST_8    = :REQU-IDOUTDEST-KEY                     
084850         OR      IDOUTDEST_9    = :REQU-IDOUTDEST-KEY                     
084860         OR      IDOUTDEST_10   = :REQU-IDOUTDEST-KEY                     
084870         OR      IDOUTDEST_11   = :REQU-IDOUTDEST-KEY                     
084880         OR      IDOUTDEST_12   = :REQU-IDOUTDEST-KEY                     
084890         OR      IDOUTDEST_13   = :REQU-IDOUTDEST-KEY                     
084891         OR      IDOUTDEST_14   = :REQU-IDOUTDEST-KEY                     
084900         OR      IDOUTDEST_15   = :REQU-IDOUTDEST-KEY)                    
085000                                                                          
085100       ORDER BY IDOUTTYPE                                                 
085200              , IDOUTREC_FROM                                             
085300              , IDOUTREC_TO                                               
085400     END-EXEC                                                             
085500                                                                          
085600     MOVE 000100  TO GOOD-SQLCODECODES                                    
085700                                                                          
085800     EXEC SQL                                                             
085900       OPEN TZ4DIRU-CRS-4                                                 
086000     END-EXEC                                                             
086100                                                                          
086200     MOVE SQLCODE TO SQLCODE-WS                                           
086300     PERFORM DB2-STATUS-CHECK                                             
086400     .                                                                    
086500 DB2-FETCH-TZ4DIRU-CRS-4 SECTION.                                         
086600                                                                          
086700     MOVE 000100  TO GOOD-SQLCODECODES                                    
086800                                                                          
086900     EXEC SQL                                                             
087000       FETCH TZ4DIRU-CRS-4                                                
087100                                                                          
087200       INTO :DIRU-IDOUTTYPE                                               
087300          , :DIRU-IDOUTREC-FROM                                           
087400          , :DIRU-IDOUTREC-TO                                             
087500          , :DIRU-TIREGDAT                                                
087600          , :DIRU-TIUPPDAT                                                
087700          , :DIRU-TIANVDAT                                                
087800          , :DIRU-IDUSER                                                  
087900     END-EXEC                                                             
088000                                                                          
088100     MOVE SQLCODE TO SQLCODE-WS                                           
088200     PERFORM DB2-STATUS-CHECK                                             
088300     .                                                                    
088400 DB2-CLOSE-TZ4DIRU-CRS-4 SECTION.                                         
088500                                                                          
088600     EXEC SQL                                                             
088700       CLOSE TZ4DIRU-CRS-4                                                
088800     END-EXEC                                                             
088900     .                                                                    
089000* * * * * * * * * * *   CURSOR-5   * * * * * * * * * * * * * * * *        
089100 DB2-COUNT-CRS-5 SECTION.                                                 
089200                                                                          
089300     EXEC SQL                                                             
089400       SELECT COUNT(*)                                                    
089500                                                                          
089600       INTO  :WS-COUNTER-TZ4DIRU                                          
089700                                                                          
089800       FROM   TZ4DIRU                                                     
089900                                                                          
090000       WHERE   IDOUTREC_FROM >= :REQU-IDOUTREC-KEY-MIN                    
090100       AND     IDOUTREC_FROM <= :REQU-IDOUTREC-KEY-MAX                    
090200     END-EXEC                                                             
090300                                                                          
090400     MOVE 000100  TO GOOD-SQLCODECODES                                    
090500     MOVE SQLCODE TO SQLCODE-WS                                           
090600     PERFORM DB2-STATUS-CHECK                                             
090700     .                                                                    
090800 DB2-DCL-OPN-TZ4DIRU-CRS-5 SECTION.                                       
090900                                                                          
091000     MOVE 000100 TO GOOD-SQLCODECODES                                     
091100                                                                          
091200     EXEC SQL                                                             
091300       DECLARE TZ4DIRU-CRS-5 CURSOR WITH HOLD FOR                         
091400                                                                          
091500       SELECT   IDOUTTYPE                                                 
091600              , IDOUTREC_FROM                                             
091700              , IDOUTREC_TO                                               
091800              , TIREGDAT                                                  
091900              , TIUPPDAT                                                  
092000              , TIANVDAT                                                  
092100              , IDUSER                                                    
092200                                                                          
092300       FROM     TZ4DIRU                                                   
092400                                                                          
092500       WHERE   IDOUTREC_FROM >= :REQU-IDOUTREC-KEY-MIN                    
092600       AND     IDOUTREC_FROM <= :REQU-IDOUTREC-KEY-MAX                    
092700                                                                          
092800       ORDER BY IDOUTTYPE                                                 
092900              , IDOUTREC_FROM                                             
093000              , IDOUTREC_TO                                               
093100     END-EXEC                                                             
093200                                                                          
093300     MOVE 000100  TO GOOD-SQLCODECODES                                    
093400                                                                          
093500     EXEC SQL                                                             
093600       OPEN TZ4DIRU-CRS-5                                                 
093700     END-EXEC                                                             
093800                                                                          
093900     MOVE SQLCODE TO SQLCODE-WS                                           
094000     PERFORM DB2-STATUS-CHECK                                             
094100     .                                                                    
094200 DB2-FETCH-TZ4DIRU-CRS-5 SECTION.                                         
094300                                                                          
094400     MOVE 000100  TO GOOD-SQLCODECODES                                    
094500                                                                          
094600     EXEC SQL                                                             
094700       FETCH TZ4DIRU-CRS-5                                                
094800                                                                          
094900       INTO :DIRU-IDOUTTYPE                                               
095000          , :DIRU-IDOUTREC-FROM                                           
095100          , :DIRU-IDOUTREC-TO                                             
095200          , :DIRU-TIREGDAT                                                
095300          , :DIRU-TIUPPDAT                                                
095400          , :DIRU-TIANVDAT                                                
095500          , :DIRU-IDUSER                                                  
095600     END-EXEC                                                             
095700                                                                          
095800     MOVE SQLCODE TO SQLCODE-WS                                           
095900     PERFORM DB2-STATUS-CHECK                                             
096000     .                                                                    
096100 DB2-CLOSE-TZ4DIRU-CRS-5 SECTION.                                         
096200                                                                          
096300     EXEC SQL                                                             
096400       CLOSE TZ4DIRU-CRS-5                                                
096500     END-EXEC                                                             
096600     .                                                                    
096700* * * * * * * * * * *   CURSOR-6   * * * * * * * * * * * * * * * *        
096800 DB2-COUNT-CRS-6 SECTION.                                                 
096900                                                                          
097000     EXEC SQL                                                             
097100       SELECT COUNT(*)                                                    
097200                                                                          
097300       INTO  :WS-COUNTER-TZ4DIRU                                          
097400                                                                          
097500       FROM   TZ4DIRU                                                     
097600                                                                          
097700        WHERE    IDOUTREC_FROM >= :REQU-IDOUTREC-KEY-MIN                  
097800        AND      IDOUTREC_FROM <= :REQU-IDOUTREC-KEY-MAX                  
097900        AND     (IDOUTDEST_1    = :REQU-IDOUTDEST-KEY                     
098000         OR      IDOUTDEST_2    = :REQU-IDOUTDEST-KEY                     
098100         OR      IDOUTDEST_3    = :REQU-IDOUTDEST-KEY                     
098200         OR      IDOUTDEST_4    = :REQU-IDOUTDEST-KEY                     
098210         OR      IDOUTDEST_5    = :REQU-IDOUTDEST-KEY                     
098220         OR      IDOUTDEST_6    = :REQU-IDOUTDEST-KEY                     
098230         OR      IDOUTDEST_7    = :REQU-IDOUTDEST-KEY                     
098240         OR      IDOUTDEST_8    = :REQU-IDOUTDEST-KEY                     
098250         OR      IDOUTDEST_9    = :REQU-IDOUTDEST-KEY                     
098260         OR      IDOUTDEST_10   = :REQU-IDOUTDEST-KEY                     
098270         OR      IDOUTDEST_11   = :REQU-IDOUTDEST-KEY                     
098280         OR      IDOUTDEST_12   = :REQU-IDOUTDEST-KEY                     
098290         OR      IDOUTDEST_13   = :REQU-IDOUTDEST-KEY                     
098291         OR      IDOUTDEST_14   = :REQU-IDOUTDEST-KEY                     
098300         OR      IDOUTDEST_15   = :REQU-IDOUTDEST-KEY)                    
098400     END-EXEC                                                             
098500                                                                          
098600     MOVE 000100  TO GOOD-SQLCODECODES                                    
098700     MOVE SQLCODE TO SQLCODE-WS                                           
098800     PERFORM DB2-STATUS-CHECK                                             
098900     .                                                                    
099000 DB2-DCL-OPN-TZ4DIRU-CRS-6 SECTION.                                       
099100                                                                          
099200     MOVE 000100 TO GOOD-SQLCODECODES                                     
099300                                                                          
099400     EXEC SQL                                                             
099500       DECLARE TZ4DIRU-CRS-6 CURSOR WITH HOLD FOR                         
099600                                                                          
099700       SELECT   IDOUTTYPE                                                 
099800              , IDOUTREC_FROM                                             
099900              , IDOUTREC_TO                                               
100000              , TIREGDAT                                                  
100100              , TIUPPDAT                                                  
100200              , TIANVDAT                                                  
100300              , IDUSER                                                    
100400                                                                          
100500       FROM     TZ4DIRU                                                   
100600                                                                          
100700        WHERE    IDOUTREC_FROM >= :REQU-IDOUTREC-KEY-MIN                  
100800        AND      IDOUTREC_FROM <= :REQU-IDOUTREC-KEY-MAX                  
100900        AND     (IDOUTDEST_1    = :REQU-IDOUTDEST-KEY                     
101000         OR      IDOUTDEST_2    = :REQU-IDOUTDEST-KEY                     
101100         OR      IDOUTDEST_3    = :REQU-IDOUTDEST-KEY                     
101200         OR      IDOUTDEST_4    = :REQU-IDOUTDEST-KEY                     
101210         OR      IDOUTDEST_5    = :REQU-IDOUTDEST-KEY                     
101220         OR      IDOUTDEST_6    = :REQU-IDOUTDEST-KEY                     
101230         OR      IDOUTDEST_7    = :REQU-IDOUTDEST-KEY                     
101240         OR      IDOUTDEST_8    = :REQU-IDOUTDEST-KEY                     
101250         OR      IDOUTDEST_9    = :REQU-IDOUTDEST-KEY                     
101260         OR      IDOUTDEST_10   = :REQU-IDOUTDEST-KEY                     
101270         OR      IDOUTDEST_11   = :REQU-IDOUTDEST-KEY                     
101280         OR      IDOUTDEST_12   = :REQU-IDOUTDEST-KEY                     
101290         OR      IDOUTDEST_13   = :REQU-IDOUTDEST-KEY                     
101291         OR      IDOUTDEST_14   = :REQU-IDOUTDEST-KEY                     
101300         OR      IDOUTDEST_15   = :REQU-IDOUTDEST-KEY)                    
101400                                                                          
101500       ORDER BY IDOUTTYPE                                                 
101600              , IDOUTREC_FROM                                             
101700              , IDOUTREC_TO                                               
101800     END-EXEC                                                             
101900                                                                          
102000     MOVE 000100  TO GOOD-SQLCODECODES                                    
102100                                                                          
102200     EXEC SQL                                                             
102300       OPEN TZ4DIRU-CRS-6                                                 
102400     END-EXEC                                                             
102500                                                                          
102600     MOVE SQLCODE TO SQLCODE-WS                                           
102700     PERFORM DB2-STATUS-CHECK                                             
102800     .                                                                    
102900 DB2-FETCH-TZ4DIRU-CRS-6 SECTION.                                         
103000                                                                          
103100     MOVE 000100  TO GOOD-SQLCODECODES                                    
103200                                                                          
103300     EXEC SQL                                                             
103400       FETCH TZ4DIRU-CRS-6                                                
103500                                                                          
103600       INTO :DIRU-IDOUTTYPE                                               
103700          , :DIRU-IDOUTREC-FROM                                           
103800          , :DIRU-IDOUTREC-TO                                             
103900          , :DIRU-TIREGDAT                                                
104000          , :DIRU-TIUPPDAT                                                
104100          , :DIRU-TIANVDAT                                                
104200          , :DIRU-IDUSER                                                  
104300     END-EXEC                                                             
104400                                                                          
104500     MOVE SQLCODE TO SQLCODE-WS                                           
104600     PERFORM DB2-STATUS-CHECK                                             
104700     .                                                                    
104800 DB2-CLOSE-TZ4DIRU-CRS-6 SECTION.                                         
104900                                                                          
105000     EXEC SQL                                                             
105100       CLOSE TZ4DIRU-CRS-6                                                
105200     END-EXEC                                                             
105300     .                                                                    
105400* * * * * * * * * * *   CURSOR-7   * * * * * * * * * * * * * * * *        
105500 DB2-COUNT-CRS-7 SECTION.                                                 
105600                                                                          
105700     EXEC SQL                                                             
105800       SELECT COUNT(*)                                                    
105900                                                                          
106000       INTO  :WS-COUNTER-TZ4DIRU                                          
106100                                                                          
106200       FROM   TZ4DIRU                                                     
106300                                                                          
106400       WHERE  (IDOUTDEST_1  >= :REQU-IDOUTDEST-KEY-MIN                    
106500         AND   IDOUTDEST_1  <= :REQU-IDOUTDEST-KEY-MAX )                  
106600       OR     (IDOUTDEST_2  >= :REQU-IDOUTDEST-KEY-MIN                    
106700         AND   IDOUTDEST_2  <= :REQU-IDOUTDEST-KEY-MAX )                  
106800       OR     (IDOUTDEST_3  >= :REQU-IDOUTDEST-KEY-MIN                    
106900         AND   IDOUTDEST_3  <= :REQU-IDOUTDEST-KEY-MAX )                  
107000       OR     (IDOUTDEST_4  >= :REQU-IDOUTDEST-KEY-MIN                    
107100         AND   IDOUTDEST_4  <= :REQU-IDOUTDEST-KEY-MAX )                  
107200       OR     (IDOUTDEST_5  >= :REQU-IDOUTDEST-KEY-MIN                    
107300         AND   IDOUTDEST_5  <= :REQU-IDOUTDEST-KEY-MAX )                  
107310       OR     (IDOUTDEST_6  >= :REQU-IDOUTDEST-KEY-MIN                    
107320         AND   IDOUTDEST_6  <= :REQU-IDOUTDEST-KEY-MAX )                  
107330       OR     (IDOUTDEST_7  >= :REQU-IDOUTDEST-KEY-MIN                    
107340         AND   IDOUTDEST_7  <= :REQU-IDOUTDEST-KEY-MAX )                  
107350       OR     (IDOUTDEST_8  >= :REQU-IDOUTDEST-KEY-MIN                    
107360         AND   IDOUTDEST_8  <= :REQU-IDOUTDEST-KEY-MAX )                  
107370       OR     (IDOUTDEST_9  >= :REQU-IDOUTDEST-KEY-MIN                    
107380         AND   IDOUTDEST_9  <= :REQU-IDOUTDEST-KEY-MAX )                  
107390       OR     (IDOUTDEST_10 >= :REQU-IDOUTDEST-KEY-MIN                    
107391         AND   IDOUTDEST_10 <= :REQU-IDOUTDEST-KEY-MAX )                  
107392       OR     (IDOUTDEST_11 >= :REQU-IDOUTDEST-KEY-MIN                    
107393         AND   IDOUTDEST_11 <= :REQU-IDOUTDEST-KEY-MAX )                  
107394       OR     (IDOUTDEST_12 >= :REQU-IDOUTDEST-KEY-MIN                    
107395         AND   IDOUTDEST_12 <= :REQU-IDOUTDEST-KEY-MAX )                  
107396       OR     (IDOUTDEST_13 >= :REQU-IDOUTDEST-KEY-MIN                    
107397         AND   IDOUTDEST_13 <= :REQU-IDOUTDEST-KEY-MAX )                  
107398       OR     (IDOUTDEST_14 >= :REQU-IDOUTDEST-KEY-MIN                    
107399         AND   IDOUTDEST_14 <= :REQU-IDOUTDEST-KEY-MAX )                  
107400       OR     (IDOUTDEST_15 >= :REQU-IDOUTDEST-KEY-MIN                    
107401         AND   IDOUTDEST_15 <= :REQU-IDOUTDEST-KEY-MAX )                  
107410     END-EXEC                                                             
107500                                                                          
107600     MOVE 000100  TO GOOD-SQLCODECODES                                    
107700     MOVE SQLCODE TO SQLCODE-WS                                           
107800     PERFORM DB2-STATUS-CHECK                                             
107900     .                                                                    
108000 DB2-DCL-OPN-TZ4DIRU-CRS-7 SECTION.                                       
108100                                                                          
108200     MOVE 000100 TO GOOD-SQLCODECODES                                     
108300                                                                          
108400     EXEC SQL                                                             
108500       DECLARE TZ4DIRU-CRS-7 CURSOR WITH HOLD FOR                         
108600                                                                          
108700       SELECT   IDOUTTYPE                                                 
108800              , IDOUTREC_FROM                                             
108900              , IDOUTREC_TO                                               
109000              , TIREGDAT                                                  
109100              , TIUPPDAT                                                  
109200              , TIANVDAT                                                  
109300              , IDUSER                                                    
109400                                                                          
109500       FROM     TZ4DIRU                                                   
109600                                                                          
109700       WHERE  (IDOUTDEST_1  >= :REQU-IDOUTDEST-KEY-MIN                    
109800         AND   IDOUTDEST_1  <= :REQU-IDOUTDEST-KEY-MAX )                  
109900       OR     (IDOUTDEST_2  >= :REQU-IDOUTDEST-KEY-MIN                    
110000         AND   IDOUTDEST_2  <= :REQU-IDOUTDEST-KEY-MAX )                  
110100       OR     (IDOUTDEST_3  >= :REQU-IDOUTDEST-KEY-MIN                    
110200         AND   IDOUTDEST_3  <= :REQU-IDOUTDEST-KEY-MAX )                  
110300       OR     (IDOUTDEST_4  >= :REQU-IDOUTDEST-KEY-MIN                    
110400         AND   IDOUTDEST_4  <= :REQU-IDOUTDEST-KEY-MAX )                  
110500       OR     (IDOUTDEST_5  >= :REQU-IDOUTDEST-KEY-MIN                    
110600         AND   IDOUTDEST_5  <= :REQU-IDOUTDEST-KEY-MAX )                  
110610       OR     (IDOUTDEST_6  >= :REQU-IDOUTDEST-KEY-MIN                    
110620         AND   IDOUTDEST_6  <= :REQU-IDOUTDEST-KEY-MAX )                  
110630       OR     (IDOUTDEST_7  >= :REQU-IDOUTDEST-KEY-MIN                    
110640         AND   IDOUTDEST_7  <= :REQU-IDOUTDEST-KEY-MAX )                  
110650       OR     (IDOUTDEST_8  >= :REQU-IDOUTDEST-KEY-MIN                    
110660         AND   IDOUTDEST_8  <= :REQU-IDOUTDEST-KEY-MAX )                  
110670       OR     (IDOUTDEST_9  >= :REQU-IDOUTDEST-KEY-MIN                    
110680         AND   IDOUTDEST_9  <= :REQU-IDOUTDEST-KEY-MAX )                  
110690       OR     (IDOUTDEST_10 >= :REQU-IDOUTDEST-KEY-MIN                    
110691         AND   IDOUTDEST_10 <= :REQU-IDOUTDEST-KEY-MAX )                  
110692       OR     (IDOUTDEST_11 >= :REQU-IDOUTDEST-KEY-MIN                    
110693         AND   IDOUTDEST_11 <= :REQU-IDOUTDEST-KEY-MAX )                  
110694       OR     (IDOUTDEST_12 >= :REQU-IDOUTDEST-KEY-MIN                    
110695         AND   IDOUTDEST_12 <= :REQU-IDOUTDEST-KEY-MAX )                  
110696       OR     (IDOUTDEST_13 >= :REQU-IDOUTDEST-KEY-MIN                    
110697         AND   IDOUTDEST_13 <= :REQU-IDOUTDEST-KEY-MAX )                  
110698       OR     (IDOUTDEST_14 >= :REQU-IDOUTDEST-KEY-MIN                    
110699         AND   IDOUTDEST_14 <= :REQU-IDOUTDEST-KEY-MAX )                  
110700       OR     (IDOUTDEST_15 >= :REQU-IDOUTDEST-KEY-MIN                    
110701         AND   IDOUTDEST_15 <= :REQU-IDOUTDEST-KEY-MAX )                  
110710                                                                          
110800       ORDER BY IDOUTTYPE                                                 
110900              , IDOUTREC_FROM                                             
111000              , IDOUTREC_TO                                               
111100     END-EXEC                                                             
111200                                                                          
111300     MOVE 000100  TO GOOD-SQLCODECODES                                    
111400                                                                          
111500     EXEC SQL                                                             
111600       OPEN TZ4DIRU-CRS-7                                                 
111700     END-EXEC                                                             
111800                                                                          
111900     MOVE SQLCODE TO SQLCODE-WS                                           
112000     PERFORM DB2-STATUS-CHECK                                             
112100     .                                                                    
112200 DB2-FETCH-TZ4DIRU-CRS-7 SECTION.                                         
112300                                                                          
112400     MOVE 000100  TO GOOD-SQLCODECODES                                    
112500                                                                          
112600     EXEC SQL                                                             
112700       FETCH TZ4DIRU-CRS-7                                                
112800                                                                          
112900       INTO :DIRU-IDOUTTYPE                                               
113000          , :DIRU-IDOUTREC-FROM                                           
113100          , :DIRU-IDOUTREC-TO                                             
113200          , :DIRU-TIREGDAT                                                
113300          , :DIRU-TIUPPDAT                                                
113400          , :DIRU-TIANVDAT                                                
113500          , :DIRU-IDUSER                                                  
113600     END-EXEC                                                             
113700                                                                          
113800     MOVE SQLCODE TO SQLCODE-WS                                           
113900     PERFORM DB2-STATUS-CHECK                                             
114000     .                                                                    
114100 DB2-CLOSE-TZ4DIRU-CRS-7 SECTION.                                         
114200                                                                          
114300     EXEC SQL                                                             
114400       CLOSE TZ4DIRU-CRS-7                                                
114500     END-EXEC                                                             
114600     .                                                                    
114700                                                                          
114800 DB2-STATUS-CHECK  SECTION.                                               
114900                                                                          
115000     SET SQLCODE-IX TO 1                                                  
115100     SEARCH GOOD-SQLCODE                                                  
115200       AT END                                                             
115300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
115400          DELIMITED BY SIZE INTO ERROR-TEXT                               
115500          CALL ABEND USING RKOD-ABEND-DB2                                 
115600       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
115700          CONTINUE                                                        
115800     END-SEARCH                                                           
115900     .                                                                    
