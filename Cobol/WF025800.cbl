000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF025800.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/03/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.PAYTERMMAINTENANCE                               
001000*    FUNCTION:                                                            
001100*        READ/UPDATE PAYMENT TERM TABLE T01PATE DEPENDING                 
001200*        ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)                     
001300*        KDPGMACT = 'S' READ                                              
001400*        KDPGMACT = 'U' UPDATE                                            
001410*        KDPGMACT = 'I' INSERT                                            
001500*                                                                         
001600*        THE PROGRAM READS   TABLE T01LSEL                                
001700*        THE PROGRAM UPDATES TABLE T01PATE                                
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: WF0258U                                             
002100*        REQUEST:     WF0258I1                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    WF0258O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'WF025800'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004100 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400*    --- CONSTANT WORK FIELDS                                             
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700 77  WS-ADRESS                   PIC X(50)                                
004800                       VALUE 'CARPARTS.BILLIT.PAYTERMMAINTENANCE'.        
004900 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005000                                                                          
005100 77  KEYS-SW                     PIC X       VALUE SPACE.                 
005200     88  KEYS-OK                             VALUE 'Y'.                   
005300     88  KEYS-WRONG                          VALUE 'N'.                   
005400                                                                          
005500 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
005600     88  ACT-CODE-VALID                      VALUE 'S', 'U', 'I'.         
005700     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
005800     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
005810     88  ACT-CODE-INSERT                     VALUE 'I'.                   
005900                                                                          
006000*    --- OTHER MAPPING FIELDS                                             
006100 01  MAP-DAREGDAT                PIC X(8)    VALUE SPACE.                 
006200 01  MAP-DAUPPDAT                PIC X(8)    VALUE SPACE.                 
006300                                                                          
006400*    --- WORK-FIELDS                                                      
006500 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
006600                                                                          
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007100     SKIP3                                                                
007200                                                                          
007300*    --- PARAMETERS TO ABEND                                              
007400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007800                                                                          
007900 01  MESSAGE-CODES.                                                       
008000     03  ERROR-CODES.                                                     
008100         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
008110         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
008200         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
008300         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
008400         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
008500         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
008600         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
008700         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
008800     03  INFO-CODES.                                                      
008900         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
008910         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009200     SKIP3                                                                
009300 01  -COPY WZ01SUB                                                        
009400     EJECT                                                                
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MAPPING-AREA'.        
009700     SKIP3                                                                
009800 01  -COPY WF0258O1  -PRE MAP-                                            
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010100     SKIP3                                                                
010200 01  REQU-AREA.                                                           
010300*    03  -COPY WZ01REQU                                                   
010400*    03  -COPY WF0258I1                                                   
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010700     SKIP3                                                                
010800 01  RESP-AREA.                                                           
010900*    03  -COPY WZ01RESP                                                   
011000*    03  -COPY WF0258O1                                                   
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
011300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011400                                                                          
011500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
011600 01  DB2-WS.                                                              
011700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011800         88  CURSOR-OK                       VALUE 000.                   
011900         88  LINES-FOUND                     VALUE 000.                   
012000         88  LINES-MISSING                   VALUE 100.                   
012100         88  RESOURCE-WRONG                  VALUE 904.                   
012200     03  GOOD-SQLCODECODES.                                               
012300         05  GOOD-SQLCODE OCCURS 5                                        
012400             INDEXED BY SQLCODE-IX PIC 9(3).                              
012500                                                                          
012600 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
012700*01  -COPY T01LSEL -PRE T01LSEL-                                          
012800                                                                          
012900 01  FILLER                      PIC X(16)   VALUE 'T01PATE-AREA'.        
013000*01  -COPY T01PATE -PRE T01PATE-                                          
013100                                                                          
013200     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
013300                                                                          
013400     EXEC SQL INCLUDE T01PATE END-EXEC.                                   
013500                                                                          
013600 LINKAGE SECTION.                                                         
013700                                                                          
013800 PROCEDURE DIVISION.                                                      
013900 MAIN SECTION.                                                            
014000                                                                          
014100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014200     IF SUB-KDRC = 0                                                      
014300       PERFORM A-INIT                                                     
014400       PERFORM B-CHECK-KEYS                                               
014500       IF KEYS-OK                                                         
014600         PERFORM C-READ-SHOW-INFO                                         
014700       END-IF                                                             
014800       IF KEYS-WRONG                                                      
014900         PERFORM S04-MOVE-TO-RESPOND-MISSING                              
015000       END-IF                                                             
015100       PERFORM S02-RETURN-RESPONSE                                        
015200     END-IF                                                               
015300                                                                          
015400     MOVE ZERO TO RETURN-CODE                                             
015500     GOBACK                                                               
015600     .                                                                    
015700     EJECT                                                                
015800                                                                          
015900 A-INIT SECTION.                                                          
016000     INITIALIZE GOOD-SQLCODECODES                                         
016100     MOVE ALL '+' TO RESP-AREA                                            
016200     MOVE SPACE TO RESP-IDMSG-ERROR                                       
016300     MOVE SPACE TO RESP-IDMSG-INFO                                        
016400     MOVE SPACE TO RESP-IDELMT-ERROR                                      
016500     INITIALIZE MAP-RESP-WF0258O1                                         
016600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
016700     .                                                                    
016800     EJECT                                                                
016900                                                                          
017000*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
017100 B-CHECK-KEYS SECTION.                                                    
017200     MOVE YES TO KEYS-SW                                                  
017300     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
017400                                                                          
017500     IF REQU-IDMSGVER NUMERIC                                             
017600       IF REQU-IDLEGSEL-KEY > SPACE                                       
017700       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
017800       AND REQU-IDSPRAK-KEY > SPACE                                       
017900       AND REQU-IDSPRAK-KEY NOT = ALL '+'                                 
018000       AND REQU-KDBETALV-KEY > SPACE                                      
018100       AND REQU-KDBETALV-KEY NOT = ALL '+'                                
018200       AND ACT-CODE-VALID                                                 
018300         CONTINUE                                                         
018400       ELSE                                                               
018500         MOVE NOO TO KEYS-SW                                              
018600       END-IF                                                             
018700     ELSE                                                                 
018800       MOVE NOO TO KEYS-SW                                                
018900     END-IF                                                               
019000                                                                          
019100     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
019200       MOVE NOO TO KEYS-SW                                                
019300     END-IF                                                               
019400                                                                          
019500     IF KEYS-WRONG                                                        
019600       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
019700       IF REQU-IDMSGVER NUMERIC                                           
019800         CONTINUE                                                         
019900       ELSE                                                               
020000         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020100         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
020200       END-IF                                                             
020300       IF ACT-CODE-VALID                                                  
020400         CONTINUE                                                         
020500       ELSE                                                               
020600         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
020700         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
020800       END-IF                                                             
020900       IF REQU-IDUSER = SPACE OR = ALL '+'                                
021000         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
021100         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
021200       END-IF                                                             
021300     END-IF                                                               
021400                                                                          
021500     IF KEYS-OK                                                           
021600       PERFORM DB2-SELECT-T01LSEL-TAB                                     
021700       IF LINES-FOUND                                                     
021800         CONTINUE                                                         
021900       ELSE                                                               
022000         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
022100         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
022200         MOVE NOO TO KEYS-SW                                              
022300       END-IF                                                             
022400     END-IF                                                               
022500     .                                                                    
022600     EJECT                                                                
022700                                                                          
022800*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
022900 C-READ-SHOW-INFO SECTION.                                                
023000     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
023100     MOVE REQU-IDSPRAK-KEY   TO RESP-IDSPRAK-KEY                          
023200     MOVE REQU-KDBETALV-KEY  TO RESP-KDBETALV-KEY                         
023300     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
023400                                                                          
023500     PERFORM CA-READ-BASICDATA                                            
023600     .                                                                    
023700                                                                          
023800*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
023900 CA-READ-BASICDATA SECTION.                                               
024000     IF ACT-CODE-SEARCH                                                   
024100       PERFORM CAA-SEARCH-T01PATE                                         
024200     ELSE                                                                 
024300       IF ACT-CODE-UPDATE                                                 
024400         PERFORM CAB-UPDATE-T01PATE                                       
024410       ELSE                                                               
024420         IF ACT-CODE-INSERT                                               
024430           PERFORM CAC-INSERT-T01PATE                                     
024440         END-IF                                                           
024500       END-IF                                                             
024600     END-IF                                                               
024700     .                                                                    
024800                                                                          
024900*** - SEARCH FOR RIGHT PAYMENT TERM                                       
025000 CAA-SEARCH-T01PATE SECTION.                                              
025100     PERFORM DB2-SELECT-T01PATE-TAB                                       
025200                                                                          
025300     IF LINES-FOUND                                                       
025400       PERFORM S03-MOVE-TO-RESPOND-OK                                     
025500     ELSE                                                                 
025600       MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                                
025700       MOVE 'KDBETALV' TO RESP-IDELMT-ERROR                               
025800       MOVE NOO        TO KEYS-SW                                         
025900     END-IF                                                               
026000     .                                                                    
026100                                                                          
026200*** - UPDATE PAYMENT TREMS                                                
026300 CAB-UPDATE-T01PATE SECTION.                                              
026400     PERFORM DB2-SELECT-T01PATE-TAB                                       
026500                                                                          
026600     IF LINES-FOUND                                                       
026700       PERFORM CABA-CHECK-UPD-DATA                                        
026800       IF RESP-IDMSG-ERROR = SPACE                                        
026900                                                                          
027000         IF MAP-DAREGDAT = '00000000'                                     
027100           PERFORM DB2-UPDATE-T01PATE-TAB2                                
027200         ELSE                                                             
027300           PERFORM DB2-UPDATE-T01PATE-TAB                                 
027400         END-IF                                                           
027500         PERFORM S03-MOVE-TO-RESPOND-OK                                   
027600         MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                            
027700       END-IF                                                             
027800     ELSE                                                                 
027900       MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                                
028000       MOVE 'KDBETALV' TO RESP-IDELMT-ERROR                               
028100       MOVE NOO        TO KEYS-SW                                         
028200     END-IF                                                               
028300     .                                                                    
028400                                                                          
028500*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON PAYMENT TERM                
028600 CABA-CHECK-UPD-DATA SECTION.                                             
028700     IF  REQU-BEBETVIL > SPACE                                            
028800     AND REQU-BEBETVIL NOT = ALL '+'                                      
028900       CONTINUE                                                           
029000     ELSE                                                                 
029100       MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
029200       MOVE 'BEBETVIL' TO RESP-IDELMT-ERROR                               
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029610 CAC-INSERT-T01PATE SECTION.                                              
029611     IF T01LSEL-FLCUSUPD = 'J'                                            
029620       PERFORM DB2-SELECT-T01PATE-TAB                                     
029640       IF LINES-FOUND                                                     
029641         MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
029670         MOVE NOO        TO KEYS-SW                                       
029697       ELSE                                                               
029698         PERFORM CACA-CHECK-INS-DATA                                      
029699         IF RESP-IDMSG-ERROR = SPACE                                      
029704           PERFORM DB2-INSERT-T01PATE                                     
029706           PERFORM S03-MOVE-TO-RESPOND-OK                                 
029707           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
029709         END-IF                                                           
029712       END-IF                                                             
029713     ELSE                                                                 
029714       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
029715       MOVE NOO        TO KEYS-SW                                         
029716     END-IF                                                               
029717     .                                                                    
029718                                                                          
029719*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON PAYMENT TERM                
029720 CACA-CHECK-INS-DATA SECTION.                                             
029722     IF  REQU-BEBETVIL > SPACE                                            
029723     AND REQU-BEBETVIL NOT = ALL '+'                                      
029724       CONTINUE                                                           
029725     ELSE                                                                 
029726       MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
029727       MOVE 'BEBETVIL' TO RESP-IDELMT-ERROR                               
029728     END-IF                                                               
029729     IF  REQU-IDLEGSEL-KEY > SPACE                                        
029730     AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                  
029731       CONTINUE                                                           
029732     ELSE                                                                 
029733       MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
029734       MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                               
029735     END-IF                                                               
029736     IF  REQU-KDBETALV-KEY > SPACE                                        
029737     AND REQU-KDBETALV-KEY NOT = ALL '+'                                  
029738       CONTINUE                                                           
029739     ELSE                                                                 
029740       MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
029741       MOVE 'KDBETALV' TO RESP-IDELMT-ERROR                               
029742     END-IF                                                               
029743     IF  REQU-IDSPRAK-KEY > SPACE                                         
029744     AND REQU-IDSPRAK-KEY NOT = ALL '+'                                   
029745       IF REQU-IDSPRAK-KEY = 'EN'                                         
029746         CONTINUE                                                         
029747       ELSE                                                               
029748         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
029749         MOVE 'IDSPRAK' TO RESP-IDELMT-ERROR                              
029750       END-IF                                                             
029751     ELSE                                                                 
029752       MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
029753       MOVE 'IDSPRAK' TO RESP-IDELMT-ERROR                                
029754     END-IF                                                               
029755     .                                                                    
029756     EJECT                                                                
029757                                                                          
029760*    --- DISPATCHER SECTIONS                                              
029800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
029900     MOVE 'GETARG'                   TO SUB-KDFUNC                        
030000     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
030100     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
030200                                                                          
030300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
030400                                                                          
030500     IF SUB-KDRC > 0                                                      
030600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
030700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
030800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031000     END-IF                                                               
031100     .                                                                    
031200                                                                          
031300 S02-RETURN-RESPONSE SECTION.                                             
031400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
031500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
031600                                                                          
031700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
031800                                                                          
031900     IF SUB-KDRC > 0                                                      
032000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
032100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
032200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
032300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032400     END-IF                                                               
032500     .                                                                    
032600                                                                          
032700*    --- MOVE TO OUTPUT SECTIONS                                          
032800 S03-MOVE-TO-RESPOND-OK SECTION.                                          
032900     IF ACT-CODE-SEARCH                                                   
033000       MOVE MAP-RESP-BEBETVIL TO RESP-BEBETVIL                            
033100       MOVE MAP-DAREGDAT      TO RESP-DAREGDAT                            
033200       MOVE MAP-DAUPPDAT      TO RESP-DAUPPDAT                            
033300       MOVE MAP-RESP-IDUSER   TO RESP-IDUSER                              
033400     ELSE                                                                 
033500       IF ACT-CODE-UPDATE                                                 
033600         MOVE REQU-BEBETVIL     TO RESP-BEBETVIL                          
033700         IF MAP-DAREGDAT = '00000000'                                     
033800           MOVE WS-CURRENT-DATE TO RESP-DAREGDAT                          
033900         ELSE                                                             
034000           MOVE MAP-DAREGDAT    TO RESP-DAREGDAT                          
034100         END-IF                                                           
034200         MOVE WS-CURRENT-DATE   TO RESP-DAUPPDAT                          
034300         MOVE REQU-IDUSER       TO RESP-IDUSER                            
034400       ELSE                                                               
034410         IF ACT-CODE-INSERT                                               
034420           MOVE REQU-BEBETVIL     TO RESP-BEBETVIL                        
034440           MOVE WS-CURRENT-DATE   TO RESP-DAREGDAT                        
034480           MOVE '00000000'        TO RESP-DAUPPDAT                        
034490           MOVE REQU-IDUSER       TO RESP-IDUSER                          
034491         END-IF                                                           
034492       END-IF                                                             
034500     END-IF                                                               
034600     .                                                                    
034700                                                                          
034800*    --- MOVE TO OUTPUT SECTIONS WHEN NOT-FOUND                           
034900 S04-MOVE-TO-RESPOND-MISSING SECTION.                                     
035000     MOVE SPACE TO RESP-BEBETVIL                                          
035100                   RESP-IDUSER                                            
035200     MOVE ZERO  TO RESP-DAREGDAT                                          
035300                   RESP-DAUPPDAT                                          
035400     .                                                                    
035500     EJECT                                                                
035600                                                                          
035700*    --- DB2 SECTIONS                                                     
035800 DB2-SELECT-T01LSEL-TAB SECTION.                                          
035900                                                                          
036000     MOVE 000100  TO GOOD-SQLCODECODES                                    
036100     EXEC SQL                                                             
036200                                                                          
036300         SELECT  BELEGRAD_1                                               
036310                ,FLCUSUPD                                                 
036400                                                                          
036500         INTO   :T01LSEL-BELEGRAD-1                                       
036510               ,:T01LSEL-FLCUSUPD                                         
036600                                                                          
036700         FROM    T01LSEL                                                  
036800                                                                          
036900         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
037000         AND     KDSTATUS = :WS-CURRENT                                   
037100     END-EXEC                                                             
037200                                                                          
037300     MOVE SQLCODE TO SQLCODE-WS                                           
037400     PERFORM DB2-STATUS-CHECK                                             
037500     .                                                                    
037600                                                                          
037700 DB2-SELECT-T01PATE-TAB SECTION.                                          
037800                                                                          
037900     MOVE 000100  TO GOOD-SQLCODECODES                                    
038000     EXEC SQL                                                             
038100          SELECT  BEBETVIL                                                
038200                , DAREGDAT                                                
038300                , DAUPPDAT                                                
038400                , IDUSER                                                  
038500                                                                          
038600           INTO :MAP-RESP-BEBETVIL                                        
038700              , :MAP-DAREGDAT                                             
038800              , :MAP-DAUPPDAT                                             
038900              , :MAP-RESP-IDUSER                                          
039000                                                                          
039100           FROM  T01PATE                                                  
039200                                                                          
039300           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
039400           AND     IDSPRAK  = :REQU-IDSPRAK-KEY                           
039500           AND     KDBETALV = :REQU-KDBETALV-KEY                          
039600     END-EXEC                                                             
039700                                                                          
039800     MOVE SQLCODE TO SQLCODE-WS                                           
039900     PERFORM DB2-STATUS-CHECK                                             
040000     .                                                                    
040100 DB2-UPDATE-T01PATE-TAB  SECTION.                                         
040200                                                                          
040300     MOVE 000     TO GOOD-SQLCODECODES                                    
040400     EXEC SQL                                                             
040500         UPDATE T01PATE                                                   
040600            SET   BEBETVIL = :REQU-BEBETVIL                               
040700                , DAUPPDAT = :WS-CURRENT-DATE                             
040800                , IDUSER   = :REQU-IDUSER                                 
040900                                                                          
041000          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
041100          AND     IDSPRAK  = :REQU-IDSPRAK-KEY                            
041200          AND     KDBETALV = :REQU-KDBETALV-KEY                           
041300     END-EXEC                                                             
041400                                                                          
041500     MOVE SQLCODE TO SQLCODE-WS                                           
041600     PERFORM DB2-STATUS-CHECK                                             
041700     .                                                                    
041800                                                                          
041900 DB2-UPDATE-T01PATE-TAB2 SECTION.                                         
042100     MOVE 000     TO GOOD-SQLCODECODES                                    
042200     EXEC SQL                                                             
042300         UPDATE T01PATE                                                   
042400            SET   BEBETVIL = :REQU-BEBETVIL                               
042500                , DAREGDAT = :WS-CURRENT-DATE                             
042600                , DAUPPDAT = :WS-CURRENT-DATE                             
042700                , IDUSER   = :REQU-IDUSER                                 
042800                                                                          
042900          WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                           
043000          AND     IDSPRAK  = :REQU-IDSPRAK-KEY                            
043100          AND     KDBETALV = :REQU-KDBETALV-KEY                           
043200     END-EXEC                                                             
043300                                                                          
043400     MOVE SQLCODE TO SQLCODE-WS                                           
043500     PERFORM DB2-STATUS-CHECK                                             
043600     .                                                                    
043700                                                                          
043710 DB2-INSERT-T01PATE      SECTION.                                         
043730     MOVE 000     TO GOOD-SQLCODECODES                                    
043740     EXEC SQL                                                             
043750         INSERT INTO T01PATE                                              
043760             (IDLEGSEL                                                    
043770             ,IDSPRAK                                                     
043771             ,KDBETALV                                                    
043772             ,BEBETVIL                                                    
043773             ,DAREGDAT                                                    
043774             ,DAUPPDAT                                                    
043775             ,IDUSER)                                                     
043780             VALUES                                                       
043790             (:REQU-IDLEGSEL-KEY                                          
043791             ,:REQU-IDSPRAK-KEY                                           
043792             ,:REQU-KDBETALV-KEY                                          
043793             ,:REQU-BEBETVIL                                              
043794             ,:WS-CURRENT-DATE                                            
043795             ,'00000000'                                                  
043796             ,:REQU-IDUSER)                                               
043797     END-EXEC                                                             
043798                                                                          
043799     MOVE SQLCODE TO SQLCODE-WS                                           
043800     PERFORM DB2-STATUS-CHECK                                             
043801     .                                                                    
043802                                                                          
043810 DB2-STATUS-CHECK  SECTION.                                               
043900                                                                          
044000     SET SQLCODE-IX TO 1                                                  
044100     SEARCH GOOD-SQLCODE                                                  
044200       AT END                                                             
044300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
044400          DELIMITED BY SIZE INTO ERROR-TEXT                               
044500          CALL ABEND USING RKOD-ABEND-DB2                                 
044600       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
044700     END-SEARCH                                                           
044800     .                                                                    
