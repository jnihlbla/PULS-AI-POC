000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF029100                                                 
000400 AUTHOR.         ANDERS HENRIKSSON                                        
000500 DATE-WRITTEN.   20070514.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.BILLIT.ABNORMALMONTHMAINTENACE                          
001000*    FUNCTION:                                                            
001100*        READ/UPDATE/INSERT/DELETE ABNORMAL SELECT TABLE (T01PDEV)        
001200*        DEPENDING ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)           
001300*        KDPGMACT = 'S' READ                                              
001400*        KDPGMACT = 'U' UPDATE                                            
001500*        KDPGMACT = 'I' INSERT                                            
001600*        KDPGMACT = 'I' INSERT                                            
001700*                                                                         
001800*        THE PROGRAM READS   TABLE T01LSEL                                
001900*        THE PROGRAM UPDATES TABLE T01PDEV                                
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: WF0291U                                             
002300*        REQUEST:     WF0291I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    WF0291O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WF029100'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600*    --- CONSTANT WORK FIELDS                                             
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004900 77  WS-ADRESS                   PIC X(50)                                
005000          VALUE 'CARPARTS.BILLIT.ABNORMALMONTHMAINTENACE'.                
005100 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005200 77  WS-COMING                   PIC S9(3)   VALUE +002    COMP-3.        
005300 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
005400 77  WS-DATE-FORMAT              PIC X(8)    VALUE 'YYYYMMDD'.            
005500                                                                          
005600 77  KEYS-SW                     PIC X       VALUE SPACE.                 
005700     88  KEYS-OK                             VALUE 'Y'.                   
005800     88  KEYS-WRONG                          VALUE 'N'.                   
005900                                                                          
006000 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
006100     88  ACT-CODE-VALID                 VALUE 'S', 'U', 'I'.              
006200     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
006300     88  ACT-CODE-UPDATE                     VALUE 'U'.                   
006400     88  ACT-CODE-INSERT                     VALUE 'I'.                   
006500                                                                          
006600*    --- WORK-FIELDS                                                      
006700 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
006800 01  WS-KVMANAD-NUM              PIC S9(2) COMP-3 VALUE ZERO.             
006900                                                                          
007000 01  WS-REARTRAB-RED             PIC Z(1)9.9(3).                          
007100 01  WS-REARTRAB-NUM             PIC S9(2)V9(3) COMP-3.                   
007200 01  WS-REARTRAB-DEC             PIC 9(2)V9(3)  VALUE ZERO.               
007300 01  WS-REARTRAB-HELTAL          PIC 9(2).                                
007400                                                                          
007500 01  WS-REARTRAB-JUST2          PIC 9(2)V9(3)  VALUE ZERO.                
007600 01  WS-REARTRAB-JUST           PIC 9(2)V9(3)  VALUE ZERO.                
007700 01  FILLER REDEFINES WS-REARTRAB-JUST.                                   
007800     03  FILLER                 PIC 9(2)V9(2).                            
007900     03  WS-REARTRAB-SIST       PIC 9(1).                                 
008000                                                                          
008100 01  WS-PRARTNTO-MIN-RED             PIC Z(6)9.9(3).                      
008200 01  WS-PRARTNTO-MIN-NUM             PIC S9(7)V9(3) COMP-3.               
008300 01  WS-PRARTNTO-MIN-DEC             PIC 9(7)V9(3)  VALUE ZERO.           
008400 01  WS-PRARTNTO-MIN-HELTAL          PIC 9(7).                            
008500                                                                          
008600 01  WS-PRARTNTO-MIN-JUST2          PIC 9(7)V9(3)  VALUE ZERO.            
008700 01  WS-PRARTNTO-MIN-JUST          PIC 9(7)V9(3)  VALUE ZERO.             
008800 01  FILLER REDEFINES WS-PRARTNTO-MIN-JUST.                               
008900     03  FILLER                 PIC 9(7)V9(2).                            
009000     03  WS-PRARTNTO-MIN-SIST       PIC 9(1).                             
009100                                                                          
009200 01  WS-PRARTNTO-MAX-RED             PIC Z(6)9.9(3).                      
009300 01  WS-PRARTNTO-MAX-NUM             PIC S9(7)V9(3) COMP-3.               
009400 01  WS-PRARTNTO-MAX-DEC             PIC 9(7)V9(3)  VALUE ZERO.           
009500 01  WS-PRARTNTO-MAX-HELTAL          PIC 9(7).                            
009600                                                                          
009700 01  WS-PRARTNTO-MAX-JUST2           PIC 9(7)V9(3)  VALUE ZERO.           
009800 01  WS-PRARTNTO-MAX-JUST            PIC 9(7)V9(3)  VALUE ZERO.           
009900 01  FILLER REDEFINES WS-PRARTNTO-MAX-JUST.                               
010000     03  FILLER                      PIC 9(7)V9(2).                       
010100     03  WS-PRARTNTO-MAX-SIST        PIC 9(1).                            
010200                                                                          
010300 01  WS-SUNTO-MIN-RED                PIC Z(10)9.9(2).                     
010400 01  WS-SUNTO-MIN-NUM                PIC S9(11)V9(3) COMP-3.              
010500 01  WS-SUNTO-MIN-DEC                PIC 9(11)V9(3)  VALUE ZERO.          
010600 01  WS-SUNTO-MIN-HELTAL             PIC 9(11).                           
010700                                                                          
010800 01  WS-SUNTO-MIN-JUST2              PIC 9(11)V9(3)  VALUE ZERO.          
010900 01  WS-SUNTO-MIN-JUST               PIC 9(11)V9(3)  VALUE ZERO.          
011000 01  FILLER REDEFINES WS-SUNTO-MIN-JUST.                                  
011100     03  FILLER                      PIC 9(11)V9(2).                      
011200     03  WS-SUNTO-MIN-SIST           PIC 9(1).                            
011300                                                                          
011400 01  WS-SUNTO-MAX-RED                PIC Z(10)9.9(2).                     
011500 01  WS-SUNTO-MAX-NUM                PIC S9(11)V9(3) COMP-3.              
011600 01  WS-SUNTO-MAX-DEC                PIC 9(11)V9(3)  VALUE ZERO.          
011700 01  WS-SUNTO-MAX-HELTAL             PIC 9(11).                           
011800                                                                          
011900 01  WS-SUNTO-MAX-JUST2              PIC 9(11)V9(3)  VALUE ZERO.          
012000 01  WS-SUNTO-MAX-JUST               PIC 9(11)V9(3)  VALUE ZERO.          
012100 01  FILLER REDEFINES WS-SUNTO-MAX-JUST.                                  
012200     03  FILLER                      PIC 9(11)V9(2).                      
012300     03  WS-SUNTO-MAX-SIST           PIC 9(1).                            
012400                                                                          
012500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
012600 01  GENERAL-SUBPROGRAMS.                                                 
012700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012900     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
013000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
013100     SKIP3                                                                
013200                                                                          
013300*    --- PARAMETERS TO ABEND                                              
013400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
013800                                                                          
013900 01  MESSAGE-CODES.                                                       
014000     03  ERROR-CODES.                                                     
014100         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
014200         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
014300         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
014400         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
014500         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
014600         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
014700         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
014800         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
014900         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
015000         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
015100     03  INFO-CODES.                                                      
015200         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
015300         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
015400         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
015500         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
015600*                                                                         
015700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
015800     SKIP3                                                                
015900 01  -COPY WZ01SUB                                                        
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
016200     SKIP3                                                                
016300 01  -COPY WZ20DATE                                                       
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'DECEDIT    '.         
016600     EJECT                                                                
016700 01  -COPY WDECAREA                                                       
016800*                                                                         
016900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
017000     SKIP3                                                                
017100 01  REQU-AREA.                                                           
017200*    03  -COPY WZ01REQU                                                   
017300*    03  -COPY WF0291I1                                                   
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
017600     SKIP3                                                                
017700 01  RESP-AREA.                                                           
017800*    03  -COPY WZ01RESP                                                   
017900*    03  -COPY WF0291O1                                                   
018000     EJECT                                                                
018100 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
018200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018300                                                                          
018400 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
018500 01  DB2-WS.                                                              
018600     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
018700         88  CURSOR-OK                       VALUE 000.                   
018800         88  LINES-FOUND                     VALUE 000.                   
018900         88  LINES-MISSING                   VALUE 100.                   
019000         88  NULL-VALUE                      VALUE 305.                   
019100         88  RESOURCE-WRONG                  VALUE 904.                   
019200     03  GOOD-SQLCODECODES.                                               
019300         05  GOOD-SQLCODE OCCURS 5                                        
019400             INDEXED BY SQLCODE-IX PIC 9(3).                              
019500                                                                          
019600     EJECT                                                                
019700 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
019800*01  -COPY T01LSEL -PRE T01LSEL-                                          
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'T01PDEV-AREA'.        
020100*01  -COPY T01PDEV -PRE T01PDEV-                                          
020200     EJECT                                                                
020300     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
020400     EJECT                                                                
020500     EXEC SQL INCLUDE T01PDEV END-EXEC.                                   
020600     EJECT                                                                
020700 LINKAGE SECTION.                                                         
020800     EJECT                                                                
020900                                                                          
021000 PROCEDURE DIVISION.                                                      
021100 MAIN SECTION.                                                            
021200                                                                          
021300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
021400     IF SUB-KDRC = 0                                                      
021500       PERFORM A-INIT                                                     
021600       PERFORM B-CHECK-KEYS                                               
021700       IF KEYS-OK                                                         
021800         PERFORM F-READ-SHOW-INFO                                         
021900       END-IF                                                             
022000       IF KEYS-WRONG                                                      
022100         PERFORM S04-MOVE-MISSING-TO-RESPOND                              
022200       END-IF                                                             
022300       PERFORM S02-RETURN-RESPONSE                                        
022400     END-IF                                                               
022500                                                                          
022600     MOVE ZERO TO RETURN-CODE                                             
022700     GOBACK                                                               
022800     .                                                                    
022900                                                                          
023000     EJECT                                                                
023100 A-INIT SECTION.                                                          
023200     INITIALIZE GOOD-SQLCODECODES                                         
023300     MOVE ALL '+' TO RESP-AREA                                            
023400     MOVE SPACE TO RESP-IDMSG-ERROR                                       
023500     MOVE SPACE TO RESP-IDMSG-INFO                                        
023600     MOVE SPACE TO RESP-IDELMT-ERROR                                      
023700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
023800     .                                                                    
023900     EJECT                                                                
024000                                                                          
024100*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
024200 B-CHECK-KEYS SECTION.                                                    
024300     MOVE YES TO KEYS-SW                                                  
024400     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
024500                                                                          
024600     IF  REQU-IDMSGVER NUMERIC                                            
024700       IF REQU-IDLEGSEL-KEY > SPACE                                       
024800       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
024900       AND REQU-KDBEHX-KEY = 'M'                                          
025000       AND ACT-CODE-VALID                                                 
025100         CONTINUE                                                         
025200       ELSE                                                               
025300         MOVE NOO TO KEYS-SW                                              
025400       END-IF                                                             
025500     ELSE                                                                 
025600       MOVE NOO TO KEYS-SW                                                
025700     END-IF                                                               
025800                                                                          
025900     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
026000       MOVE NOO TO KEYS-SW                                                
026100     END-IF                                                               
026200                                                                          
026300     IF KEYS-WRONG                                                        
026400       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
026500       IF REQU-IDMSGVER NUMERIC                                           
026600         CONTINUE                                                         
026700       ELSE                                                               
026800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
026900         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
027000       END-IF                                                             
027100       IF ACT-CODE-VALID                                                  
027200         CONTINUE                                                         
027300       ELSE                                                               
027400         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
027500         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
027600       END-IF                                                             
027700       IF REQU-IDUSER = SPACE OR = ALL '+'                                
027800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
027900         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
028000       END-IF                                                             
028100     END-IF                                                               
028200                                                                          
028300     IF KEYS-OK                                                           
028400       PERFORM DB2-SELECT-T01LSEL                                         
028500       IF LINES-FOUND                                                     
028600         PERFORM DB2-SELECT-T01PDEV-SELECT                                
028700         IF LINES-FOUND                                                   
028800           CONTINUE                                                       
028900         ELSE                                                             
029000           MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                     
029100           MOVE 'KDBEHX'          TO RESP-IDELMT-ERROR                    
029200           MOVE NOO TO KEYS-SW                                            
029300         END-IF                                                           
029400       ELSE                                                               
029500         MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                       
029600         MOVE 'IDLEGSEL'        TO RESP-IDELMT-ERROR                      
029700         MOVE NOO TO KEYS-SW                                              
029800       END-IF                                                             
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030300*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
030400 F-READ-SHOW-INFO SECTION.                                                
030500     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
030600     MOVE REQU-KDBEHX-KEY    TO RESP-KDBEHX-KEY                           
030700     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
030800                                                                          
030900     PERFORM FA-READ-BASICDATA                                            
031000     .                                                                    
031100     EJECT                                                                
031200                                                                          
031300*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
031400 FA-READ-BASICDATA SECTION.                                               
031500     IF ACT-CODE-SEARCH                                                   
031600       PERFORM FAA-SEARCH-T01PDEV                                         
031700     ELSE                                                                 
031800       IF ACT-CODE-UPDATE                                                 
031900         PERFORM FAB-UPDATE-T01PDEV                                       
032000       ELSE                                                               
032100         IF ACT-CODE-INSERT                                               
032200           PERFORM FAC-INSERT-T01PDEV                                     
032300         END-IF                                                           
032400       END-IF                                                             
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800                                                                          
032900*** - SEARCH FOR RIGHT ABNORMAL SELECT AND MARK CURRENT LINE              
033000 FAA-SEARCH-T01PDEV SECTION.                                              
033100     PERFORM DB2-SELECT-T01PDEV                                           
033200                                                                          
033300     IF LINES-FOUND                                                       
033400       PERFORM S03-MOVE-TO-RESPOND                                        
033500     ELSE                                                                 
033510       MOVE 'S' TO REQU-KDBEHX-KEY                                        
033511       PERFORM DB2-SELECT-T01PDEV                                         
033520       IF LINES-FOUND                                                     
033521         MOVE 'M' TO REQU-KDBEHX-KEY                                      
033530         PERFORM S03-MOVE-TO-RESPOND                                      
033540       ELSE                                                               
033600         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
033700         MOVE 'KDBEHX'   TO RESP-IDELMT-ERROR                             
033800         MOVE NOO        TO KEYS-SW                                       
033900       END-IF                                                             
033910     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200                                                                          
034300*** - CHECK IF UPDATE IS ON CURRENT LINE                                  
034400 FAB-UPDATE-T01PDEV SECTION.                                              
034500     PERFORM FABB-UPD-CURRENT                                             
034600     .                                                                    
034700     EJECT                                                                
034800                                                                          
034900*** - VALIDATE REQUESTED FIELDS FOR UPDATE ON SENDING COUNTRY             
035000 FABA-CHECK-UPDATE-DATA SECTION.                                          
035100     IF RESP-IDMSG-ERROR = SPACE                                          
035200       IF  REQU-FLPAYTE = 'N'                                             
035300       OR  REQU-FLPAYTE = 'Y'                                             
035400          IF REQU-FLPAYTE = 'Y'                                           
035500            MOVE 'J' TO REQU-FLPAYTE                                      
035600          END-IF                                                          
035700       ELSE                                                               
035800         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
035900         MOVE 'FLPAYTE'           TO RESP-IDELMT-ERROR                    
036000       END-IF                                                             
036100     END-IF                                                               
036200                                                                          
036300     IF RESP-IDMSG-ERROR = SPACE                                          
036400       IF  REQU-FLDELTE = 'N'                                             
036500       OR  REQU-FLDELTE = 'Y'                                             
036600          IF REQU-FLDELTE = 'Y'                                           
036700            MOVE 'J' TO REQU-FLDELTE                                      
036800          END-IF                                                          
036900       ELSE                                                               
037000         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
037100         MOVE 'FLDELTE'           TO RESP-IDELMT-ERROR                    
037200       END-IF                                                             
037300     END-IF                                                               
037400                                                                          
037500     IF RESP-IDMSG-ERROR = SPACE                                          
037600       IF  REQU-FLSOFT  = 'N'                                             
037700       OR  REQU-FLSOFT  = 'Y'                                             
037800          IF REQU-FLSOFT = 'Y'                                            
037900            MOVE 'J' TO REQU-FLSOFT                                       
038000          END-IF                                                          
038100       ELSE                                                               
038200         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
038300         MOVE 'FLSOFT'            TO RESP-IDELMT-ERROR                    
038400       END-IF                                                             
038500     END-IF                                                               
038600                                                                          
038700     IF RESP-IDMSG-ERROR = SPACE                                          
038800       IF  REQU-FLFREE  = 'N'                                             
038900       OR  REQU-FLFREE  = 'Y'                                             
039000          IF REQU-FLFREE = 'Y'                                            
039100            MOVE 'J' TO REQU-FLFREE                                       
039200          END-IF                                                          
039300       ELSE                                                               
039400         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
039500         MOVE 'FLFREE'            TO RESP-IDELMT-ERROR                    
039600       END-IF                                                             
039700     END-IF                                                               
039800                                                                          
039900     IF RESP-IDMSG-ERROR = SPACE                                          
040000       IF  REQU-FLSERV  = 'N'                                             
040100       OR  REQU-FLSERV  = 'Y'                                             
040200          IF REQU-FLSERV = 'Y'                                            
040300            MOVE 'J' TO REQU-FLSERV                                       
040400          END-IF                                                          
040500       ELSE                                                               
040600         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
040700         MOVE 'FLSERV'            TO RESP-IDELMT-ERROR                    
040800       END-IF                                                             
040900     END-IF                                                               
041000                                                                          
041100     IF RESP-IDMSG-ERROR = SPACE                                          
041200       IF  REQU-FLINVOIC = 'N'                                            
041300       OR  REQU-FLINVOIC = 'Y'                                            
041400          IF REQU-FLINVOIC = 'Y'                                          
041500            MOVE 'J' TO REQU-FLINVOIC                                     
041600          END-IF                                                          
041700       ELSE                                                               
041800         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
041900         MOVE 'FLINVOIC'          TO RESP-IDELMT-ERROR                    
042000       END-IF                                                             
042100     END-IF                                                               
042200                                                                          
042300     PERFORM S05-CHECK-NUMERIC-VALUES                                     
042400                                                                          
042500     IF RESP-IDMSG-ERROR = SPACE                                          
042600       MOVE WS-CURRENT-DATE       TO T01PDEV-DAUPPDAT                     
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000                                                                          
043100*** - UPDATE CURRENT LINE ON T01PDEV.                                     
043200 FABB-UPD-CURRENT SECTION.                                                
043300     PERFORM DB2-SELECT-T01PDEV                                           
043400                                                                          
043500     IF LINES-FOUND                                                       
043600       PERFORM FABA-CHECK-UPDATE-DATA                                     
043700       IF RESP-IDMSG-ERROR = SPACE                                        
043800         PERFORM DB2-UPDATE-T01PDEV                                       
043900         PERFORM S03-MOVE-TO-RESPOND                                      
044000         MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                            
044100       END-IF                                                             
044200     ELSE                                                                 
044300       MOVE NOT-FOUND      TO RESP-IDMSG-ERROR                            
044400       MOVE 'KDBEHX'       TO RESP-IDELMT-ERROR                           
044500       MOVE NOO TO KEYS-SW                                                
044600     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000                                                                          
045100*** - INSERT NEW CURRENT LINE. COMING LINE COULD NOT BE INSERTED.         
045200*** - IF CURRENT LINE EXIST WITH DELETE DATE, DELETE CURRENT LINE         
045300***   PHYSICAL AND INSERT NEW CURRENT LINE.                               
045400 FAC-INSERT-T01PDEV SECTION.                                              
045500     PERFORM DB2-SELECT-T01PDEV                                           
045600     IF LINES-FOUND                                                       
045700       MOVE ERR-INSERT-NOT-ALLOWED TO RESP-IDMSG-ERROR                    
045800     ELSE                                                                 
045900       PERFORM FACA-CHECK-INSERT-DATA                                     
046000       IF RESP-IDMSG-ERROR = SPACE                                        
046100         PERFORM DB2-INSERT-T01PDEV                                       
046200         PERFORM S03-MOVE-TO-RESPOND                                      
046300         MOVE INF-INSERT-OK        TO RESP-IDMSG-INFO                     
046400       END-IF                                                             
046500     END-IF                                                               
046600     .                                                                    
046700     EJECT                                                                
046800                                                                          
046900*** - VALIDATE REQUESTED FIELDS FOR INSERT ON SENDING COUNTRY             
047000 FACA-CHECK-INSERT-DATA SECTION.                                          
047100     IF RESP-IDMSG-ERROR = SPACE                                          
047200       IF  REQU-FLPAYTE = 'N'                                             
047300       OR  REQU-FLPAYTE = 'Y'                                             
047400          IF REQU-FLPAYTE = 'Y'                                           
047500            MOVE 'J' TO REQU-FLPAYTE                                      
047600          END-IF                                                          
047700       ELSE                                                               
047800         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
047900         MOVE 'FLPAYTE'           TO RESP-IDELMT-ERROR                    
048000       END-IF                                                             
048100     END-IF                                                               
048200                                                                          
048300     IF RESP-IDMSG-ERROR = SPACE                                          
048400       IF  REQU-FLDELTE = 'N'                                             
048500       OR  REQU-FLDELTE = 'Y'                                             
048600          IF REQU-FLDELTE = 'Y'                                           
048700            MOVE 'J' TO REQU-FLDELTE                                      
048800          END-IF                                                          
048900       ELSE                                                               
049000         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
049100         MOVE 'FLDELTE'           TO RESP-IDELMT-ERROR                    
049200       END-IF                                                             
049300     END-IF                                                               
049400                                                                          
049500     IF RESP-IDMSG-ERROR = SPACE                                          
049600       IF  REQU-FLSOFT  = 'N'                                             
049700       OR  REQU-FLSOFT  = 'Y'                                             
049800          IF REQU-FLSOFT = 'Y'                                            
049900            MOVE 'J' TO REQU-FLSOFT                                       
050000          END-IF                                                          
050100       ELSE                                                               
050200         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
050300         MOVE 'FLSOFT'            TO RESP-IDELMT-ERROR                    
050400       END-IF                                                             
050500     END-IF                                                               
050600                                                                          
050700     IF RESP-IDMSG-ERROR = SPACE                                          
050800       IF  REQU-FLFREE  = 'N'                                             
050900       OR  REQU-FLFREE  = 'Y'                                             
051000          IF REQU-FLFREE = 'Y'                                            
051100            MOVE 'J' TO REQU-FLFREE                                       
051200          END-IF                                                          
051300       ELSE                                                               
051400         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
051500         MOVE 'FLFREE'            TO RESP-IDELMT-ERROR                    
051600       END-IF                                                             
051700     END-IF                                                               
051800                                                                          
051900     IF RESP-IDMSG-ERROR = SPACE                                          
052000       IF  REQU-FLSERV  = 'N'                                             
052100       OR  REQU-FLSERV  = 'Y'                                             
052200          IF REQU-FLSERV = 'Y'                                            
052300            MOVE 'J' TO REQU-FLSERV                                       
052400          END-IF                                                          
052500       ELSE                                                               
052600         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
052700         MOVE 'FLSERV'            TO RESP-IDELMT-ERROR                    
052800       END-IF                                                             
052900     END-IF                                                               
053000                                                                          
053100     IF RESP-IDMSG-ERROR = SPACE                                          
053200       IF  REQU-FLINVOIC = 'N'                                            
053300       OR  REQU-FLINVOIC = 'Y'                                            
053400          IF REQU-FLINVOIC = 'Y'                                          
053500            MOVE 'J' TO REQU-FLINVOIC                                     
053600          END-IF                                                          
053700       ELSE                                                               
053800         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
053900         MOVE 'FLINVOIC'          TO RESP-IDELMT-ERROR                    
054000       END-IF                                                             
054100     END-IF                                                               
054200                                                                          
054300     PERFORM S05-CHECK-NUMERIC-VALUES                                     
054400                                                                          
054500     IF RESP-IDMSG-ERROR = SPACE                                          
054600       MOVE WS-CURRENT-DATE       TO T01PDEV-DAREGDAT                     
054700       MOVE WS-ACTIVE             TO T01PDEV-DAUPPDAT                     
054800     END-IF                                                               
054900     .                                                                    
055000     EJECT                                                                
055100                                                                          
055200*    --- DISPATCHER SECTIONS                                              
055300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
055400     MOVE 'GETARG'                   TO SUB-KDFUNC                        
055500     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
055600     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
055700                                                                          
055800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
055900                                                                          
056000     IF SUB-KDRC > 0                                                      
056100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
056200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
056300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
056400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056500     END-IF                                                               
056600     .                                                                    
056700                                                                          
056800 S02-RETURN-RESPONSE SECTION.                                             
056900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
057000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
057100                                                                          
057200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
057300                                                                          
057400     IF SUB-KDRC > 0                                                      
057500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
057600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
057700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057900     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
058200                                                                          
058300*    --- MOVE TO OUTPUT SECTIONS                                          
058400 S03-MOVE-TO-RESPOND SECTION.                                             
058500     IF ACT-CODE-SEARCH                                                   
058600       IF T01PDEV-FLPAYTE = 'J'                                           
058700         MOVE 'Y'                 TO RESP-FLPAYTE                         
058800       ELSE                                                               
058900         MOVE T01PDEV-FLPAYTE     TO RESP-FLPAYTE                         
059000       END-IF                                                             
059100       IF T01PDEV-FLDELTE = 'J'                                           
059200         MOVE 'Y'                 TO RESP-FLDELTE                         
059300       ELSE                                                               
059400         MOVE T01PDEV-FLDELTE     TO RESP-FLDELTE                         
059500       END-IF                                                             
059600       MOVE T01PDEV-REARTRAB      TO RESP-REARTRAB                        
059700       MOVE T01PDEV-PRARTNTO-MIN  TO RESP-PRARTNTO-MIN                    
059800       MOVE T01PDEV-PRARTNTO-MAX  TO RESP-PRARTNTO-MAX                    
059900       MOVE T01PDEV-SUNTO-MIN     TO RESP-SUNTO-MIN                       
060000       MOVE T01PDEV-SUNTO-MAX     TO RESP-SUNTO-MAX                       
060100       IF T01PDEV-FLSOFT  = 'J'                                           
060200         MOVE 'Y'                 TO RESP-FLSOFT                          
060300       ELSE                                                               
060400         MOVE T01PDEV-FLSOFT      TO RESP-FLSOFT                          
060500       END-IF                                                             
060600       IF T01PDEV-FLFREE  = 'J'                                           
060700         MOVE 'Y'                 TO RESP-FLFREE                          
060800       ELSE                                                               
060900         MOVE T01PDEV-FLFREE      TO RESP-FLFREE                          
061000       END-IF                                                             
061100       IF T01PDEV-FLSERV  = 'J'                                           
061200         MOVE 'Y'                 TO RESP-FLSERV                          
061300       ELSE                                                               
061400         MOVE T01PDEV-FLSERV      TO RESP-FLSERV                          
061500       END-IF                                                             
061600       IF T01PDEV-FLINVOIC = 'J'                                          
061700         MOVE 'Y'                 TO RESP-FLINVOIC                        
061800       ELSE                                                               
061900         MOVE T01PDEV-FLINVOIC    TO RESP-FLINVOIC                        
062000       END-IF                                                             
062100       MOVE T01PDEV-DAREGDAT      TO RESP-DAREGDAT                        
062200       MOVE T01PDEV-DAUPPDAT      TO RESP-DAUPPDAT                        
062300       MOVE T01PDEV-IDUSER        TO RESP-IDUSER                          
062400     ELSE                                                                 
062500       IF ACT-CODE-UPDATE                                                 
062600         IF REQU-FLPAYTE = 'J'                                            
062700           MOVE 'Y'               TO RESP-FLPAYTE                         
062800         ELSE                                                             
062900           MOVE REQU-FLPAYTE      TO RESP-FLPAYTE                         
063000         END-IF                                                           
063100         IF REQU-FLDELTE = 'J'                                            
063200           MOVE 'Y'               TO RESP-FLDELTE                         
063300         ELSE                                                             
063400           MOVE REQU-FLDELTE      TO RESP-FLDELTE                         
063500         END-IF                                                           
063600         MOVE WS-REARTRAB-NUM     TO RESP-REARTRAB                        
063700         MOVE WS-PRARTNTO-MIN-NUM TO RESP-PRARTNTO-MIN                    
063800         MOVE WS-PRARTNTO-MAX-NUM TO RESP-PRARTNTO-MAX                    
063900         MOVE WS-SUNTO-MIN-NUM    TO RESP-SUNTO-MIN                       
064000         MOVE WS-SUNTO-MAX-NUM    TO RESP-SUNTO-MAX                       
064100         IF REQU-FLSOFT  = 'J'                                            
064200           MOVE 'Y'               TO RESP-FLSOFT                          
064300         ELSE                                                             
064400           MOVE REQU-FLSOFT       TO RESP-FLSOFT                          
064500         END-IF                                                           
064600         IF REQU-FLFREE  = 'J'                                            
064700           MOVE 'Y'               TO RESP-FLFREE                          
064800         ELSE                                                             
064900           MOVE REQU-FLFREE       TO RESP-FLFREE                          
065000         END-IF                                                           
065100         IF REQU-FLSERV  = 'J'                                            
065200           MOVE 'Y'               TO RESP-FLSERV                          
065300         ELSE                                                             
065400           MOVE REQU-FLSERV       TO RESP-FLSERV                          
065500         END-IF                                                           
065600         IF REQU-FLINVOIC = 'J'                                           
065700           MOVE 'Y'               TO RESP-FLINVOIC                        
065800         ELSE                                                             
065900           MOVE REQU-FLINVOIC     TO RESP-FLINVOIC                        
066000         END-IF                                                           
066100         MOVE T01PDEV-DAREGDAT    TO RESP-DAREGDAT                        
066200         MOVE T01PDEV-DAUPPDAT    TO RESP-DAUPPDAT                        
066300         MOVE REQU-IDUSER         TO RESP-IDUSER                          
066400       ELSE                                                               
066500         IF ACT-CODE-INSERT                                               
066600           IF REQU-FLPAYTE  = 'J'                                         
066700             MOVE 'Y'               TO RESP-FLPAYTE                       
066800           ELSE                                                           
066900             MOVE REQU-FLPAYTE      TO RESP-FLPAYTE                       
067000           END-IF                                                         
067100           IF REQU-FLDELTE  = 'J'                                         
067200             MOVE 'Y'               TO RESP-FLDELTE                       
067300           ELSE                                                           
067400             MOVE REQU-FLDELTE      TO RESP-FLDELTE                       
067500           END-IF                                                         
067600           MOVE WS-REARTRAB-NUM     TO RESP-REARTRAB                      
067700           MOVE WS-PRARTNTO-MIN-NUM TO RESP-PRARTNTO-MIN                  
067800           MOVE WS-PRARTNTO-MAX-NUM TO RESP-PRARTNTO-MAX                  
067900           MOVE WS-SUNTO-MIN-NUM    TO RESP-SUNTO-MIN                     
068000           MOVE WS-SUNTO-MAX-NUM    TO RESP-SUNTO-MAX                     
068100           IF REQU-FLSOFT   = 'J'                                         
068200             MOVE 'Y'               TO RESP-FLSOFT                        
068300           ELSE                                                           
068400             MOVE REQU-FLSOFT       TO RESP-FLSOFT                        
068500           END-IF                                                         
068600           IF REQU-FLFREE   = 'J'                                         
068700             MOVE 'Y'               TO RESP-FLFREE                        
068800           ELSE                                                           
068900             MOVE REQU-FLFREE       TO RESP-FLFREE                        
069000           END-IF                                                         
069100           IF REQU-FLSERV   = 'J'                                         
069200             MOVE 'Y'               TO RESP-FLSERV                        
069300           ELSE                                                           
069400             MOVE REQU-FLSERV       TO RESP-FLSERV                        
069500           END-IF                                                         
069600           IF REQU-FLINVOIC = 'J'                                         
069700             MOVE 'Y'               TO RESP-FLINVOIC                      
069800           ELSE                                                           
069900             MOVE REQU-FLINVOIC     TO RESP-FLINVOIC                      
070000           END-IF                                                         
070100           MOVE T01PDEV-DAREGDAT    TO RESP-DAREGDAT                      
070200           MOVE T01PDEV-DAUPPDAT    TO RESP-DAUPPDAT                      
070300           MOVE REQU-IDUSER         TO RESP-IDUSER                        
070400         END-IF                                                           
070500       END-IF                                                             
070600     END-IF                                                               
070700     .                                                                    
070800                                                                          
070900 S04-MOVE-MISSING-TO-RESPOND SECTION.                                     
071000     MOVE SPACE             TO RESP-FLPAYTE                               
071100                               RESP-FLDELTE                               
071200                               RESP-FLSOFT                                
071300                               RESP-FLFREE                                
071400                               RESP-FLSERV                                
071500                               RESP-FLINVOIC                              
071600                               RESP-IDUSER                                
071700     MOVE ZERO              TO RESP-DAREGDAT                              
071800                               RESP-DAUPPDAT                              
071900                               RESP-REARTRAB                              
072000                               RESP-PRARTNTO-MIN                          
072100                               RESP-PRARTNTO-MAX                          
072200                               RESP-SUNTO-MIN                             
072300                               RESP-SUNTO-MAX                             
072400     .                                                                    
072500     EJECT                                                                
072600                                                                          
072700 S05-CHECK-NUMERIC-VALUES    SECTION.                                     
072800     IF RESP-IDMSG-ERROR = SPACE                                          
072900       IF REQU-REARTRAB(1:1) = '.'                                        
073000         IF REQU-REARTRAB(4:1) NUMERIC                                    
073100           MOVE REQU-REARTRAB(4:1) TO WS-REARTRAB-SIST                    
073200           MOVE SPACE              TO REQU-REARTRAB(4:1)                  
073300         END-IF                                                           
073400       END-IF                                                             
073500       IF REQU-REARTRAB(2:1) = '.'                                        
073600         IF REQU-REARTRAB(5:1) NUMERIC                                    
073700           MOVE REQU-REARTRAB(5:1) TO WS-REARTRAB-SIST                    
073800           MOVE SPACE              TO REQU-REARTRAB(5:1)                  
073900         END-IF                                                           
074000       END-IF                                                             
074100       MOVE REQU-REARTRAB          TO DEC-IDFRIDATA                       
074200       MOVE 2                      TO DEC-KVHELTAL                        
074300       MOVE 2                      TO DEC-KVDECIMAL                       
074400                                                                          
074500       CALL WDECEDIT USING DEC-WDECAREA                                   
074600                                                                          
074700       IF DEC-KDSVAR-OK                                                   
074800         MOVE DEC-IDEDITDATA       TO WS-REARTRAB-DEC                     
074900         IF WS-REARTRAB-DEC NUMERIC                                       
075000           MOVE WS-REARTRAB-DEC    TO WS-REARTRAB-HELTAL                  
075100           IF WS-REARTRAB-HELTAL > 99                                     
075200             MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                 
075300             MOVE 'REARTRAB'       TO RESP-IDELMT-ERROR                   
075400           ELSE                                                           
075500             MOVE WS-REARTRAB-DEC  TO WS-REARTRAB-NUM                     
075600             IF REQU-REARTRAB(1:1) = '.'                                  
075700               IF WS-REARTRAB-SIST > ZERO                                 
075800                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
075900                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
076000                                           WS-REARTRAB-JUST2              
076100               END-IF                                                     
076200             END-IF                                                       
076300             IF REQU-REARTRAB(2:1) = '.'                                  
076400               IF WS-REARTRAB-SIST > ZERO                                 
076500                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
076600                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
076700                                           WS-REARTRAB-JUST2              
076800               END-IF                                                     
076900             END-IF                                                       
077000             IF REQU-REARTRAB(3:1) = '.'                                  
077100               IF WS-REARTRAB-SIST > ZERO                                 
077200                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
077300                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
077400                                           WS-REARTRAB-JUST2              
077500               END-IF                                                     
077600             END-IF                                                       
077700             IF REQU-REARTRAB(4:1) = '.'                                  
077800               IF WS-REARTRAB-SIST > ZERO                                 
077900                 MOVE WS-REARTRAB-JUST TO WS-REARTRAB-JUST2               
078000                 COMPUTE WS-REARTRAB-NUM = WS-REARTRAB-NUM +              
078100                                           WS-REARTRAB-JUST2              
078200               END-IF                                                     
078300             END-IF                                                       
078400           END-IF                                                         
078500         ELSE                                                             
078600           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
078700           MOVE 'REARTRAB'          TO RESP-IDELMT-ERROR                  
078800         END-IF                                                           
078900       ELSE                                                               
079000         MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                   
079100         MOVE 'REARTRAB'            TO RESP-IDELMT-ERROR                  
079200       END-IF                                                             
079300     END-IF                                                               
079400                                                                          
079500     IF RESP-IDMSG-ERROR = SPACE                                          
079600       IF REQU-PRARTNTO-MIN(1:1) = '.'                                    
079700         IF REQU-PRARTNTO-MIN(4:1) NUMERIC                                
079800           MOVE REQU-PRARTNTO-MIN(4:1) TO WS-PRARTNTO-MIN-SIST            
079900           MOVE SPACE                  TO REQU-PRARTNTO-MIN(4:1)          
080000         END-IF                                                           
080100       END-IF                                                             
080200       IF REQU-PRARTNTO-MIN(2:1) = '.'                                    
080300         IF REQU-PRARTNTO-MIN(5:1) NUMERIC                                
080400           MOVE REQU-PRARTNTO-MIN(5:1) TO WS-PRARTNTO-MIN-SIST            
080500           MOVE SPACE                  TO REQU-PRARTNTO-MIN(5:1)          
080600         END-IF                                                           
080700       END-IF                                                             
080800       IF REQU-PRARTNTO-MIN(3:1) = '.'                                    
080900         IF REQU-PRARTNTO-MIN(6:1) NUMERIC                                
081000           MOVE REQU-PRARTNTO-MIN(6:1) TO WS-PRARTNTO-MIN-SIST            
081100           MOVE SPACE                  TO REQU-PRARTNTO-MIN(6:1)          
081200         END-IF                                                           
081300       END-IF                                                             
081400       IF REQU-PRARTNTO-MIN(4:1) = '.'                                    
081500         IF REQU-PRARTNTO-MIN(7:1) NUMERIC                                
081600           MOVE REQU-PRARTNTO-MIN(7:1) TO WS-PRARTNTO-MIN-SIST            
081700           MOVE SPACE                  TO REQU-PRARTNTO-MIN(7:1)          
081800         END-IF                                                           
081900       END-IF                                                             
082000       IF REQU-PRARTNTO-MIN(5:1) = '.'                                    
082100         IF REQU-PRARTNTO-MIN(8:1) NUMERIC                                
082200           MOVE REQU-PRARTNTO-MIN(8:1) TO WS-PRARTNTO-MIN-SIST            
082300           MOVE SPACE                  TO REQU-PRARTNTO-MIN(8:1)          
082400         END-IF                                                           
082500       END-IF                                                             
082600       IF REQU-PRARTNTO-MIN(6:1) = '.'                                    
082700         IF REQU-PRARTNTO-MIN(9:1) NUMERIC                                
082800           MOVE REQU-PRARTNTO-MIN(9:1) TO WS-PRARTNTO-MIN-SIST            
082900           MOVE SPACE                  TO REQU-PRARTNTO-MIN(9:1)          
083000         END-IF                                                           
083100       END-IF                                                             
083200       IF REQU-PRARTNTO-MIN(7:1) = '.'                                    
083300         IF REQU-PRARTNTO-MIN(10:1) NUMERIC                               
083400           MOVE REQU-PRARTNTO-MIN(10:1) TO WS-PRARTNTO-MIN-SIST           
083500           MOVE SPACE                   TO REQU-PRARTNTO-MIN(10:1)        
083600         END-IF                                                           
083700       END-IF                                                             
083800       MOVE REQU-PRARTNTO-MIN           TO DEC-IDFRIDATA                  
083900       MOVE 7                           TO DEC-KVHELTAL                   
084000       MOVE 2                           TO DEC-KVDECIMAL                  
084100                                                                          
084200       CALL WDECEDIT USING DEC-WDECAREA                                   
084300                                                                          
084400       IF DEC-KDSVAR-OK                                                   
084500         MOVE DEC-IDEDITDATA            TO WS-PRARTNTO-MIN-DEC            
084600         IF WS-PRARTNTO-MIN-DEC NUMERIC                                   
084700           MOVE WS-PRARTNTO-MIN-DEC     TO WS-PRARTNTO-MIN-HELTAL         
084800           IF WS-PRARTNTO-MIN-HELTAL > 9999999                            
084900             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
085000             MOVE 'PRARTNTO-MIN'        TO RESP-IDELMT-ERROR              
085100           ELSE                                                           
085200             MOVE WS-PRARTNTO-MIN-DEC   TO WS-PRARTNTO-MIN-NUM            
085300             IF REQU-PRARTNTO-MIN(1:1) = '.'                              
085400              IF WS-PRARTNTO-MIN-SIST > ZERO                              
085500               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
085600               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
085700                                             WS-PRARTNTO-MIN-JUST2        
085800              END-IF                                                      
085900             END-IF                                                       
086000             IF REQU-PRARTNTO-MIN(2:1) = '.'                              
086100              IF WS-PRARTNTO-MIN-SIST > ZERO                              
086200               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
086300               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
086400                                             WS-PRARTNTO-MIN-JUST2        
086500              END-IF                                                      
086600             END-IF                                                       
086700             IF REQU-PRARTNTO-MIN(3:1) = '.'                              
086800              IF WS-PRARTNTO-MIN-SIST > ZERO                              
086900               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
087000               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
087100                                             WS-PRARTNTO-MIN-JUST2        
087200              END-IF                                                      
087300             END-IF                                                       
087400             IF REQU-PRARTNTO-MIN(4:1) = '.'                              
087500              IF WS-PRARTNTO-MIN-SIST > ZERO                              
087600               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
087700               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
087800                                             WS-PRARTNTO-MIN-JUST2        
087900              END-IF                                                      
088000             END-IF                                                       
088100             IF REQU-PRARTNTO-MIN(5:1) = '.'                              
088200              IF WS-PRARTNTO-MIN-SIST > ZERO                              
088300               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
088400               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
088500                                             WS-PRARTNTO-MIN-JUST2        
088600              END-IF                                                      
088700             END-IF                                                       
088800             IF REQU-PRARTNTO-MIN(6:1) = '.'                              
088900              IF WS-PRARTNTO-MIN-SIST > ZERO                              
089000               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
089100               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
089200                                             WS-PRARTNTO-MIN-JUST2        
089300              END-IF                                                      
089400             END-IF                                                       
089500             IF REQU-PRARTNTO-MIN(7:1) = '.'                              
089600              IF WS-PRARTNTO-MIN-SIST > ZERO                              
089700               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
089800               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
089900                                             WS-PRARTNTO-MIN-JUST2        
090000              END-IF                                                      
090100             END-IF                                                       
090200             IF REQU-PRARTNTO-MIN(8:1) = '.'                              
090300              IF WS-PRARTNTO-MIN-SIST > ZERO                              
090400               MOVE WS-PRARTNTO-MIN-JUST TO WS-PRARTNTO-MIN-JUST2         
090500               COMPUTE WS-PRARTNTO-MIN-NUM = WS-PRARTNTO-MIN-NUM +        
090600                                             WS-PRARTNTO-MIN-JUST2        
090700              END-IF                                                      
090800             END-IF                                                       
090900           END-IF                                                         
091000         ELSE                                                             
091100           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
091200           MOVE 'PRARTNTO-MIN'        TO RESP-IDELMT-ERROR                
091300         END-IF                                                           
091400       ELSE                                                               
091500         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
091600         MOVE 'PRARTNTO-MIN'          TO RESP-IDELMT-ERROR                
091700       END-IF                                                             
091800     END-IF                                                               
091900                                                                          
092000     IF RESP-IDMSG-ERROR = SPACE                                          
092100       IF REQU-PRARTNTO-MAX(1:1) = '.'                                    
092200         IF REQU-PRARTNTO-MAX(4:1) NUMERIC                                
092300           MOVE REQU-PRARTNTO-MAX(4:1) TO WS-PRARTNTO-MAX-SIST            
092400           MOVE SPACE                  TO REQU-PRARTNTO-MAX(4:1)          
092500         END-IF                                                           
092600       END-IF                                                             
092700       IF REQU-PRARTNTO-MAX(2:1) = '.'                                    
092800         IF REQU-PRARTNTO-MAX(5:1) NUMERIC                                
092900           MOVE REQU-PRARTNTO-MAX(5:1) TO WS-PRARTNTO-MAX-SIST            
093000           MOVE SPACE                  TO REQU-PRARTNTO-MAX(5:1)          
093100         END-IF                                                           
093200       END-IF                                                             
093300       IF REQU-PRARTNTO-MAX(3:1) = '.'                                    
093400         IF REQU-PRARTNTO-MAX(6:1) NUMERIC                                
093500           MOVE REQU-PRARTNTO-MAX(6:1) TO WS-PRARTNTO-MAX-SIST            
093600           MOVE SPACE                  TO REQU-PRARTNTO-MAX(6:1)          
093700         END-IF                                                           
093800       END-IF                                                             
093900       IF REQU-PRARTNTO-MAX(4:1) = '.'                                    
094000         IF REQU-PRARTNTO-MAX(7:1) NUMERIC                                
094100           MOVE REQU-PRARTNTO-MAX(7:1) TO WS-PRARTNTO-MAX-SIST            
094200           MOVE SPACE                  TO REQU-PRARTNTO-MAX(7:1)          
094300         END-IF                                                           
094400       END-IF                                                             
094500       IF REQU-PRARTNTO-MAX(5:1) = '.'                                    
094600         IF REQU-PRARTNTO-MAX(8:1) NUMERIC                                
094700           MOVE REQU-PRARTNTO-MAX(8:1) TO WS-PRARTNTO-MAX-SIST            
094800           MOVE SPACE                  TO REQU-PRARTNTO-MAX(8:1)          
094900         END-IF                                                           
095000       END-IF                                                             
095100       IF REQU-PRARTNTO-MAX(6:1) = '.'                                    
095200         IF REQU-PRARTNTO-MAX(9:1) NUMERIC                                
095300           MOVE REQU-PRARTNTO-MAX(9:1) TO WS-PRARTNTO-MAX-SIST            
095400           MOVE SPACE                  TO REQU-PRARTNTO-MAX(9:1)          
095500         END-IF                                                           
095600       END-IF                                                             
095700       IF REQU-PRARTNTO-MAX(7:1) = '.'                                    
095800         IF REQU-PRARTNTO-MAX(10:1) NUMERIC                               
095900           MOVE REQU-PRARTNTO-MAX(10:1) TO WS-PRARTNTO-MAX-SIST           
096000           MOVE SPACE                   TO REQU-PRARTNTO-MAX(10:1)        
096100         END-IF                                                           
096200       END-IF                                                             
096300       MOVE REQU-PRARTNTO-MAX           TO DEC-IDFRIDATA                  
096400       MOVE 7                           TO DEC-KVHELTAL                   
096500       MOVE 2                           TO DEC-KVDECIMAL                  
096600                                                                          
096700       CALL WDECEDIT USING DEC-WDECAREA                                   
096800                                                                          
096900       IF DEC-KDSVAR-OK                                                   
097000         MOVE DEC-IDEDITDATA            TO WS-PRARTNTO-MAX-DEC            
097100         IF WS-PRARTNTO-MAX-DEC NUMERIC                                   
097200           MOVE WS-PRARTNTO-MAX-DEC     TO WS-PRARTNTO-MAX-HELTAL         
097300           IF WS-PRARTNTO-MAX-HELTAL > 9999999                            
097400             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
097500             MOVE 'PRARTNTO-MAX'        TO RESP-IDELMT-ERROR              
097600           ELSE                                                           
097700             MOVE WS-PRARTNTO-MAX-DEC   TO WS-PRARTNTO-MAX-NUM            
097800             IF REQU-PRARTNTO-MAX(1:1) = '.'                              
097900              IF WS-PRARTNTO-MAX-SIST > ZERO                              
098000               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
098100               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
098200                                             WS-PRARTNTO-MAX-JUST2        
098300              END-IF                                                      
098400             END-IF                                                       
098500             IF REQU-PRARTNTO-MAX(2:1) = '.'                              
098600              IF WS-PRARTNTO-MAX-SIST > ZERO                              
098700               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
098800               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
098900                                             WS-PRARTNTO-MAX-JUST2        
099000              END-IF                                                      
099100             END-IF                                                       
099200             IF REQU-PRARTNTO-MAX(3:1) = '.'                              
099300              IF WS-PRARTNTO-MAX-SIST > ZERO                              
099400               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
099500               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
099600                                             WS-PRARTNTO-MAX-JUST2        
099700              END-IF                                                      
099800             END-IF                                                       
099900             IF REQU-PRARTNTO-MAX(4:1) = '.'                              
100000              IF WS-PRARTNTO-MAX-SIST > ZERO                              
100100               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
100200               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
100300                                             WS-PRARTNTO-MAX-JUST2        
100400              END-IF                                                      
100500             END-IF                                                       
100600             IF REQU-PRARTNTO-MAX(5:1) = '.'                              
100700              IF WS-PRARTNTO-MAX-SIST > ZERO                              
100800               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
100900               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
101000                                             WS-PRARTNTO-MAX-JUST2        
101100              END-IF                                                      
101200             END-IF                                                       
101300             IF REQU-PRARTNTO-MAX(6:1) = '.'                              
101400              IF WS-PRARTNTO-MAX-SIST > ZERO                              
101500               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
101600               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
101700                                             WS-PRARTNTO-MAX-JUST2        
101800              END-IF                                                      
101900             END-IF                                                       
102000             IF REQU-PRARTNTO-MAX(7:1) = '.'                              
102100              IF WS-PRARTNTO-MAX-SIST > ZERO                              
102200               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
102300               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
102400                                             WS-PRARTNTO-MAX-JUST2        
102500              END-IF                                                      
102600             END-IF                                                       
102700             IF REQU-PRARTNTO-MAX(8:1) = '.'                              
102800              IF WS-PRARTNTO-MAX-SIST > ZERO                              
102900               MOVE WS-PRARTNTO-MAX-JUST TO WS-PRARTNTO-MAX-JUST2         
103000               COMPUTE WS-PRARTNTO-MAX-NUM = WS-PRARTNTO-MAX-NUM +        
103100                                             WS-PRARTNTO-MAX-JUST2        
103200              END-IF                                                      
103300             END-IF                                                       
103400           END-IF                                                         
103500         ELSE                                                             
103600           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
103700           MOVE 'PRARTNTO-MAX'        TO RESP-IDELMT-ERROR                
103800         END-IF                                                           
103900       ELSE                                                               
104000         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
104100         MOVE 'PRARTNTO-MAX'          TO RESP-IDELMT-ERROR                
104200       END-IF                                                             
104300     END-IF                                                               
104400                                                                          
104500     IF RESP-IDMSG-ERROR = SPACE                                          
104600       IF REQU-SUNTO-MIN(1:1) = '.'                                       
104700         IF REQU-SUNTO-MIN(4:1) NUMERIC                                   
104800           MOVE REQU-SUNTO-MIN(4:1)    TO WS-SUNTO-MIN-SIST               
104900           MOVE SPACE                  TO REQU-SUNTO-MIN(4:1)             
105000         END-IF                                                           
105100       END-IF                                                             
105200       IF REQU-SUNTO-MIN(2:1) = '.'                                       
105300         IF REQU-SUNTO-MIN(5:1) NUMERIC                                   
105400           MOVE REQU-SUNTO-MIN(5:1)    TO WS-SUNTO-MIN-SIST               
105500           MOVE SPACE                  TO REQU-SUNTO-MIN(5:1)             
105600         END-IF                                                           
105700       END-IF                                                             
105800       IF REQU-SUNTO-MIN(3:1) = '.'                                       
105900         IF REQU-SUNTO-MIN(6:1) NUMERIC                                   
106000           MOVE REQU-SUNTO-MIN(6:1)    TO WS-SUNTO-MIN-SIST               
106100           MOVE SPACE                  TO REQU-SUNTO-MIN(6:1)             
106200         END-IF                                                           
106300       END-IF                                                             
106400       IF REQU-SUNTO-MIN(4:1) = '.'                                       
106500         IF REQU-SUNTO-MIN(7:1) NUMERIC                                   
106600           MOVE REQU-SUNTO-MIN(7:1)    TO WS-SUNTO-MIN-SIST               
106700           MOVE SPACE                  TO REQU-SUNTO-MIN(7:1)             
106800         END-IF                                                           
106900       END-IF                                                             
107000       IF REQU-SUNTO-MIN(5:1) = '.'                                       
107100         IF REQU-SUNTO-MIN(8:1) NUMERIC                                   
107200           MOVE REQU-SUNTO-MIN(8:1)    TO WS-SUNTO-MIN-SIST               
107300           MOVE SPACE                  TO REQU-SUNTO-MIN(8:1)             
107400         END-IF                                                           
107500       END-IF                                                             
107600       IF REQU-SUNTO-MIN(6:1) = '.'                                       
107700         IF REQU-SUNTO-MIN(9:1) NUMERIC                                   
107800           MOVE REQU-SUNTO-MIN(9:1)    TO WS-SUNTO-MIN-SIST               
107900           MOVE SPACE                  TO REQU-SUNTO-MIN(9:1)             
108000         END-IF                                                           
108100       END-IF                                                             
108200       IF REQU-SUNTO-MIN(7:1) = '.'                                       
108300         IF REQU-SUNTO-MIN(10:1) NUMERIC                                  
108400           MOVE REQU-SUNTO-MIN(10:1)    TO WS-SUNTO-MIN-SIST              
108500           MOVE SPACE                   TO REQU-SUNTO-MIN(10:1)           
108600         END-IF                                                           
108700       END-IF                                                             
108800       IF REQU-SUNTO-MIN(8:1) = '.'                                       
108900         IF REQU-SUNTO-MIN(11:1) NUMERIC                                  
109000           MOVE REQU-SUNTO-MIN(11:1)    TO WS-SUNTO-MIN-SIST              
109100           MOVE SPACE                   TO REQU-SUNTO-MIN(11:1)           
109200         END-IF                                                           
109300       END-IF                                                             
109400       IF REQU-SUNTO-MIN(9:1) = '.'                                       
109500         IF REQU-SUNTO-MIN(12:1) NUMERIC                                  
109600           MOVE REQU-SUNTO-MIN(12:1)    TO WS-SUNTO-MIN-SIST              
109700           MOVE SPACE                   TO REQU-SUNTO-MIN(12:1)           
109800         END-IF                                                           
109900       END-IF                                                             
110000       IF REQU-SUNTO-MIN(10:1) = '.'                                      
110100         IF REQU-SUNTO-MIN(13:1) NUMERIC                                  
110200           MOVE REQU-SUNTO-MIN(13:1)    TO WS-SUNTO-MIN-SIST              
110300           MOVE SPACE                   TO REQU-SUNTO-MIN(13:1)           
110400         END-IF                                                           
110500       END-IF                                                             
110600       IF REQU-SUNTO-MIN(11:1) = '.'                                      
110700         IF REQU-SUNTO-MIN(14:1) NUMERIC                                  
110800           MOVE REQU-SUNTO-MIN(14:1)    TO WS-SUNTO-MIN-SIST              
110900           MOVE SPACE                   TO REQU-SUNTO-MIN(14:1)           
111000         END-IF                                                           
111100       END-IF                                                             
111200       MOVE REQU-SUNTO-MIN              TO DEC-IDFRIDATA                  
111300       MOVE 11                          TO DEC-KVHELTAL                   
111400       MOVE 2                           TO DEC-KVDECIMAL                  
111500                                                                          
111600       CALL WDECEDIT USING DEC-WDECAREA                                   
111700                                                                          
111800       IF DEC-KDSVAR-OK                                                   
111900         MOVE DEC-IDEDITDATA            TO WS-SUNTO-MIN-DEC               
112000         IF WS-SUNTO-MIN-DEC NUMERIC                                      
112100           MOVE WS-SUNTO-MIN-DEC        TO WS-SUNTO-MIN-HELTAL            
112200           IF WS-SUNTO-MIN-HELTAL > 99999999999                           
112300             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
112400             MOVE 'SUNTO-MIN'           TO RESP-IDELMT-ERROR              
112500           ELSE                                                           
112600             MOVE WS-SUNTO-MIN-DEC      TO WS-SUNTO-MIN-NUM               
112700             IF REQU-SUNTO-MIN(1:1) = '.'                                 
112800              IF WS-SUNTO-MIN-SIST > ZERO                                 
112900               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
113000               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
113100                                           WS-SUNTO-MIN-JUST2             
113200              END-IF                                                      
113300             END-IF                                                       
113400             IF REQU-SUNTO-MIN(2:1) = '.'                                 
113500              IF WS-SUNTO-MIN-SIST > ZERO                                 
113600               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
113700               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
113800                                           WS-SUNTO-MIN-JUST2             
113900              END-IF                                                      
114000             END-IF                                                       
114100             IF REQU-SUNTO-MIN(3:1) = '.'                                 
114200              IF WS-SUNTO-MIN-SIST > ZERO                                 
114300               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
114400               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
114500                                           WS-SUNTO-MIN-JUST2             
114600              END-IF                                                      
114700             END-IF                                                       
114800             IF REQU-SUNTO-MIN(4:1) = '.'                                 
114900              IF WS-SUNTO-MIN-SIST > ZERO                                 
115000               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
115100               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
115200                                           WS-SUNTO-MIN-JUST2             
115300              END-IF                                                      
115400             END-IF                                                       
115500             IF REQU-SUNTO-MIN(5:1) = '.'                                 
115600              IF WS-SUNTO-MIN-SIST > ZERO                                 
115700               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
115800               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
115900                                           WS-SUNTO-MIN-JUST2             
116000              END-IF                                                      
116100             END-IF                                                       
116200             IF REQU-SUNTO-MIN(6:1) = '.'                                 
116300              IF WS-SUNTO-MIN-SIST > ZERO                                 
116400               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
116500               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
116600                                           WS-SUNTO-MIN-JUST2             
116700              END-IF                                                      
116800             END-IF                                                       
116900             IF REQU-SUNTO-MIN(7:1) = '.'                                 
117000              IF WS-SUNTO-MIN-SIST > ZERO                                 
117100               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
117200               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
117300                                           WS-SUNTO-MIN-JUST2             
117400              END-IF                                                      
117500             END-IF                                                       
117600             IF REQU-SUNTO-MIN(8:1) = '.'                                 
117700              IF WS-SUNTO-MIN-SIST > ZERO                                 
117800               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
117900               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
118000                                           WS-SUNTO-MIN-JUST2             
118100              END-IF                                                      
118200             END-IF                                                       
118300             IF REQU-SUNTO-MIN(9:1) = '.'                                 
118400              IF WS-SUNTO-MIN-SIST > ZERO                                 
118500               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
118600               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
118700                                           WS-SUNTO-MIN-JUST2             
118800              END-IF                                                      
118900             END-IF                                                       
119000             IF REQU-SUNTO-MIN(10:1) = '.'                                
119100              IF WS-SUNTO-MIN-SIST > ZERO                                 
119200               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
119300               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
119400                                           WS-SUNTO-MIN-JUST2             
119500              END-IF                                                      
119600             END-IF                                                       
119700             IF REQU-SUNTO-MIN(11:1) = '.'                                
119800              IF WS-SUNTO-MIN-SIST > ZERO                                 
119900               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
120000               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
120100                                           WS-SUNTO-MIN-JUST2             
120200              END-IF                                                      
120300             END-IF                                                       
120400             IF REQU-SUNTO-MIN(12:1) = '.'                                
120500              IF WS-SUNTO-MIN-SIST > ZERO                                 
120600               MOVE WS-SUNTO-MIN-JUST   TO WS-SUNTO-MIN-JUST2             
120700               COMPUTE WS-SUNTO-MIN-NUM =  WS-SUNTO-MIN-NUM +             
120800                                           WS-SUNTO-MIN-JUST2             
120900              END-IF                                                      
121000             END-IF                                                       
121100           END-IF                                                         
121200         ELSE                                                             
121300           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
121400           MOVE 'SUNTO-MIN'           TO RESP-IDELMT-ERROR                
121500         END-IF                                                           
121600       ELSE                                                               
121700         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
121800         MOVE 'SUNTO-MIN'             TO RESP-IDELMT-ERROR                
121900       END-IF                                                             
122000     END-IF                                                               
122100                                                                          
122200     IF RESP-IDMSG-ERROR = SPACE                                          
122300       IF REQU-SUNTO-MAX(1:1) = '.'                                       
122400         IF REQU-SUNTO-MAX(4:1) NUMERIC                                   
122500           MOVE REQU-SUNTO-MAX(4:1)    TO WS-SUNTO-MAX-SIST               
122600           MOVE SPACE                  TO REQU-SUNTO-MAX(4:1)             
122700         END-IF                                                           
122800       END-IF                                                             
122900       IF REQU-SUNTO-MAX(2:1) = '.'                                       
123000         IF REQU-SUNTO-MAX(5:1) NUMERIC                                   
123100           MOVE REQU-SUNTO-MAX(5:1)    TO WS-SUNTO-MAX-SIST               
123200           MOVE SPACE                  TO REQU-SUNTO-MAX(5:1)             
123300         END-IF                                                           
123400       END-IF                                                             
123500       IF REQU-SUNTO-MAX(3:1) = '.'                                       
123600         IF REQU-SUNTO-MAX(6:1) NUMERIC                                   
123700           MOVE REQU-SUNTO-MAX(6:1)    TO WS-SUNTO-MAX-SIST               
123800           MOVE SPACE                  TO REQU-SUNTO-MAX(6:1)             
123900         END-IF                                                           
124000       END-IF                                                             
124100       IF REQU-SUNTO-MAX(4:1) = '.'                                       
124200         IF REQU-SUNTO-MAX(7:1) NUMERIC                                   
124300           MOVE REQU-SUNTO-MAX(7:1)    TO WS-SUNTO-MAX-SIST               
124400           MOVE SPACE                  TO REQU-SUNTO-MAX(7:1)             
124500         END-IF                                                           
124600       END-IF                                                             
124700       IF REQU-SUNTO-MAX(5:1) = '.'                                       
124800         IF REQU-SUNTO-MAX(8:1) NUMERIC                                   
124900           MOVE REQU-SUNTO-MAX(8:1)    TO WS-SUNTO-MAX-SIST               
125000           MOVE SPACE                  TO REQU-SUNTO-MAX(8:1)             
125100         END-IF                                                           
125200       END-IF                                                             
125300       IF REQU-SUNTO-MAX(6:1) = '.'                                       
125400         IF REQU-SUNTO-MAX(9:1) NUMERIC                                   
125500           MOVE REQU-SUNTO-MAX(9:1)    TO WS-SUNTO-MAX-SIST               
125600           MOVE SPACE                  TO REQU-SUNTO-MAX(9:1)             
125700         END-IF                                                           
125800       END-IF                                                             
125900       IF REQU-SUNTO-MAX(7:1) = '.'                                       
126000         IF REQU-SUNTO-MAX(10:1) NUMERIC                                  
126100           MOVE REQU-SUNTO-MAX(10:1)    TO WS-SUNTO-MAX-SIST              
126200           MOVE SPACE                   TO REQU-SUNTO-MAX(10:1)           
126300         END-IF                                                           
126400       END-IF                                                             
126500       IF REQU-SUNTO-MAX(8:1) = '.'                                       
126600         IF REQU-SUNTO-MAX(11:1) NUMERIC                                  
126700           MOVE REQU-SUNTO-MAX(11:1)    TO WS-SUNTO-MAX-SIST              
126800           MOVE SPACE                   TO REQU-SUNTO-MAX(11:1)           
126900         END-IF                                                           
127000       END-IF                                                             
127100       IF REQU-SUNTO-MAX(9:1) = '.'                                       
127200         IF REQU-SUNTO-MAX(12:1) NUMERIC                                  
127300           MOVE REQU-SUNTO-MAX(12:1)    TO WS-SUNTO-MAX-SIST              
127400           MOVE SPACE                   TO REQU-SUNTO-MAX(12:1)           
127500         END-IF                                                           
127600       END-IF                                                             
127700       IF REQU-SUNTO-MAX(10:1) = '.'                                      
127800         IF REQU-SUNTO-MAX(13:1) NUMERIC                                  
127900           MOVE REQU-SUNTO-MAX(13:1)    TO WS-SUNTO-MAX-SIST              
128000           MOVE SPACE                   TO REQU-SUNTO-MAX(13:1)           
128100         END-IF                                                           
128200       END-IF                                                             
128300       IF REQU-SUNTO-MAX(11:1) = '.'                                      
128400         IF REQU-SUNTO-MAX(14:1) NUMERIC                                  
128500           MOVE REQU-SUNTO-MAX(14:1)    TO WS-SUNTO-MAX-SIST              
128600           MOVE SPACE                   TO REQU-SUNTO-MAX(14:1)           
128700         END-IF                                                           
128800       END-IF                                                             
128900       MOVE REQU-SUNTO-MAX              TO DEC-IDFRIDATA                  
129000       MOVE 11                          TO DEC-KVHELTAL                   
129100       MOVE 2                           TO DEC-KVDECIMAL                  
129200                                                                          
129300       CALL WDECEDIT USING DEC-WDECAREA                                   
129400                                                                          
129500       IF DEC-KDSVAR-OK                                                   
129600         MOVE DEC-IDEDITDATA            TO WS-SUNTO-MAX-DEC               
129700         IF WS-SUNTO-MAX-DEC NUMERIC                                      
129800           MOVE WS-SUNTO-MAX-DEC        TO WS-SUNTO-MAX-HELTAL            
129900           IF WS-SUNTO-MAX-HELTAL > 99999999999                           
130000             MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR               
130100             MOVE 'SUNTO-MAX'           TO RESP-IDELMT-ERROR              
130200           ELSE                                                           
130300             MOVE WS-SUNTO-MAX-DEC      TO WS-SUNTO-MAX-NUM               
130400             IF REQU-SUNTO-MAX(1:1) = '.'                                 
130500              IF WS-SUNTO-MAX-SIST > ZERO                                 
130600               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
130700               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
130800                                           WS-SUNTO-MAX-JUST2             
130900              END-IF                                                      
131000             END-IF                                                       
131100             IF REQU-SUNTO-MAX(2:1) = '.'                                 
131200              IF WS-SUNTO-MAX-SIST > ZERO                                 
131300               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
131400               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
131500                                           WS-SUNTO-MAX-JUST2             
131600              END-IF                                                      
131700             END-IF                                                       
131800             IF REQU-SUNTO-MAX(3:1) = '.'                                 
131900              IF WS-SUNTO-MAX-SIST > ZERO                                 
132000               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
132100               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
132200                                           WS-SUNTO-MAX-JUST2             
132300              END-IF                                                      
132400             END-IF                                                       
132500             IF REQU-SUNTO-MAX(4:1) = '.'                                 
132600              IF WS-SUNTO-MAX-SIST > ZERO                                 
132700               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
132800               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
132900                                           WS-SUNTO-MAX-JUST2             
133000              END-IF                                                      
133100             END-IF                                                       
133200             IF REQU-SUNTO-MAX(5:1) = '.'                                 
133300              IF WS-SUNTO-MAX-SIST > ZERO                                 
133400               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
133500               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
133600                                           WS-SUNTO-MAX-JUST2             
133700              END-IF                                                      
133800             END-IF                                                       
133900             IF REQU-SUNTO-MAX(6:1) = '.'                                 
134000              IF WS-SUNTO-MAX-SIST > ZERO                                 
134100               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
134200               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
134300                                           WS-SUNTO-MAX-JUST2             
134400              END-IF                                                      
134500             END-IF                                                       
134600             IF REQU-SUNTO-MAX(7:1) = '.'                                 
134700              IF WS-SUNTO-MAX-SIST > ZERO                                 
134800               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
134900               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
135000                                           WS-SUNTO-MAX-JUST2             
135100              END-IF                                                      
135200             END-IF                                                       
135300             IF REQU-SUNTO-MAX(8:1) = '.'                                 
135400              IF WS-SUNTO-MAX-SIST > ZERO                                 
135500               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
135600               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
135700                                           WS-SUNTO-MAX-JUST2             
135800              END-IF                                                      
135900             END-IF                                                       
136000             IF REQU-SUNTO-MAX(9:1) = '.'                                 
136100              IF WS-SUNTO-MAX-SIST > ZERO                                 
136200               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
136300               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
136400                                           WS-SUNTO-MAX-JUST2             
136500              END-IF                                                      
136600             END-IF                                                       
136700             IF REQU-SUNTO-MAX(10:1) = '.'                                
136800              IF WS-SUNTO-MAX-SIST > ZERO                                 
136900               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
137000               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
137100                                           WS-SUNTO-MAX-JUST2             
137200              END-IF                                                      
137300             END-IF                                                       
137400             IF REQU-SUNTO-MAX(11:1) = '.'                                
137500              IF WS-SUNTO-MAX-SIST > ZERO                                 
137600               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
137700               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
137800                                           WS-SUNTO-MAX-JUST2             
137900              END-IF                                                      
138000             END-IF                                                       
138100             IF REQU-SUNTO-MAX(12:1) = '.'                                
138200              IF WS-SUNTO-MAX-SIST > ZERO                                 
138300               MOVE WS-SUNTO-MAX-JUST   TO WS-SUNTO-MAX-JUST2             
138400               COMPUTE WS-SUNTO-MAX-NUM =  WS-SUNTO-MAX-NUM +             
138500                                           WS-SUNTO-MAX-JUST2             
138600              END-IF                                                      
138700             END-IF                                                       
138800           END-IF                                                         
138900         ELSE                                                             
139000           MOVE ERR-MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                 
139100           MOVE 'SUNTO-MAX'           TO RESP-IDELMT-ERROR                
139200         END-IF                                                           
139300       ELSE                                                               
139400         MOVE ERR-MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                 
139500         MOVE 'SUNTO-MAX'             TO RESP-IDELMT-ERROR                
139600       END-IF                                                             
139700     END-IF                                                               
139800     .                                                                    
139900                                                                          
140000*    --- DB2 SECTIONS                                                     
140100 DB2-SELECT-T01LSEL       SECTION.                                        
140200     MOVE 000100 TO GOOD-SQLCODECODES                                     
140300                                                                          
140400     EXEC SQL                                                             
140500        SELECT  BELEGRAD_1                                                
140600                                                                          
140700        INTO   :T01LSEL-BELEGRAD-1                                        
140800                                                                          
140900        FROM    T01LSEL                                                   
141000                                                                          
141100        WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                             
141200            AND KDSTATUS = :WS-CURRENT                                    
141300     END-EXEC                                                             
141400                                                                          
141500     MOVE SQLCODE TO SQLCODE-WS                                           
141600     PERFORM DB2-STATUS-CHECK                                             
141700     .                                                                    
141800                                                                          
141900 DB2-SELECT-T01PDEV-SELECT SECTION.                                       
142000     MOVE 000100305  TO GOOD-SQLCODECODES                                 
142100                                                                          
142200     EXEC SQL                                                             
142300         SELECT  IDLEGSEL                                                 
142400                                                                          
142500         INTO  :T01PDEV-IDLEGSEL                                          
142600                                                                          
142700         FROM  T01PDEV                                                    
142800                                                                          
142900         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
143000         AND   KDBEHX   = 'S'                                             
143100     END-EXEC                                                             
143200                                                                          
143300     MOVE SQLCODE TO SQLCODE-WS                                           
143400     PERFORM DB2-STATUS-CHECK                                             
143500     .                                                                    
143600                                                                          
143700 DB2-SELECT-T01PDEV SECTION.                                              
143800     MOVE 000100305  TO GOOD-SQLCODECODES                                 
143900                                                                          
144000     EXEC SQL                                                             
144100         SELECT  IDLEGSEL                                                 
144200                ,KDBEHX                                                   
144300                ,FLPAYTE                                                  
144400                ,FLDELTE                                                  
144500                ,REARTRAB                                                 
144600                ,PRARTNTO_MIN                                             
144700                ,PRARTNTO_MAX                                             
144800                ,SUNTO_MIN                                                
144900                ,SUNTO_MAX                                                
145000                ,FLSOFT                                                   
145100                ,FLFREE                                                   
145200                ,FLSERV                                                   
145300                ,FLINVOIC                                                 
145400                ,KVMANAD                                                  
145500                ,DAREGDAT                                                 
145600                ,DAUPPDAT                                                 
145700                ,IDUSER                                                   
145800                                                                          
145900         INTO  :T01PDEV-IDLEGSEL                                          
146000             , :T01PDEV-KDBEHX                                            
146100             , :T01PDEV-FLPAYTE                                           
146200             , :T01PDEV-FLDELTE                                           
146300             , :T01PDEV-REARTRAB                                          
146400             , :T01PDEV-PRARTNTO-MIN                                      
146500             , :T01PDEV-PRARTNTO-MAX                                      
146600             , :T01PDEV-SUNTO-MIN                                         
146700             , :T01PDEV-SUNTO-MAX                                         
146800             , :T01PDEV-FLSOFT                                            
146900             , :T01PDEV-FLFREE                                            
147000             , :T01PDEV-FLSERV                                            
147100             , :T01PDEV-FLINVOIC                                          
147200             , :T01PDEV-KVMANAD                                           
147300             , :T01PDEV-DAREGDAT                                          
147400             , :T01PDEV-DAUPPDAT                                          
147500             , :T01PDEV-IDUSER                                            
147600                                                                          
147700         FROM  T01PDEV                                                    
147800                                                                          
147900         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
148000         AND   KDBEHX   = :REQU-KDBEHX-KEY                                
148100     END-EXEC                                                             
148200                                                                          
148300     MOVE SQLCODE TO SQLCODE-WS                                           
148400     PERFORM DB2-STATUS-CHECK                                             
148500     .                                                                    
148600                                                                          
148700 DB2-UPDATE-T01PDEV SECTION.                                              
148800     MOVE 000     TO GOOD-SQLCODECODES                                    
148900                                                                          
149000     EXEC SQL                                                             
149100        UPDATE T01PDEV                                                    
149200           SET                                                            
149300                 IDLEGSEL     = :REQU-IDLEGSEL-KEY                        
149400               , KDBEHX       = :REQU-KDBEHX-KEY                          
149500               , FLPAYTE      = :REQU-FLPAYTE                             
149600               , FLDELTE      = :REQU-FLDELTE                             
149700               , REARTRAB     = :WS-REARTRAB-NUM                          
149800               , PRARTNTO_MIN = :WS-PRARTNTO-MIN-NUM                      
149900               , PRARTNTO_MAX = :WS-PRARTNTO-MAX-NUM                      
150000               , SUNTO_MIN    = :WS-SUNTO-MIN-NUM                         
150100               , SUNTO_MAX    = :WS-SUNTO-MAX-NUM                         
150200               , FLSOFT       = :REQU-FLSOFT                              
150300               , FLFREE       = :REQU-FLFREE                              
150400               , FLSERV       = :REQU-FLSERV                              
150500               , FLINVOIC     = :REQU-FLINVOIC                            
150600               , KVMANAD      = :WS-KVMANAD-NUM                           
150700               , DAREGDAT     = :T01PDEV-DAREGDAT                         
150800               , DAUPPDAT     = :T01PDEV-DAUPPDAT                         
150900               , IDUSER       = :REQU-IDUSER                              
151000                                                                          
151100         WHERE   IDLEGSEL     = :REQU-IDLEGSEL-KEY                        
151200         AND     KDBEHX       = :REQU-KDBEHX-KEY                          
151300     END-EXEC                                                             
151400                                                                          
151500     MOVE SQLCODE TO SQLCODE-WS                                           
151600     PERFORM DB2-STATUS-CHECK                                             
151700     .                                                                    
151800                                                                          
151900 DB2-INSERT-T01PDEV  SECTION.                                             
152000     MOVE 000   TO GOOD-SQLCODECODES                                      
152100                                                                          
152200     EXEC SQL                                                             
152300         INSERT INTO T01PDEV                                              
152400            (IDLEGSEL                                                     
152500            ,KDBEHX                                                       
152600            ,FLPAYTE                                                      
152700            ,FLDELTE                                                      
152800            ,REARTRAB                                                     
152900            ,PRARTNTO_MIN                                                 
153000            ,PRARTNTO_MAX                                                 
153100            ,SUNTO_MIN                                                    
153200            ,SUNTO_MAX                                                    
153300            ,FLSOFT                                                       
153400            ,FLFREE                                                       
153500            ,FLSERV                                                       
153600            ,FLINVOIC                                                     
153700            ,KVMANAD                                                      
153800            ,DAREGDAT                                                     
153900            ,DAUPPDAT                                                     
154000            ,IDUSER)                                                      
154100         VALUES                                                           
154200            (:REQU-IDLEGSEL-KEY                                           
154300            ,:REQU-KDBEHX-KEY                                             
154400            ,:REQU-FLPAYTE                                                
154500            ,:REQU-FLDELTE                                                
154600            ,:WS-REARTRAB-NUM                                             
154700            ,:WS-PRARTNTO-MIN-NUM                                         
154800            ,:WS-PRARTNTO-MAX-NUM                                         
154900            ,:WS-SUNTO-MIN-NUM                                            
155000            ,:WS-SUNTO-MAX-NUM                                            
155100            ,:REQU-FLSOFT                                                 
155200            ,:REQU-FLFREE                                                 
155300            ,:REQU-FLSERV                                                 
155400            ,:REQU-FLINVOIC                                               
155500            ,:WS-KVMANAD-NUM                                              
155600            ,:T01PDEV-DAREGDAT                                            
155700            ,:T01PDEV-DAUPPDAT                                            
155800            ,:REQU-IDUSER)                                                
155900     END-EXEC                                                             
156000     MOVE SQLCODE TO SQLCODE-WS                                           
156100     PERFORM DB2-STATUS-CHECK                                             
156200     .                                                                    
156300                                                                          
156400 DB2-STATUS-CHECK  SECTION.                                               
156500     SET SQLCODE-IX TO 1                                                  
156600     SEARCH GOOD-SQLCODE                                                  
156700       AT END                                                             
156800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
156900          DELIMITED BY SIZE INTO ERROR-TEXT                               
157000          CALL ABEND USING RKOD-ABEND-DB2                                 
157100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
157200     END-SEARCH                                                           
157300     .                                                                    
