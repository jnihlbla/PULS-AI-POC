000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF028700.                                                
000400 AUTHOR.         BARSHARANI BISHOYE.                                      
000500 DATE-WRITTEN.   22/06/2020.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    NAME:                                                                
001000*        CARPARTS.BILLIT.ROLLINGDOCNUMSERASSMAINTENANCE                   
001100*    FUNCTION:                                                            
001200*        READ/INSERT/DELETE DOCUMENT NUMBER SERIES ASSIGNMENT             
001300*        TABEL T01ASNS DEPENDING ON REQUESTED PROGRAMS                    
001400*        ACTION CODE (KDPGMACT)                                           
001500*        KDPGMACT = 'S' READ                                              
001600*        KDPGMACT = 'I' INSERT                                            
001700*        KDPGMACT = 'D' DELETE                                            
001800*                                                                         
001900*        THE PROGRAM READS   TABLE T01LSEL                                
002000*        THE PROGRAM READS   TABLE T01COCO                                
002100*        THE PROGRAM READS   TABLE T01CUGR                                
002200*        THE PROGRAM READS   TABLE T01DOTY                                
002300*        THE PROGRAM READS   TABLE T01BURE                                
002400*        THE PROGRAM READS   TABLE T01NSDO                                
002500*        THE PROGRAM UPDATES TABLE T01ASNS                                
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSACTION: WF0287U                                             
002900*        REQUEST:     WF0287I1                                            
003000*                                                                         
003100*    OUTDATA.                                                             
003200*        RESPONSE:    WF0287O1                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP2                                                                
003700 INPUT-OUTPUT SECTION.                                                    
003800                                                                          
003900 FILE-CONTROL.                                                            
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'WF028700'.            
004700                                                                          
004800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004900 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005100                                                                          
005200*    --- CONSTANT WORK FIELDS                                             
005300 77  YES                         PIC X       VALUE 'Y'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005500 77  WS-ADRESS                   PIC X(50)                                
005600       VALUE 'CARPARTS.BILLIT.ROLLINGDOCNUMSERASSMAINTENANCE'.            
005700 77  WS-CURRENT                  PIC S9(3)   VALUE +001    COMP-3.        
005800 77  WS-ACTIVE                   PIC X(8)    VALUE '00000000'.            
005900                                                                          
006000 77  KEYS-SW                     PIC X       VALUE SPACE.                 
006100     88  KEYS-OK                             VALUE 'Y'.                   
006200     88  KEYS-WRONG                          VALUE 'N'.                   
006300                                                                          
006400 77  ACTION-CODE-SW              PIC X       VALUE SPACE.                 
006500     88  ACT-CODE-VALID                 VALUE 'S', 'I', 'D'.              
006600     88  ACT-CODE-SEARCH                     VALUE 'S'.                   
006700     88  ACT-CODE-INSERT                     VALUE 'I'.                   
006800     88  ACT-CODE-DELETE                     VALUE 'D'.                   
006900                                                                          
007000*    --- OTHER MAPPING-FIELDS THAN COPYTEXT WF0287O1                      
007100 01  MAP-IDLOPNR-KEY             PIC S9(3)   VALUE ZERO  COMP-3.          
007200 01  MAP-DAREGDAT                PIC X(8)    VALUE SPACE.                 
007300 01  MAP-DADELDAT                PIC 9(8)    VALUE ZERO.                  
007400                                                                          
007500*    --- WORK-FIELDS                                                      
007600 01  WS-IDLEGSEL                 PIC X(4)    VALUE SPACE.                 
007700 01  WS-CURRENT-DATE.                                                     
007800     03 FILLER                   PIC X(2)    VALUE SPACE.                 
007900     03 WS-CURRENT-YY            PIC 9(2)    VALUE ZERO.                  
008000     03 FILLER                   PIC X(4)    VALUE SPACE.                 
008100                                                                          
008200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008300 01  GENERAL-SUBPROGRAMS.                                                 
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008600     SKIP3                                                                
008700                                                                          
008800*    --- PARAMETERS TO ABEND                                              
008900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009200 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
009300                                                                          
009400 01  MESSAGE-CODES.                                                       
009500     03  ERROR-CODES.                                                     
009600         05  ERR-DELETE-FAILED       PIC X(3)    VALUE '006'.             
009700         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
009800         05  ERR-DELETE-NOT-ALLOWED  PIC X(3)    VALUE '009'.             
009900         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
010000         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
010100         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
010200         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
010300         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
010400         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
010500         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
010600         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
010700     03  INFO-CODES.                                                      
010800         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
010900         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011200     SKIP3                                                                
011300 01  -COPY WZ01SUB                                                        
011400     EJECT                                                                
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'MAPPING-AREA'.        
011700     SKIP3                                                                
011800 01  -COPY WF0287O1  -PRE MAP-                                            
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012100     SKIP3                                                                
012200 01  REQU-AREA.                                                           
012300*    03  -COPY WZ01REQU                                                   
012400*    03  -COPY WF0287I1                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012700     SKIP3                                                                
012800 01  RESP-AREA.                                                           
012900*    03  -COPY WZ01RESP                                                   
013000*    03  -COPY WF0287O1                                                   
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
013300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013400                                                                          
013500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
013600 01  DB2-WS.                                                              
013700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
013800         88  CURSOR-OK                       VALUE 000.                   
013900         88  LINES-FOUND                     VALUE 000.                   
014000         88  LINES-MISSING                   VALUE 100.                   
014100         88  RESOURCE-WRONG                  VALUE 904.                   
014200     03  GOOD-SQLCODECODES.                                               
014300         05  GOOD-SQLCODE OCCURS 5                                        
014400             INDEXED BY SQLCODE-IX PIC 9(3).                              
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'T01LSEL-AREA'.        
014700*01  -COPY T01LSEL -PRE T01LSEL-                                          
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'T01COCO-AREA'.        
015000*01  -COPY T01COCO -PRE T01COCO-                                          
015100     EJECT                                                                
015200 01  FILLER                      PIC X(16)   VALUE 'T01DOTY-AREA'.        
015300*01  -COPY T01DOTY -PRE T01DOTY-                                          
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)   VALUE 'T01CUGR-AREA'.        
015600*01  -COPY T01CUGR -PRE T01CUGR-                                          
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)   VALUE 'T01BURE-AREA'.        
015900*01  -COPY T01BURE -PRE T01BURE-                                          
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'T01NSDO-AREA'.        
016200*01  -COPY T01NSDO -PRE T01NSDO-                                          
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)   VALUE 'T01ASNS-AREA'.        
016500*01  -COPY T01ASNS -PRE T01ASNS-                                          
016600     EJECT                                                                
016700     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
016800     EJECT                                                                
016900     EXEC SQL INCLUDE T01COCO END-EXEC.                                   
017000     EJECT                                                                
017100     EXEC SQL INCLUDE T01DOTY END-EXEC.                                   
017200     EJECT                                                                
017300     EXEC SQL INCLUDE T01CUGR END-EXEC.                                   
017400     EJECT                                                                
017500     EXEC SQL INCLUDE T01BURE END-EXEC.                                   
017600     EJECT                                                                
017700     EXEC SQL INCLUDE T01NSDO END-EXEC.                                   
017800     EJECT                                                                
017900     EXEC SQL INCLUDE T01ASNS END-EXEC.                                   
018000     EJECT                                                                
018100 LINKAGE SECTION.                                                         
018200     EJECT                                                                
018300                                                                          
018400 PROCEDURE DIVISION.                                                      
018500 MAIN SECTION.                                                            
018600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
018700     IF SUB-KDRC = 0                                                      
018800       PERFORM A-INIT                                                     
018900       PERFORM B-CHECK-KEYS                                               
019000       IF KEYS-OK                                                         
019100         PERFORM F-READ-SHOW-INFO                                         
019200       END-IF                                                             
019300       IF KEYS-WRONG                                                      
019400         PERFORM S04-MOVE-MISSING-TO-RESPOND                              
019500       END-IF                                                             
019600       PERFORM S02-RETURN-RESPONSE                                        
019700     END-IF                                                               
019800                                                                          
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300                                                                          
020400 A-INIT SECTION.                                                          
020500     INITIALIZE GOOD-SQLCODECODES                                         
020600     MOVE ALL '+' TO RESP-AREA                                            
020700     MOVE SPACE TO RESP-IDMSG-ERROR                                       
020800     MOVE SPACE TO RESP-IDMSG-INFO                                        
020900     MOVE SPACE TO RESP-IDELMT-ERROR                                      
021000     INITIALIZE MAP-RESP-WF0287O1                                         
021100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
021200     .                                                                    
021300     EJECT                                                                
021400                                                                          
021500*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
021600 B-CHECK-KEYS SECTION.                                                    
021700     MOVE YES TO KEYS-SW                                                  
021800     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
021900                                                                          
022000     IF REQU-IDMSGVER NUMERIC                                             
022100       IF REQU-IDLEGSEL-KEY > SPACE                                       
022200       AND REQU-IDLEGSEL-KEY NOT = ALL '+'                                
022300       AND REQU-KDFINDOC-KEY > SPACE                                      
022400       AND REQU-KDFINDOC-KEY NOT = ALL '+'                                
022500       AND REQU-KDPARTTY-KEY > SPACE                                      
022600       AND REQU-KDPARTTY-KEY NOT = ALL '+'                                
022700       AND REQU-KDPARTGR-KEY > SPACE                                      
022800       AND REQU-KDPARTGR-KEY NOT = ALL '+'                                
022900       AND REQU-IDLANDX3-KEY > SPACE                                      
023000       AND REQU-IDLANDX3-KEY NOT = ALL '+'                                
023200       AND REQU-IDLOPNR-KEY NUMERIC                                       
023300       AND REQU-IDLOPNR-KEY > ZERO                                        
023400       AND ACT-CODE-VALID                                                 
023500         CONTINUE                                                         
023600       ELSE                                                               
023700         MOVE NOO TO KEYS-SW                                              
023800       END-IF                                                             
023900     ELSE                                                                 
024000       MOVE NOO TO KEYS-SW                                                
024100     END-IF                                                               
024200                                                                          
024300     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
024400       MOVE NOO TO KEYS-SW                                                
024500     END-IF                                                               
024600                                                                          
024700     IF KEYS-WRONG                                                        
024800       MOVE ERR-INVALID-KEY       TO RESP-IDMSG-ERROR                     
024900       IF REQU-IDMSGVER NUMERIC                                           
025000         CONTINUE                                                         
025100       ELSE                                                               
025200         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
025300         MOVE 'IDMSGVER'   TO RESP-IDELMT-ERROR                           
025400       END-IF                                                             
025500       IF ACT-CODE-VALID                                                  
025600         CONTINUE                                                         
025700       ELSE                                                               
025800         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
025900         MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                           
026000       END-IF                                                             
026100       IF REQU-IDUSER = SPACE OR = ALL '+'                                
026200         MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                            
026300         MOVE 'IDUSER'     TO RESP-IDELMT-ERROR                           
026400       END-IF                                                             
026500     END-IF                                                               
026600     IF KEYS-OK                                                           
026700       IF REQU-IDLANDX3-KEY = 'XX'                                        
026800         CONTINUE                                                         
026900       ELSE                                                               
027000         PERFORM DB2-SELECT-T01COCO-TAB                                   
027100         IF LINES-FOUND                                                   
027200           CONTINUE                                                       
027300         ELSE                                                             
027400           MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                            
027500           MOVE 'IDLANDX3' TO RESP-IDELMT-ERROR                           
027600           MOVE NOO TO KEYS-SW                                            
027700         END-IF                                                           
027800       END-IF                                                             
027900     END-IF                                                               
028000     IF KEYS-OK                                                           
028100       PERFORM DB2-SELECT-T01LSEL-TAB                                     
028200       IF LINES-FOUND                                                     
028300         CONTINUE                                                         
028400       ELSE                                                               
028500         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
028600         MOVE 'IDLEGSEL' TO RESP-IDELMT-ERROR                             
028700         MOVE NOO TO KEYS-SW                                              
028800       END-IF                                                             
028900     END-IF                                                               
029000     IF KEYS-OK                                                           
029100       PERFORM DB2-SELECT-T01DOTY-TAB                                     
029200       IF LINES-FOUND                                                     
029300         CONTINUE                                                         
029400       ELSE                                                               
029500         MOVE NOT-FOUND  TO RESP-IDMSG-ERROR                              
029600         MOVE 'KDFINDOC' TO RESP-IDELMT-ERROR                             
029700         MOVE NOO TO KEYS-SW                                              
029800       END-IF                                                             
029900     END-IF                                                               
030000     IF KEYS-OK                                                           
030100       PERFORM DB2-SELECT-T01CUGR-TAB                                     
030200       IF LINES-FOUND                                                     
030300         CONTINUE                                                         
030400       ELSE                                                               
030500         MOVE NOT-FOUND          TO RESP-IDMSG-ERROR                      
030600         MOVE 'PARTNER GROUPING' TO RESP-IDELMT-ERROR                     
030700         MOVE NOO TO KEYS-SW                                              
030800       END-IF                                                             
030900     END-IF                                                               
031000     .                                                                    
031100     EJECT                                                                
031200                                                                          
031300*** - MOVE SEARCHING KEYS AND COMPULSORY FIELDS TO RESPOND                
031400 F-READ-SHOW-INFO SECTION.                                                
031500     MOVE REQU-IDLEGSEL-KEY  TO RESP-IDLEGSEL-KEY                         
031600     MOVE REQU-KDFINDOC-KEY  TO RESP-KDFINDOC-KEY                         
031700     MOVE REQU-KDPARTTY-KEY  TO RESP-KDPARTTY-KEY                         
031800     MOVE REQU-KDPARTGR-KEY  TO RESP-KDPARTGR-KEY                         
031900     MOVE REQU-IDLANDX3-KEY  TO RESP-IDLANDX3-KEY                         
032200     MOVE REQU-IDLOPNR-KEY   TO RESP-IDLOPNR-KEY                          
032300                                MAP-IDLOPNR-KEY                           
032400     MOVE T01LSEL-BELEGRAD-1 TO RESP-BELEGRAD-1                           
032500                                                                          
032600     PERFORM FA-READ-BASICDATA                                            
032700     .                                                                    
032800     EJECT                                                                
032900                                                                          
033000*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
033100 FA-READ-BASICDATA SECTION.                                               
033200     IF ACT-CODE-SEARCH                                                   
033300       PERFORM FAA-SEARCH-T01ASNS                                         
033400     ELSE                                                                 
033500       IF ACT-CODE-INSERT                                                 
033600         PERFORM FAB-INSERT-T01ASNS                                       
033700       ELSE                                                               
033800         IF ACT-CODE-DELETE                                               
033900           PERFORM FAC-DELETE-T01ASNS                                     
034000         END-IF                                                           
034100       END-IF                                                             
034200     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034500                                                                          
034600*** - SEARCH FOR RIGHT DOCUMENT NUMBER SERIE ASSIGNMENT                   
034700 FAA-SEARCH-T01ASNS SECTION.                                              
034800     PERFORM DB2-SELECT-T01ASNS-TAB                                       
034900     IF LINES-FOUND                                                       
035000       PERFORM S03-MOVE-TO-RESPOND                                        
035100     ELSE                                                                 
035200       MOVE NOT-FOUND          TO RESP-IDMSG-ERROR                        
035300       MOVE 'DOCNOSEREASS'     TO RESP-IDELMT-ERROR                       
035400       MOVE NOO TO KEYS-SW                                                
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800                                                                          
035900*** - INSERT NEW LINE.                                                    
036000 FAB-INSERT-T01ASNS SECTION.                                              
036100     PERFORM DB2-SELECT-T01ASNS-TAB-INS                                   
036200     IF LINES-FOUND                                                       
036300       MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                         
036400       MOVE 'DOCNOSERIEASS'   TO RESP-IDELMT-ERROR                        
036500     ELSE                                                                 
036600       PERFORM DB2-SELECT-T01BURE-TAB                                     
036700       IF LINES-FOUND                                                     
036800         PERFORM FABA-CHECK-INSERT-DATA                                   
036900         IF RESP-IDMSG-ERROR = SPACE                                      
037000           PERFORM DB2-INSERT-T01ASNS-TAB                                 
037100           MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                          
037200           PERFORM S03-MOVE-TO-RESPOND                                    
037300         END-IF                                                           
037400       ELSE                                                               
037500         MOVE NOT-FOUND           TO RESP-IDMSG-ERROR                     
037600         MOVE 'BUSINESSRELATION' TO RESP-IDELMT-ERROR                     
037700         MOVE NOO TO KEYS-SW                                              
037800       END-IF                                                             
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200                                                                          
038300*** - VALIDATE REQUESTED FIELDS FOR INSERT ON DOCUMENT NUMBER             
038400***   SERIE ASSIGNMENT.                                                   
038500 FABA-CHECK-INSERT-DATA SECTION.                                          
038600     IF RESP-IDMSG-ERROR = SPACE                                          
038700       PERFORM DB2-SELECT-T01NSDO-TAB                                     
038800       IF LINES-MISSING                                                   
039600         MOVE NOT-FOUND              TO RESP-IDMSG-ERROR                  
039700         MOVE 'IDLOPNR'              TO RESP-IDELMT-ERROR                 
039800         MOVE NOO TO KEYS-SW                                              
039900       END-IF                                                             
040000     END-IF                                                               
040100                                                                          
040200     IF RESP-IDMSG-ERROR = SPACE                                          
040300       MOVE WS-CURRENT-DATE TO MAP-DAREGDAT                               
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700                                                                          
040800*** - PHYSICAL DELETE FROM TABLE T01ASNS                                  
040900 FAC-DELETE-T01ASNS SECTION.                                              
041000     PERFORM DB2-SELECT-T01ASNS-TAB                                       
041100     IF LINES-FOUND                                                       
041200       PERFORM FACA-CHECK-DELETE-DATA                                     
041300       IF RESP-IDMSG-ERROR = SPACE                                        
041400         PERFORM DB2-DELETE-T01ASNS-TAB                                   
041500         PERFORM S03-MOVE-TO-RESPOND                                      
041600         MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                            
041700       END-IF                                                             
041800     ELSE                                                                 
041900       MOVE NOT-FOUND           TO RESP-IDMSG-ERROR                       
042000       MOVE 'DOCNOSERIEASS'     TO RESP-IDELMT-ERROR                      
042100       MOVE NOO TO KEYS-SW                                                
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500                                                                          
042600*** - VALIDATE REQUESTED FIELDS FOR DELETE ON DOCUMENT NUMBER             
042700***   SERIE ASSIGNMENT.                                                   
042800*** - CALL ABEND WHEN RECORD NOT IN T01NSDO (SERIOUS DB2 ERROR) !         
042900 FACA-CHECK-DELETE-DATA SECTION.                                          
043000     IF RESP-IDMSG-ERROR = SPACE                                          
043100       PERFORM DB2-SELECT-T01NSDO-TAB                                     
043200       IF LINES-FOUND                                                     
043300         IF T01NSDO-IDFINDOC-START  NOT = T01NSDO-IDFINDOC-NEXT           
043800           MOVE ERR-DELETE-NOT-ALLOWED TO RESP-IDMSG-ERROR                
043900         END-IF                                                           
044000       ELSE                                                               
044100         MOVE NOT-FOUND              TO RESP-IDMSG-ERROR                  
044200         MOVE 'IDLOPNR'              TO RESP-IDELMT-ERROR                 
044300         MOVE NOO TO KEYS-SW                                              
044400       END-IF                                                             
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800                                                                          
044900*    --- DISPATCHER SECTIONS                                              
045000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
045100     MOVE 'GETARG'                   TO SUB-KDFUNC                        
045200     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
045300     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
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
046400     EJECT                                                                
046500                                                                          
046600 S02-RETURN-RESPONSE SECTION.                                             
046700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
046800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
046900                                                                          
047000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
047100                                                                          
047200     IF SUB-KDRC > 0                                                      
047300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
047400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
047500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
047600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000                                                                          
048100*    --- MOVE TO OUTPUT SECTIONS                                          
048200 S03-MOVE-TO-RESPOND SECTION.                                             
048300     IF ACT-CODE-SEARCH                                                   
048400       MOVE MAP-DAREGDAT    TO RESP-DAREGDAT                              
048500       MOVE MAP-DADELDAT    TO RESP-DADELDAT                              
048600       MOVE MAP-RESP-IDUSER TO RESP-IDUSER                                
048700     ELSE                                                                 
048800       IF ACT-CODE-INSERT                                                 
048900         MOVE MAP-DAREGDAT  TO RESP-DAREGDAT                              
049000         MOVE MAP-DADELDAT  TO RESP-DADELDAT                              
049100         MOVE REQU-IDUSER   TO RESP-IDUSER                                
049200       ELSE                                                               
049300         IF ACT-CODE-DELETE                                               
049400           MOVE ZERO            TO RESP-DAREGDAT                          
049500           MOVE WS-CURRENT-DATE TO RESP-DADELDAT                          
049600           MOVE REQU-IDUSER     TO RESP-IDUSER                            
049700         END-IF                                                           
049800       END-IF                                                             
049900     END-IF                                                               
050000     .                                                                    
050100     EJECT                                                                
050200                                                                          
050300 S04-MOVE-MISSING-TO-RESPOND SECTION.                                     
050400     MOVE SPACE             TO RESP-IDUSER                                
050500     MOVE ZERO              TO RESP-DAREGDAT                              
050600                               RESP-DADELDAT                              
050700     .                                                                    
050800     EJECT                                                                
050900                                                                          
051000*    --- DB2 SECTIONS                                                     
051100 DB2-SELECT-T01LSEL-TAB   SECTION.                                        
051200     MOVE 000100 TO GOOD-SQLCODECODES                                     
051300                                                                          
051400     EXEC SQL                                                             
051500        SELECT  BELEGRAD_1                                                
051600                                                                          
051700        INTO   :T01LSEL-BELEGRAD-1                                        
051800                                                                          
051900        FROM    T01LSEL                                                   
052000                                                                          
052100        WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                             
052200            AND KDSTATUS = :WS-CURRENT                                    
052300     END-EXEC                                                             
052400                                                                          
052500     MOVE SQLCODE TO SQLCODE-WS                                           
052600     PERFORM DB2-STATUS-CHECK                                             
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000 DB2-SELECT-T01DOTY-TAB  SECTION.                                         
053100     MOVE 000100 TO GOOD-SQLCODECODES                                     
053200     EXEC SQL                                                             
053300           SELECT  BEFINDOC                                               
053400                                                                          
053500           INTO   :T01DOTY-BEFINDOC                                       
053600                                                                          
053700           FROM    T01DOTY                                                
053800                                                                          
053900           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
054000               AND KDFINDOC = :REQU-KDFINDOC-KEY                          
054100               AND KDSTATUS = :WS-CURRENT                                 
054200     END-EXEC                                                             
054300     MOVE SQLCODE TO SQLCODE-WS                                           
054400     PERFORM DB2-STATUS-CHECK                                             
054500     .                                                                    
054600     EJECT                                                                
054700                                                                          
054800 DB2-SELECT-T01CUGR-TAB  SECTION.                                         
054900     MOVE 000100 TO GOOD-SQLCODECODES                                     
055000     EXEC SQL                                                             
055100           SELECT  KDINVFRQ                                               
055200                                                                          
055300           INTO   :T01CUGR-KDINVFRQ                                       
055400                                                                          
055500           FROM    T01CUGR                                                
055600                                                                          
055700           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
055800               AND KDPARTTY = :REQU-KDPARTTY-KEY                          
055900               AND KDPARTGR = :REQU-KDPARTGR-KEY                          
056000               AND KDSTATUS = :WS-CURRENT                                 
056100     END-EXEC                                                             
056200     MOVE SQLCODE TO SQLCODE-WS                                           
056300     PERFORM DB2-STATUS-CHECK                                             
056400     .                                                                    
056500     EJECT                                                                
056600                                                                          
056700 DB2-SELECT-T01COCO-TAB   SECTION.                                        
056800     MOVE 000100 TO GOOD-SQLCODECODES                                     
056900                                                                          
057000     EXEC SQL                                                             
057100        SELECT  IDLANDX3                                                  
057200                                                                          
057300        INTO   :T01COCO-IDLANDX3                                          
057400                                                                          
057500        FROM    T01COCO                                                   
057600                                                                          
057700        WHERE   IDLANDX3 = :REQU-IDLANDX3-KEY                             
057800     END-EXEC                                                             
057900                                                                          
058000     MOVE SQLCODE TO SQLCODE-WS                                           
058100     PERFORM DB2-STATUS-CHECK                                             
058200     .                                                                    
058300     EJECT                                                                
058400                                                                          
058500 DB2-SELECT-T01NSDO-TAB SECTION.                                          
058600     MOVE 000100 TO GOOD-SQLCODECODES                                     
058700                                                                          
058800     EXEC SQL                                                             
058900        SELECT IDFINDOC_START                                             
059100             , IDFINDOC_NEXT                                              
059200                                                                          
059300        INTO  :T01NSDO-IDFINDOC-START                                     
059500            , :T01NSDO-IDFINDOC-NEXT                                      
059600                                                                          
059700        FROM   T01NSDO                                                    
059800                                                                          
059900        WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                              
060100        AND    IDLOPNR  = :MAP-IDLOPNR-KEY                                
060200     END-EXEC                                                             
060300                                                                          
060400     MOVE SQLCODE TO SQLCODE-WS                                           
060500     PERFORM DB2-STATUS-CHECK                                             
060600     .                                                                    
060700     EJECT                                                                
060800                                                                          
060900 DB2-SELECT-T01ASNS-TAB SECTION.                                          
061000     MOVE 000100  TO GOOD-SQLCODECODES                                    
061100                                                                          
061200     EXEC SQL                                                             
061300         SELECT DAREGDAT                                                  
061400             ,  IDUSER                                                    
061500                                                                          
061600         INTO  :MAP-DAREGDAT                                              
061700             , :MAP-RESP-IDUSER                                           
061800                                                                          
061900         FROM  T01ASNS                                                    
062000                                                                          
062100         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
062200         AND   KDFINDOC = :REQU-KDFINDOC-KEY                              
062300         AND   KDPARTTY = :REQU-KDPARTTY-KEY                              
062400         AND   KDPARTGR = :REQU-KDPARTGR-KEY                              
062500         AND   IDLANDX3 = :REQU-IDLANDX3-KEY                              
062700         AND   IDLOPNR  = :MAP-IDLOPNR-KEY                                
062800     END-EXEC                                                             
062900                                                                          
063000     MOVE SQLCODE TO SQLCODE-WS                                           
063100     PERFORM DB2-STATUS-CHECK                                             
063200     .                                                                    
063300     EJECT                                                                
063400                                                                          
063500 DB2-SELECT-T01ASNS-TAB-INS SECTION.                                      
063600     MOVE 000100  TO GOOD-SQLCODECODES                                    
063700                                                                          
063800     EXEC SQL                                                             
063900         SELECT DAREGDAT                                                  
064000             ,  IDUSER                                                    
064100                                                                          
064200         INTO  :MAP-DAREGDAT                                              
064300             , :MAP-RESP-IDUSER                                           
064400                                                                          
064500         FROM  T01ASNS                                                    
064600                                                                          
064700         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
064800         AND   KDFINDOC = :REQU-KDFINDOC-KEY                              
064900         AND   KDPARTTY = :REQU-KDPARTTY-KEY                              
065000         AND   KDPARTGR = :REQU-KDPARTGR-KEY                              
065100         AND   IDLANDX3 = :REQU-IDLANDX3-KEY                              
065300     END-EXEC                                                             
065400                                                                          
065500     MOVE SQLCODE TO SQLCODE-WS                                           
065600     PERFORM DB2-STATUS-CHECK                                             
065700     .                                                                    
065800     EJECT                                                                
065900                                                                          
066000 DB2-SELECT-T01BURE-TAB SECTION.                                          
066100     MOVE 000100  TO GOOD-SQLCODECODES                                    
066200                                                                          
066300     EXEC SQL                                                             
066400         SELECT IDLEGSEL                                                  
066500                                                                          
066600         INTO  :WS-IDLEGSEL                                               
066700                                                                          
066800         FROM  T01BURE                                                    
066900                                                                          
067000         WHERE IDLEGSEL = :REQU-IDLEGSEL-KEY                              
067100         AND   KDFINDOC = :REQU-KDFINDOC-KEY                              
067200         AND   KDPARTTY = :REQU-KDPARTTY-KEY                              
067300         AND   KDPARTGR = :REQU-KDPARTGR-KEY                              
067400         AND   KDSTATUS = :WS-CURRENT                                     
067500         AND   DADELDAT = :WS-ACTIVE                                      
067600     END-EXEC                                                             
067700                                                                          
067800     MOVE SQLCODE TO SQLCODE-WS                                           
067900     PERFORM DB2-STATUS-CHECK                                             
068000     .                                                                    
068100     EJECT                                                                
068200                                                                          
068300 DB2-INSERT-T01ASNS-TAB SECTION.                                          
068400     MOVE 000   TO GOOD-SQLCODECODES                                      
068500                                                                          
068600     EXEC SQL                                                             
068700         INSERT INTO T01ASNS                                              
068800            (IDLEGSEL,KDFINDOC,KDPARTTY,KDPARTGR,IDLANDX3                 
068900            ,IDLOPNR,DAREGDAT,IDUSER)                                     
069000         VALUES                                                           
069100            (:REQU-IDLEGSEL-KEY,:REQU-KDFINDOC-KEY                        
069200            ,:REQU-KDPARTTY-KEY,:REQU-KDPARTGR-KEY                        
069300            ,:REQU-IDLANDX3-KEY                                           
069400            ,:MAP-IDLOPNR-KEY,:MAP-DAREGDAT,:REQU-IDUSER)                 
069500     END-EXEC                                                             
069600                                                                          
069700     MOVE SQLCODE TO SQLCODE-WS                                           
069800     PERFORM DB2-STATUS-CHECK                                             
069900     .                                                                    
070000     EJECT                                                                
070100                                                                          
070200 DB2-DELETE-T01ASNS-TAB SECTION.                                          
070300     MOVE 000   TO GOOD-SQLCODECODES                                      
070400                                                                          
070500     EXEC SQL                                                             
070600         DELETE FROM T01ASNS                                              
070700                                                                          
070800         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
070900            AND KDFINDOC = :REQU-KDFINDOC-KEY                             
071000            AND KDPARTTY = :REQU-KDPARTTY-KEY                             
071100            AND KDPARTGR = :REQU-KDPARTGR-KEY                             
071200            AND IDLANDX3 = :REQU-IDLANDX3-KEY                             
071400            AND IDLOPNR  = :MAP-IDLOPNR-KEY                               
071500     END-EXEC                                                             
071600                                                                          
071700     MOVE SQLCODE TO SQLCODE-WS                                           
071800     PERFORM DB2-STATUS-CHECK                                             
071900     .                                                                    
072000     EJECT                                                                
072100                                                                          
072200 DB2-STATUS-CHECK  SECTION.                                               
072300     SET SQLCODE-IX TO 1                                                  
072400     SEARCH GOOD-SQLCODE                                                  
072500       AT END                                                             
072600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
072700          DELIMITED BY SIZE INTO ERROR-TEXT                               
072800          CALL ABEND USING RKOD-ABEND-DB2                                 
072900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
073000     END-SEARCH                                                           
073100     .                                                                    
