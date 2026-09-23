000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF020400.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   02/02/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNCTION:                                                            
001000*      - STARTS EVERY 5TH MINUTE                                          
001100*      - DECIDES FOR EACH LEGAL SELLER IF                                 
001200*        . A  PERIOD  PROCESS IS TO BE INITIATED                          
001300*        . A  WEEKLY  PROCESS IS TO BE INITIATED                          
001400*        . A  DAILY   PROCESS IS TO BE INITIATED                          
001500*        . AN INSTANT PROCESS IS TO BE INITIATED                          
001600*      - SELECTS LINES FROM THE APPROVED LINE REGISTER                    
001700*      - BUILDS LINES IN THE SELECTED LINE REGISTER                       
001800*                                                                         
001900*        THE PROGRAM UPDATES ROWS IN TABLE  T01ALIN                       
002000*        THE PROGRAM READS   ROWS IN TABLE  T01ABUN                       
002100*        THE PROGRAM READS   ROWS IN TABLE  T01LSEL                       
002200*        THE PROGRAM UPDATES ROWS IN TABLE  T01PECA                       
002300*        THE PROGRAM UPDATES ROWS IN TABLE  T01PROC                       
002400*        THE PROGRAM INSERTS ROWS IN TABLE  T01WEEK                       
002500*        THE PROGRAM INSERTS ROWS IN TABLE  T01DAY                        
002600*        THE PROGRAM INSERTS ROWS IN TABLE  T01SLIN                       
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300 DATA DIVISION.                                                           
003400 FILE SECTION.                                                            
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'WF020400'.            
003800                                                                          
003900*    --- WORK FIELD FOR ERROR MESSAGES WHEN CALLING ABEND.                
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004200 77  YES                         PIC X       VALUE 'J'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 77  WS-IX2                      PIC S9(7) VALUE +0     COMP-3.           
004600 77  MAX-LINES                   PIC S9(7) VALUE +95000 COMP-3.           
004700                                                                          
004800 01  WS-DATUM                    PIC  X(8).                               
004900 01  WS-TIDSKOLL                 PIC  X(14).                              
005000                                                                          
005100 01  WS-VECKA                    PIC S9(5)  COMP-3 VALUE ZERO.            
005200 01  WS-AAR-VECKA-DAG            PIC 9(5)   VALUE ZERO.                   
005300 77  WS-TIME-WAIT                PIC S9(9)   COMP VALUE +6000.            
005400                                                                          
005500     EJECT                                                                
005600                                                                          
005700 77  PROCESS-RUNNING-SW          PIC X      VALUE 'N'.                    
005800     88  PROCESS-RUNNING                    VALUE 'J'.                    
005900                                                                          
006000 77  PERIOD-SW                   PIC X.                                   
006100     88  PERIOD-YES                         VALUE 'J'.                    
006200     88  PERIOD-NOO                         VALUE 'N'.                    
006300                                                                          
006400 77  WEEK-SW                     PIC X.                                   
006500     88  WEEK-YES                           VALUE 'J'.                    
006600     88  WEEK-NOO                           VALUE 'N'.                    
006700                                                                          
006800 77  DAY-SW                      PIC X.                                   
006900     88  DAY-YES                            VALUE 'J'.                    
007000     88  DAY-NOO                            VALUE 'N'.                    
007100                                                                          
007200 77  NOW-SW                      PIC X.                                   
007300     88  NOW-YES                            VALUE 'J'.                    
007400     88  NOW-NOO                            VALUE 'N'.                    
007500                                                                          
007600 01  ERRORTEXT.                                                           
007700     03  FILLER                  PIC X(9)    VALUE 'ERRORTEXT'.           
007800     03  ERRORTEXT-STR           PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000                                                                          
008100*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
008200 01  GENERAL-SUBPROGRAMS.                                                 
008300     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
008400     EJECT                                                                
008500                                                                          
008600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008700 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
008800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
008900 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
009000 77  RKOD-ABEND-IMS              PIC S9(4)  COMP VALUE +1000.             
009100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
009200     SKIP2                                                                
009300                                                                          
009400 01  MESSAGE-CODES.                                                       
009500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009600     EJECT                                                                
009700                                                                          
009800 01  DYNAMIC-SUBPROGRAMS.                                                 
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  W980SOP                 PIC X(8)    VALUE 'WW980SOP'.            
010100     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
010200     EJECT                                                                
010300                                                                          
010400 01  FILLER              PIC X(16)   VALUE ' WMSGSOP-AREA'.               
010500*01      -COPY WMSGSOP                                                    
010600     EJECT                                                                
010700                                                                          
010800*01      -COPY WMSGAREA                                                   
010900                                                                          
011000*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100*                                                                         
011200 01  IMS-WS.                                                              
011300   03    FILLER              PIC X(16)   VALUE ' IMS-WS     '.            
011400     SKIP3                                                                
011500   03    STATUS-WS           PIC XX.                                      
011600     88  STATUS-OK                       VALUE '  '.                      
011700     88  SEGMENT-FINNS                   VALUE '  '.                      
011800     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
011900     88  TRANSKOD-FEL                    VALUE 'A1'.                      
012000     88  SECURITY-FEL                    VALUE 'A4'.                      
012100     SKIP3                                                                
012200   03    GODK-STATUSKODER.                                                
012300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012400     EJECT                                                                
012500                                                                          
012600 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
012700     SKIP3                                                                
012800*    -COPY WZ20DAYS                                                       
012900     EJECT                                                                
013000                                                                          
013100 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
013200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
013300                                                                          
013400 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
013500 01  DB2-WS.                                                              
013600     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
013700        88  CURSOR-OK                       VALUE 000.                    
013800        88  LINES-FOUND                     VALUE 000.                    
013900        88  LINES-MISSING                   VALUE 100.                    
014000        88  LINES-MISSING-EMPTY             VALUE 305.                    
014100        88  RESOURCE-WRONG                  VALUE 904.                    
014200                                                                          
014300     03  T01LSEL-WS              PIC 9(3)   VALUE ZERO.                   
014400        88 T01LSEL-OK                       VALUE 000.                    
014500        88 T01LSEL-MISSING                  VALUE 100.                    
014600        88 T01LSEL-ERROR                    VALUE 904.                    
014700                                                                          
014800     03  T01PROC-WS              PIC 9(3)   VALUE ZERO.                   
014900        88 T01PROC-OK                       VALUE 000.                    
015000        88 T01PROC-MISSING                  VALUE 100.                    
015100        88 T01PROC-ERROR                    VALUE 904.                    
015200                                                                          
015300     03  T01WEEK-WS              PIC 9(3)   VALUE ZERO.                   
015400        88 T01WEEK-OK                       VALUE 000.                    
015500        88 T01WEEK-MISSING                  VALUE 100.                    
015600        88 T01WEEK-ERROR                    VALUE 904.                    
015700                                                                          
015800     03  T01DAY-WS               PIC 9(3)   VALUE ZERO.                   
015900        88 T01DAY-OK                        VALUE 000.                    
016000        88 T01DAY-MISSING                   VALUE 100.                    
016100        88 T01DAY-ERROR                     VALUE 904.                    
016200                                                                          
016300     03  PERIOD-WS               PIC 9(3)   VALUE ZERO.                   
016400        88 PERIOD-OK                        VALUE 000.                    
016500        88 PERIOD-MISSING                   VALUE 100.                    
016600        88 PERIOD-ERROR                     VALUE 904.                    
016700                                                                          
016800     03  T01ABUN-WS              PIC 9(3)   VALUE ZERO.                   
016900        88 T01ABUN-OK                       VALUE 000.                    
017000        88 T01ABUN-MISSING                  VALUE 100.                    
017100        88 T01ABUN-ERROR                    VALUE 904.                    
017200                                                                          
017300     03  T01ALIN-WS              PIC 9(3)   VALUE ZERO.                   
017400        88 T01ALIN-OK                       VALUE 000.                    
017500        88 T01ALIN-MISSING                  VALUE 100.                    
017600        88 T01ALIN-ERROR                    VALUE 904.                    
017700                                                                          
017800     03  T01SLIN-WS              PIC 9(3)   VALUE ZERO.                   
017900        88 T01SLIN-OK                       VALUE 000.                    
018000        88 T01SLIN-MISSING                  VALUE 100.                    
018100        88 T01SLIN-ERROR                    VALUE 904.                    
018200                                                                          
018300     03  GOOD-SQLCODEKODER.                                               
018400         05  GOOD-SQLCODE OCCURS 5                                        
018500             INDEXED BY SQLCODE-IX PIC 9(3).                              
018600                                                                          
018700 01  WS-IX                      PIC S9(5) VALUE ZERO COMP-3.              
018800 01  WS-TOMKORNING              PIC S9(5) VALUE ZERO COMP-3.              
018900                                                                          
019000 01  FILLER                    PIC X(16)  VALUE 'WS-AREA'.                
019100 01  WS-AREA.                                                             
019200     03 WS-KDBEH               PIC X(1)   VALUE SPACE.                    
019300     03 WS-IDLEGSEL            PIC X(4)   VALUE SPACE.                    
019400     03 WS-DAEXDAT             PIC X(8)   VALUE SPACE.                    
019500     03 WS-TIEXTID             PIC S9(7)  COMP-3 VALUE ZERO.              
019600     03 WS-KDVALISO            PIC X(3)   VALUE SPACE.                    
019700     03 WS-IDLANDX3-SEND       PIC X(3)   VALUE SPACE.                    
019800     03 WS-IDLEVNR             PIC X(5)   VALUE SPACE.                    
019900     03 WS-IDPARTNR            PIC X(9)   VALUE SPACE.                    
020000     03 WS-KDFINDOC            PIC X(4)   VALUE SPACE.                    
020100     03 WS-FLSOFT              PIC X(1)   VALUE SPACE.                    
020200     03 WS-FLFREE              PIC X(1)   VALUE SPACE.                    
020300     03 WS-FLPRIV              PIC X(1)   VALUE SPACE.                    
020400     03 WS-IDBREAK-1           PIC X(8)   VALUE SPACE.                    
020500     03 WS-IDBREAK-2           PIC X(8)   VALUE SPACE.                    
020600     03 WS-IDTRANS-1           PIC X(4)   VALUE SPACE.                    
020700     03 WS-IDLOPNR             PIC S9(5)  COMP-3 VALUE ZERO.              
020800     03 WS-IDLOPNR-TXT         PIC X(5)   VALUE SPACE.                    
020900     03 WS-IDBUNDLE            PIC X(15)  VALUE SPACE.                    
021000     03 WS-DAREGDAT            PIC X(8)   VALUE SPACE.                    
021100     03 WS-TIREGTID            PIC S9(10) COMP-3 VALUE ZERO.              
021200     03 WS-IDREF               PIC X(15)  VALUE SPACE.                    
021300     03 WS-IDREFRAD            PIC S9(5)  COMP-3 VALUE ZERO.              
021400     03 WS-DAREFDAT            PIC X(8)   VALUE SPACE.                    
021500     03 WS-KDPARTTY            PIC X(3)   VALUE SPACE.                    
021600     03 WS-KDPARTGR            PIC X(4)   VALUE SPACE.                    
021700     03 WS-DASTADAT            PIC X(8)   VALUE SPACE.                    
021800     03 WS-KDINVFRQ            PIC X(4)   VALUE SPACE.                    
021900     03 WS-FLPERIOD            PIC X(1)   VALUE SPACE.                    
022000     03 WS-TIRP                PIC S9(2)  COMP-3 VALUE ZERO.              
022100     03 WS-IDSEQ-1             PIC X(8)   VALUE SPACE.                    
022200     03 WS-IDSEQ-2             PIC X(8)   VALUE SPACE.                    
022300     03 WS-IDSEQ-3             PIC X(8)   VALUE SPACE.                    
022400     EJECT                                                                
022500                                                                          
022600 01  FILLER                    PIC X(16)  VALUE 'T01ALIN-AREA'.           
022700*01  -COPY T01ALIN -PRE T01ALIN-                                          
022800     EJECT                                                                
022900                                                                          
023000 01  FILLER                    PIC X(16)  VALUE 'ALIN-AREA'.              
023100*01  -COPY T01ALIN -PRE ALIN-                                             
023200     EJECT                                                                
023300                                                                          
023400 01  FILLER                    PIC X(16)  VALUE 'T01ABUN-AREA'.           
023500*01  -COPY T01ABUN -PRE T01ABUN-                                          
023600     EJECT                                                                
023700                                                                          
023800 01  FILLER                    PIC X(16)  VALUE 'T01LSEL-AREA'.           
023900*01  -COPY T01LSEL -PRE T01LSEL-                                          
024000     EJECT                                                                
024100                                                                          
024200 01  FILLER                    PIC X(16)  VALUE 'LSEL-AREA'.              
024300*01  -COPY T01LSEL -PRE LSEL-                                             
024400     EJECT                                                                
024500                                                                          
024600 01  FILLER                    PIC X(16)  VALUE 'T01PECA-AREA'.           
024700*01  -COPY T01PECA -PRE T01PECA-                                          
024800     EJECT                                                                
024900                                                                          
025000 01  FILLER                    PIC X(16)  VALUE 'PECA-AREA'.              
025100*01  -COPY T01PECA -PRE PECA-                                             
025200     EJECT                                                                
025300                                                                          
025400 01  FILLER                    PIC X(16)  VALUE 'T01PROC-AREA'.           
025500*01  -COPY T01PROC -PRE T01PROC-                                          
025600     EJECT                                                                
025700                                                                          
025800 01  FILLER                    PIC X(16)  VALUE 'PROC-AREA'.              
025900*01  -COPY T01PROC -PRE PROC-                                             
026000     EJECT                                                                
026100                                                                          
026200 01  FILLER                    PIC X(16)  VALUE 'T01WEEK-AREA'.           
026300*01  -COPY T01WEEK -PRE T01WEEK-                                          
026400     EJECT                                                                
026500                                                                          
026600 01  FILLER                    PIC X(16)  VALUE 'WEEK-AREA'.              
026700*01  -COPY T01WEEK -PRE WEEK-                                             
026800     EJECT                                                                
026900                                                                          
027000 01  FILLER                    PIC X(16)  VALUE 'T01DAY-AREA'.            
027100*01  -COPY T01DAY -PRE T01DAY-                                            
027200     EJECT                                                                
027300                                                                          
027400 01  FILLER                    PIC X(16)  VALUE 'DAY-AREA'.               
027500*01  -COPY T01DAY -PRE DAY-                                               
027600     EJECT                                                                
027700                                                                          
027800 01  FILLER                    PIC X(16)  VALUE 'T01SLIN-AREA'.           
027900*01  -COPY T01SLIN -PRE T01SLIN-                                          
028000     EJECT                                                                
028100                                                                          
028200     EXEC SQL INCLUDE T01ALIN END-EXEC.                                   
028300     EJECT                                                                
028400     EXEC SQL INCLUDE T01ABUN END-EXEC.                                   
028500     EJECT                                                                
028600     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
028700     EJECT                                                                
028800     EXEC SQL INCLUDE T01PECA END-EXEC.                                   
028900     EJECT                                                                
029000     EXEC SQL INCLUDE T01PROC END-EXEC.                                   
029100     EJECT                                                                
029200     EXEC SQL INCLUDE T01WEEK END-EXEC.                                   
029300     EJECT                                                                
029400     EXEC SQL INCLUDE T01DAY END-EXEC.                                    
029500     EJECT                                                                
029600     EXEC SQL INCLUDE T01SLIN END-EXEC.                                   
029700     EJECT                                                                
029800                                                                          
029900*01      -COPY W0003                                                      
030000     EJECT                                                                
030100                                                                          
030200 LINKAGE SECTION.                                                         
030300*01  -COPY W0009     -PRE MSG-                                            
030400     EJECT                                                                
030500*01  -COPY W0009     -PRE ALTF204-                                        
030600     EJECT                                                                
030700*01  -COPY W0009     -PRE ALT0606-                                        
030800     EJECT                                                                
030900                                                                          
031000 PROCEDURE DIVISION USING  MSG-PCB ALT0606-PCB ALTF204-PCB.               
031100 MAIN SECTION.                                                            
031200     ENTRY 'DLITCBL' USING MSG-PCB ALT0606-PCB ALTF204-PCB.               
031300                                                                          
031400     PERFORM IMS-GET-MSG                                                  
031500     IF SEGMENT-FINNS                                                     
031600* CREDIT SHALL BE ABLE TO START THE PROGRAM, IF THERE IS NOTHING          
031700* TO PROCESS, THE PROGRAM SHALL END WBAT2KRE                              
031800* MSG-IDTRANS-1 = REQU-IDMSGVER IN NEW WZ01SEND                           
031900       IF MSG-IDTRANS-1 = 'W418'                                          
032000         CALL W009WAIT USING WS-TIME-WAIT                                 
032100         PERFORM DB2-SELECT-T01ALIN-FINNS                                 
032200         IF LINES-FOUND                                                   
032300           PERFORM A-RUN                                                  
032400         ELSE                                                             
032500           MOVE 'F204' TO MSGSOP-IDTRANS                                  
032600           MOVE 'WBAT2KRE' TO MSGSOP-IDPROCESS                            
032700           MOVE 'E' TO MSGSOP-KDSOPFUNK                                   
032800                                                                          
032900           PERFORM IMS-PURGE-ALTMSG-0606                                  
033000         END-IF                                                           
033100       ELSE                                                               
033200* ORDINARY START FROM W00507                                              
033300         PERFORM A-RUN                                                    
033400       END-IF                                                             
033500     END-IF                                                               
033600     MOVE ZERO TO RETURN-CODE                                             
033700     GOBACK                                                               
033800     .                                                                    
033900     EJECT                                                                
034000                                                                          
034100 A-RUN SECTION.                                                           
034200     MOVE NOO     TO PROCESS-RUNNING-SW                                   
034300     PERFORM DB2-OPEN-T01PROC-CRS                                         
034400     PERFORM DB2-FETCH-T01PROC-CRS                                        
034500     PERFORM UNTIL T01PROC-MISSING OR                                     
034600                   PROCESS-RUNNING                                        
034700       IF PROC-KDBEH > SPACE                                              
034800         MOVE YES TO PROCESS-RUNNING-SW                                   
034900       END-IF                                                             
035000       PERFORM DB2-FETCH-T01PROC-CRS                                      
035100     END-PERFORM                                                          
035200     PERFORM DB2-CLOSE-T01PROC-CRS                                        
035300                                                                          
035400     IF PROCESS-RUNNING                                                   
035500* ANOTHER PROCESS IS STILL RUNNING, EXECUTION ENDED                       
035600       MOVE ZERO TO WS-IX                                                 
035700       MOVE ZERO TO WS-TOMKORNING                                         
035800     ELSE                                                                 
035900       PERFORM A-INIT                                                     
036000       PERFORM DB2-OPEN-T01LSEL-CRS                                       
036100       PERFORM DB2-FETCH-T01LSEL-CRS                                      
036200* ALL LEGAL SELLERS ARE CHECKED                                           
036300       PERFORM UNTIL T01LSEL-MISSING OR WS-IX2 > MAX-LINES                
036400         PERFORM S01-DECIDE-INTERVALL                                     
036500         IF PERIOD-YES                                                    
036600* PERIOD PROCESS TO BE INTIATED                                           
036700           PERFORM B-PERIOD-PROCESS                                       
036800         ELSE                                                             
036900           IF WEEK-YES                                                    
037000* WEEKLY PROCESS TO BE INITIATED                                          
037100             PERFORM C-WEEKLY-PROCESS                                     
037200           ELSE                                                           
037300             IF DAY-YES                                                   
037400* DAILY PROCESS TO BE INITIATED                                           
037500               PERFORM D-DAILY-PROCESS                                    
037600             ELSE                                                         
037700               IF NOW-YES                                                 
037800* INSTANT PROCESS TO BE INITIATED                                         
037900                 PERFORM E-INSTANT-PROCESS                                
038000               END-IF                                                     
038100             END-IF                                                       
038200           END-IF                                                         
038300         END-IF                                                           
038400         PERFORM DB2-FETCH-T01LSEL-CRS                                    
038500       END-PERFORM                                                        
038600       PERFORM DB2-CLOSE-T01LSEL-CRS                                      
038700                                                                          
038800* PERFORMED TO MANY LINES INCREASE MAX-LINES OR MAKE A RESTART            
038900       IF WS-IX2 > MAX-LINES                                              
039000         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
039100       END-IF                                                             
039200                                                                          
039300       IF WS-IX > ZERO OR WS-TOMKORNING > ZERO                            
039400* IF AT LEAST ONE LINE IS SELECTED THE PROCESS IS INITIATED               
039500         MOVE 'F204' TO MSGSOP-IDTRANS                                    
039600         MOVE 'WF20S2' TO MSGSOP-IDPROCESS                                
039700         MOVE 'A' TO MSGSOP-KDSOPFUNK                                     
039800                                                                          
039900         PERFORM IMS-PURGE-ALTMSG-0606                                    
040000       ELSE                                                               
040100* NO LINES TO PROCESS, DON'T START SUCCEEDING PROCESSES                   
040200         PERFORM DB2-OPEN-T01PROC-CRS2                                    
040300         PERFORM DB2-FETCH-T01PROC-CRS2                                   
040400         PERFORM UNTIL T01PROC-MISSING                                    
040500           PERFORM DB2-UPDATE-T01PROC                                     
040600           PERFORM DB2-FETCH-T01PROC-CRS2                                 
040700         END-PERFORM                                                      
040800         PERFORM DB2-CLOSE-T01PROC-CRS2                                   
040900       END-IF                                                             
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300                                                                          
041400 A-INIT SECTION.                                                          
041500     MOVE NOO                    TO PERIOD-SW                             
041600                                    WEEK-SW                               
041700                                    DAY-SW                                
041800                                    NOW-SW                                
041900                                                                          
042000     MOVE ZERO                   TO WS-IX                                 
042100                                    WS-IX2                                
042200                                    WS-TOMKORNING                         
042300                                                                          
042400     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
042500                                         WS-DAEXDAT                       
042600     MOVE FUNCTION CURRENT-DATE (9:7) TO WS-TIEXTID                       
042700                                                                          
042800     MOVE WS-DATUM (1:8) TO DAYS-TIDATE1                                  
042900     MOVE 'YYYYMMDD' TO DAYS-KDDATFMT1                                    
043000     MOVE 0 TO DAYS-KVDAYS                                                
043100     MOVE 'WEEKDAYS' TO DAYS-IDCALEND                                     
043200     MOVE SPACE TO DAYS-TIDATE2                                           
043300     MOVE 'YYWW' TO DAYS-KDDATFMT2                                        
043400     CALL WZ20DAYS USING                                                  
043500          DAYS-WZ20DAYS                                                   
043600     IF DAYS-KDRC = ZERO                                                  
043700       MOVE DAYS-TIDATE2 (1:4) TO WS-VECKA                                
043800     END-IF                                                               
043900                                                                          
044000     INITIALIZE GOOD-SQLCODEKODER                                         
044100     .                                                                    
044200     EJECT                                                                
044300                                                                          
044400 B-PERIOD-PROCESS SECTION.                                                
044500     MOVE 'P' TO WS-KDBEH                                                 
044600* THE DATE ON THE DOCUMENT WILL BE THE LAST DAY IN THE PERIOD             
044700     MOVE PECA-DAFINDOC TO WS-DAEXDAT                                     
044800* THE TIME FOR PERIOD RUN WILL BE A FICTIVE ONE 9999999                   
044900     MOVE 9999999       TO WS-TIEXTID                                     
045000     PERFORM DB2-UPDATE-T01PROC-PER                                       
045100     PERFORM S02-PERFORM-PERIOD-LINES                                     
045200     ADD 1 TO WS-TOMKORNING                                               
045300     .                                                                    
045400     EJECT                                                                
045500                                                                          
045600 C-WEEKLY-PROCESS SECTION.                                                
045700     MOVE 'W' TO WS-KDBEH                                                 
045800* THE DATE ON THE DOCUMENT WILL BE THE LAST DAY IN THE PREVIUS            
045900* WEEK                                                                    
046000     PERFORM S99-DATESETTER                                               
046100* THE TIME FOR WEEK   RUN WILL BE A FICTIVE ONE 8888888                   
046200     MOVE 8888888       TO WS-TIEXTID                                     
046300     PERFORM DB2-UPDATE-T01PROC-WDN                                       
046400     PERFORM S03-PERFORM-WEEK-DAY-NOW-LINES                               
046500     PERFORM DB2-INSERT-T01WEEK                                           
046600                                                                          
046700     PERFORM S01-DECIDE-INTERVALL                                         
046800**** CAN'T RUN WEEK WITH DAY                                              
046900*    IF DAY-YES                                                           
047000* DAILY PROCESS TO BE INITIATED TOGETHER WITH WEEKLY PROCESS              
047100*      MOVE 'D' TO WS-KDBEH                                               
047200* THE DATE ON THE DOCUMENT WILL BE THE PREVIUS DAY                        
047300*      PERFORM S99-DATESETTER                                             
047400* THE TIME FOR DAY    RUN WILL BE A FICTIVE ONE 7777777                   
047500*      MOVE 7777777       TO WS-TIEXTID                                   
047600*      PERFORM DB2-UPDATE-T01PROC-WDN                                     
047700*      PERFORM S03-PERFORM-WEEK-DAY-NOW-LINES                             
047800*      PERFORM DB2-INSERT-T01DAY                                          
047900*    END-IF                                                               
048000     ADD 1 TO WS-TOMKORNING                                               
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
048400 D-DAILY-PROCESS SECTION.                                                 
048500     MOVE 'D' TO WS-KDBEH                                                 
048600* THE DATE ON THE DOCUMENT WILL BE THE PREVIUS DAY                        
048700     PERFORM S99-DATESETTER                                               
048800* THE TIME FOR DAY    RUN WILL BE A FICTIVE ONE 7777777                   
048900     MOVE 7777777       TO WS-TIEXTID                                     
049000     PERFORM DB2-UPDATE-T01PROC-WDN                                       
049100     PERFORM S03-PERFORM-WEEK-DAY-NOW-LINES                               
049200     PERFORM DB2-INSERT-T01DAY                                            
049300     ADD 1 TO WS-TOMKORNING                                               
049400     .                                                                    
049500     EJECT                                                                
049600                                                                          
049700 E-INSTANT-PROCESS SECTION.                                               
049800     MOVE 'N' TO WS-KDBEH                                                 
049900     PERFORM DB2-UPDATE-T01PROC-WDN                                       
050000     PERFORM S03-PERFORM-WEEK-DAY-NOW-LINES                               
050100     .                                                                    
050200     EJECT                                                                
050300                                                                          
050400 S01-DECIDE-INTERVALL SECTION.                                            
050500* CHECK IF LEGAL SELLER USES PERIOD-PROCESSING                            
050600     IF LSEL-FLSLUT = YES                                                 
050700* DECIDE IF PERIOD PROCESSING IS TO BE PERFORMED                          
050800       PERFORM DB2-SELECT-T01PECA-MAX                                     
050900       IF LINES-FOUND AND                                                 
051000          PECA-FLPERIOD = 'N'                                             
051100* PERIOD PROCESSING NOT YET PERFORMED                                     
051200         MOVE YES TO PERIOD-SW                                            
051300       ELSE                                                               
051400         MOVE NOO TO PERIOD-SW                                            
051500       END-IF                                                             
051600     END-IF                                                               
051700     IF PERIOD-NOO                                                        
051800* DECIDE IF WEEKLY PROCESSING IS TO BE PERFORMED                          
051900       PERFORM DB2-SELECT-T01WEEK                                         
052000       IF T01WEEK-MISSING                                                 
052100         MOVE YES TO WEEK-SW                                              
052200         MOVE 'WEEK' TO WS-KDINVFRQ                                       
052300       ELSE                                                               
052400         MOVE NOO TO WEEK-SW                                              
052500* DECIDE IF DAILY PROCESSING IS TO BE PERFORMED                           
052600         PERFORM DB2-SELECT-T01DAY                                        
052700         IF T01DAY-MISSING                                                
052800           MOVE YES TO DAY-SW                                             
052900           MOVE 'DAY' TO WS-KDINVFRQ                                      
053000         ELSE                                                             
053100           MOVE NOO TO DAY-SW                                             
053200* INSTANT PROCESSING IS TO BE PERFORMED                                   
053300           MOVE YES TO NOW-SW                                             
053400           MOVE 'NOW' TO WS-KDINVFRQ                                      
053500         END-IF                                                           
053600       END-IF                                                             
053700     END-IF                                                               
053800     .                                                                    
053900     EJECT                                                                
054000                                                                          
054100 S02-PERFORM-PERIOD-LINES SECTION.                                        
054200* KEEP THE BUNDLE TOGEHTER                                                
054300     PERFORM DB2-OPEN-T01ALIN-PER-CRS                                     
054400     PERFORM DB2-FETCH-T01ALIN-PER-CRS                                    
054500     PERFORM UNTIL LINES-MISSING OR WS-IX2 > MAX-LINES                    
054600         PERFORM S10-ROWCOUNTER                                           
054700         PERFORM DB2-INSERT-T01SLIN                                       
054800         PERFORM DB2-UPDATE-T01ALIN                                       
054900         PERFORM DB2-FETCH-T01ALIN-PER-CRS                                
055000     END-PERFORM                                                          
055100     PERFORM DB2-CLOSE-T01ALIN-PER-CRS                                    
055200                                                                          
055300     PERFORM DB2-UPDATE-T01PECA                                           
055400     .                                                                    
055500     EJECT                                                                
055600                                                                          
055700 S03-PERFORM-WEEK-DAY-NOW-LINES SECTION.                                  
055800* KEEP THE BUNDLE TOGEHTER                                                
055900     PERFORM DB2-OPEN-T01ALIN-WDN-CRS                                     
056000     PERFORM DB2-FETCH-T01ALIN-WDN-CRS                                    
056100     PERFORM UNTIL LINES-MISSING OR WS-IX2 > MAX-LINES                    
056200       PERFORM S10-ROWCOUNTER                                             
056300       PERFORM DB2-INSERT-T01SLIN                                         
056400       PERFORM DB2-UPDATE-T01ALIN                                         
056500       PERFORM DB2-FETCH-T01ALIN-WDN-CRS                                  
056600     END-PERFORM                                                          
056700     PERFORM DB2-CLOSE-T01ALIN-WDN-CRS                                    
056800     .                                                                    
056900     EJECT                                                                
057000                                                                          
057100 S10-ROWCOUNTER SECTION.                                                  
057200     IF ALIN-IDLEGSEL = WS-IDLEGSEL                                       
057300       AND ALIN-KDVALISO = WS-KDVALISO                                    
057400       AND ALIN-IDLANDX3-SEND = WS-IDLANDX3-SEND                          
057500       AND ALIN-IDLEVNR  = WS-IDLEVNR                                     
057600       AND ALIN-IDPARTNR = WS-IDPARTNR                                    
057700       AND ALIN-KDFINDOC = WS-KDFINDOC                                    
057800       AND ALIN-FLSOFT = WS-FLSOFT                                        
057900       AND ALIN-FLFREE = WS-FLFREE                                        
058000       AND ALIN-FLPRIV = WS-FLPRIV                                        
058100       AND ALIN-IDBREAK-1 = WS-IDBREAK-1                                  
058200       AND ALIN-IDBREAK-2 = WS-IDBREAK-2                                  
058300       ADD 1 TO WS-IX                                                     
058400       MOVE WS-IX TO WS-IDLOPNR                                           
058500     ELSE                                                                 
058600       MOVE ALIN-IDLEGSEL      TO WS-IDLEGSEL                             
058700       MOVE ALIN-KDVALISO      TO WS-KDVALISO                             
058800       MOVE ALIN-IDLANDX3-SEND TO WS-IDLANDX3-SEND                        
058900       MOVE ALIN-IDLEVNR       TO WS-IDLEVNR                              
059000       MOVE ALIN-IDPARTNR      TO WS-IDPARTNR                             
059100       MOVE ALIN-KDFINDOC      TO WS-KDFINDOC                             
059200       MOVE ALIN-FLSOFT        TO WS-FLSOFT                               
059300       MOVE ALIN-FLFREE        TO WS-FLFREE                               
059400       MOVE ALIN-FLPRIV        TO WS-FLPRIV                               
059500       MOVE ALIN-IDBREAK-1     TO WS-IDBREAK-1                            
059600       MOVE ALIN-IDBREAK-2     TO WS-IDBREAK-2                            
059700       ADD  1 TO WS-IX                                                    
059800       MOVE WS-IX TO WS-IDLOPNR                                           
059900     END-IF                                                               
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300 S99-DATESETTER SECTION.                                                  
060400     MOVE SPACE TO DAYS-TIDATE1                                           
060500     MOVE 'YYYYMMDD' TO DAYS-KDDATFMT1                                    
060600     MOVE 1 TO DAYS-KVDAYS                                                
060700     MOVE ' ' TO DAYS-IDCALEND                                            
060800     MOVE WS-DATUM (1:8) TO DAYS-TIDATE2                                  
060900     MOVE 'YYYYMMDD' TO DAYS-KDDATFMT2                                    
061000     CALL WZ20DAYS USING                                                  
061100          DAYS-WZ20DAYS                                                   
061200     IF DAYS-KDRC = ZERO                                                  
061300       MOVE DAYS-TIDATE1 (1:8) TO WS-DAEXDAT                              
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700                                                                          
061800 DB2-OPEN-T01PROC-CRS SECTION.                                            
061900     EXEC SQL DECLARE T01PROC-CRS CURSOR FOR                              
062000     SELECT   IDSYSTEM,                                                   
062100              IDLEGSEL,                                                   
062200              KDBEH                                                       
062300                                                                          
062400     FROM     T01PROC                                                     
062500                                                                          
062600     END-EXEC                                                             
062700                                                                          
062800     EXEC SQL OPEN T01PROC-CRS                                            
062900     END-EXEC                                                             
063000                                                                          
063100     MOVE 000            TO GOOD-SQLCODEKODER                             
063200     MOVE SQLCODE        TO SQLCODE-WS                                    
063300     PERFORM DB2-STATUS-CHECK                                             
063400     .                                                                    
063500     EJECT                                                                
063600                                                                          
063700 DB2-OPEN-T01PROC-CRS2 SECTION.                                           
063800     EXEC SQL DECLARE T01PROC-CRS2 CURSOR FOR                             
063900     SELECT   KDBEH,                                                      
064000              DAEXDAT,                                                    
064100              TIEXTID                                                     
064200                                                                          
064300     FROM     T01PROC                                                     
064400                                                                          
064500     FOR UPDATE OF                                                        
064600              KDBEH,                                                      
064700              DAEXDAT,                                                    
064800              TIEXTID                                                     
064900     END-EXEC                                                             
065000                                                                          
065100     EXEC SQL OPEN T01PROC-CRS2                                           
065200     END-EXEC                                                             
065300                                                                          
065400     MOVE 000            TO GOOD-SQLCODEKODER                             
065500     MOVE SQLCODE        TO SQLCODE-WS                                    
065600     PERFORM DB2-STATUS-CHECK                                             
065700     .                                                                    
065800     EJECT                                                                
065900                                                                          
066000 DB2-FETCH-T01PROC-CRS SECTION.                                           
066100     EXEC SQL FETCH T01PROC-CRS INTO                                      
066200            :PROC-IDSYSTEM,                                               
066300            :PROC-IDLEGSEL,                                               
066400            :PROC-KDBEH                                                   
066500     END-EXEC                                                             
066600                                                                          
066700     MOVE 000100         TO GOOD-SQLCODEKODER                             
066800     MOVE SQLCODE        TO SQLCODE-WS                                    
066900                            T01PROC-WS                                    
067000     PERFORM DB2-STATUS-CHECK                                             
067100     .                                                                    
067200     EJECT                                                                
067300                                                                          
067400 DB2-FETCH-T01PROC-CRS2 SECTION.                                          
067500     EXEC SQL FETCH T01PROC-CRS2 INTO                                     
067600            :PROC-KDBEH,                                                  
067700            :PROC-DAEXDAT,                                                
067800            :PROC-TIEXTID                                                 
067900     END-EXEC                                                             
068000                                                                          
068100     MOVE 000100         TO GOOD-SQLCODEKODER                             
068200     MOVE SQLCODE        TO SQLCODE-WS                                    
068300                            T01PROC-WS                                    
068400     PERFORM DB2-STATUS-CHECK                                             
068500     .                                                                    
068600     EJECT                                                                
068700                                                                          
068800 DB2-CLOSE-T01PROC-CRS SECTION.                                           
068900     EXEC SQL CLOSE T01PROC-CRS                                           
069000     END-EXEC                                                             
069100     .                                                                    
069200     EJECT                                                                
069300                                                                          
069400 DB2-CLOSE-T01PROC-CRS2    SECTION.                                       
069500     EXEC SQL CLOSE T01PROC-CRS2                                          
069600     END-EXEC                                                             
069700     .                                                                    
069800     EJECT                                                                
069900                                                                          
070000 DB2-OPEN-T01LSEL-CRS SECTION.                                            
070100     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
070200     SELECT   T01LSEL.IDLEGSEL,                                           
070300              T01LSEL.FLSLUT                                              
070400                                                                          
070500     FROM     T01LSEL                                                     
070600                                                                          
070700     WHERE    KDSTATUS = 1                                                
070800     END-EXEC                                                             
070900                                                                          
071000     EXEC SQL OPEN T01LSEL-CRS                                            
071100     END-EXEC                                                             
071200                                                                          
071300     MOVE 000            TO GOOD-SQLCODEKODER                             
071400     MOVE SQLCODE        TO SQLCODE-WS                                    
071500     PERFORM DB2-STATUS-CHECK                                             
071600     .                                                                    
071700     EJECT                                                                
071800                                                                          
071900 DB2-FETCH-T01LSEL-CRS SECTION.                                           
072000     EXEC SQL FETCH T01LSEL-CRS INTO                                      
072100            :LSEL-IDLEGSEL,                                               
072200            :LSEL-FLSLUT                                                  
072300     END-EXEC                                                             
072400                                                                          
072500     MOVE 000100         TO GOOD-SQLCODEKODER                             
072600     MOVE SQLCODE        TO SQLCODE-WS                                    
072700                            T01LSEL-WS                                    
072800     PERFORM DB2-STATUS-CHECK                                             
072900     .                                                                    
073000     EJECT                                                                
073100                                                                          
073200 DB2-CLOSE-T01LSEL-CRS SECTION.                                           
073300     EXEC SQL CLOSE T01LSEL-CRS                                           
073400     END-EXEC                                                             
073500     .                                                                    
073600     EJECT                                                                
073700                                                                          
073800 DB2-OPEN-T01ALIN-PER-CRS  SECTION.                                       
073900     EXEC SQL                                                             
074000         DECLARE T01ALIN-PER-CRS CURSOR FOR                               
074100           SELECT  T01ALIN.IDLEGSEL                                       
074200                  ,T01ALIN.IDBUNDLE                                       
074300                  ,T01ALIN.DAREGDAT                                       
074400                  ,T01ALIN.TIREGTID                                       
074500                  ,T01ALIN.IDREF                                          
074600                  ,T01ALIN.DAREFDAT                                       
074700                  ,T01ALIN.IDREFRAD                                       
074800                  ,T01ALIN.BEVOLREF                                       
074900                  ,T01ALIN.IDLANDX3_SEND                                  
075000                  ,T01ALIN.IDLANDX3_REC                                   
075100                  ,T01ALIN.IDLEVNR                                        
075200                  ,T01ALIN.IDPARTNR                                       
075300                  ,T01ALIN.IDEXCUST_1                                     
075400                  ,T01ALIN.IDEXCUST_2                                     
075500                  ,T01ALIN.IDEXCUST_3                                     
075600                  ,T01ALIN.IDOPTION_1                                     
075700                  ,T01ALIN.IDOPTION_2                                     
075800                  ,T01ALIN.IDOPTION_3                                     
075900                  ,T01ALIN.IDOPTION_4                                     
076000                  ,T01ALIN.IDOPTION_5                                     
076100                  ,T01ALIN.IDAPPEND                                       
076200                  ,T01ALIN.IDARTNR_FINANCE                                
076300                  ,T01ALIN.IDSTATNR                                       
076400                  ,T01ALIN.VKORDBTO_KOLLI                                 
076500                  ,T01ALIN.VKARTNTO                                       
076600                  ,T01ALIN.PRARTBTO                                       
076700                  ,T01ALIN.PRARTNTO                                       
076800                  ,T01ALIN.REARTRAB                                       
076900                  ,T01ALIN.KVBEART                                        
077000                  ,T01ALIN.KVLEVART                                       
077100                  ,T01ALIN.BEART                                          
077200                  ,T01ALIN.FLSOFT                                         
077300                  ,T01ALIN.FLSPECPR                                       
077400                  ,T01ALIN.FLFREE                                         
077500                  ,T01ALIN.FLPRIV                                         
077600                  ,T01ALIN.KDVAT                                          
077700                  ,T01ALIN.KDVALISO                                       
077800                  ,T01ALIN.KDINVFRQ                                       
077900                  ,T01ALIN.KDFINDOC                                       
078000                  ,T01ALIN.IDBREAK_1                                      
078100                  ,T01ALIN.IDBREAK_2                                      
078200                  ,T01ALIN.IDSEQ_1                                        
078300                  ,T01ALIN.IDSEQ_2                                        
078400                  ,T01ALIN.IDSEQ_3                                        
078500                  ,T01ALIN.KDARTURS                                       
078600                  ,T01ALIN.KDANMORS                                       
078700                  ,T01ALIN.IDFAKREF                                       
078800                  ,T01ALIN.DAFAKREF                                       
078900                  ,T01ALIN.IDDC                                           
079000                  ,T01ALIN.KDFRAKT                                        
079100                  ,T01ALIN.BELEVVIL                                       
079200                  ,T01ALIN.IDACCNT_1                                      
079300                  ,T01ALIN.IDACCNT_2                                      
079400                  ,T01ALIN.IDACCNT_3                                      
079500                  ,T01ALIN.IDACCNT_4                                      
079600                  ,T01ALIN.IDSYSTEM_SEND                                  
079700                  ,T01ALIN.IDSYSTEM_REC                                   
079800                  ,T01ALIN.DASTADAT                                       
079900                  ,T01ALIN.BEANST                                         
080000                  ,T01ALIN.IDUSER                                         
080100                  ,T01ALIN.BETEXT                                         
080200                  ,T01ALIN.BETEXT_CRE                                     
080300                  ,T01ALIN.IDARTNR_CNTRL                                  
080400                  ,T01ALIN.FLPCOO                                         
080410                  ,T01ALIN.IDLEVNR_ART                                    
080420                  ,T01ALIN.IDTRACK_1                                      
080430                  ,T01ALIN.KVANT_TRACK_1                                  
080440                  ,T01ALIN.IDTRACK_2                                      
080450                  ,T01ALIN.KVANT_TRACK_2                                  
080460                  ,T01ALIN.IDTRACK_3                                      
080470                  ,T01ALIN.KVANT_TRACK_3                                  
080480                  ,T01ALIN.IDTRACK_4                                      
080490                  ,T01ALIN.KVANT_TRACK_4                                  
080491                  ,T01ALIN.IDTRACK_5                                      
080492                  ,T01ALIN.KVANT_TRACK_5                                  
080493                  ,T01ALIN.KDPRMOD                                        
080500                                                                          
080600           FROM    T01ABUN                                                
080700                  ,T01ALIN                                                
080800                                                                          
080900           WHERE  T01ABUN.IDLEGSEL = :LSEL-IDLEGSEL                       
081000           AND    T01ABUN.DAREGDAT < :PECA-DASTADAT                       
081100           AND    T01ABUN.IDLEGSEL = T01ALIN.IDLEGSEL                     
081200           AND    T01ABUN.IDBUNDLE = T01ALIN.IDBUNDLE                     
081300           AND    T01ABUN.DAREGDAT = T01ALIN.DAREGDAT                     
081400           AND    T01ABUN.TIREGTID = T01ALIN.TIREGTID                     
081500           AND    T01ALIN.DASTADAT = '00000000'                           
081600                                                                          
081700           ORDER BY                                                       
081800                  T01ALIN.IDLEGSEL                                        
081900                 ,T01ALIN.KDVALISO                                        
082000                 ,T01ALIN.IDLANDX3_SEND                                   
082100                 ,T01ALIN.IDLEVNR                                         
082200                 ,T01ALIN.IDPARTNR                                        
082300                 ,T01ALIN.KDFINDOC                                        
082400                 ,T01ALIN.FLSOFT                                          
082500                 ,T01ALIN.FLFREE                                          
082600                 ,T01ALIN.FLPRIV                                          
082700                 ,T01ALIN.IDBREAK_1                                       
082800                 ,T01ALIN.IDBREAK_2                                       
082900                 ,T01ALIN.IDSEQ_1                                         
083000                 ,T01ALIN.IDSEQ_2                                         
083100                 ,T01ALIN.IDSEQ_3                                         
083200                 ,T01ALIN.IDARTNR_FINANCE                                 
083300     END-EXEC                                                             
083400                                                                          
083500     MOVE 000100  TO GOOD-SQLCODEKODER                                    
083600     EXEC SQL OPEN T01ALIN-PER-CRS END-EXEC                               
083700     .                                                                    
083800     EJECT                                                                
083900                                                                          
084000 DB2-FETCH-T01ALIN-PER-CRS  SECTION.                                      
084100     EXEC SQL                                                             
084200         FETCH T01ALIN-PER-CRS INTO                                       
084300             :ALIN-IDLEGSEL                                               
084400            ,:ALIN-IDBUNDLE                                               
084500            ,:ALIN-DAREGDAT                                               
084600            ,:ALIN-TIREGTID                                               
084700            ,:ALIN-IDREF                                                  
084800            ,:ALIN-DAREFDAT                                               
084900            ,:WS-IDREFRAD                                                 
085000            ,:ALIN-BEVOLREF                                               
085100            ,:ALIN-IDLANDX3-SEND                                          
085200            ,:ALIN-IDLANDX3-REC                                           
085300            ,:ALIN-IDLEVNR                                                
085400            ,:ALIN-IDPARTNR                                               
085500            ,:ALIN-IDEXCUST-1                                             
085600            ,:ALIN-IDEXCUST-2                                             
085700            ,:ALIN-IDEXCUST-3                                             
085800            ,:ALIN-IDOPTION-1                                             
085900            ,:ALIN-IDOPTION-2                                             
086000            ,:ALIN-IDOPTION-3                                             
086100            ,:ALIN-IDOPTION-4                                             
086200            ,:ALIN-IDOPTION-5                                             
086300            ,:ALIN-IDAPPEND                                               
086400            ,:ALIN-IDARTNR-FINANCE                                        
086500            ,:ALIN-IDSTATNR                                               
086600            ,:ALIN-VKORDBTO-KOLLI                                         
086700            ,:ALIN-VKARTNTO                                               
086800            ,:ALIN-PRARTBTO                                               
086900            ,:ALIN-PRARTNTO                                               
087000            ,:ALIN-REARTRAB                                               
087100            ,:ALIN-KVBEART                                                
087200            ,:ALIN-KVLEVART                                               
087300            ,:ALIN-BEART                                                  
087400            ,:ALIN-FLSOFT                                                 
087500            ,:ALIN-FLSPECPR                                               
087600            ,:ALIN-FLFREE                                                 
087700            ,:ALIN-FLPRIV                                                 
087800            ,:ALIN-KDVAT                                                  
087900            ,:ALIN-KDVALISO                                               
088000            ,:ALIN-KDINVFRQ                                               
088100            ,:ALIN-KDFINDOC                                               
088200            ,:ALIN-IDBREAK-1                                              
088300            ,:ALIN-IDBREAK-2                                              
088400            ,:ALIN-IDSEQ-1                                                
088500            ,:ALIN-IDSEQ-2                                                
088600            ,:ALIN-IDSEQ-3                                                
088700            ,:ALIN-KDARTURS                                               
088800            ,:ALIN-KDANMORS                                               
088900            ,:ALIN-IDFAKREF                                               
089000            ,:ALIN-DAFAKREF                                               
089100            ,:ALIN-IDDC                                                   
089200            ,:ALIN-KDFRAKT                                                
089300            ,:ALIN-BELEVVIL                                               
089400            ,:ALIN-IDACCNT-1                                              
089500            ,:ALIN-IDACCNT-2                                              
089600            ,:ALIN-IDACCNT-3                                              
089700            ,:ALIN-IDACCNT-4                                              
089800            ,:ALIN-IDSYSTEM-SEND                                          
089900            ,:ALIN-IDSYSTEM-REC                                           
090000            ,:ALIN-DASTADAT                                               
090100            ,:ALIN-BEANST                                                 
090200            ,:ALIN-IDUSER                                                 
090300            ,:ALIN-BETEXT                                                 
090400            ,:ALIN-BETEXT-CRE                                             
090500            ,:ALIN-IDARTNR-CNTRL                                          
090600            ,:ALIN-FLPCOO                                                 
090601            ,:ALIN-IDLEVNR-ART                                            
090610            ,:ALIN-IDTRACK-1                                              
090620            ,:ALIN-KVANT-TRACK-1                                          
090630            ,:ALIN-IDTRACK-2                                              
090640            ,:ALIN-KVANT-TRACK-2                                          
090650            ,:ALIN-IDTRACK-3                                              
090660            ,:ALIN-KVANT-TRACK-3                                          
090670            ,:ALIN-IDTRACK-4                                              
090680            ,:ALIN-KVANT-TRACK-4                                          
090690            ,:ALIN-IDTRACK-5                                              
090691            ,:ALIN-KVANT-TRACK-5                                          
090692            ,:ALIN-KDPRMOD                                                
090700     END-EXEC                                                             
090800                                                                          
090900     MOVE 000100  TO GOOD-SQLCODEKODER                                    
091000     MOVE SQLCODE TO SQLCODE-WS                                           
091100                     T01ALIN-WS                                           
091200     PERFORM DB2-STATUS-CHECK                                             
091300     .                                                                    
091400     EJECT                                                                
091500                                                                          
091600 DB2-CLOSE-T01ALIN-PER-CRS  SECTION.                                      
091700     EXEC SQL CLOSE T01ALIN-PER-CRS END-EXEC                              
091800     .                                                                    
091900     EJECT                                                                
092000                                                                          
092100 DB2-OPEN-T01ALIN-WDN-CRS  SECTION.                                       
092200     EXEC SQL                                                             
092300         DECLARE T01ALIN-WDN-CRS CURSOR FOR                               
092400           SELECT  T01ALIN.IDLEGSEL                                       
092500                  ,T01ALIN.IDBUNDLE                                       
092600                  ,T01ALIN.DAREGDAT                                       
092700                  ,T01ALIN.TIREGTID                                       
092800                  ,T01ALIN.IDREF                                          
092900                  ,T01ALIN.DAREFDAT                                       
093000                  ,T01ALIN.IDREFRAD                                       
093100                  ,T01ALIN.BEVOLREF                                       
093200                  ,T01ALIN.IDLANDX3_SEND                                  
093300                  ,T01ALIN.IDLANDX3_REC                                   
093400                  ,T01ALIN.IDLEVNR                                        
093500                  ,T01ALIN.IDPARTNR                                       
093600                  ,T01ALIN.IDEXCUST_1                                     
093700                  ,T01ALIN.IDEXCUST_2                                     
093800                  ,T01ALIN.IDEXCUST_3                                     
093900                  ,T01ALIN.IDOPTION_1                                     
094000                  ,T01ALIN.IDOPTION_2                                     
094100                  ,T01ALIN.IDOPTION_3                                     
094200                  ,T01ALIN.IDOPTION_4                                     
094300                  ,T01ALIN.IDOPTION_5                                     
094400                  ,T01ALIN.IDAPPEND                                       
094500                  ,T01ALIN.IDARTNR_FINANCE                                
094600                  ,T01ALIN.IDSTATNR                                       
094700                  ,T01ALIN.VKORDBTO_KOLLI                                 
094800                  ,T01ALIN.VKARTNTO                                       
094900                  ,T01ALIN.PRARTBTO                                       
095000                  ,T01ALIN.PRARTNTO                                       
095100                  ,T01ALIN.REARTRAB                                       
095200                  ,T01ALIN.KVBEART                                        
095300                  ,T01ALIN.KVLEVART                                       
095400                  ,T01ALIN.BEART                                          
095500                  ,T01ALIN.FLSOFT                                         
095600                  ,T01ALIN.FLSPECPR                                       
095700                  ,T01ALIN.FLFREE                                         
095800                  ,T01ALIN.FLPRIV                                         
095900                  ,T01ALIN.KDVAT                                          
096000                  ,T01ALIN.KDVALISO                                       
096100                  ,T01ALIN.KDINVFRQ                                       
096200                  ,T01ALIN.KDFINDOC                                       
096300                  ,T01ALIN.IDBREAK_1                                      
096400                  ,T01ALIN.IDBREAK_2                                      
096500                  ,T01ALIN.IDSEQ_1                                        
096600                  ,T01ALIN.IDSEQ_2                                        
096700                  ,T01ALIN.IDSEQ_3                                        
096800                  ,T01ALIN.KDARTURS                                       
096900                  ,T01ALIN.KDANMORS                                       
097000                  ,T01ALIN.IDFAKREF                                       
097100                  ,T01ALIN.DAFAKREF                                       
097200                  ,T01ALIN.IDDC                                           
097300                  ,T01ALIN.KDFRAKT                                        
097400                  ,T01ALIN.BELEVVIL                                       
097500                  ,T01ALIN.IDACCNT_1                                      
097600                  ,T01ALIN.IDACCNT_2                                      
097700                  ,T01ALIN.IDACCNT_3                                      
097800                  ,T01ALIN.IDACCNT_4                                      
097900                  ,T01ALIN.IDSYSTEM_SEND                                  
098000                  ,T01ALIN.IDSYSTEM_REC                                   
098100                  ,T01ALIN.DASTADAT                                       
098200                  ,T01ALIN.BEANST                                         
098300                  ,T01ALIN.IDUSER                                         
098400                  ,T01ALIN.BETEXT                                         
098500                  ,T01ALIN.BETEXT_CRE                                     
098600                  ,T01ALIN.IDARTNR_CNTRL                                  
098700                  ,T01ALIN.FLPCOO                                         
098710                  ,T01ALIN.IDLEVNR_ART                                    
098720                  ,T01ALIN.IDTRACK_1                                      
098730                  ,T01ALIN.KVANT_TRACK_1                                  
098740                  ,T01ALIN.IDTRACK_2                                      
098750                  ,T01ALIN.KVANT_TRACK_2                                  
098760                  ,T01ALIN.IDTRACK_3                                      
098770                  ,T01ALIN.KVANT_TRACK_3                                  
098780                  ,T01ALIN.IDTRACK_4                                      
098790                  ,T01ALIN.KVANT_TRACK_4                                  
098791                  ,T01ALIN.IDTRACK_5                                      
098792                  ,T01ALIN.KVANT_TRACK_5                                  
098793                  ,T01ALIN.KDPRMOD                                        
098800                                                                          
098900           FROM    T01ABUN                                                
099000                  ,T01ALIN                                                
099100                                                                          
099200           WHERE  T01ABUN.IDLEGSEL = :LSEL-IDLEGSEL                       
099300           AND    T01ABUN.IDLEGSEL = T01ALIN.IDLEGSEL                     
099400           AND    T01ABUN.IDBUNDLE = T01ALIN.IDBUNDLE                     
099500           AND    T01ABUN.DAREGDAT = T01ALIN.DAREGDAT                     
099600           AND    T01ABUN.TIREGTID = T01ALIN.TIREGTID                     
099700           AND    T01ALIN.KDINVFRQ = :WS-KDINVFRQ                         
099800           AND    T01ALIN.DASTADAT = '00000000'                           
099900                                                                          
100000           ORDER BY                                                       
100100                  T01ALIN.IDLEGSEL                                        
100200                 ,T01ALIN.KDVALISO                                        
100300                 ,T01ALIN.IDLANDX3_SEND                                   
100400                 ,T01ALIN.IDLEVNR                                         
100500                 ,T01ALIN.IDPARTNR                                        
100600                 ,T01ALIN.KDFINDOC                                        
100700                 ,T01ALIN.FLSOFT                                          
100800                 ,T01ALIN.FLFREE                                          
100900                 ,T01ALIN.FLPRIV                                          
101000                 ,T01ALIN.IDBREAK_1                                       
101100                 ,T01ALIN.IDBREAK_2                                       
101200                 ,T01ALIN.IDSEQ_1                                         
101300                 ,T01ALIN.IDSEQ_2                                         
101400                 ,T01ALIN.IDSEQ_3                                         
101500                 ,T01ALIN.IDARTNR_FINANCE                                 
101600     END-EXEC                                                             
101700                                                                          
101800     MOVE 000100  TO GOOD-SQLCODEKODER                                    
101900     EXEC SQL OPEN T01ALIN-WDN-CRS END-EXEC                               
102000     .                                                                    
102100     EJECT                                                                
102200                                                                          
102300 DB2-FETCH-T01ALIN-WDN-CRS  SECTION.                                      
102400     EXEC SQL                                                             
102500         FETCH T01ALIN-WDN-CRS INTO                                       
102600             :ALIN-IDLEGSEL                                               
102700            ,:ALIN-IDBUNDLE                                               
102800            ,:ALIN-DAREGDAT                                               
102900            ,:ALIN-TIREGTID                                               
103000            ,:ALIN-IDREF                                                  
103100            ,:ALIN-DAREFDAT                                               
103200            ,:WS-IDREFRAD                                                 
103300            ,:ALIN-BEVOLREF                                               
103400            ,:ALIN-IDLANDX3-SEND                                          
103500            ,:ALIN-IDLANDX3-REC                                           
103600            ,:ALIN-IDLEVNR                                                
103700            ,:ALIN-IDPARTNR                                               
103800            ,:ALIN-IDEXCUST-1                                             
103900            ,:ALIN-IDEXCUST-2                                             
104000            ,:ALIN-IDEXCUST-3                                             
104100            ,:ALIN-IDOPTION-1                                             
104200            ,:ALIN-IDOPTION-2                                             
104300            ,:ALIN-IDOPTION-3                                             
104400            ,:ALIN-IDOPTION-4                                             
104500            ,:ALIN-IDOPTION-5                                             
104600            ,:ALIN-IDAPPEND                                               
104700            ,:ALIN-IDARTNR-FINANCE                                        
104800            ,:ALIN-IDSTATNR                                               
104900            ,:ALIN-VKORDBTO-KOLLI                                         
105000            ,:ALIN-VKARTNTO                                               
105100            ,:ALIN-PRARTBTO                                               
105200            ,:ALIN-PRARTNTO                                               
105300            ,:ALIN-REARTRAB                                               
105400            ,:ALIN-KVBEART                                                
105500            ,:ALIN-KVLEVART                                               
105600            ,:ALIN-BEART                                                  
105700            ,:ALIN-FLSOFT                                                 
105800            ,:ALIN-FLSPECPR                                               
105900            ,:ALIN-FLFREE                                                 
106000            ,:ALIN-FLPRIV                                                 
106100            ,:ALIN-KDVAT                                                  
106200            ,:ALIN-KDVALISO                                               
106300            ,:ALIN-KDINVFRQ                                               
106400            ,:ALIN-KDFINDOC                                               
106500            ,:ALIN-IDBREAK-1                                              
106600            ,:ALIN-IDBREAK-2                                              
106700            ,:ALIN-IDSEQ-1                                                
106800            ,:ALIN-IDSEQ-2                                                
106900            ,:ALIN-IDSEQ-3                                                
107000            ,:ALIN-KDARTURS                                               
107100            ,:ALIN-KDANMORS                                               
107200            ,:ALIN-IDFAKREF                                               
107300            ,:ALIN-DAFAKREF                                               
107400            ,:ALIN-IDDC                                                   
107500            ,:ALIN-KDFRAKT                                                
107600            ,:ALIN-BELEVVIL                                               
107700            ,:ALIN-IDACCNT-1                                              
107800            ,:ALIN-IDACCNT-2                                              
107900            ,:ALIN-IDACCNT-3                                              
108000            ,:ALIN-IDACCNT-4                                              
108100            ,:ALIN-IDSYSTEM-SEND                                          
108200            ,:ALIN-IDSYSTEM-REC                                           
108300            ,:ALIN-DASTADAT                                               
108400            ,:ALIN-BEANST                                                 
108500            ,:ALIN-IDUSER                                                 
108600            ,:ALIN-BETEXT                                                 
108700            ,:ALIN-BETEXT-CRE                                             
108800            ,:ALIN-IDARTNR-CNTRL                                          
108900            ,:ALIN-FLPCOO                                                 
108910            ,:ALIN-IDLEVNR-ART                                            
108920            ,:ALIN-IDTRACK-1                                              
108930            ,:ALIN-KVANT-TRACK-1                                          
108940            ,:ALIN-IDTRACK-2                                              
108950            ,:ALIN-KVANT-TRACK-2                                          
108960            ,:ALIN-IDTRACK-3                                              
108970            ,:ALIN-KVANT-TRACK-3                                          
108980            ,:ALIN-IDTRACK-4                                              
108990            ,:ALIN-KVANT-TRACK-4                                          
108991            ,:ALIN-IDTRACK-5                                              
108992            ,:ALIN-KVANT-TRACK-5                                          
108993            ,:ALIN-KDPRMOD                                                
109000     END-EXEC                                                             
109100                                                                          
109200     MOVE 000100  TO GOOD-SQLCODEKODER                                    
109300     MOVE SQLCODE TO SQLCODE-WS                                           
109400                     T01ALIN-WS                                           
109500     PERFORM DB2-STATUS-CHECK                                             
109600     .                                                                    
109700     EJECT                                                                
109800                                                                          
109900 DB2-CLOSE-T01ALIN-WDN-CRS  SECTION.                                      
110000     EXEC SQL CLOSE T01ALIN-WDN-CRS END-EXEC                              
110100     .                                                                    
110200     EJECT                                                                
110300                                                                          
110400 DB2-SELECT-T01PECA-MAX SECTION.                                          
110500     EXEC SQL                                                             
110600         SELECT   MAX(DASTADAT)                                           
110700                 ,MAX(DAFINDOC)                                           
110800                 ,MAX(FLPERIOD)                                           
110900                                                                          
111000         INTO    :PECA-DASTADAT                                           
111100                ,:PECA-DAFINDOC                                           
111200                ,:PECA-FLPERIOD                                           
111300                                                                          
111400         FROM     T01PECA                                                 
111500                                                                          
111600         WHERE    IDLEGSEL = :LSEL-IDLEGSEL                               
111700         AND     (DASTADAT = :WS-DATUM                                    
111800         OR       DASTADAT < :WS-DATUM)                                   
111900     END-EXEC                                                             
112000                                                                          
112100     MOVE 000100305  TO GOOD-SQLCODEKODER                                 
112200     MOVE SQLCODE    TO SQLCODE-WS                                        
112300     PERFORM DB2-STATUS-CHECK                                             
112400     .                                                                    
112500     EJECT                                                                
112600                                                                          
112700 DB2-SELECT-T01ALIN-FINNS  SECTION.                                       
112800     EXEC SQL                                                             
112900         SELECT   MAX(IDBUNDLE)                                           
113000                                                                          
113100         INTO    :ALIN-IDBUNDLE                                           
113200                                                                          
113300         FROM     T01ALIN                                                 
113400                                                                          
113600         WHERE    DASTADAT = '00000000'                                   
113700         AND      KDINVFRQ = 'NOW'                                        
113800         AND     (IDSYSTEM_SEND = 'W418'                                  
113900         OR       IDSYSTEM_SEND = 'W41X')                                 
114500     END-EXEC                                                             
114600                                                                          
114700     MOVE 000100305  TO GOOD-SQLCODEKODER                                 
114800     MOVE SQLCODE    TO SQLCODE-WS                                        
114900     PERFORM DB2-STATUS-CHECK                                             
115000     .                                                                    
115100     EJECT                                                                
115200                                                                          
115300 DB2-UPDATE-T01ALIN SECTION.                                              
115400     EXEC SQL                                                             
115500         UPDATE T01ALIN                                                   
115600         SET DASTADAT = :WS-DATUM                                         
115700         WHERE  IDLEGSEL = :ALIN-IDLEGSEL                                 
115800         AND    IDBUNDLE = :ALIN-IDBUNDLE                                 
115900         AND    DAREGDAT = :ALIN-DAREGDAT                                 
116000         AND    TIREGTID = :ALIN-TIREGTID                                 
116100         AND    IDREF    = :ALIN-IDREF                                    
116200         AND    DAREFDAT = :ALIN-DAREFDAT                                 
116300         AND    IDREFRAD = :WS-IDREFRAD                                   
116400     END-EXEC                                                             
116500                                                                          
116600     ADD 1        TO WS-IX2                                               
116700     MOVE 000     TO GOOD-SQLCODEKODER                                    
116800     MOVE SQLCODE TO SQLCODE-WS                                           
116900     PERFORM DB2-STATUS-CHECK                                             
117000     .                                                                    
117100     EJECT                                                                
117200                                                                          
117300 DB2-UPDATE-T01PECA SECTION.                                              
117400     EXEC SQL                                                             
117500         UPDATE T01PECA                                                   
117600         SET FLPERIOD = 'J'                                               
117700         WHERE  IDLEGSEL = :LSEL-IDLEGSEL                                 
117800         AND    DASTADAT = :PECA-DASTADAT                                 
117900     END-EXEC                                                             
118000                                                                          
118100     MOVE 000     TO GOOD-SQLCODEKODER                                    
118200     MOVE SQLCODE TO SQLCODE-WS                                           
118300     PERFORM DB2-STATUS-CHECK                                             
118400     .                                                                    
118500     EJECT                                                                
118600                                                                          
118700 DB2-UPDATE-T01PROC SECTION.                                              
118800     EXEC SQL                                                             
118900         UPDATE T01PROC                                                   
119000         SET KDBEH   = ' '                                                
119100         ,   DAEXDAT = '00000000'                                         
119200         ,   TIEXTID = 0                                                  
119300         WHERE CURRENT OF T01PROC-CRS2                                    
119400     END-EXEC                                                             
119500                                                                          
119600     MOVE 000     TO GOOD-SQLCODEKODER                                    
119700     MOVE SQLCODE TO SQLCODE-WS                                           
119800                     T01PROC-WS                                           
119900     PERFORM DB2-STATUS-CHECK                                             
120000     .                                                                    
120100     EJECT                                                                
120200                                                                          
120300 DB2-UPDATE-T01PROC-WDN SECTION.                                          
120400     EXEC SQL                                                             
120500         UPDATE T01PROC                                                   
120600         SET KDBEH   = :WS-KDBEH                                          
120700         ,   DAEXDAT = :WS-DAEXDAT                                        
120800         ,   TIEXTID = :WS-TIEXTID                                        
120900         WHERE   IDSYSTEM = 'WF02'                                        
121000         AND     IDLEGSEL = :LSEL-IDLEGSEL                                
121100     END-EXEC                                                             
121200                                                                          
121300     MOVE 000     TO GOOD-SQLCODEKODER                                    
121400     MOVE SQLCODE TO SQLCODE-WS                                           
121500                     T01PROC-WS                                           
121600     PERFORM DB2-STATUS-CHECK                                             
121700     .                                                                    
121800     EJECT                                                                
121900                                                                          
122000 DB2-UPDATE-T01PROC-PER SECTION.                                          
122100     EXEC SQL                                                             
122200         UPDATE T01PROC                                                   
122300         SET KDBEH   = :WS-KDBEH                                          
122400         ,   DAEXDAT = :PECA-DAFINDOC                                     
122500         ,   TIEXTID = :WS-TIEXTID                                        
122600         WHERE   IDSYSTEM = 'WF02'                                        
122700         AND     IDLEGSEL = :LSEL-IDLEGSEL                                
122800     END-EXEC                                                             
122900                                                                          
123000     MOVE 000     TO GOOD-SQLCODEKODER                                    
123100     MOVE SQLCODE TO SQLCODE-WS                                           
123200                     T01PROC-WS                                           
123300     PERFORM DB2-STATUS-CHECK                                             
123400     .                                                                    
123500     EJECT                                                                
123600                                                                          
123700 DB2-SELECT-T01WEEK SECTION.                                              
123800     EXEC SQL                                                             
123900         SELECT IDLEGSEL                                                  
124000                                                                          
124100         INTO :WEEK-IDLEGSEL                                              
124200                                                                          
124300         FROM    T01WEEK                                                  
124400                                                                          
124500         WHERE   IDLEGSEL = :LSEL-IDLEGSEL                                
124600         AND     TIAAVV   = :WS-VECKA                                     
124700     END-EXEC                                                             
124800                                                                          
124900     MOVE 000100  TO GOOD-SQLCODEKODER                                    
125000     MOVE SQLCODE TO SQLCODE-WS                                           
125100                     T01WEEK-WS                                           
125200     PERFORM DB2-STATUS-CHECK                                             
125300     .                                                                    
125400     EJECT                                                                
125500                                                                          
125600 DB2-INSERT-T01WEEK SECTION.                                              
125700     EXEC SQL                                                             
125800         INSERT INTO T01WEEK                                              
125900            (IDLEGSEL, TIAAVV)                                            
126000           VALUES(:LSEL-IDLEGSEL, :WS-VECKA)                              
126100     END-EXEC                                                             
126200                                                                          
126300     MOVE 000    TO GOOD-SQLCODEKODER                                     
126400     MOVE SQLCODE TO SQLCODE-WS                                           
126500                     T01WEEK-WS                                           
126600     PERFORM DB2-STATUS-CHECK                                             
126700     .                                                                    
126800     EJECT                                                                
126900                                                                          
127000 DB2-SELECT-T01DAY SECTION.                                               
127100     EXEC SQL                                                             
127200         SELECT  IDLEGSEL                                                 
127300                                                                          
127400         INTO :DAY-IDLEGSEL                                               
127500                                                                          
127600         FROM    T01DAY                                                   
127700                                                                          
127800         WHERE   IDLEGSEL = :LSEL-IDLEGSEL                                
127900         AND     DAREGDAT = :WS-DATUM                                     
128000     END-EXEC                                                             
128100                                                                          
128200     MOVE 000100  TO GOOD-SQLCODEKODER                                    
128300     MOVE SQLCODE TO SQLCODE-WS                                           
128400                     T01DAY-WS                                            
128500     PERFORM DB2-STATUS-CHECK                                             
128600     .                                                                    
128700     EJECT                                                                
128800                                                                          
128900 DB2-INSERT-T01DAY SECTION.                                               
129000     EXEC SQL                                                             
129100         INSERT INTO T01DAY                                               
129200            (IDLEGSEL, DAREGDAT)                                          
129300           VALUES(:LSEL-IDLEGSEL, :WS-DATUM)                              
129400     END-EXEC                                                             
129500                                                                          
129600     MOVE 000    TO GOOD-SQLCODEKODER                                     
129700     MOVE SQLCODE TO SQLCODE-WS                                           
129800                     T01DAY-WS                                            
129900     PERFORM DB2-STATUS-CHECK                                             
130000     .                                                                    
130100     EJECT                                                                
130200                                                                          
130300 DB2-INSERT-T01SLIN SECTION.                                              
130400     EXEC SQL                                                             
130500         INSERT INTO T01SLIN                                              
130600                  (IDLEGSEL                                               
130700                  ,DAEXDAT                                                
130800                  ,TIEXTID                                                
130900                  ,KDVALISO                                               
131000                  ,IDLANDX3_SEND                                          
131100                  ,IDLEVNR                                                
131200                  ,IDPARTNR                                               
131300                  ,KDFINDOC                                               
131400                  ,FLSOFT                                                 
131500                  ,FLFREE                                                 
131600                  ,FLPRIV                                                 
131700                  ,IDBREAK_1                                              
131800                  ,IDBREAK_2                                              
131900                  ,IDLOPNR                                                
132000                  ,IDBUNDLE                                               
132100                  ,IDREF                                                  
132200                  ,DAREFDAT                                               
132300                  ,IDREFRAD                                               
132400                  ,BEVOLREF                                               
132500                  ,IDLANDX3_REC                                           
132600                  ,IDEXCUST_1                                             
132700                  ,IDEXCUST_2                                             
132800                  ,IDEXCUST_3                                             
132900                  ,IDOPTION_1                                             
133000                  ,IDOPTION_2                                             
133100                  ,IDOPTION_3                                             
133200                  ,IDOPTION_4                                             
133300                  ,IDOPTION_5                                             
133400                  ,IDAPPEND                                               
133500                  ,IDARTNR_FINANCE                                        
133600                  ,IDSTATNR                                               
133700                  ,VKORDBTO_KOLLI                                         
133800                  ,VKARTNTO                                               
133900                  ,PRARTBTO                                               
134000                  ,PRARTNTO                                               
134100                  ,REARTRAB                                               
134200                  ,KVBEART                                                
134300                  ,KVLEVART                                               
134400                  ,BEART                                                  
134500                  ,FLSPECPR                                               
134600                  ,KDVAT                                                  
134700                  ,IDSEQ_1                                                
134800                  ,IDSEQ_2                                                
134900                  ,IDSEQ_3                                                
135000                  ,KDARTURS                                               
135100                  ,KDANMORS                                               
135200                  ,IDFAKREF                                               
135300                  ,DAFAKREF                                               
135400                  ,IDDC                                                   
135500                  ,KDFRAKT                                                
135600                  ,BELEVVIL                                               
135700                  ,IDACCNT_1                                              
135800                  ,IDACCNT_2                                              
135900                  ,IDACCNT_3                                              
136000                  ,IDACCNT_4                                              
136100                  ,IDSYSTEM_SEND                                          
136200                  ,IDSYSTEM_REC                                           
136300                  ,BEANST                                                 
136400                  ,IDUSER                                                 
136500                  ,BETEXT                                                 
136600                  ,BETEXT_CRE                                             
136700                  ,IDARTNR_CNTRL                                          
136800                  ,FLPCOO                                                 
136810                  ,IDLEVNR_ART                                            
136820                  ,IDTRACK_1                                              
136830                  ,KVANT_TRACK_1                                          
136840                  ,IDTRACK_2                                              
136850                  ,KVANT_TRACK_2                                          
136860                  ,IDTRACK_3                                              
136870                  ,KVANT_TRACK_3                                          
136880                  ,IDTRACK_4                                              
136890                  ,KVANT_TRACK_4                                          
136891                  ,IDTRACK_5                                              
136892                  ,KVANT_TRACK_5                                          
136893                  ,KDPRMOD                                                
136900                   )                                                      
137000         VALUES                                                           
137100            (:ALIN-IDLEGSEL                                               
137200            ,:WS-DAEXDAT                                                  
137300            ,:WS-TIEXTID                                                  
137400            ,:ALIN-KDVALISO                                               
137500            ,:ALIN-IDLANDX3-SEND                                          
137600            ,:ALIN-IDLEVNR                                                
137700            ,:ALIN-IDPARTNR                                               
137800            ,:ALIN-KDFINDOC                                               
137900            ,:ALIN-FLSOFT                                                 
138000            ,:ALIN-FLFREE                                                 
138100            ,:ALIN-FLPRIV                                                 
138200            ,:ALIN-IDBREAK-1                                              
138300            ,:ALIN-IDBREAK-2                                              
138400            ,:WS-IDLOPNR                                                  
138500            ,:ALIN-IDBUNDLE                                               
138600            ,:ALIN-IDREF                                                  
138700            ,:ALIN-DAREFDAT                                               
138800            ,:WS-IDREFRAD                                                 
138900            ,:ALIN-BEVOLREF                                               
139000            ,:ALIN-IDLANDX3-REC                                           
139100            ,:ALIN-IDEXCUST-1                                             
139200            ,:ALIN-IDEXCUST-2                                             
139300            ,:ALIN-IDEXCUST-3                                             
139400            ,:ALIN-IDOPTION-1                                             
139500            ,:ALIN-IDOPTION-2                                             
139600            ,:ALIN-IDOPTION-3                                             
139700            ,:ALIN-IDOPTION-4                                             
139800            ,:ALIN-IDOPTION-5                                             
139900            ,:ALIN-IDAPPEND                                               
140000            ,:ALIN-IDARTNR-FINANCE                                        
140100            ,:ALIN-IDSTATNR                                               
140200            ,:ALIN-VKORDBTO-KOLLI                                         
140300            ,:ALIN-VKARTNTO                                               
140400            ,:ALIN-PRARTBTO                                               
140500            ,:ALIN-PRARTNTO                                               
140600            ,:ALIN-REARTRAB                                               
140700            ,:ALIN-KVBEART                                                
140800            ,:ALIN-KVLEVART                                               
140900            ,:ALIN-BEART                                                  
141000            ,:ALIN-FLSPECPR                                               
141100            ,:ALIN-KDVAT                                                  
141200            ,:ALIN-IDSEQ-1                                                
141300            ,:ALIN-IDSEQ-2                                                
141400            ,:ALIN-IDSEQ-3                                                
141500            ,:ALIN-KDARTURS                                               
141600            ,:ALIN-KDANMORS                                               
141700            ,:ALIN-IDFAKREF                                               
141800            ,:ALIN-DAFAKREF                                               
141900            ,:ALIN-IDDC                                                   
142000            ,:ALIN-KDFRAKT                                                
142100            ,:ALIN-BELEVVIL                                               
142200            ,:ALIN-IDACCNT-1                                              
142300            ,:ALIN-IDACCNT-2                                              
142400            ,:ALIN-IDACCNT-3                                              
142500            ,:ALIN-IDACCNT-4                                              
142600            ,:ALIN-IDSYSTEM-SEND                                          
142700            ,:ALIN-IDSYSTEM-REC                                           
142800            ,:ALIN-BEANST                                                 
142900            ,:ALIN-IDUSER                                                 
143000            ,:ALIN-BETEXT                                                 
143100            ,:ALIN-BETEXT-CRE                                             
143200            ,:ALIN-IDARTNR-CNTRL                                          
143300            ,:ALIN-FLPCOO                                                 
143310            ,:ALIN-IDLEVNR-ART                                            
143320            ,:ALIN-IDTRACK-1                                              
143330            ,:ALIN-KVANT-TRACK-1                                          
143340            ,:ALIN-IDTRACK-2                                              
143350            ,:ALIN-KVANT-TRACK-2                                          
143360            ,:ALIN-IDTRACK-3                                              
143370            ,:ALIN-KVANT-TRACK-3                                          
143380            ,:ALIN-IDTRACK-4                                              
143390            ,:ALIN-KVANT-TRACK-4                                          
143391            ,:ALIN-IDTRACK-5                                              
143392            ,:ALIN-KVANT-TRACK-5                                          
143393            ,:ALIN-KDPRMOD                                                
143400             )                                                            
143500     END-EXEC                                                             
143600                                                                          
143700     MOVE 000     TO GOOD-SQLCODEKODER                                    
143800     MOVE SQLCODE TO SQLCODE-WS                                           
143900                     T01SLIN-WS                                           
144000     PERFORM DB2-STATUS-CHECK                                             
144100     .                                                                    
144200     EJECT                                                                
144300                                                                          
144400 DB2-STATUS-CHECK     SECTION.                                            
144500     SET SQLCODE-IX TO 1                                                  
144600     SEARCH GOOD-SQLCODE                                                  
144700       AT END                                                             
144800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
144900          DELIMITED BY SIZE INTO ERRORTEXT                                
145000          CALL ABEND USING RKOD-ABEND-DB2                                 
145100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
145200     END-SEARCH                                                           
145300     .                                                                    
145400     EJECT                                                                
145500                                                                          
145600 IMS-GET-MSG  SECTION.                                                    
145700     MOVE    '  QC'          TO    GODK-STATUSKODER                       
145800     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
145900     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
146000     PERFORM IMS-STATUSKONTROLL                                           
146100     .                                                                    
146200     EJECT                                                                
146300                                                                          
146400 IMS-PURGE-ALTMSG-0606 SECTION.                                           
146500     MOVE SPACE TO GODK-STATUSKODER                                       
146600     CALL CBLTDLI USING PURG ALT0606-PCB MSGSOP-WMSGSOP                   
146700     MOVE ALT0606-STATUS-CODE TO STATUS-WS                                
146800     PERFORM IMS-STATUSKONTROLL                                           
146900     .                                                                    
147000     EJECT                                                                
147100                                                                          
147200 IMS-STATUSKONTROLL SECTION.                                              
147300     SET STATUS-IX TO 1                                                   
147400     SEARCH GODK-STATUS                                                   
147500       AT END                                                             
147600         MOVE 'FEL STATUSKOD FRÅN IMS ' TO ERRORTEXT                      
147700         CALL ABEND USING RKOD-ABEND-IMS                                  
147800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
147900         CONTINUE                                                         
148000     END-SEARCH                                                           
148100     .                                                                    
