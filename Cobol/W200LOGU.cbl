000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W200LOGU.                                                
000400 AUTHOR.         ARUP DATTA.                                              
000500 DATE-WRITTEN.   JUNE 2023.                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SUBPROGRAM TO UPDATE LOGG INFORMATION                            
001100*                                                                         
001200*        PROGRAM READS     WDP5                                           
001300*        PROGRAM UPDATES   WDP5                                           
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(8)    VALUE 'W200LOGU'.            
003100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003200 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500     EJECT                                                                
003600                                                                          
003700 01  WORKING-FIELDS.                                                      
003800*                                                                         
003900     03 INDX                     PIC 9(3)   VALUE ZERO.                   
004000     03 INDX-L                   PIC 9(3)   VALUE ZERO.                   
004100     03 INDX-MAX                 PIC 9(3)   VALUE   15.                   
004200     03 TEINFO-LENG              PIC S9(4)  COMP VALUE 1200.              
004300     03 FLENGTH                  PIC S9(4)  COMP VALUE ZERO.              
004400     03 WS-LENGTH                PIC S9(4)  COMP VALUE ZERO.              
004500     03 WS-POS-S                 PIC S9(4)  COMP VALUE ZERO.              
004600     03 WS-IDSID                 PIC S9(3)  COMP-3 VALUE ZERO.            
004700                                                                          
004800     03 WS-IDDOK                 PIC X(8)   VALUE SPACES.                 
004900                                                                          
005000     03 WS-TEINFO                PIC X(1200)                              
005100                                            VALUE SPACES.                 
005200     03 FILLER REDEFINES WS-TEINFO.                                       
005300        05 WS-TEINFO-LINE OCCURS 15                                       
005400                                 PIC X(80).                               
005500*SWITCHES                                                                 
005600                                                                          
005700 77  SW-ISRT-TEINFO              PIC X      VALUE 'N'.                    
005800     88 ISRT-TEINFO                         VALUE 'J'.                    
005900                                                                          
006000 77  SW-UPD-TEINFO               PIC X      VALUE 'N'.                    
006100     88 UPD-TEINFO                          VALUE 'J'.                    
006200                                                                          
006300 01  TODAYS-DATE                 PIC 9(8)    VALUE ZERO.                  
006400*                                                                         
006500 01  W-CURRENT-DATE              PIC 9(12).                               
006600 01  FILLER REDEFINES W-CURRENT-DATE.                                     
006700     03  W-DATE                  PIC 9(6).                                
006800     03  W-TIME                  PIC 9(6).                                
006900                                                                          
007000*                                                                         
007100 01  GENERAL-SUBPROGRAMS.                                                 
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
007400     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
007500     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
007600     03  POSTSUM                 PIC X(8)   VALUE 'POSTSUM'.              
007700     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
007800     SKIP2                                                                
007900***************************************************************           
008000*       C O P Y T E X T E R    (DYNAMISKA ANROP)                          
008100***************************************************************           
008200 01  FILLER                      PIC X(16) VALUE 'WDATAREA     '.         
008300*01   -COPY WDATAREA.                                                     
008400                                                                          
008500                                                                          
008600*    --- PARAMETERS TO ABEND                                              
008700                                                                          
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009100*                                                                         
009200*SWITCHES                                                                 
009300                                                                          
009400 77  INPUT-DATA-SW               PIC X(01)   VALUE 'J'.                   
009500     88  INPUT-DATA-OK                       VALUE 'J'.                   
009600     88  INPUT-DATA-FEL                      VALUE 'N'.                   
009700*                                                                         
009800                                                                          
009900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010000*                                                                         
010100 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
010200 01  KEYS-TO-DLI.                                                         
010300     03 W-WDP501KY-X.                                                     
010400        05  W-IDSKYLT            PIC X(3)   VALUE SPACE.                  
010500        05  W-IDDOKTYP           PIC X(8)   VALUE SPACE.                  
010600        05  W-IDDOK              PIC X(8)   VALUE SPACE.                  
010700                                                                          
010800     03 W-IDSID-X.                                                        
010900        05  W-IDSID              PIC S9(3)  COMP-3 VALUE ZERO.            
011000                                                                          
011100*    --- IMS FUNCTION CODES                                               
011200*01  -COPY W0003                                                          
011300                                                                          
011400                                                                          
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600                                                                          
011700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP501'.                      
011800 01  DLI-IO-WDP501.                                                       
011900*    03  -COPY WDP501                                                     
012000     EJECT                                                                
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP512'.                      
012200 01  DLI-IO-WDP512.                                                       
012300*    03  -COPY WDP512                                                     
012400     EJECT                                                                
012500                                                                          
012600*    --- STATUS-CODE FROM IMS                                             
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FOUND                       VALUE '  '.                  
012900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013100     88  END-OF-BASE                         VALUE 'GB'.                  
013200     SKIP2                                                                
013300 01  GOOD-STATUSCODES.                                                    
013400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013500                                                                          
013600 01  ALL-SSA.                                                             
013700     03 SSA1                     PIC X(64).                               
013800     03 SSA2                     PIC X(64).                               
013900                                                                          
014000 LINKAGE SECTION.                                                         
014100*    -COPY W200LOGU                                                       
014200                                                                          
014300     EJECT                                                                
014400*01  -COPY W0008      -PRE WDP5-                                          
014500     05  FILLER                  PIC X.                                   
014600                                                                          
014700                                                                          
014800 PROCEDURE DIVISION  USING LOGU-W200LOGU WDP5-PCB.                        
014900                                                                          
015000     PERFORM A-INIT                                                       
015100     PERFORM B-VALIDATE-INPUT                                             
015200     IF INPUT-DATA-OK                                                     
015300        PERFORM C-LOGG-INFO-UPD-WDP5                                      
015400     END-IF                                                               
015500                                                                          
015600     GOBACK                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000     MOVE 'A-INIT         ' TO CURRENT-SECTION                            
016100                                                                          
016200     MOVE JA                         TO INPUT-DATA-SW                     
016300                                                                          
016400     MOVE SPACES                     TO LOGU-KDSVAR                       
016500                                        LOGU-FEL-TEXT                     
016600                                        LOGU-IDMSG-ERROR                  
016700                                        LOGU-IDELMT-ERROR                 
016800                                        WS-IDDOK                          
016900                                                                          
017000     MOVE FUNCTION CURRENT-DATE (3:12)                                    
017100                                     TO W-CURRENT-DATE                    
017200     .                                                                    
017300     EJECT                                                                
017400                                                                          
017500 B-VALIDATE-INPUT SECTION.                                                
017600     MOVE 'B-VALIDATE-    ' TO CURRENT-SECTION                            
017700                                                                          
017800     IF  LOGU-IDSKYLT       > SPACES                                      
017900         CONTINUE                                                         
018000     ELSE                                                                 
018100         MOVE NEJ                 TO INPUT-DATA-SW                        
018200         SET  LOGU-KDSVAR-FEL     TO TRUE                                 
018300         MOVE '022'               TO LOGU-IDMSG-ERROR                     
018400         MOVE 'IDSKYLT'           TO LOGU-IDELMT-ERROR                    
018500         MOVE 'INVALID NATIONALITY'                                       
018600                                  TO LOGU-FEL-TEXT                        
018700     END-IF                                                               
018800*                                                                         
018900     IF  LOGU-IDDOKTYP      > SPACES                                      
019000         CONTINUE                                                         
019100     ELSE                                                                 
019200         MOVE NEJ                 TO INPUT-DATA-SW                        
019300         SET  LOGU-KDSVAR-FEL     TO TRUE                                 
019400         MOVE '022'               TO LOGU-IDMSG-ERROR                     
019500         MOVE 'IDDOKTYP'          TO LOGU-IDELMT-ERROR                    
019600         MOVE 'INVALID DOC TYPE'  TO LOGU-FEL-TEXT                        
019700     END-IF                                                               
019800*                                                                         
019900     IF  LOGU-IDDOK         > SPACES                                      
020000         MOVE ZERO                TO FLENGTH                              
020100         INSPECT FUNCTION REVERSE (LOGU-IDDOK)                            
020200                              TALLYING FLENGTH FOR LEADING SPACE          
020300         COMPUTE FLENGTH           = FLENGTH + 1                          
020400         MOVE LOGU-IDDOK          TO WS-IDDOK (FLENGTH : )                
020500     ELSE                                                                 
020600         MOVE NEJ                 TO INPUT-DATA-SW                        
020700         SET  LOGU-KDSVAR-FEL     TO TRUE                                 
020800         MOVE '022'               TO LOGU-IDMSG-ERROR                     
020900         MOVE 'IDDOK   '          TO LOGU-IDELMT-ERROR                    
021000         MOVE 'INVALID ID '       TO LOGU-FEL-TEXT                        
021100     END-IF                                                               
021200*                                                                         
021300     IF  LOGU-IDUSER        > SPACES                                      
021400         CONTINUE                                                         
021500     ELSE                                                                 
021600         MOVE NEJ                 TO INPUT-DATA-SW                        
021700         SET  LOGU-KDSVAR-FEL     TO TRUE                                 
021800         MOVE '022'               TO LOGU-IDMSG-ERROR                     
021900         MOVE 'IDUSER  '          TO LOGU-IDELMT-ERROR                    
022000         MOVE 'INVALID USER ID'   TO LOGU-FEL-TEXT                        
022100     END-IF                                                               
022200*                                                                         
022300     IF  LOGU-TEINFO        > SPACES                                      
022400         CONTINUE                                                         
022500     ELSE                                                                 
022600         MOVE NEJ                 TO INPUT-DATA-SW                        
022700         SET  LOGU-KDSVAR-FEL     TO TRUE                                 
022800         MOVE '022'               TO LOGU-IDMSG-ERROR                     
022900         MOVE 'TEINFO  '          TO LOGU-IDELMT-ERROR                    
023000         MOVE 'NO LOG INFO    '   TO LOGU-FEL-TEXT                        
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 C-LOGG-INFO-UPD-WDP5 SECTION.                                            
023600     MOVE 'C-LOGG-INFO-WDP5'     TO CURRENT-SECTION                       
023700                                                                          
023800     MOVE LOGU-IDSKYLT           TO W-IDSKYLT                             
023900     MOVE LOGU-IDDOKTYP          TO W-IDDOKTYP                            
024000     MOVE WS-IDDOK               TO W-IDDOK                               
024100     PERFORM IMS-GU-WDP501                                                
024200     IF SEGMENT-FOUND                                                     
024300        PERFORM CA-UPD-WDP512                                             
024400     ELSE                                                                 
024500        PERFORM CB-INSERT-LOGG                                            
024600     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900                                                                          
025000 CA-UPD-WDP512 SECTION.                                                   
025100     MOVE 'CA-UPD-WDP512   '     TO CURRENT-SECTION                       
025200                                                                          
025300     PERFORM IMS-GHNP-WDP512                                              
025400     IF SEGMENT-FOUND                                                     
025500        MOVE LOGU-IDUSER             TO TEXT-IDUSER                       
025600        MOVE W-DATE                  TO TEXT-TIREGDAT                     
025700        MOVE W-TIME                  TO TEXT-TIREGTID                     
025800        PERFORM CAA-POPULATE-LOGG-TEXT                                    
025900        IF UPD-TEINFO                                                     
026000           PERFORM IMS-REPL-WDP512                                        
026100        ELSE                                                              
026200          IF ISRT-TEINFO                                                  
026300             PERFORM IMS-ISRT-WDP512                                      
026400          END-IF                                                          
026500        END-IF                                                            
026600     END-IF                                                               
026700     .                                                                    
026800     EJECT                                                                
026900                                                                          
027000 CAA-POPULATE-LOGG-TEXT SECTION.                                          
027100     MOVE 'CAA-POPULATE-LOGG-TEXT' TO CURRENT-SECTION                     
027200                                                                          
027300     MOVE NEJ                     TO SW-UPD-TEINFO                        
027400                                     SW-ISRT-TEINFO                       
027500     MOVE TEXT-IDSID              TO WS-IDSID                             
027600     MOVE TEXT-TEINFO             TO WS-TEINFO                            
027700     MOVE 1                       TO INDX                                 
027800     PERFORM UNTIL INDX > INDX-MAX OR UPD-TEINFO                          
027900       IF WS-TEINFO-LINE (INDX) > SPACES                                  
028000          MOVE INDX               TO INDX-L                               
028100          ADD 1                   TO INDX                                 
028200       ELSE                                                               
028300          MOVE JA                 TO SW-UPD-TEINFO                        
028400       END-IF                                                             
028500     END-PERFORM                                                          
028600*                                                                         
028700***  IF ALL 1200 BYTES OF TEINFO IS FULL, INSERT NEW PAGE                 
028800*                                                                         
028900     IF INDX > 15                                                         
029000        PERFORM CAB-PREP-ISRT-WDP512                                      
029100        MOVE LOGU-TEINFO      TO TEXT-TEINFO                              
029200        MOVE WS-IDSID         TO TEXT-IDSID                               
029300        MOVE JA               TO SW-ISRT-TEINFO                           
029400        MOVE NEJ              TO SW-UPD-TEINFO                            
029500     END-IF                                                               
029600*                                                                         
029700***  CALCULATE LENGTH REMAINING AND STARTING POSITION                     
029800*                                                                         
029900     IF UPD-TEINFO                                                        
030000        COMPUTE WS-LENGTH = ((INDX-MAX - INDX-L) * 80)                    
030100        COMPUTE WS-POS-S  = (INDX-L * 80) + 1                             
030200*                                                                         
030300***  CALCULATE LENGTH OF INPUT TEXT TO UPDATE                             
030400*                                                                         
030500        MOVE ZERO TO FLENGTH                                              
030600        INSPECT FUNCTION REVERSE (LOGU-TEINFO)                            
030700                              TALLYING FLENGTH FOR LEADING SPACE          
030800        COMPUTE FLENGTH = TEINFO-LENG - FLENGTH                           
030900*                                                                         
031000***  CALCULATE LENGTH OF INPUT TEXT TO UPDATE                             
031100*                                                                         
031200        IF WS-LENGTH < FLENGTH                                            
031300           PERFORM CAB-PREP-ISRT-WDP512                                   
031400           MOVE LOGU-TEINFO      TO TEXT-TEINFO                           
031500           MOVE WS-IDSID         TO TEXT-IDSID                            
031600           MOVE JA               TO SW-ISRT-TEINFO                        
031700           MOVE NEJ              TO SW-UPD-TEINFO                         
031800        ELSE                                                              
031900           MOVE LOGU-TEINFO      TO TEXT-TEINFO (WS-POS-S:FLENGTH)        
032000           MOVE JA               TO SW-UPD-TEINFO                         
032100           MOVE NEJ              TO SW-ISRT-TEINFO                        
032200        END-IF                                                            
032300     END-IF                                                               
032400     .                                                                    
032500     EJECT                                                                
032600                                                                          
032700 CAB-PREP-ISRT-WDP512 SECTION.                                            
032800     MOVE 'CAB-PREP-ISRT-WDP512' TO CURRENT-SECTION                       
032900                                                                          
033000     MOVE SPACES              TO WS-TEINFO                                
033100     ADD  +1                  TO WS-IDSID                                 
033200     MOVE  1                  TO INDX                                     
033300     MOVE ZERO                TO INDX-L                                   
033400     .                                                                    
033500     EJECT                                                                
033600                                                                          
033700 CB-INSERT-LOGG SECTION.                                                  
033800     MOVE 'CB-INSERT-LOGG  '     TO CURRENT-SECTION                       
033900                                                                          
034000     MOVE LOGU-IDSKYLT               TO INFO-IDSKYLT                      
034100     MOVE LOGU-IDDOKTYP              TO INFO-IDDOKTYP                     
034200     MOVE WS-IDDOK                   TO INFO-IDDOK                        
034300     MOVE W-DATE                     TO INFO-TIREGDAT                     
034400     MOVE W-TIME                     TO INFO-TIREGTID                     
034500     MOVE LOGU-IDUSER                TO INFO-IDUSER                       
034600     MOVE LOGU-TEINFO                TO INFO-BEDOK                        
034700     PERFORM IMS-ISRT-WDP501                                              
034800*                                                                         
034900     MOVE +1                         TO TEXT-IDSID                        
035000     MOVE LOGU-IDUSER                TO TEXT-IDUSER                       
035100     MOVE W-DATE                     TO TEXT-TIREGDAT                     
035200     MOVE W-TIME                     TO TEXT-TIREGTID                     
035300     MOVE LOGU-TEINFO                TO TEXT-TEINFO                       
035400     PERFORM IMS-ISRT-WDP512                                              
035500     .                                                                    
035600     EJECT                                                                
035700                                                                          
035800* ---                                                                     
035900* --- IMS SECTIONS  ---                                                   
036000* ---                                                                     
036100 IMS-GU-WDP501 SECTION.                                                   
036200     MOVE 'IMS-GU-WDP501 '  TO CURR-IMS-SECTION                           
036300                                                                          
036400     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
036500            DELIMITED BY SIZE INTO SSA1                                   
036600     MOVE '  GE'           TO GOOD-STATUSCODES                            
036700     CALL CBLTDLI USING GU   WDP5-PCB DLI-IO-WDP501 SSA1                  
036800     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
036900     PERFORM IMS-STATUSCHECK                                              
037000     .                                                                    
037100     EJECT                                                                
037200                                                                          
037300 IMS-GHNP-WDP512    SECTION.                                              
037400     MOVE 'IMS-GHNP-WDP512' TO CURR-IMS-SECTION                           
037500                                                                          
037600     MOVE 'WDP512  *L'     TO SSA1                                        
037700     MOVE '  GEGB'         TO GOOD-STATUSCODES                            
037800     CALL CBLTDLI USING GHNP WDP5-PCB DLI-IO-WDP512 SSA1                  
037900     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
038000     PERFORM IMS-STATUSCHECK                                              
038100     .                                                                    
038200     EJECT                                                                
038300                                                                          
038400 IMS-ISRT-WDP501 SECTION.                                                 
038500     MOVE 'IMS-ISRT-WDP501' TO CURR-IMS-SECTION                           
038600                                                                          
038700     MOVE 'WDP501   '      TO SSA1                                        
038800     MOVE '  II'           TO GOOD-STATUSCODES                            
038900     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-WDP501 SSA1                  
039000     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
039100     PERFORM IMS-STATUSCHECK                                              
039200     .                                                                    
039300     EJECT                                                                
039400                                                                          
039500 IMS-ISRT-WDP512 SECTION.                                                 
039600     MOVE 'IMS-ISRT-WDP512' TO CURR-IMS-SECTION                           
039700                                                                          
039800     STRING 'WDP501  (WDP501KY =' W-WDP501KY-X ')'                        
039900            DELIMITED BY SIZE INTO SSA1                                   
040000     MOVE 'WDP512   '      TO SSA2                                        
040100     MOVE '  II'           TO GOOD-STATUSCODES                            
040200     CALL CBLTDLI USING ISRT WDP5-PCB DLI-IO-WDP512 SSA1 SSA2             
040300     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
040400     PERFORM IMS-STATUSCHECK                                              
040500     .                                                                    
040600     EJECT                                                                
040700                                                                          
040800 IMS-REPL-WDP512 SECTION.                                                 
040900     MOVE 'IMS-REPL-WDP512' TO CURR-IMS-SECTION                           
041000                                                                          
041100     MOVE '  '             TO GOOD-STATUSCODES                            
041200     CALL CBLTDLI USING REPL WDP5-PCB DLI-IO-WDP512                       
041300     MOVE WDP5-STATUS-CODE TO STATUS-WS                                   
041400     PERFORM IMS-STATUSCHECK                                              
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800 IMS-STATUSCHECK SECTION.                                                 
041900                                                                          
042000     SET STATUS-IX TO 1                                                   
042100     SEARCH GOOD-STATUS                                                   
042200       AT END                                                             
042300         CALL FELLOG                                                      
042400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
042500         CONTINUE                                                         
042600     END-SEARCH                                                           
042700     .                                                                    
