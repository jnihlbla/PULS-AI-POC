000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF102200.                                                
000400 AUTHOR.         BHAT ARCHANA.                                            
000500 DATE-WRITTEN.   19/08/28.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*   FUNCTION:                                                             
001000*       THIS PROGRAM ADDS ADDITIONAL INFORMATION TO THE                   
001100*       CURRENCY INFORMATION FILE                                         
001200*                                                                         
001300*                                                                         
001400*   ABENDCODES:                                                           
001500*       U0016 -  . . . .                                                  
001600*       U1000 -  . . . .                                                  
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*         --- MONTHLY CURRENCY DATA - CHINA                               
002700     SELECT WF1020                     ASSIGN TO WF1022D1.                
002800*         --- MONTHLY CURRENCY DATA - INDIA                               
002900     SELECT WF1024                     ASSIGN TO WF1022D2.                
003000*         --- MONTHLY CURRENCY DATA - US                                  
003100     SELECT WF1028                     ASSIGN TO WF1022D3.                
003200*         --- MONTHLY CURRENCY DATA - NON-VCC                             
003300     SELECT WF1040                     ASSIGN TO WF1022D4.                
003400     SKIP2                                                                
003500*         --- OUTPUT MONTHLY CURRENCY DATA                                
003600     SELECT WF1022                     ASSIGN TO WF1022D5.                
003700     SKIP2                                                                
003800*         --- ERROR FILE                                                  
003900     SELECT WF1022A                    ASSIGN TO WF1022D6.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  WF1020                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01  IN1-RECORD                  PIC X(42).                               
005000     SKIP3                                                                
005100 FD  WF1024                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500 01  IN2-RECORD                  PIC X(42).                               
005600     SKIP3                                                                
005700 FD  WF1028                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100 01  IN3-RECORD                  PIC X(42).                               
006200     SKIP3                                                                
006300 FD  WF1040                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700 01  IN4-RECORD                  PIC X(44).                               
006800     SKIP3                                                                
006900 FD  WF1022                                                               
007000     RECORDING       F                                                    
007100     BLOCK CONTAINS  0.                                                   
007200                                                                          
007300 01  UT-RECORD                   PIC X(46).                               
007400     SKIP3                                                                
007500 FD  WF1022A                                                              
007600     RECORDING  V                                                         
007700     BLOCK CONTAINS 0.                                                    
007800 01  UT-ERR-RECORD               PIC X(42).                               
007900     EJECT                                                                
008000 WORKING-STORAGE SECTION.                                                 
008100                                                                          
008200 77  IDPGM                       PIC X(8)    VALUE 'WF102200'.            
008300 77  YES                         PIC X       VALUE 'J'.                   
008400 77  NOO                         PIC X       VALUE 'N'.                   
008500 77  WS-SAVE-IDLAND              PIC X(2)    VALUE SPACES.                
008600 77  WS-IDLAND                   PIC X(2)    VALUE SPACES.                
008700 77  WS-IDLEGSEL                 PIC X(4)    VALUE SPACES.                
008800 77  WS-KDVALISO                 PIC X(3)    VALUE SPACES.                
008900 77  WS-HDR                      PIC X(1)    VALUE SPACES.                
009000 77  WS-WRITE-CTRY               PIC X(1)    VALUE SPACES.                
009100                                                                          
009200 77  WS-VALID-CTRY               PIC X       VALUE SPACES.                
009300 77  WS-VCIN-CNT                 PIC 9(3)    VALUE 0.                     
009400 77  WS-VCCN-CNT                 PIC 9(3)    VALUE 0.                     
009500 77  WS-VCUS-CNT                 PIC 9(3)    VALUE 0.                     
009600     EJECT                                                                
009700 77  WF1020-EOF-SW               PIC X       VALUE 'N'.                   
009800     88  END-OF-WF1020                       VALUE 'J'.                   
009900 77  WF1024-EOF-SW               PIC X       VALUE 'N'.                   
010000     88  END-OF-WF1024                       VALUE 'J'.                   
010100 77  WF1028-EOF-SW               PIC X       VALUE 'N'.                   
010200     88  END-OF-WF1028                       VALUE 'J'.                   
010300 77  WF1040-EOF-SW               PIC X       VALUE 'N'.                   
010400     88  END-OF-WF1040                       VALUE 'J'.                   
010500     EJECT                                                                
010600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
010700 01  FILLER REDEFINES TODAYS-DATE.                                        
010800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
010900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
011000     03  TODAYS-DATE-DAY         PIC 9(2).                                
011100     EJECT                                                                
011200 01  WS-YYYYMMDD                 PIC 9(8)    VALUE ZERO.                  
011300 01  FILLER REDEFINES WS-YYYYMMDD.                                        
011400     03  WS-CC                   PIC 9(2).                                
011500     03  WS-YYMMDD.                                                       
011600         05  WS-YY               PIC 9(2).                                
011700         05  FILLER              PIC 9(4).                                
011800 01  GENERAL-SUBPROGRAMS.                                                 
011900*                                                                         
012000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012200     SKIP2                                                                
012300*    --- PARAMETERS TO ABEND                                              
012400                                                                          
012500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012800 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
012900     SKIP2                                                                
013000 01  ERROR-TEXT.                                                          
013100     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
013200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
013300     EJECT                                                                
013400*    --- PARAMETRAR TILL POSTSUM                                          
013500*                                                                         
013600*01  -COPY W0005   -PRE  POSTSUM-                                         
013700     EJECT                                                                
013800 01  IN-AREA-START               PIC X(24)   VALUE                        
013900                                 'IN-AREA-START  '.                       
014000     SKIP2                                                                
014100 01  IN-AREA.                                                             
014200     03 IN-IDLAND            PIC X(02).                                   
014300     03 IN-DATUM-FOM         PIC 9(06).                                   
014400     03 IN-KDVALISO          PIC X(03).                                   
014500     03 IN-REVALUTA-FROM     PIC 9(05).                                   
014600     03 IN-REVALUTA-TO       PIC 9(05).                                   
014700     03 FILLER               PIC X(03).                                   
014800     03 IN-PRKURS            PIC 9(04)V9(06).                             
014900     03 FILLER               PIC X(10).                                   
015000     EJECT                                                                
015100 01  IN-AREA1.                                                            
015200     03 IN1-DATUM-FOM        PIC 9(06).                                   
015300     03 IN1-KDVALISO         PIC X(03).                                   
015400     03 IN1-REVALUTA         PIC 9(03).                                   
015500     03 FILLER               PIC X(10).                                   
015600     03 IN1-PRKURS           PIC 9(04)V9(06).                             
015700     03 FILLER               PIC X(10).                                   
015800     EJECT                                                                
015900 01  UT-AREA-START               PIC X(24)   VALUE                        
016000                                 'UT-AREA-START  '.                       
016100     SKIP2                                                                
016200*01  AREA -COPY WF10CURR -PRE UT-                                         
016300      EJECT                                                               
016400 01  WF1022A-HEAD.                                                        
016500     03  FILLER               PIC X(12) VALUE 'COUNTRY CODE'.             
016600     03  FILLER               PIC X(1)  VALUE ';'.                        
016700     03  FILLER               PIC X(4)  VALUE 'DATE'.                     
016800     03  FILLER               PIC X(1)  VALUE ';'.                        
016900                                                                          
017000 01  WF1022A-DATA.                                                        
017100     03  WF1022A-ERR-LINE        PIC X(42) VALUE SPACES.                  
017200     EJECT                                                                
017300 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
017400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
017500                                                                          
017600 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
017700 01  DB2-WS.                                                              
017800     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
017900         88  CURSOR-OK                      VALUE 000.                    
018000         88  LINES-FOUND                    VALUE 000.                    
018100         88  LINES-MISSING                  VALUE 100.                    
018200         88  RESOURCE-WRONG                 VALUE 904.                    
018300     03  GOOD-SQLCODES.                                                   
018400         05  GOOD-SQLCODE OCCURS 5                                        
018500             INDEXED BY SQLCODE-IX PIC 9(3).                              
018600     EJECT                                                                
018700*    --------------- DB2 INPUT-OUTPUT AREA ---------------                
018800                                                                          
018900 01  FILLER                      PIC X(16)  VALUE 'T01LSEL-AREA'.         
019000                                                                          
019100*01  -COPY T01LSEL -PRE LSEL-                                             
019200     EJECT                                                                
019300     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
019400     EJECT                                                                
019500 PROCEDURE DIVISION.                                                      
019600 MAIN SECTION.                                                            
019700     SKIP2                                                                
019800                                                                          
019900     PERFORM A-INIT                                                       
020000     PERFORM B-PROCESS-VCOM-DATA                                          
020100     PERFORM C-PROCESS-MQ-DATA                                            
020200                                                                          
020300     PERFORM Z-FINIT                                                      
020400                                                                          
020500     MOVE ZERO TO RETURN-CODE                                             
020600     GOBACK                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 A-INIT SECTION.                                                          
021000                                                                          
021100     OPEN INPUT  WF1020                                                   
021200                 WF1024                                                   
021300                 WF1028                                                   
021400                 WF1040                                                   
021500                                                                          
021600     OPEN OUTPUT WF1022                                                   
021700                 WF1022A                                                  
021800     SKIP2                                                                
021900     ACCEPT TODAYS-DATE  FROM DATE                                        
022000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022100     .                                                                    
022200     EJECT                                                                
022300 B-PROCESS-VCOM-DATA SECTION.                                             
022400                                                                          
022500     PERFORM S01-READ-WF1020                                              
022600     PERFORM UNTIL END-OF-WF1020                                          
022700       MOVE 'VCCN'             TO UT-IDLEGSEL                             
022800       PERFORM BA-MOVE-DATA                                               
022900       PERFORM S11-WRITE-WF1022                                           
023000       PERFORM S01-READ-WF1020                                            
023100       ADD +1                  TO WS-VCCN-CNT                             
023200     END-PERFORM                                                          
023300     IF WS-VCCN-CNT > 0                                                   
023400       MOVE 'CNY'              TO UT-KDVALISO                             
023500       MOVE 1                  TO UT-REVALUTA-FROM                        
023600       MOVE 1                  TO UT-REVALUTA-TO                          
023700       MOVE 1                  TO UT-PRKURS                               
023800       PERFORM S11-WRITE-WF1022                                           
023900     END-IF                                                               
024000                                                                          
024100     PERFORM S01-READ-WF1024                                              
024200     PERFORM UNTIL END-OF-WF1024                                          
024300       MOVE 'VCIN'             TO UT-IDLEGSEL                             
024400       PERFORM BA-MOVE-DATA                                               
024500       PERFORM S11-WRITE-WF1022                                           
024600       PERFORM S01-READ-WF1024                                            
024700       ADD +1                  TO WS-VCIN-CNT                             
024800     END-PERFORM                                                          
024900     IF WS-VCIN-CNT > 0                                                   
025000       MOVE 'INR'              TO UT-KDVALISO                             
025100       MOVE 1                  TO UT-REVALUTA-FROM                        
025200       MOVE 1                  TO UT-REVALUTA-TO                          
025300       MOVE 1                  TO UT-PRKURS                               
025400       PERFORM S11-WRITE-WF1022                                           
025500     END-IF                                                               
025600                                                                          
025700     PERFORM S01-READ-WF1028                                              
025800     PERFORM UNTIL END-OF-WF1028                                          
025900       MOVE 'VCUS'             TO UT-IDLEGSEL                             
026000       PERFORM BA-MOVE-DATA                                               
026100       PERFORM S11-WRITE-WF1022                                           
026200       PERFORM S01-READ-WF1028                                            
026300       ADD +1                  TO WS-VCUS-CNT                             
026400     END-PERFORM                                                          
026500     IF WS-VCUS-CNT > 0                                                   
026600       MOVE 'USD'              TO UT-KDVALISO                             
026700       MOVE 1                  TO UT-REVALUTA-FROM                        
026800       MOVE 1                  TO UT-REVALUTA-TO                          
026900       MOVE 1                  TO UT-PRKURS                               
027000       PERFORM S11-WRITE-WF1022                                           
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 BA-MOVE-DATA SECTION.                                                    
027500                                                                          
027600     MOVE IN1-KDVALISO         TO UT-KDVALISO                             
027700     MOVE 1                    TO UT-REVALUTA-FROM                        
027800     MOVE 1                    TO UT-REVALUTA-TO                          
027900     MOVE IN1-PRKURS           TO UT-PRKURS                               
028000     MOVE IN1-DATUM-FOM        TO WS-YYMMDD                               
028100     IF WS-YY > 49                                                        
028200       MOVE 19                 TO WS-CC                                   
028300     ELSE                                                                 
028400       MOVE 20                 TO WS-CC                                   
028500     END-IF                                                               
028600     MOVE WS-YYYYMMDD          TO UT-DASTADAT                             
028700     MOVE FUNCTION CURRENT-DATE(1:8)                                      
028800                               TO UT-DAREGDAT                             
028900     .                                                                    
029000     EJECT                                                                
029100 C-PROCESS-MQ-DATA SECTION.                                               
029200                                                                          
029300     PERFORM S01-READ-WF1040                                              
029400                                                                          
029500     MOVE NOO     TO WS-WRITE-CTRY                                        
029600     PERFORM UNTIL END-OF-WF1040                                          
029700       IF IN-IDLAND = WS-SAVE-IDLAND                                      
029800         PERFORM CB-PROCESS-CURRENCY                                      
029900       ELSE                                                               
030000         IF WS-WRITE-CTRY = YES                                           
030100           PERFORM CC-WRITE-CTRY                                          
030200         END-IF                                                           
030300         MOVE IN-IDLAND  TO WS-SAVE-IDLAND                                
030400         PERFORM CA-PROCESS-CTRY                                          
030500         PERFORM CB-PROCESS-CURRENCY                                      
030600       END-IF                                                             
030700      PERFORM S01-READ-WF1040                                             
030800     END-PERFORM                                                          
030900                                                                          
031000     IF WS-VALID-CTRY = YES                                               
031100       PERFORM CC-WRITE-CTRY                                              
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 CA-PROCESS-CTRY SECTION.                                                 
031600                                                                          
031700     MOVE FUNCTION UPPER-CASE(IN-IDLAND) TO WS-IDLAND                     
031800     PERFORM DB2-SEARCH-T01LSEL-TAB                                       
031900     IF LINES-FOUND                                                       
032000       MOVE LSEL-KDVALISO      TO WS-KDVALISO                             
032100       MOVE LSEL-IDLEGSEL      TO WS-IDLEGSEL                             
032200       MOVE YES                TO WS-VALID-CTRY                           
032300                                  WS-WRITE-CTRY                           
032400     ELSE                                                                 
032500       MOVE NOO                TO WS-VALID-CTRY                           
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 CB-PROCESS-CURRENCY SECTION.                                             
033000                                                                          
033100     IF WS-VALID-CTRY = YES                                               
033200       MOVE IN-KDVALISO        TO UT-KDVALISO                             
033300       MOVE IN-REVALUTA-FROM   TO UT-REVALUTA-FROM                        
033400       MOVE IN-REVALUTA-TO     TO UT-REVALUTA-TO                          
033500       MOVE IN-PRKURS          TO UT-PRKURS                               
033600       MOVE WS-IDLEGSEL        TO UT-IDLEGSEL                             
033700       MOVE IN-DATUM-FOM       TO WS-YYMMDD                               
033800       IF WS-YY > 49                                                      
033900         MOVE 19               TO WS-CC                                   
034000       ELSE                                                               
034100         MOVE 20               TO WS-CC                                   
034200       END-IF                                                             
034300       MOVE WS-YYYYMMDD        TO UT-DASTADAT                             
034400       MOVE FUNCTION CURRENT-DATE(1:8)                                    
034500                               TO UT-DAREGDAT                             
034600       PERFORM S11-WRITE-WF1022                                           
034700     ELSE                                                                 
034800       PERFORM S12-CREATE-WF1022A                                         
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 CC-WRITE-CTRY SECTION.                                                   
035300                                                                          
035400*** FOR DUBAI, THE CURRENCY IS USD. WE USE AED HERE TO RECEIVE THE        
035500*** EXCHANGE RATE FROM SAP                                                
035600     IF WS-IDLEGSEL = 'VCAE'                                              
035700       MOVE 'AED'              TO UT-KDVALISO                             
035800     ELSE                                                                 
035900       MOVE WS-KDVALISO        TO UT-KDVALISO                             
036000     END-IF                                                               
036100     MOVE WS-IDLEGSEL          TO UT-IDLEGSEL                             
036200     MOVE IN-REVALUTA-FROM     TO UT-REVALUTA-FROM                        
036300     MOVE IN-REVALUTA-TO       TO UT-REVALUTA-TO                          
036400     MOVE 1                    TO UT-PRKURS                               
036500     IF IN-IDLAND IS NOT = WS-SAVE-IDLAND                                 
036600        MOVE 1                 TO UT-REVALUTA-FROM                        
036700        MOVE 1                 TO UT-REVALUTA-TO                          
036800     END-IF                                                               
036900     PERFORM S11-WRITE-WF1022                                             
037000     .                                                                    
037100     EJECT                                                                
037200                                                                          
037300 Z-FINIT SECTION.                                                         
037400     CLOSE WF1020                                                         
037500           WF1024                                                         
037600           WF1028                                                         
037700           WF1040                                                         
037800           WF1022                                                         
037900           WF1022A                                                        
038000     SKIP2                                                                
038100     MOVE 'S' TO POSTSUM-OPKOD                                            
038200     CALL POSTSUM USING POSTSUM-PARM                                      
038300     .                                                                    
038400     EJECT                                                                
038500 S01-READ-WF1020  SECTION.                                                
038600     READ WF1020               INTO IN-AREA1                              
038700     AT END                                                               
038800        MOVE HIGH-VALUE          TO IN-AREA1                              
038900        SET END-OF-WF1020        TO TRUE                                  
039000                                                                          
039100     NOT AT END                                                           
039200        MOVE 'WF1020'            TO POSTSUM-FDNAMN                        
039300        MOVE 'WF1022D1'          TO POSTSUM-DDNAMN2                       
039400        MOVE SPACE               TO POSTSUM-TRANSTYP                      
039500        CALL POSTSUM          USING POSTSUM-PARM                          
039600     END-READ                                                             
039700     .                                                                    
039800     EJECT                                                                
039900 S01-READ-WF1024  SECTION.                                                
040000     READ WF1024               INTO IN-AREA1                              
040100     AT END                                                               
040200        MOVE HIGH-VALUE          TO IN-AREA1                              
040300        SET END-OF-WF1024        TO TRUE                                  
040400                                                                          
040500     NOT AT END                                                           
040600        MOVE 'WF1024'            TO POSTSUM-FDNAMN                        
040700        MOVE 'WF1022D2'          TO POSTSUM-DDNAMN2                       
040800        MOVE SPACE               TO POSTSUM-TRANSTYP                      
040900        CALL POSTSUM          USING POSTSUM-PARM                          
041000     END-READ                                                             
041100     .                                                                    
041200     EJECT                                                                
041300 S01-READ-WF1028  SECTION.                                                
041400     READ WF1028               INTO IN-AREA1                              
041500     AT END                                                               
041600        MOVE HIGH-VALUE          TO IN-AREA1                              
041700        SET END-OF-WF1028        TO TRUE                                  
041800                                                                          
041900     NOT AT END                                                           
042000        MOVE 'WF1028'            TO POSTSUM-FDNAMN                        
042100        MOVE 'WF1022D3'          TO POSTSUM-DDNAMN2                       
042200        MOVE SPACE               TO POSTSUM-TRANSTYP                      
042300        CALL POSTSUM          USING POSTSUM-PARM                          
042400     END-READ                                                             
042500     .                                                                    
042600     EJECT                                                                
042700 S01-READ-WF1040  SECTION.                                                
042800     READ WF1040 INTO IN-AREA                                             
042900     AT END                                                               
043000        MOVE HIGH-VALUE TO IN-AREA                                        
043100        SET END-OF-WF1040 TO TRUE                                         
043200                                                                          
043300     NOT AT END                                                           
043400        MOVE 'WF1040' TO POSTSUM-FDNAMN                                   
043500        MOVE 'WF1022D4' TO POSTSUM-DDNAMN2                                
043600        MOVE SPACE     TO POSTSUM-TRANSTYP                                
043700        CALL POSTSUM USING POSTSUM-PARM                                   
043800     END-READ                                                             
043900     .                                                                    
044000     EJECT                                                                
044100 S11-WRITE-WF1022 SECTION.                                                
044200                                                                          
044300     WRITE UT-RECORD FROM UT-AREA                                         
044400                                                                          
044500     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
044600     MOVE 'WF1022' TO POSTSUM-FDNAMN                                      
044700     MOVE 'WF1022D5' TO POSTSUM-DDNAMN2                                   
044800     CALL POSTSUM USING POSTSUM-PARM                                      
044900     .                                                                    
045000     EJECT                                                                
045100 S12-CREATE-WF1022A SECTION.                                              
045200                                                                          
045300     MOVE IN-AREA            TO WF1022A-ERR-LINE                          
045400     WRITE UT-ERR-RECORD   FROM WF1022A-ERR-LINE                          
045500     .                                                                    
045600 DB2-SEARCH-T01LSEL-TAB SECTION.                                          
045700                                                                          
045800     MOVE 000100305      TO GOOD-SQLCODES                                 
045900                                                                          
046000     EXEC SQL                                                             
046100      SELECT  KDVALISO,                                                   
046200              IDLEGSEL                                                    
046300                                                                          
046400      INTO    :LSEL-KDVALISO                                              
046500             ,:LSEL-IDLEGSEL                                              
046600                                                                          
046700      FROM    T01LSEL                                                     
046800                                                                          
046900      WHERE   IDLANDX3 = :WS-IDLAND                                       
047000     END-EXEC                                                             
047100                                                                          
047200     MOVE SQLCODE        TO SQLCODE-WS                                    
047300     PERFORM DB2-STATUS-CHECK                                             
047400     .                                                                    
047500 DB2-STATUS-CHECK  SECTION.                                               
047600     SET SQLCODE-IX TO 1                                                  
047700     SEARCH GOOD-SQLCODE                                                  
047800       AT END                                                             
047900          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
048000          DELIMITED BY SIZE INTO ERROR-TEXT                               
048100          CALL ABEND USING RKOD-ABEND-DB2                                 
048200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
048300          CONTINUE                                                        
048400     END-SEARCH                                                           
048500     .                                                                    
