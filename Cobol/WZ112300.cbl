000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ112300.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   21/06/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        PROGRAM FOR SENDING FILE TO MQ                                   
001000*                                                                         
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300 CONFIGURATION SECTION.                                                   
001400 SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
001500                                                                          
001600 DATA DIVISION.                                                           
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001900 77  IDPGM                       PIC X(8)    VALUE 'WZ112300'.            
002000 77  YES                         PIC X       VALUE 'J'.                   
002100 77  NOO                         PIC X       VALUE 'N'.                   
002200 77  WS-REC-COUNT                PIC S9(9)   VALUE ZERO  COMP-3.          
002300 77  WS-RECLEN                   PIC 9(5)    VALUE ZERO.                  
002400 77  IX                          PIC 9       VALUE ZERO.                  
002500 77  SUB-POS                     PIC 99      VALUE ZERO.                  
002600 77  WS-PREV-ID-NUM              PIC 9       VALUE ZERO.                  
002700                                                                          
002800 77  WS-ADDISPABS                PIC X(50).                               
002900                                                                          
003000 77  WS-CURRENT-TIMESTAMP        PIC X(21)   VALUE SPACES.                
003100                                                                          
003200 77  WS-TEXT                     PIC X(60)   VALUE SPACES.                
003300 77  WS-TEMP                     PIC X(60)   VALUE SPACES.                
003400                                                                          
003500 77  SW-END-OF-INDATA            PIC X       VALUE 'N'.                   
003600     88  END-OF-INDATA                       VALUE 'J'.                   
003700     88  NOT-END-OF-INDATA                   VALUE 'N'.                   
003800                                                                          
003900 77  SW-PROP-MODE                PIC X       VALUE 'N'.                   
004000     88  PROP-IN-SYSIN                       VALUE 'S'.                   
004100     88  NO-PROP                             VALUE 'N'.                   
004200     88  PROP-IN-FILE                        VALUE 'F'.                   
004300                                                                          
004400 01  WS-PROP-TABLE.                                                       
004500     03  FILLER OCCURS 5 TIMES.                                           
004600         05  WS-PROP-NAME        PIC X(20)   VALUE SPACES.                
004700         05  WS-PROP-VALUE       PIC X(60)   VALUE SPACES.                
004800                                                                          
004900 01  KDRC-DISPLAY                PIC Z(5).                                
005000                                                                          
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005400     03  WZ11OUTQ                PIC X(8)    VALUE 'WZ11OUTQ'.            
005500     03  WFILREAD                PIC X(8)    VALUE 'WFILREAD'.            
005600     03  WDSINFO                 PIC X(8)    VALUE 'WDSINFO'.             
005700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005800                                                                          
005900*    --- PARAMETERS TO ABEND                                              
006000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006300                                                                          
006400*    --- PARAMETERS TO POSTSUM                                            
006500                                                                          
006600 01  -COPY W0005   -PRE  POSTSUM-                                         
006700                                                                          
006800*    --- PARAMETERS TO WDSINFO                                            
006900                                                                          
007000 01  -COPY WDSAREA -PRE  DSINFO-                                          
007100                                                                          
007200 01  ERROR-TEXT.                                                          
007300     03  FILLER                  PIC X(12)  VALUE 'ERROR-TEXT: '.         
007400     03  ERROR-TEXT-STR          PIC X(80)  VALUE SPACE.                  
007500                                                                          
007600 01  SYSIN-AREA                  PIC X(80)  VALUE SPACES.                 
007700 01  WS-IN-PROP-TO-SET.                                                   
007800     03  IN-PROP-IDENTIFIER      PIC X(09)  VALUE SPACES.                 
007900     03  IN-PROPERTY             PIC X(63)  VALUE SPACES.                 
008000     03  FILLER                  PIC X(08)  VALUE SPACES.                 
008100                                                                          
008200 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
008300 01  IN-RECLEN                   PIC 9(5).                                
008400 01  IN-AREA                     PIC X(10000) VALUE SPACES.               
008500 01  MAX-IN-RECLEN               PIC 9(5)    VALUE 10000.                 
008600                                                                          
008700 01  READ-INFILE                 PIC S9(9)   COMP.                        
008800 01  READ-RECLEN                 PIC S9(4)   COMP SYNC.                   
008900 01  READ-FUNC.                                                           
009000     03  READ-FUNC-INIT          PIC X       VALUE 'I'.                   
009100     03  READ-FUNC-OPEN          PIC X       VALUE 'O'.                   
009200     03  READ-FUNC-READ          PIC X       VALUE ' '.                   
009300     03  READ-FUNC-CLOSE         PIC X       VALUE 'C'.                   
009400                                                                          
009500 01 OUTQ-AREA.                                                            
009600    03  OUTQ-CONTROL-AREA.                                                
009700        05  OUTQ-KDFUNC          PIC X(10).                               
009800        05  OUTQ-KDRC            PIC S9(9) COMP.                          
009900        05  OUTQ-IDCOM           PIC S9(9) COMP.                          
010000    03  OUTQ-OPEN-AREA.                                                   
010100        05  OUTQ-ADDISPABS       PIC X(50).                               
010200    03  OUTQ-PROPERTY-AREA.                                               
010300        05  OUTQ-PROPERTY-NAME   PIC X(100).                              
010400        05  OUTQ-PROPERTY-VALUE  PIC X(100).                              
010500    03  OUTQ-ADDITIONAL-INFO.                                             
010600        05  OUTQ-PHYSICALID      PIC X(100).                              
010700    03  OUTQ-KVDLEN              PIC S9(9) BINARY.                        
010800    03  OUTQ-DATA.                                                        
010900        05 FILLER OCCURS 1 TO 104857600 TIMES                             
011000           DEPENDING ON OUTQ-KVDLEN PIC X.                                
011100                                                                          
011200 LINKAGE SECTION.                                                         
011300                                                                          
011400 PROCEDURE DIVISION.                                                      
011500 MAIN SECTION.                                                            
011600                                                                          
011700     PERFORM A-INIT                                                       
011800                                                                          
011900     PERFORM S01-READ-INDATA                                              
012000     PERFORM S02-GET-DSINFO                                               
012100                                                                          
012200     IF END-OF-INDATA                                                     
012300       DISPLAY 'WZ112300 - EMPTY FILE!'                                   
012400       MOVE 4                      TO RETURN-CODE                         
012500     ELSE                                                                 
012600       IF IN-AREA (1:4) = '¤MQM'                                          
012700         IF PROP-IN-SYSIN                                                 
012800           DISPLAY 'PROP IN BOTH SYSIN AND DATA NOT SUPPORTED'            
012900           CALL ABEND                                                     
013000         ELSE                                                             
013100           SET PROP-IN-FILE      TO TRUE                                  
013200         END-IF                                                           
013300       END-IF                                                             
013400                                                                          
013500       IF NO-PROP OR PROP-IN-SYSIN                                        
013600         PERFORM S04-OUTQ-OPEN                                            
013700                                                                          
013800         IF PROP-IN-SYSIN                                                 
013900           PERFORM S03-SEND-MSGPROP                                       
014000         END-IF                                                           
014100                                                                          
014200         PERFORM                                                          
014300           UNTIL END-OF-INDATA                                            
014400                                                                          
014500           IF IN-AREA (1:4) = '¤MQM'                                      
014600             DISPLAY 'INVALID HEADERS IN THE FILE!'                       
014700             CALL ABEND                                                   
014800           END-IF                                                         
014900                                                                          
015000           PERFORM B-PROCESS-MESSAGE                                      
015100           PERFORM S01-READ-INDATA                                        
015200         END-PERFORM                                                      
015300       END-IF                                                             
015400                                                                          
015500       IF PROP-IN-FILE                                                    
015600         PERFORM                                                          
015700           UNTIL END-OF-INDATA OR                                         
015800                 IN-AREA (1:4) NOT = '¤MQM'                               
015900           PERFORM S04-OUTQ-OPEN                                          
016000                                                                          
016100           PERFORM S03-HANDLE-INPUT-MSGPROP                               
016200           PERFORM S03-SEND-MSGPROP                                       
016300                                                                          
016400           PERFORM                                                        
016500             UNTIL END-OF-INDATA OR                                       
016600                 IN-AREA (1:4) = '¤MQM'                                   
016700             PERFORM B-PROCESS-MESSAGE                                    
016800             PERFORM S01-READ-INDATA                                      
016900           END-PERFORM                                                    
017000                                                                          
017100           PERFORM S04-OUTQ-SEND                                          
017200                                                                          
017300         END-PERFORM                                                      
017400       END-IF                                                             
017500       PERFORM S04-OUTQ-CLOSE                                             
017600       MOVE ZERO                 TO RETURN-CODE                           
017700     END-IF                                                               
017800                                                                          
017900     PERFORM Z-FINIT                                                      
018000                                                                          
018100     GOBACK                                                               
018200     .                                                                    
018300                                                                          
018400 A-INIT SECTION.                                                          
018500                                                                          
018600     MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-TIMESTAMP                  
018700     PERFORM S01-OPEN-FILE                                                
018800                                                                          
018900     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
019000                                                                          
019100     MOVE SPACES                 TO SYSIN-AREA                            
019200     SET NO-PROP                 TO TRUE                                  
019300     MOVE 1                      TO IX                                    
019400                                                                          
019500     ACCEPT SYSIN-AREA         FROM SYSIN                                 
019600     IF SYSIN-AREA (1:11) = '¤ADDISPABS '                                 
019700       MOVE SYSIN-AREA(12:50)    TO WS-ADDISPABS                          
019800       MOVE WS-ADDISPABS         TO OUTQ-ADDISPABS                        
019900     ELSE                                                                 
020000       DISPLAY 'INVALID/MISSING ADDISPABS !'                              
020100       CALL ABEND                                                         
020200     END-IF                                                               
020300     DISPLAY '************ PARM DATA ***************'                     
020400     DISPLAY 'ADDRESS            : ' OUTQ-ADDISPABS                       
020500     DISPLAY '****** END OF PARM DATA **************'                     
020600                                                                          
020700     MOVE SPACES                 TO SYSIN-AREA                            
020800     ACCEPT SYSIN-AREA         FROM SYSIN                                 
020900                                                                          
021000     PERFORM UNTIL SYSIN-AREA = SPACES                                    
021100       MOVE SYSIN-AREA           TO WS-IN-PROP-TO-SET                     
021200       IF IN-PROP-IDENTIFIER = '¤MQMPROP '                                
021300         IF IX > 5                                                        
021400           DISPLAY 'CAN HANDLE ONLY 5 PROPERTIES'                         
021500           CALL ABEND                                                     
021600         ELSE                                                             
021700           MOVE SPACES           TO WS-PROP-NAME (IX)                     
021800                                    WS-TEXT                               
021900           UNSTRING IN-PROPERTY DELIMITED BY '='                          
022000                               INTO WS-PROP-NAME  (IX)                    
022100                                    WS-TEXT                               
022200           PERFORM AA-CHECK-SUBSTITUTE                                    
022300           MOVE WS-TEXT          TO WS-PROP-VALUE (IX)                    
022400           SET PROP-IN-SYSIN     TO TRUE                                  
022500           ADD 1                 TO IX                                    
022600         END-IF                                                           
022700       END-IF                                                             
022800       MOVE SPACES               TO SYSIN-AREA                            
022900       ACCEPT SYSIN-AREA       FROM SYSIN                                 
023000     END-PERFORM                                                          
023100     .                                                                    
023200 AA-CHECK-SUBSTITUTE SECTION.                                             
023300                                                                          
023400     MOVE 1                      TO SUB-POS                               
023500     INSPECT WS-TEXT       TALLYING SUB-POS                               
023600       FOR CHARACTERS BEFORE INITIAL '%'                                  
023700     PERFORM                                                              
023800       UNTIL SUB-POS >= LENGTH OF WS-TEXT                                 
023900                                                                          
024000       MOVE WS-TEXT              TO WS-TEMP                               
024100       MOVE SPACES               TO WS-TEXT                               
024200                                                                          
024300       EVALUATE TRUE                                                      
024400         WHEN WS-TEMP (SUB-POS : 9) = '%YYYYMMDD'                         
024500           STRING WS-TEMP (1 : SUB-POS - 1)   DELIMITED BY SIZE           
024600                  WS-CURRENT-TIMESTAMP (1:8)  DELIMITED BY SIZE           
024700                  WS-TEMP (SUB-POS + 9 : )    DELIMITED BY SPACE          
024800                               INTO WS-TEXT                               
024900         WHEN WS-TEMP (SUB-POS : 7) = '%YYMMDD'                           
025000           STRING WS-TEMP (1 : SUB-POS - 1)   DELIMITED BY SIZE           
025100                  WS-CURRENT-TIMESTAMP (3:6)  DELIMITED BY SIZE           
025200                  WS-TEMP (SUB-POS + 7 : )    DELIMITED BY SPACE          
025300                               INTO WS-TEXT                               
025400         WHEN WS-TEMP (SUB-POS : 7) = '%HHMMSS'                           
025500           STRING WS-TEMP (1 : SUB-POS - 1)   DELIMITED BY SIZE           
025600                  WS-CURRENT-TIMESTAMP (9:6)  DELIMITED BY SIZE           
025700                  WS-TEMP (SUB-POS + 7 : )    DELIMITED BY SPACE          
025800                               INTO WS-TEXT                               
025900         WHEN OTHER                                                       
026000           DISPLAY 'UNSUPPORTED SUBSTITUTION - '                          
026100                     WS-TEMP (SUB-POS :)                                  
026200           CALL ABEND                                                     
026300       END-EVALUATE                                                       
026400       MOVE 1                    TO SUB-POS                               
026500       INSPECT WS-TEXT     TALLYING SUB-POS                               
026600         FOR CHARACTERS BEFORE INITIAL '%'                                
026700                                                                          
026800     END-PERFORM                                                          
026900                                                                          
027000     .                                                                    
027100 S03-SEND-MSGPROP SECTION.                                                
027200                                                                          
027300     PERFORM                                                              
027400     VARYING IX FROM 1 BY 1                                               
027500       UNTIL IX > 5                                                       
027600       IF WS-PROP-NAME  (IX) = SPACES OR                                  
027700          WS-PROP-VALUE (IX) = SPACES                                     
027800         CONTINUE                                                         
027900       ELSE                                                               
028000         MOVE WS-PROP-NAME  (IX)                                          
028100                                 TO OUTQ-PROPERTY-NAME                    
028200         MOVE WS-PROP-VALUE (IX)                                          
028300                                 TO OUTQ-PROPERTY-VALUE                   
028400         PERFORM S04-SET-PROP                                             
028500       END-IF                                                             
028600     END-PERFORM                                                          
028700     .                                                                    
028800                                                                          
028900 S03-HANDLE-INPUT-MSGPROP SECTION.                                        
029000                                                                          
029100     MOVE 1                      TO IX                                    
029200     MOVE SPACES                 TO WS-IN-PROP-TO-SET                     
029300                                    WS-PROP-TABLE                         
029400                                                                          
029500     PERFORM UNTIL IN-AREA (1:4) NOT = '¤MQM'                             
029600       IF IN-AREA (1:9) = '¤MQMPROP '                                     
029700         IF IX > 5                                                        
029800           DISPLAY 'CAN HANDLE ONLY 5 PROPERTIES PER MSG'                 
029900           CALL ABEND                                                     
030000         ELSE                                                             
030100           MOVE IN-AREA          TO WS-IN-PROP-TO-SET                     
030200           UNSTRING IN-PROPERTY                                           
030300                       DELIMITED BY '='                                   
030400                               INTO WS-PROP-NAME  (IX)                    
030500                                    WS-PROP-VALUE (IX)                    
030600           ADD 1                 TO IX                                    
030700         END-IF                                                           
030800       END-IF                                                             
030900       PERFORM S01-READ-INDATA                                            
031000     END-PERFORM                                                          
031100     .                                                                    
031200 B-PROCESS-MESSAGE  SECTION.                                              
031300                                                                          
031400     MOVE IN-RECLEN              TO OUTQ-KVDLEN                           
031500     MOVE IN-AREA (1:IN-RECLEN)  TO OUTQ-DATA (1:OUTQ-KVDLEN)             
031600     PERFORM S04-PUT-MESSAGE                                              
031700     .                                                                    
031800                                                                          
031900                                                                          
032000 Z-FINIT SECTION.                                                         
032100                                                                          
032200     PERFORM S01-CLOSE-FILE                                               
032300                                                                          
032400     MOVE 'S'                    TO POSTSUM-OPKOD                         
032500     CALL POSTSUM             USING POSTSUM-PARM                          
032600                                                                          
032700     .                                                                    
032800 S01-OPEN-FILE  SECTION.                                                  
032900                                                                          
033000*--  INITIALIZE DCB IN WFILREAD SO PROGRAM KNOWS                          
033100*--  WHICH FILE IT SHOULD OPEN FOR READ                                   
033200     CALL WFILREAD            USING READ-INFILE                           
033300                                    READ-FUNC-INIT                        
033400                                                                          
033500     IF READ-FUNC-INIT = 'F'                                              
033600       MOVE 'INIT CALL TO WFILREAD FAILED'                                
033700                                 TO ERROR-TEXT-STR                        
033800       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
033900     END-IF                                                               
034000                                                                          
034100     CALL WFILREAD            USING READ-INFILE                           
034200                                    READ-FUNC-OPEN                        
034300                                                                          
034400     IF READ-FUNC-OPEN = 'F'                                              
034500       MOVE 'INPUT FILE COULD NOT BE OPENED'                              
034600                                 TO ERROR-TEXT-STR                        
034700       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
034800     ELSE                                                                 
034900       DISPLAY 'INPUT FILE OPENED  OK'                                    
035000     END-IF                                                               
035100     .                                                                    
035200 S01-READ-INDATA  SECTION.                                                
035300                                                                          
035400     INITIALIZE IN-AREA                                                   
035500                                                                          
035600     CALL WFILREAD            USING READ-INFILE                           
035700                                    READ-FUNC-READ                        
035800                                    IN-AREA                               
035900                                    READ-RECLEN                           
036000                                                                          
036100     IF READ-FUNC-READ = 'F'                                              
036200       MOVE 'ERROR WHEN READING INPUT FILE'                               
036300                                 TO ERROR-TEXT-STR                        
036400       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
036500     ELSE                                                                 
036600       IF READ-FUNC-READ = 'E'                                            
036700         SET END-OF-INDATA       TO TRUE                                  
036800       ELSE                                                               
036900         COMPUTE IN-RECLEN = READ-RECLEN                                  
037000         MOVE 'INDATA'           TO POSTSUM-FDNAMN                        
037100         MOVE 'FILDD1  '         TO POSTSUM-DDNAMN2                       
037200         MOVE SPACE              TO POSTSUM-TRANSTYP                      
037300         CALL POSTSUM         USING POSTSUM-PARM                          
037400       END-IF                                                             
037500     END-IF                                                               
037600                                                                          
037700     IF END-OF-INDATA                                                     
037800       CONTINUE                                                           
037900     ELSE                                                                 
038000       IF IN-RECLEN > MAX-IN-RECLEN                                       
038100         MOVE SPACE              TO ERROR-TEXT-STR                        
038200         STRING 'WZ112300 - INPUT RECORD LENGTH IS TOO LONG ('            
038300                IN-RECLEN '). MAX ALLLOWED IS ' MAX-IN-RECLEN             
038400           DELIMITED BY SIZE   INTO ERROR-TEXT-STR                        
038500                                                                          
038600         DISPLAY IDPGM ' ' ERROR-TEXT                                     
038700         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
038800       END-IF                                                             
038900     END-IF                                                               
039000     .                                                                    
039100 S01-CLOSE-FILE    SECTION.                                               
039200                                                                          
039300     CALL WFILREAD            USING READ-INFILE                           
039400                                    READ-FUNC-CLOSE                       
039500                                                                          
039600     IF READ-FUNC-CLOSE = 'F'                                             
039700       MOVE 'ERROR WHEN CLOSING INPUT FILE'                               
039800                                 TO ERROR-TEXT-STR                        
039900       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
040000     ELSE                                                                 
040100       DISPLAY 'INPUT FILE CLOSED  OK'                                    
040200     END-IF                                                               
040300     .                                                                    
040400 S02-GET-DSINFO SECTION.                                                  
040500     MOVE 'FILDD1  '             TO DSINFO-DDNAME                         
040600     CALL WDSINFO             USING DSINFO-WDSAREA                        
040700     IF DSINFO-KDSVAR-OK                                                  
040800       MOVE DSINFO-DSNAME        TO OUTQ-PHYSICALID                       
040900     ELSE                                                                 
041000       MOVE 'NOT AVAILABLE'      TO OUTQ-PHYSICALID                       
041100     END-IF                                                               
041200*    MOVE 'test.zip'             TO OUTQ-PHYSICALID                       
041300                                                                          
041400     .                                                                    
041500 S04-OUTQ-OPEN  SECTION.                                                  
041600                                                                          
041700     MOVE 'OPEN'                 TO OUTQ-KDFUNC                           
041800     MOVE ZERO                   TO OUTQ-KVDLEN                           
041900     CALL WZ11OUTQ            USING OUTQ-AREA                             
042000                                                                          
042100     IF OUTQ-KDRC > 0                                                     
042200       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
042300       STRING 'WZ11OUTQ OPEN ERROR RC = ' KDRC-DISPLAY                    
042400         DELIMITED BY SIZE     INTO ERROR-TEXT                            
042500       CALL ABEND                                                         
042600     END-IF                                                               
042700     .                                                                    
042800                                                                          
042900 S04-SET-PROP SECTION.                                                    
043000                                                                          
043100     MOVE 'SETPROP'              TO OUTQ-KDFUNC                           
043200     MOVE ZERO                   TO OUTQ-KVDLEN                           
043300     CALL WZ11OUTQ            USING OUTQ-AREA                             
043400                                                                          
043500     IF OUTQ-KDRC > 0                                                     
043600       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
043700       STRING 'WZ11OUTQ INQPROP ERROR RC = ' KDRC-DISPLAY                 
043800         DELIMITED BY SIZE     INTO ERROR-TEXT                            
043900       CALL ABEND                                                         
044000     ELSE                                                                 
044100       DISPLAY OUTQ-PROPERTY-NAME '=' OUTQ-PROPERTY-VALUE                 
044200     END-IF                                                               
044300                                                                          
044400     .                                                                    
044500 S04-PUT-MESSAGE  SECTION.                                                
044600*    -- PUT THE MESSAGE                                                   
044700     MOVE 'PUT'                  TO OUTQ-KDFUNC                           
044800                                                                          
044900     CALL WZ11OUTQ            USING OUTQ-AREA                             
045000     IF OUTQ-KDRC > 0                                                     
045100       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
045200       STRING 'WZ11OUTQ PUT ERROR RC = ' KDRC-DISPLAY                     
045300         DELIMITED BY SIZE     INTO ERROR-TEXT                            
045400       CALL ABEND                                                         
045500     END-IF                                                               
045600     .                                                                    
045700                                                                          
045800 S04-OUTQ-SEND SECTION.                                                   
045900     MOVE 'SEND'                 TO OUTQ-KDFUNC                           
046000     MOVE ZERO                   TO OUTQ-KVDLEN                           
046100     CALL WZ11OUTQ            USING OUTQ-AREA                             
046200                                                                          
046300     IF OUTQ-KDRC > 0                                                     
046400       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
046500       STRING 'WZ11OUTQ CLOSE ERROR RC = ' KDRC-DISPLAY                   
046600         DELIMITED BY SIZE     INTO ERROR-TEXT                            
046700       CALL ABEND                                                         
046800     END-IF                                                               
046900                                                                          
047000     .                                                                    
047100 S04-OUTQ-CLOSE  SECTION.                                                 
047200     MOVE 'CLOSE'                TO OUTQ-KDFUNC                           
047300     MOVE ZERO                   TO OUTQ-KVDLEN                           
047400     CALL WZ11OUTQ            USING OUTQ-AREA                             
047500                                                                          
047600     IF OUTQ-KDRC > 0                                                     
047700       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
047800       STRING 'WZ11OUTQ CLOSE ERROR RC = ' KDRC-DISPLAY                   
047900         DELIMITED BY SIZE     INTO ERROR-TEXT                            
048000       CALL ABEND                                                         
048100     END-IF                                                               
048200                                                                          
048300     .                                                                    
048400                                                                          
