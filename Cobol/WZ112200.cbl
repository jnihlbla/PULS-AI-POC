000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ112200.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   19/03/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        PROGRAM FOR SENDING FILE TO MQ                                   
001000*                                                                         
001100*    ABENDCODES:                                                          
001200*        U0016 -  IF ADDRESS IS MISSING IN WZ01ATAB                       
001300*                                                                         
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700 DATA DIVISION.                                                           
001800 WORKING-STORAGE SECTION.                                                 
001900                                                                          
002000 77  IDPGM                       PIC X(8)    VALUE 'WZ112200'.            
002100 77  YES                         PIC X       VALUE 'J'.                   
002200 77  NOO                         PIC X       VALUE 'N'.                   
002300 77  WS-REC-COUNT                PIC S9(9)   VALUE ZERO  COMP-3.          
002400 77  WS-RECLEN                   PIC 9(5)    VALUE ZERO.                  
002500 77  WS-CR                       PIC X(2)    VALUE 'CR'.                  
002600 77  WS-LF                       PIC X(2)    VALUE 'LF'.                  
002700 77  CR                          PIC X       VALUE X'0D'.                 
002800 77  LF-EBCDIC                   PIC X       VALUE X'25'.                 
002900 77  LF-ASCII                    PIC X       VALUE X'0A'.                 
003000 77  SWEDISH-EBCDIC              PIC S9(9)   VALUE 278  BINARY.           
003100 77  WS-QMGR                     PIC X(4)    VALUE SPACE.                 
003200                                                                          
003300 77  SW-END-OF-INDATA            PIC X       VALUE 'N'.                   
003400     88 END-OF-INDATA                        VALUE 'J'.                   
003500     88 NOT-END-OF-INDATA                    VALUE 'N'.                   
003600                                                                          
003700 77  SW-MQ-GROUP-MSG             PIC X       VALUE 'N'.                   
003800     88 MSG-GRP-NO                           VALUE 'N'.                   
003900     88 MSG-GRP-YES                          VALUE 'Y'.                   
004000                                                                          
004100 77  SW-MQ-GROUP-INDICATOR       PIC X       VALUE 'N'.                   
004200     88 FIRST-MSG-IN-GRP                     VALUE 'F'.                   
004300     88 LAST-MSG-IN-GRP                      VALUE 'L'.                   
004400     88 MSG-IN-GRP                           VALUE 'M'.                   
004500     88 MSG-GRP-VALID                        VALUE 'F' 'L' 'M'.           
004600     88 MSG-GRP-NOT-SET                      VALUE 'N'.                   
004700                                                                          
004800 77  SW-MQ-SPLIT-MSG             PIC X       VALUE 'N'.                   
004900     88 MSG-SPLIT-NO                         VALUE 'N'.                   
005000     88 MSG-SPLIT-YES                        VALUE 'Y'.                   
005100                                                                          
005200 01  WS-DELIM-LEN                PIC 9       VALUE ZERO.                  
005300 01  WS-DELIMITER                PIC X(2)    VALUE SPACES.                
005400                                                                          
005500 01  RC-DISPLAY                  PIC Z(8)9.                               
005600 01  REASON-DISPLAY              PIC Z(8)9.                               
005700                                                                          
005800 01  SMALL-LETTERS              PIC X(31)    VALUE                        
005900     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
006000 01  CAPS-LETTERS               PIC X(31)    VALUE                        
006100     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
006200                                                                          
006300 01  MQ-CALLS.                                                            
006400*                                                                         
006500     03  MQCONN                  PIC X(8)    VALUE 'CSQBCONN'.            
006600     03  MQDISC                  PIC X(8)    VALUE 'CSQBDISC'.            
006700     03  MQOPEN                  PIC X(8)    VALUE 'CSQBOPEN'.            
006800     03  MQCLOSE                 PIC X(8)    VALUE 'CSQBCLOS'.            
006900     03  MQPUT                   PIC X(8)    VALUE 'CSQBPUT '.            
007000     03  MQCRTMH                 PIC X(8)    VALUE 'CSQBCTMH'.            
007100     03  MQSETMP                 PIC X(8)    VALUE 'CSQBSTMP'.            
007200                                                                          
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400*                                                                         
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  WZ01ATAB                PIC X(8)    VALUE 'WZ01ATAB'.            
007700     03  WFILREAD                PIC X(8)    VALUE 'WFILREAD'.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007900                                                                          
008000*    --- PARAMETERS TO ABEND                                              
008100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008400                                                                          
008500*    --- PARAMETERS TO POSTSUM                                            
008600                                                                          
008700 01  -COPY W0005   -PRE  POSTSUM-                                         
008800                                                                          
008900 01  ERROR-TEXT.                                                          
009000     03  FILLER                  PIC X(12)  VALUE 'ERROR-TEXT: '.         
009100     03  ERROR-TEXT-STR          PIC X(80)  VALUE SPACE.                  
009200                                                                          
009300*    -- FIELDS USED WHEN CUTTING UP THE DATA INTO                         
009400*    -- PIECES OF TRANMSITTABLE SIZE                                      
009500 01  WLEN                        PIC S9(9) BINARY VALUE ZERO.             
009600 01  WSTART                      PIC S9(9) BINARY VALUE 1.                
009700                                                                          
009800 01  WDATA                       PIC X(100000000)  VALUE SPACE.           
009900                                                                          
010000* WS-AREA = IN-AREA + 2 (DELIMITER)                                       
010100 01  WS-AREA                     PIC X(3002) VALUE SPACES.                
010200                                                                          
010300 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
010400 01  IN-RECLEN                   PIC 9(5).                                
010500 01  IN-AREA                     PIC X(3000) VALUE SPACES.                
010600 01  MAX-IN-RECLEN               PIC 9(4)    VALUE 3000.                  
010700                                                                          
010800 01  READ-INFILE                 PIC S9(9)   COMP.                        
010900 01  READ-RECLEN                 PIC S9(4)   COMP SYNC.                   
011000 01  READ-FUNC.                                                           
011100     03  READ-FUNC-INIT          PIC X       VALUE 'I'.                   
011200     03  READ-FUNC-OPEN          PIC X       VALUE 'O'.                   
011300     03  READ-FUNC-READ          PIC X       VALUE ' '.                   
011400     03  READ-FUNC-CLOSE         PIC X       VALUE 'C'.                   
011500                                                                          
011600*    -- PARAMETERS TO WZ01ATAB                                            
011700 01  -COPY WZ01ATAB                                                       
011800                                                                          
011900 01  MQ-AREA-START               PIC X(24)   VALUE  'MQ-AREA'.            
012000 01  MQ-AREA                     PIC X(80).                               
012100                                                                          
012200 01  MQ-QMGR                     PIC X(48) VALUE SPACE.                   
012300 01  MQ-QNAME                    PIC X(48) VALUE SPACE.                   
012400 01  MQ-COMPCODE                 PIC S9(9) BINARY.                        
012500 01  MQ-REASON                   PIC S9(9) BINARY.                        
012600 01  MQ-HCONN                    PIC S9(9) BINARY VALUE ZERO.             
012700 01  MQ-OPENOPTIONS              PIC S9(9) BINARY VALUE ZERO.             
012800 01  MQ-HOBJ                     PIC S9(9) BINARY VALUE ZERO.             
012900 01  MQ-MSGLENGTH                PIC S9(9) BINARY.                        
013000 01  MQ-DATALENGTH               PIC S9(9) BINARY.                        
013100                                                                          
013200 01  MQ-INTEGRATION-ID           PIC X(100)  VALUE SPACE.                 
013300 01  MQ-CONTRACT-ID              PIC X(100)  VALUE SPACE.                 
013400 01  MQ-TOPIC-STRING             PIC X(100)  VALUE SPACE.                 
013500 01  MQ-RECORD-DELIMITER         PIC X(10)   VALUE SPACE.                 
013600 01  MQ-TYPE                     PIC X(100)  VALUE SPACE.                 
013700 01  MQ-SELECTOR                 PIC X(100)  VALUE SPACE.                 
013800 01  MQ-MSGTYPE                  PIC X       VALUE SPACE.                 
013900 01  MQ-VCOM-DELIMITER           PIC X(100)  VALUE SPACE.                 
014000 01  MQ-PROPHANDLE               PIC S9(18) BINARY.                       
014100 01  MQ-PROPNAME-TO-SET          PIC X(50).                               
014200 01  MQ-PROPTYPE                 PIC S9(9)  BINARY.                       
014300 01  MQ-PROPVALUELENGTH          PIC S9(9)  BINARY.                       
014400 01  MQ-PROPVALUE                PIC X(100).                              
014500 01  MQ-PROPDATALENGTH           PIC S9(9)  BINARY.                       
014600                                                                          
014700 01  MQ-MESSAGE-DESCRIPTOR.                                               
014800*    -COPY CMQMD2V                                                        
014900                                                                          
015000 01  MQ-PUT-MESSAGE-OPTIONS.                                              
015100*    -COPY CMQPMOV                                                        
015200                                                                          
015300 01  MQ-OBJECT-DESCRIPTOR.                                                
015400*    -COPY CMQODV                                                         
015500                                                                          
015600 01  MQ-CONSTANTS.                                                        
015700*    -COPY CMQV                                                           
015800                                                                          
015900 01  MQ-CREATE-HANDLE-OPTIONS.                                            
016000*    -COPY CMQCMHOV                                                       
016100                                                                          
016200 01  MQ-SET-PROP-OPTIONS.                                                 
016300*    -COPY CMQSMPOV                                                       
016400                                                                          
016500 01  MQ-PROPNAME.                                                         
016600*    -COPY CMQCHRVV                                                       
016700                                                                          
016800 01  MQ-PROPDESC.                                                         
016900*    -COPY CMQPDV                                                         
017000                                                                          
017100 LINKAGE SECTION.                                                         
017200                                                                          
017300 01  PARM-AREA.                                                           
017400     03  PARM-LENGTH             PIC S9(4) BINARY.                        
017500     03  PARM-RTENV              PIC X(4).                                
017600     03  PARM-QMGR               PIC X(4).                                
017700     03  PARM-ADDISPABS          PIC X(50).                               
017800                                                                          
017900 PROCEDURE DIVISION USING PARM-AREA.                                      
018000 MAIN SECTION.                                                            
018100                                                                          
018200     PERFORM A-INIT                                                       
018300                                                                          
018400     PERFORM S04-MQ-CONN-OPEN                                             
018500                                                                          
018600     PERFORM S01-READ-INDATA                                              
018700                                                                          
018800     IF NOT-END-OF-INDATA                                                 
018900       PERFORM S04-MQ-SET-HEADERS                                         
019000     END-IF                                                               
019100                                                                          
019200     PERFORM UNTIL END-OF-INDATA                                          
019300       PERFORM B-PROCESS-MESSAGE                                          
019400       PERFORM S01-READ-INDATA                                            
019500     END-PERFORM                                                          
019600                                                                          
019700     PERFORM S04-MQ-CLOSE                                                 
019800     PERFORM S04-MQ-DISC                                                  
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO                   TO RETURN-CODE                           
020200     GOBACK                                                               
020300     .                                                                    
020400 A-INIT SECTION.                                                          
020500                                                                          
020600     PERFORM S01-OPEN-FILE                                                
020700                                                                          
020800     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
020900                                                                          
021000     IF PARM-LENGTH > 8                                                   
021100       MOVE PARM-ADDISPABS(1:PARM-LENGTH - 8)                             
021200                                 TO ATAB-ADDISPABS                        
021300     ELSE                                                                 
021400       MOVE SPACE                TO ATAB-ADDISPABS                        
021500     END-IF                                                               
021600                                                                          
021700     DISPLAY '************ PARM DATA ***************'                     
021800     DISPLAY 'ENVIRONMENT        : ' PARM-RTENV                           
021900     DISPLAY 'QUEUE MANAGER      : ' PARM-QMGR                            
022000     DISPLAY 'ADDRESS            : ' ATAB-ADDISPABS                       
022100     DISPLAY '****** END OF PARM DATA **************'                     
022200                                                                          
022300     INSPECT ATAB-ADDISPABS CONVERTING                                    
022400             SMALL-LETTERS TO CAPS-LETTERS                                
022500     CALL WZ01ATAB            USING ATAB-WZ01ATAB                         
022600                                                                          
022700     IF RETURN-CODE > ZERO                                                
022800*      -- ABSTRACT ADDRESS NOT FOUND IN ATAB                              
022900       MOVE SPACE                TO ERROR-TEXT-STR                        
023000       STRING ATAB-ADDISPABS DELIMITED BY SPACE                           
023100              ' NOT FOUND IN WZ01ATAB' DELIMITED BY SIZE                  
023200                               INTO ERROR-TEXT-STR                        
023300       DISPLAY IDPGM ' ' ERROR-TEXT                                       
023400       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
023500     ELSE                                                                 
023600       PERFORM AA-EXTRACT-MQ-PARAMETERS                                   
023700     END-IF                                                               
023800                                                                          
023900     .                                                                    
024000                                                                          
024100 AA-EXTRACT-MQ-PARAMETERS SECTION.                                        
024200                                                                          
024300*    -- QMGR NAME IS DETERMINED FROM ENVIRONMENT SPECIFIED                
024400*    -- ON EXEC-CARD PARM                                                 
024500*    -- CAN BE OVERRIDED FROM PARM AS WELL.                               
024600                                                                          
024700     IF PARM-QMGR = 'NULL'                                                
024800       IF PARM-RTENV = 'QASE'                                             
024900         MOVE 'MC0P'             TO MQ-QMGR                               
025000       ELSE                                                               
025100         IF PARM-RTENV = 'ACPT'                                           
025200           MOVE 'MC0Q'           TO MQ-QMGR                               
025300         ELSE                                                             
025400           MOVE 'MC0T'           TO MQ-QMGR                               
025500         END-IF                                                           
025600       END-IF                                                             
025700     ELSE                                                                 
025800       MOVE PARM-QMGR            TO MQ-QMGR                               
025900     END-IF                                                               
026000     DISPLAY 'QUEUE MANAGER      : ' MQ-QMGR                              
026100                                                                          
026200*    -- QUEUE NAME                                                        
026300     MOVE ZERO                   TO TALLY                                 
026400     MOVE SPACE                  TO MQ-QNAME                              
026500     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
026600             FOR CHARACTERS BEFORE INITIAL 'RQ:'                          
026700     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
026800       UNSTRING ATAB-ADDISPINT(TALLY + 4:)                                
026900         DELIMITED BY ';'                                                 
027000                               INTO MQ-QNAME                              
027100     ELSE                                                                 
027200       MOVE SPACE                TO ERROR-TEXT-STR                        
027300       MOVE 'ALIAS/REMOTE QUEUE NAME I MISSING IN WZ01ATAB'               
027400                                 TO ERROR-TEXT-STR                        
027500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
027600       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
027700     END-IF                                                               
027800     DISPLAY 'QUEUE NAME         : ' MQ-QNAME                             
027900                                                                          
028000*    -- META DATA: INTEGRATION ID                                         
028100     MOVE ZERO                   TO TALLY                                 
028200     MOVE SPACE                  TO MQ-INTEGRATION-ID                     
028300     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
028400             FOR CHARACTERS BEFORE INITIAL 'I:'                           
028500     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
028600       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
028700                                 INTO MQ-INTEGRATION-ID                   
028800     END-IF                                                               
028900     DISPLAY 'INTEGRATION ID     : ' MQ-INTEGRATION-ID                    
029000                                                                          
029100*    -- META DATA: CONTRACT ID                                            
029200     MOVE ZERO                   TO TALLY                                 
029300     MOVE SPACE                  TO MQ-CONTRACT-ID                        
029400     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
029500             FOR CHARACTERS BEFORE INITIAL 'C:'                           
029600     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
029700       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
029800                               INTO MQ-CONTRACT-ID                        
029900     END-IF                                                               
030000     DISPLAY 'CONTRACT ID        : ' MQ-CONTRACT-ID                       
030100                                                                          
030200*    -- META DATA: TOPIC STRING                                           
030300     MOVE ZERO                   TO TALLY                                 
030400     MOVE SPACE                  TO MQ-TOPIC-STRING                       
030500     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
030600             FOR CHARACTERS BEFORE INITIAL 'T:'                           
030700     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
030800       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
030900                               INTO MQ-TOPIC-STRING                       
031000     END-IF                                                               
031100     DISPLAY 'TOPIC STRING       : ' MQ-TOPIC-STRING                      
031200                                                                          
031300*    -- META DATA: RECORD DELIMITER                                       
031400     MOVE ZERO                   TO TALLY                                 
031500     INITIALIZE MQ-RECORD-DELIMITER                                       
031600     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
031700             FOR CHARACTERS BEFORE INITIAL 'D:'                           
031800     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
031900       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
032000                               INTO MQ-RECORD-DELIMITER                   
032100       DISPLAY 'RECORD DELIMITER   : ' MQ-RECORD-DELIMITER                
032200     END-IF                                                               
032300     EVALUATE MQ-RECORD-DELIMITER                                         
032400       WHEN WS-CR                                                         
032500         MOVE CR                 TO WS-DELIMITER                          
032600         MOVE 1                  TO WS-DELIM-LEN                          
032700       WHEN WS-LF                                                         
032800         MOVE LF-EBCDIC          TO WS-DELIMITER                          
032900         MOVE 1                  TO WS-DELIM-LEN                          
033000       WHEN SPACE                                                         
033100         DISPLAY 'RECORD DELIMITER   : CRLF (DEFAULT)'                    
033200         STRING CR LF-EBCDIC                                              
033300           DELIMITED BY SIZE   INTO WS-DELIMITER                          
033400         MOVE 2                  TO WS-DELIM-LEN                          
033500       WHEN OTHER                                                         
033600         DISPLAY 'RECORD DELIMITER   : CRLF ( ! OVERRIDED ! )'            
033700         STRING CR LF-EBCDIC                                              
033800           DELIMITED BY SIZE   INTO WS-DELIMITER                          
033900         MOVE 2                  TO WS-DELIM-LEN                          
034000     END-EVALUATE                                                         
034100                                                                          
034200*    -- META DATA: TYPE                                                   
034300     MOVE ZERO                   TO TALLY                                 
034400     MOVE SPACE                  TO MQ-TYPE                               
034500     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
034600             FOR CHARACTERS BEFORE INITIAL 'TY:'                          
034700     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
034800       UNSTRING ATAB-ADDISPMETA(TALLY + 4:)  DELIMITED BY ';'             
034900                               INTO MQ-TYPE                               
035000     END-IF                                                               
035100     DISPLAY 'TYPE               : ' MQ-TYPE                              
035200                                                                          
035300*    -- META DATA: MQMD MSGTYPE                                           
035400     MOVE ZERO                   TO TALLY                                 
035500     MOVE SPACE                  TO MQ-MSGTYPE                            
035600     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
035700             FOR CHARACTERS BEFORE INITIAL 'MSGTYP:'                      
035800     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
035900       UNSTRING ATAB-ADDISPMETA(TALLY + 8:)  DELIMITED BY ';'             
036000                               INTO MQ-MSGTYPE                            
036100     END-IF                                                               
036200     DISPLAY 'MQMD MSGTYPE       : ' MQ-MSGTYPE                           
036300                                                                          
036400*    -- META DATA: SELECTOR                                               
036500     MOVE ZERO                   TO TALLY                                 
036600     MOVE SPACE                  TO MQ-SELECTOR                           
036700     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
036800             FOR CHARACTERS BEFORE INITIAL 'SEL:'                         
036900     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
037000       UNSTRING ATAB-ADDISPMETA(TALLY + 5:)  DELIMITED BY ';'             
037100                               INTO MQ-SELECTOR                           
037200     END-IF                                                               
037300     DISPLAY 'MQMD SELECTOR      : ' MQ-SELECTOR                          
037400                                                                          
037500*    -- META DATA: VCOM DELIMITER                                         
037600     MOVE ZERO                   TO TALLY                                 
037700     MOVE SPACE                  TO MQ-VCOM-DELIMITER                     
037800     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
037900             FOR CHARACTERS BEFORE INITIAL 'DVCOM:'                       
038000     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
038100       UNSTRING ATAB-ADDISPMETA(TALLY + 7:)  DELIMITED BY ';'             
038200                               INTO MQ-VCOM-DELIMITER                     
038300     END-IF                                                               
038400     DISPLAY 'VCOM-DELIMITER     : ' MQ-VCOM-DELIMITER                    
038500                                                                          
038600*    -- META DATA: GROUPING SWITCH                                        
038700     MOVE ZERO                   TO TALLY                                 
038800     MOVE 'N'                    TO SW-MQ-GROUP-MSG                       
038900     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
039000             FOR CHARACTERS BEFORE INITIAL 'GRP:'                         
039100     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
039200       UNSTRING ATAB-ADDISPMETA(TALLY + 5:)  DELIMITED BY ';'             
039300                               INTO SW-MQ-GROUP-MSG                       
039400     END-IF                                                               
039500     DISPLAY 'GROUP MSG SWITCH   : ' SW-MQ-GROUP-MSG                      
039600                                                                          
039700*    -- META DATA: SPLIT MESSAGE SWITCH                                   
039800     MOVE ZERO                   TO TALLY                                 
039900     MOVE 'N'                    TO SW-MQ-SPLIT-MSG                       
040000     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
040100             FOR CHARACTERS BEFORE INITIAL 'SPL:'                         
040200     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
040300       UNSTRING ATAB-ADDISPMETA(TALLY + 5:)  DELIMITED BY ';'             
040400                               INTO SW-MQ-SPLIT-MSG                       
040500     END-IF                                                               
040600     DISPLAY 'SPLIT MSG SWITCH   : ' SW-MQ-SPLIT-MSG                      
040700                                                                          
040800     DISPLAY '                     '                                      
040900     DISPLAY '                     '                                      
041000     .                                                                    
041100                                                                          
041200 B-PROCESS-MESSAGE  SECTION.                                              
041300                                                                          
041400     IF WSTART + IN-RECLEN - 1 > LENGTH OF WDATA                          
041500                                                                          
041600*      MESSAGE LONGER THAN MAX MESSAGE LENGTH,                            
041700*        1. IF NOT ALLOWED TO SEND AS A GROUP OR                          
041800*           SPLIT MESSAGES, ERROR.                                        
041900*        2. IF NOT ALLOWED TO SEND AS A GROUP, BUT                        
042000*           CAN SPLIT INTO MULTIPLE MESSAGES.                             
042100*        3. IF ALLOWED TO SEND AS A GROUP, BUT                            
042200*           EACH MESSAGE AS A CONTINUATION OF PREVIOUS ONE                
042300*           (SPLIT CAN HAPPEN IN THE MIDDLE OF A RECORD).                 
042400*        4. IF ALLOWED TO SEND AS A GROUP AND                             
042500*           SPLIT AT THE END OF A RECORD.                                 
042600                                                                          
042700       EVALUATE TRUE                                                      
042800         WHEN MSG-GRP-NO AND MSG-SPLIT-NO                                 
042900           PERFORM BA-SEND-ERROR                                          
043000         WHEN MSG-GRP-NO AND MSG-SPLIT-YES                                
043100           PERFORM BB-SPLIT-SEND                                          
043200         WHEN MSG-GRP-YES AND MSG-SPLIT-NO                                
043300           PERFORM BC-SEND-CONT-GRP                                       
043400         WHEN MSG-GRP-YES AND MSG-SPLIT-YES                               
043500           PERFORM BD-SPLIT-SEND-GRP                                      
043600       END-EVALUATE                                                       
043700     ELSE                                                                 
043800       STRING WS-AREA (1:IN-RECLEN) DELIMITED BY SIZE                     
043900                               INTO WDATA                                 
044000                       WITH POINTER WSTART                                
044100       COMPUTE WLEN = WSTART - 1                                          
044200     END-IF                                                               
044300     .                                                                    
044400                                                                          
044500 BA-SEND-ERROR      SECTION.                                              
044600                                                                          
044700     MOVE LENGTH OF WDATA        TO RC-DISPLAY                            
044800     MOVE SPACE                  TO ERROR-TEXT-STR                        
044900     STRING 'TOTAL MQ MESSAGE LENGTH TOO LONG. '                          
045000            ' MAX ALLLOWED IS ' RC-DISPLAY                                
045100       DELIMITED BY SIZE       INTO ERROR-TEXT-STR                        
045200                                                                          
045300     DISPLAY IDPGM ' ' ERROR-TEXT                                         
045400     CALL ABEND               USING RKOD-ABEND-NO-DUMP                    
045500     .                                                                    
045600                                                                          
045700 BB-SPLIT-SEND      SECTION.                                              
045800                                                                          
045900     PERFORM S04-MQ-PUT                                                   
046000                                                                          
046100*    -- INITIATE A NEW FRESH MESSAGE                                      
046200     INITIALIZE WLEN                                                      
046300                WDATA                                                     
046400     MOVE 1                      TO WSTART                                
046500                                                                          
046600     STRING WS-AREA (1:IN-RECLEN) DELIMITED BY SIZE                       
046700                               INTO WDATA                                 
046800                       WITH POINTER WSTART                                
046900     COMPUTE WLEN = WSTART - 1                                            
047000     .                                                                    
047100                                                                          
047200 BC-SEND-CONT-GRP   SECTION.                                              
047300                                                                          
047400     IF MSG-GRP-NOT-SET                                                   
047500       SET FIRST-MSG-IN-GRP      TO TRUE                                  
047600     END-IF                                                               
047700                                                                          
047800     COMPUTE WS-RECLEN = LENGTH OF WDATA - WSTART + 1                     
047900                                                                          
048000     STRING WS-AREA (1:WS-RECLEN) DELIMITED BY SIZE                       
048100                               INTO WDATA                                 
048200                       WITH POINTER WSTART                                
048300     COMPUTE WLEN = WSTART - 1                                            
048400                                                                          
048500     PERFORM S04-MQ-PUT-GROUP                                             
048600                                                                          
048700*    -- INITIATE A NEW FRESH MESSAGE                                      
048800     INITIALIZE WLEN                                                      
048900                WDATA                                                     
049000     MOVE 1                      TO WSTART                                
049100                                                                          
049200     STRING WS-AREA (WS-RECLEN + 1:IN-RECLEN - WS-RECLEN)                 
049300                       DELIMITED BY SIZE                                  
049400                               INTO WDATA                                 
049500                       WITH POINTER WSTART                                
049600     COMPUTE WLEN = WSTART - 1                                            
049700     .                                                                    
049800                                                                          
049900 BD-SPLIT-SEND-GRP  SECTION.                                              
050000                                                                          
050100     IF MSG-GRP-NOT-SET                                                   
050200       SET FIRST-MSG-IN-GRP      TO TRUE                                  
050300     END-IF                                                               
050400                                                                          
050500     PERFORM S04-MQ-PUT-GROUP                                             
050600                                                                          
050700*    -- INITIATE A NEW FRESH MESSAGE                                      
050800     INITIALIZE WLEN                                                      
050900                WDATA                                                     
051000     MOVE 1                      TO WSTART                                
051100                                                                          
051200     STRING WS-AREA (1:IN-RECLEN) DELIMITED BY SIZE                       
051300                               INTO WDATA                                 
051400                       WITH POINTER WSTART                                
051500     COMPUTE WLEN = WSTART - 1                                            
051600     .                                                                    
051700                                                                          
051800 Z-FINIT SECTION.                                                         
051900                                                                          
052000     PERFORM S01-CLOSE-FILE                                               
052100                                                                          
052200     MOVE 'S'                    TO POSTSUM-OPKOD                         
052300     CALL POSTSUM             USING POSTSUM-PARM                          
052400                                                                          
052500     .                                                                    
052600 S01-OPEN-FILE  SECTION.                                                  
052700                                                                          
052800*--  INITIALIZE DCB IN WFILREAD SO PROGRAM KNOWS                          
052900*--  WHICH FILE IT SHOULD OPEN FOR READ                                   
053000     CALL WFILREAD            USING READ-INFILE                           
053100                                    READ-FUNC-INIT                        
053200                                                                          
053300     IF READ-FUNC-INIT = 'F'                                              
053400       MOVE 'INIT CALL TO WFILREAD FAILED'                                
053500                                 TO ERROR-TEXT-STR                        
053600       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
053700     END-IF                                                               
053800                                                                          
053900     CALL WFILREAD            USING READ-INFILE                           
054000                                    READ-FUNC-OPEN                        
054100                                                                          
054200     IF READ-FUNC-OPEN = 'F'                                              
054300       MOVE 'INPUT FILE COULD NOT BE OPENED'                              
054400                                 TO ERROR-TEXT-STR                        
054500       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
054600     ELSE                                                                 
054700       DISPLAY 'INPUT FILE OPENED  OK'                                    
054800     END-IF                                                               
054900     .                                                                    
055000 S01-READ-INDATA  SECTION.                                                
055100                                                                          
055200     INITIALIZE IN-AREA                                                   
055300                WS-AREA                                                   
055400                                                                          
055500     CALL WFILREAD            USING READ-INFILE                           
055600                                    READ-FUNC-READ                        
055700                                    IN-AREA                               
055800                                    READ-RECLEN                           
055900                                                                          
056000     IF READ-FUNC-READ = 'F'                                              
056100       MOVE 'ERROR WHEN READING INPUT FILE'                               
056200                                 TO ERROR-TEXT-STR                        
056300       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
056400     ELSE                                                                 
056500       IF READ-FUNC-READ = 'E'                                            
056600         SET END-OF-INDATA       TO TRUE                                  
056700       ELSE                                                               
056800         COMPUTE IN-RECLEN = READ-RECLEN                                  
056900         MOVE 'INDATA'           TO POSTSUM-FDNAMN                        
057000         MOVE 'FILDD1  '         TO POSTSUM-DDNAMN2                       
057100         MOVE SPACE              TO POSTSUM-TRANSTYP                      
057200         CALL POSTSUM         USING POSTSUM-PARM                          
057300       END-IF                                                             
057400     END-IF                                                               
057500                                                                          
057600     IF END-OF-INDATA                                                     
057700       CONTINUE                                                           
057800     ELSE                                                                 
057900       IF IN-RECLEN > MAX-IN-RECLEN                                       
058000         MOVE IN-RECLEN          TO RC-DISPLAY                            
058100         MOVE SPACE              TO ERROR-TEXT-STR                        
058200         STRING 'WZ112200 - INPUT RECORD LENGTH IS TOO LONG ('            
058300                IN-RECLEN '). MAX ALLLOWED IS ' MAX-IN-RECLEN             
058400           DELIMITED BY SIZE   INTO ERROR-TEXT-STR                        
058500                                                                          
058600         DISPLAY IDPGM ' ' ERROR-TEXT                                     
058700         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
058800                                                                          
058900       ELSE                                                               
059000*      -- INSERT CR/LF SEPARATOR                                          
059100         STRING IN-AREA(1:IN-RECLEN) DELIMITED BY SIZE                    
059200                WS-DELIMITER       DELIMITED BY SPACE                     
059300                               INTO WS-AREA                               
059400         COMPUTE IN-RECLEN = IN-RECLEN + WS-DELIM-LEN                     
059500       END-IF                                                             
059600     END-IF                                                               
059700     .                                                                    
059800 S01-CLOSE-FILE    SECTION.                                               
059900                                                                          
060000     CALL WFILREAD            USING READ-INFILE                           
060100                                    READ-FUNC-CLOSE                       
060200                                                                          
060300     IF READ-FUNC-CLOSE = 'F'                                             
060400       MOVE 'ERROR WHEN CLOSING INPUT FILE'                               
060500                                 TO ERROR-TEXT-STR                        
060600       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
060700     ELSE                                                                 
060800       DISPLAY 'INPUT FILE CLOSED  OK'                                    
060900     END-IF                                                               
061000     .                                                                    
061100 S04-MQ-CONN-OPEN  SECTION.                                               
061200                                                                          
061300     CALL  MQCONN             USING MQ-QMGR                               
061400                                    MQ-HCONN                              
061500                                    MQ-COMPCODE                           
061600                                    MQ-REASON                             
061700                                                                          
061800     IF MQ-COMPCODE NOT = MQCC-OK AND                                     
061900        MQ-REASON   NOT = MQRC-ALREADY-CONNECTED                          
062000       MOVE SPACE                TO ERROR-TEXT-STR                        
062100       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
062200       MOVE MQ-REASON            TO REASON-DISPLAY                        
062300       STRING 'RC FROM MQCONN ' RC-DISPLAY                                
062400              ' REASON: ' REASON-DISPLAY                                  
062500              ' ' MQ-QMGR                                                 
062600         DELIMITED BY SIZE                                                
062700                               INTO ERROR-TEXT-STR                        
062800       DISPLAY IDPGM ' ' ERROR-TEXT                                       
062900       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
063000     END-IF                                                               
063100                                                                          
063200     DISPLAY 'MQCONN SUCCESSFUL    '                                      
063300                                                                          
063400     MOVE MQ-QNAME               TO MQOD-OBJECTNAME                       
063500                                                                          
063600     COMPUTE MQ-OPENOPTIONS = MQOO-FAIL-IF-QUIESCING +                    
063700                              MQOO-OUTPUT                                 
063800                                                                          
063900     CALL  MQOPEN             USING MQ-HCONN                              
064000                                    MQOD                                  
064100                                    MQ-OPENOPTIONS                        
064200                                    MQ-HOBJ                               
064300                                    MQ-COMPCODE                           
064400                                    MQ-REASON                             
064500                                                                          
064600     IF MQ-COMPCODE NOT = MQCC-OK                                         
064700       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
064800       MOVE MQ-REASON            TO REASON-DISPLAY                        
064900       MOVE SPACE                TO ERROR-TEXT-STR                        
065000       STRING 'RC FROM MQOPEN ' RC-DISPLAY                                
065100              ' REASON: ' REASON-DISPLAY                                  
065200              ' ' MQ-QNAME                                                
065300          DELIMITED BY SIZE                                               
065400                               INTO ERROR-TEXT-STR                        
065500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
065600       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
065700     END-IF                                                               
065800     DISPLAY 'MQOPEN SUCCESSFUL    '                                      
065900*    -- INITIATE A NEW FRESH MESSAGE                                      
066000     INITIALIZE WLEN                                                      
066100                WDATA                                                     
066200     MOVE 1                      TO WSTART                                
066300     .                                                                    
066400 S04-MQ-SET-HEADERS SECTION.                                              
066500                                                                          
066600*    -- FIRST, CREATE A HANDLE FOR HEADER PROPERTIES                      
066700     MOVE MQCMHO-VALIDATE        TO MQCMHO-OPTIONS                        
066800     CALL  MQCRTMH            USING MQ-HCONN                              
066900                                    MQ-CREATE-HANDLE-OPTIONS              
067000                                    MQ-PROPHANDLE                         
067100                                    MQ-COMPCODE                           
067200                                    MQ-REASON                             
067300                                                                          
067400     IF MQ-COMPCODE NOT = MQCC-OK                                         
067500       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
067600       MOVE MQ-REASON            TO REASON-DISPLAY                        
067700                                                                          
067800       MOVE SPACE                TO ERROR-TEXT-STR                        
067900       STRING 'RC FROM MQCRTMH' RC-DISPLAY                                
068000              ' REASON: ' REASON-DISPLAY                                  
068100              ' ' MQ-QNAME                                                
068200          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
068300                                                                          
068400       DISPLAY IDPGM ' ' ERROR-TEXT                                       
068500       CALL ABEND             USING RKOD-ABEND-NO-DUMP                    
068600     END-IF                                                               
068700                                                                          
068800*    -- THEN SET THE MANDATORY PROPERTIES IF THEY HAVE VALUES             
068900                                                                          
069000     IF MQ-INTEGRATION-ID NOT = SPACE                                     
069100       SET MQCHARV-VSPTR         TO ADDRESS OF MQ-PROPNAME-TO-SET         
069200       MOVE 'IntegrationId'      TO MQ-PROPNAME-TO-SET                    
069300       MOVE 13                   TO MQCHARV-VSLENGTH                      
069400                                                                          
069500       MOVE SWEDISH-EBCDIC       TO MQCHARV-VSCCSID                       
069600       MOVE MQTYPE-STRING        TO MQ-PROPTYPE                           
069700                                                                          
069800       MOVE MQ-INTEGRATION-ID    TO MQ-PROPVALUE                          
069900                                                                          
070000       MOVE ZERO                 TO MQ-PROPVALUELENGTH                    
070100       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
070200               FOR CHARACTERS BEFORE INITIAL SPACE                        
070300                                                                          
070400       CALL  MQSETMP          USING MQ-HCONN                              
070500                                    MQ-PROPHANDLE                         
070600                                    MQ-SET-PROP-OPTIONS                   
070700                                    MQ-PROPNAME                           
070800                                    MQ-PROPDESC                           
070900                                    MQ-PROPTYPE                           
071000                                    MQ-PROPVALUELENGTH                    
071100                                    MQ-PROPVALUE                          
071200                                    MQ-COMPCODE                           
071300                                    MQ-REASON                             
071400                                                                          
071500       IF MQ-COMPCODE NOT = MQCC-OK                                       
071600         MOVE MQ-COMPCODE        TO RC-DISPLAY                            
071700         MOVE MQ-REASON          TO REASON-DISPLAY                        
071800         MOVE SPACE              TO ERROR-TEXT-STR                        
071900         STRING 'RC FROM MQSETMP (1)' RC-DISPLAY                          
072000                ' REASON: ' REASON-DISPLAY                                
072100            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
072200         DISPLAY IDPGM ' ' ERROR-TEXT                                     
072300         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
072400       END-IF                                                             
072500     END-IF                                                               
072600                                                                          
072700     IF MQ-CONTRACT-ID NOT = SPACE                                        
072800       SET MQCHARV-VSPTR         TO ADDRESS OF MQ-PROPNAME-TO-SET         
072900       MOVE 'ContractId'         TO MQ-PROPNAME-TO-SET                    
073000       MOVE 10                   TO MQCHARV-VSLENGTH                      
073100                                                                          
073200       MOVE SWEDISH-EBCDIC       TO MQCHARV-VSCCSID                       
073300       MOVE MQTYPE-STRING        TO MQ-PROPTYPE                           
073400                                                                          
073500       MOVE MQ-CONTRACT-ID       TO MQ-PROPVALUE                          
073600                                                                          
073700       MOVE ZERO                 TO MQ-PROPVALUELENGTH                    
073800       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
073900               FOR CHARACTERS BEFORE INITIAL SPACE                        
074000                                                                          
074100       CALL  MQSETMP          USING MQ-HCONN                              
074200                                    MQ-PROPHANDLE                         
074300                                    MQ-SET-PROP-OPTIONS                   
074400                                    MQ-PROPNAME                           
074500                                    MQ-PROPDESC                           
074600                                    MQ-PROPTYPE                           
074700                                    MQ-PROPVALUELENGTH                    
074800                                    MQ-PROPVALUE                          
074900                                    MQ-COMPCODE                           
075000                                    MQ-REASON                             
075100                                                                          
075200       IF MQ-COMPCODE NOT = MQCC-OK                                       
075300         MOVE MQ-COMPCODE        TO RC-DISPLAY                            
075400         MOVE MQ-REASON          TO REASON-DISPLAY                        
075500         MOVE SPACE              TO ERROR-TEXT-STR                        
075600         STRING 'RC FROM MQSETMP (2)' RC-DISPLAY                          
075700                ' REASON: ' REASON-DISPLAY                                
075800            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
075900         DISPLAY IDPGM ' ' ERROR-TEXT                                     
076000         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
076100       END-IF                                                             
076200     END-IF                                                               
076300                                                                          
076400     IF MQ-TOPIC-STRING NOT = SPACE                                       
076500       SET MQCHARV-VSPTR         TO ADDRESS OF MQ-PROPNAME-TO-SET         
076600       MOVE 'TopicStr'           TO MQ-PROPNAME-TO-SET                    
076700       MOVE 8                    TO MQCHARV-VSLENGTH                      
076800                                                                          
076900       MOVE SWEDISH-EBCDIC       TO MQCHARV-VSCCSID                       
077000       MOVE MQTYPE-STRING        TO MQ-PROPTYPE                           
077100                                                                          
077200       MOVE MQ-TOPIC-STRING      TO MQ-PROPVALUE                          
077300                                                                          
077400       MOVE ZERO                 TO MQ-PROPVALUELENGTH                    
077500       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
077600               FOR CHARACTERS BEFORE INITIAL SPACE                        
077700                                                                          
077800       CALL  MQSETMP          USING MQ-HCONN                              
077900                                    MQ-PROPHANDLE                         
078000                                    MQ-SET-PROP-OPTIONS                   
078100                                    MQ-PROPNAME                           
078200                                    MQ-PROPDESC                           
078300                                    MQ-PROPTYPE                           
078400                                    MQ-PROPVALUELENGTH                    
078500                                    MQ-PROPVALUE                          
078600                                    MQ-COMPCODE                           
078700                                    MQ-REASON                             
078800                                                                          
078900       IF MQ-COMPCODE NOT = MQCC-OK                                       
079000         MOVE MQ-COMPCODE        TO RC-DISPLAY                            
079100         MOVE MQ-REASON          TO REASON-DISPLAY                        
079200         MOVE SPACE              TO ERROR-TEXT-STR                        
079300         STRING 'RC FROM MQSETMP (3)' RC-DISPLAY                          
079400                ' REASON: ' REASON-DISPLAY                                
079500            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
079600         DISPLAY IDPGM ' ' ERROR-TEXT                                     
079700         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
079800       END-IF                                                             
079900     END-IF                                                               
080000                                                                          
080100     IF MQ-TYPE NOT = SPACE                                               
080200       SET MQCHARV-VSPTR         TO ADDRESS OF MQ-PROPNAME-TO-SET         
080300       MOVE 'Type'               TO MQ-PROPNAME-TO-SET                    
080400       MOVE 4                    TO MQCHARV-VSLENGTH                      
080500                                                                          
080600       MOVE SWEDISH-EBCDIC       TO MQCHARV-VSCCSID                       
080700       MOVE MQTYPE-STRING        TO MQ-PROPTYPE                           
080800                                                                          
080900       MOVE MQ-TYPE              TO MQ-PROPVALUE                          
081000                                                                          
081100       MOVE ZERO                 TO MQ-PROPVALUELENGTH                    
081200       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
081300               FOR CHARACTERS BEFORE INITIAL SPACE                        
081400                                                                          
081500       CALL  MQSETMP          USING MQ-HCONN                              
081600                                    MQ-PROPHANDLE                         
081700                                    MQ-SET-PROP-OPTIONS                   
081800                                    MQ-PROPNAME                           
081900                                    MQ-PROPDESC                           
082000                                    MQ-PROPTYPE                           
082100                                    MQ-PROPVALUELENGTH                    
082200                                    MQ-PROPVALUE                          
082300                                    MQ-COMPCODE                           
082400                                    MQ-REASON                             
082500                                                                          
082600       IF MQ-COMPCODE NOT = MQCC-OK                                       
082700         MOVE MQ-COMPCODE        TO RC-DISPLAY                            
082800         MOVE MQ-REASON          TO REASON-DISPLAY                        
082900         MOVE SPACE              TO ERROR-TEXT-STR                        
083000         STRING 'RC FROM MQSETMP (4)' RC-DISPLAY                          
083100                ' REASON: ' REASON-DISPLAY                                
083200            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
083300         DISPLAY IDPGM ' ' ERROR-TEXT                                     
083400         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
083500       END-IF                                                             
083600     END-IF                                                               
083700                                                                          
083800     IF MQ-VCOM-DELIMITER NOT = SPACE                                     
083900       SET MQCHARV-VSPTR         TO ADDRESS OF MQ-PROPNAME-TO-SET         
084000       MOVE 'BASELINE_VCOM_Adapter_Delimiter'                             
084100                                 TO MQ-PROPNAME-TO-SET                    
084200       MOVE 31                   TO MQCHARV-VSLENGTH                      
084300                                                                          
084400       MOVE SWEDISH-EBCDIC       TO MQCHARV-VSCCSID                       
084500       MOVE MQTYPE-STRING        TO MQ-PROPTYPE                           
084600                                                                          
084700       MOVE MQ-VCOM-DELIMITER    TO MQ-PROPVALUE                          
084800                                                                          
084900       MOVE ZERO                 TO MQ-PROPVALUELENGTH                    
085000       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
085100               FOR CHARACTERS BEFORE INITIAL SPACE                        
085200                                                                          
085300       CALL  MQSETMP          USING MQ-HCONN                              
085400                                    MQ-PROPHANDLE                         
085500                                    MQ-SET-PROP-OPTIONS                   
085600                                    MQ-PROPNAME                           
085700                                    MQ-PROPDESC                           
085800                                    MQ-PROPTYPE                           
085900                                    MQ-PROPVALUELENGTH                    
086000                                    MQ-PROPVALUE                          
086100                                    MQ-COMPCODE                           
086200                                    MQ-REASON                             
086300                                                                          
086400       IF MQ-COMPCODE NOT = MQCC-OK                                       
086500         MOVE MQ-COMPCODE        TO RC-DISPLAY                            
086600         MOVE MQ-REASON          TO REASON-DISPLAY                        
086700         MOVE SPACE              TO ERROR-TEXT-STR                        
086800         STRING 'RC FROM MQSETMP (5)' RC-DISPLAY                          
086900                ' REASON: ' REASON-DISPLAY                                
087000            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
087100         DISPLAY IDPGM ' ' ERROR-TEXT                                     
087200         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
087300       END-IF                                                             
087400     END-IF                                                               
087500                                                                          
087600     IF MQ-SELECTOR NOT = SPACE                                           
087700       SET MQCHARV-VSPTR         TO ADDRESS OF MQ-PROPNAME-TO-SET         
087800       UNSTRING MQ-SELECTOR DELIMITED BY '.'                              
087900         INTO MQ-PROPNAME-TO-SET                                          
088000              MQ-PROPVALUE                                                
088100       COMPUTE MQCHARV-VSLENGTH =                                         
088200         FUNCTION LENGTH (FUNCTION TRIM(MQ-PROPNAME-TO-SET))              
088300                                                                          
088400       MOVE SWEDISH-EBCDIC       TO MQCHARV-VSCCSID                       
088500       MOVE MQTYPE-STRING        TO MQ-PROPTYPE                           
088600                                                                          
088700       MOVE ZERO                 TO MQ-PROPVALUELENGTH                    
088800       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
088900               FOR CHARACTERS BEFORE INITIAL SPACE                        
089000                                                                          
089100       CALL  MQSETMP          USING MQ-HCONN                              
089200                                    MQ-PROPHANDLE                         
089300                                    MQ-SET-PROP-OPTIONS                   
089400                                    MQ-PROPNAME                           
089500                                    MQ-PROPDESC                           
089600                                    MQ-PROPTYPE                           
089700                                    MQ-PROPVALUELENGTH                    
089800                                    MQ-PROPVALUE                          
089900                                    MQ-COMPCODE                           
090000                                    MQ-REASON                             
090100                                                                          
090200       IF MQ-COMPCODE NOT = MQCC-OK                                       
090300         MOVE MQ-COMPCODE        TO RC-DISPLAY                            
090400         MOVE MQ-REASON          TO REASON-DISPLAY                        
090500         MOVE SPACE              TO ERROR-TEXT-STR                        
090600         STRING 'RC FROM MQSETMP (4)' RC-DISPLAY                          
090700                ' REASON: ' REASON-DISPLAY                                
090800            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
090900         DISPLAY IDPGM ' ' ERROR-TEXT                                     
091000         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
091100       END-IF                                                             
091200     END-IF                                                               
091300     DISPLAY 'SET PROPERTIES SUCCESSFUL'                                  
091400     .                                                                    
091500 S04-MQ-PUT  SECTION.                                                     
091600*    -- PUT THE MESSAGE                                                   
091700                                                                          
091800     COMPUTE MQPMO-OPTIONS        = MQPMO-FAIL-IF-QUIESCING               
091900                                                                          
092000     MOVE MQMI-NONE              TO MQMD-MSGID                            
092100     MOVE MQCI-NONE              TO MQMD-CORRELID                         
092200     MOVE MQFMT-STRING           TO MQMD-FORMAT                           
092300     MOVE SWEDISH-EBCDIC         TO MQMD-CODEDCHARSETID                   
092400     IF MQ-MSGTYPE = '1'                                                  
092500       MOVE MQMT-REQUEST         TO MQMD-MSGTYPE                          
092600     END-IF                                                               
092700                                                                          
092800     MOVE MQPMO-VERSION-3        TO MQPMO-VERSION                         
092900     MOVE MQ-PROPHANDLE          TO MQPMO-ORIGINALMSGHANDLE               
093000     MOVE MQACTP-NEW             TO MQPMO-ACTION                          
093100                                                                          
093200     CALL  MQPUT              USING MQ-HCONN                              
093300                                    MQ-HOBJ                               
093400                                    MQMD                                  
093500                                    MQPMO                                 
093600                                    WLEN                                  
093700                                    WDATA                                 
093800                                    MQ-COMPCODE                           
093900                                    MQ-REASON                             
094000                                                                          
094100     IF MQ-COMPCODE NOT = MQCC-OK                                         
094200       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
094300       MOVE MQ-REASON            TO REASON-DISPLAY                        
094400       MOVE SPACE                TO ERROR-TEXT-STR                        
094500       STRING 'RC FROM MQPUT '   RC-DISPLAY                               
094600              ' REASON: ' REASON-DISPLAY                                  
094700          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
094800       DISPLAY IDPGM ' ' ERROR-TEXT                                       
094900       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
095000     END-IF                                                               
095100     DISPLAY 'MQPUT   SUCCESSFUL   '                                      
095200     .                                                                    
095300                                                                          
095400 S04-MQ-PUT-GROUP  SECTION.                                               
095500*    -- PUT THE MESSAGE                                                   
095600                                                                          
095700     COMPUTE MQPMO-OPTIONS        = MQPMO-FAIL-IF-QUIESCING               
095800                                  + MQPMO-LOGICAL-ORDER                   
095900                                  + MQPMO-NEW-MSG-ID                      
096000                                                                          
096100     MOVE MQMI-NONE              TO MQMD-MSGID                            
096200     MOVE MQCI-NONE              TO MQMD-CORRELID                         
096300     MOVE MQFMT-STRING           TO MQMD-FORMAT                           
096400     MOVE SWEDISH-EBCDIC         TO MQMD-CODEDCHARSETID                   
096500     IF MQ-MSGTYPE = '1'                                                  
096600       MOVE MQMT-REQUEST         TO MQMD-MSGTYPE                          
096700     END-IF                                                               
096800                                                                          
096900     MOVE MQPMO-VERSION-3        TO MQPMO-VERSION                         
097000     MOVE MQ-PROPHANDLE          TO MQPMO-ORIGINALMSGHANDLE               
097100     MOVE MQACTP-NEW             TO MQPMO-ACTION                          
097200                                                                          
097300     EVALUATE TRUE                                                        
097400       WHEN FIRST-MSG-IN-GRP                                              
097500         MOVE 1                  TO MQMD-MSGSEQNUMBER                     
097600         MOVE MQMF-MSG-IN-GROUP  TO MQMD-MSGFLAGS                         
097700         SET MSG-IN-GRP          TO TRUE                                  
097800       WHEN LAST-MSG-IN-GRP                                               
097900         ADD 1                   TO MQMD-MSGSEQNUMBER                     
098000         MOVE MQMF-LAST-MSG-IN-GROUP                                      
098100                                 TO MQMD-MSGFLAGS                         
098200       WHEN OTHER                                                         
098300         ADD 1                   TO MQMD-MSGSEQNUMBER                     
098400         MOVE MQMF-MSG-IN-GROUP  TO MQMD-MSGFLAGS                         
098500     END-EVALUATE                                                         
098600                                                                          
098700     CALL  MQPUT              USING MQ-HCONN                              
098800                                    MQ-HOBJ                               
098900                                    MQMD                                  
099000                                    MQPMO                                 
099100                                    WLEN                                  
099200                                    WDATA                                 
099300                                    MQ-COMPCODE                           
099400                                    MQ-REASON                             
099500                                                                          
099600     IF MQ-COMPCODE NOT = MQCC-OK                                         
099700       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
099800       MOVE MQ-REASON            TO REASON-DISPLAY                        
099900       MOVE SPACE                TO ERROR-TEXT-STR                        
100000       STRING 'RC FROM MQPUT GROUP '   RC-DISPLAY                         
100100              ' REASON: ' REASON-DISPLAY                                  
100200          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
100300       DISPLAY IDPGM ' ' ERROR-TEXT                                       
100400       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
100500     END-IF                                                               
100600     DISPLAY 'MQPUT   SUCCESSFUL   '                                      
100700     .                                                                    
100800                                                                          
100900 S04-MQ-CLOSE  SECTION.                                                   
101000*    -- PUT THE LAST MESSAGE TO QUEUE                                     
101100                                                                          
101200     IF MSG-GRP-VALID                                                     
101300       SET LAST-MSG-IN-GRP       TO TRUE                                  
101400       PERFORM S04-MQ-PUT-GROUP                                           
101500     ELSE                                                                 
101600       PERFORM S04-MQ-PUT                                                 
101700     END-IF                                                               
101800                                                                          
101900                                                                          
102000*    -- FINALLY, CLOSE THE QUEUE                                          
102100     CALL  MQCLOSE            USING MQ-HCONN                              
102200                                    MQ-HOBJ                               
102300                                    MQCO-NONE                             
102400                                    MQ-COMPCODE                           
102500                                    MQ-REASON                             
102600                                                                          
102700     IF MQ-COMPCODE NOT = MQCC-OK                                         
102800       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
102900       MOVE MQ-REASON            TO REASON-DISPLAY                        
103000       MOVE SPACE                TO ERROR-TEXT-STR                        
103100       STRING 'RC FROM MQCLOSE ' RC-DISPLAY                               
103200              ' REASON: ' REASON-DISPLAY                                  
103300          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
103400       DISPLAY IDPGM ' ' ERROR-TEXT                                       
103500       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
103600     END-IF                                                               
103700                                                                          
103800     DISPLAY 'MQCLOSE SUCCESSFUL   '                                      
103900     .                                                                    
104000 S04-MQ-DISC  SECTION.                                                    
104100                                                                          
104200*    -- BREAK CONTACT WITH MQ QUEUE MANAGER                               
104300                                                                          
104400     CALL  MQDISC             USING MQ-HCONN                              
104500                                    MQ-COMPCODE                           
104600                                    MQ-REASON                             
104700                                                                          
104800     IF MQ-COMPCODE NOT = MQCC-OK                                         
104900       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
105000       MOVE MQ-REASON            TO REASON-DISPLAY                        
105100       MOVE SPACE                TO ERROR-TEXT-STR                        
105200       STRING 'RC FROM MQDISC '  RC-DISPLAY                               
105300              ' REASON: ' REASON-DISPLAY                                  
105400          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
105500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
105600       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
105700     END-IF                                                               
105800                                                                          
105900     DISPLAY 'MQDISC SUCCESSFUL    '                                      
106000     .                                                                    
