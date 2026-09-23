000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ111200.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   19/05/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        PROGRAM FOR RECEIVING FILE FROM MQ                               
001000*                                                                         
001100*    ABENDCODES:                                                          
001200*        U0016 -  IF ADDRESS IS MISSING IN WZ01ATAB                       
001300*                                                                         
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200 FILE SECTION.                                                            
002300                                                                          
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600 77  IDPGM                       PIC X(8)    VALUE 'WZ111200'.            
002700 77  YES                         PIC X       VALUE 'J'.                   
002800 77  NOO                         PIC X       VALUE 'N'.                   
002900 77  WS-CR                       PIC X(2)    VALUE 'CR'.                  
003000 77  WS-LF                       PIC X(2)    VALUE 'LF'.                  
003100 77  CR                          PIC X       VALUE X'0D'.                 
003200 77  LF-EBCDIC                   PIC X       VALUE X'25'.                 
003300 77  LF-ASCII                    PIC X       VALUE X'0A'.                 
003400 77  CRLF-EBCDIC                 PIC X(2)    VALUE X'0D25'.               
003500 77  CRLF-ASCII                  PIC X(2)    VALUE X'0D0A'.               
003600 77  SWEDISH-EBCDIC              PIC S9(9)   VALUE 278  BINARY.           
003700                                                                          
003800*    -- "OUTFILE" CORRESPONDS TO AN FD NAME FOR NORMAL FILES              
003900 01  OUTFILE                     PIC S9(9)   COMP.                        
004000                                                                          
004100 01  W-RECORD-COUNT              PIC S9(9)   COMP-3 VALUE ZERO.           
004200                                                                          
004300*    -- CONTROL PARAMETERS READ VIA PARM                                  
004400 01  WS-ADDISPABS                PIC X(50)   VALUE SPACE.                 
004500 01  WS-FILE-PATTERN             PIC X(44)   VALUE SPACE.                 
004600 01  WS-RECFM                    PIC XX      VALUE SPACE.                 
004700 01  LL                          PIC 9(4)    VALUE ZERO BINARY.           
004800 01  WS-LEN                      PIC X(5)    VALUE SPACE.                 
004900 01  WS-LRECL-NUM                PIC 9(5)    VALUE ZERO BINARY.           
005000 01  WS-HEADER-LEN               PIC 9(4)    VALUE ZERO BINARY.           
005100 01  WS-HEADER-VAL               PIC X(40)   VALUE SPACES.                
005200 01  WS-HEADER-RECORD            PIC X(100)  VALUE SPACE.                 
005300 01  WS-QMGR                     PIC X(4)    VALUE SPACE.                 
005400                                                                          
005500 77  SW-MQ-CONV                  PIC X       VALUE 'Y'.                   
005600     88 MSG-CONV-NO                          VALUE 'N'.                   
005700     88 MSG-CONV-YES                         VALUE 'Y'.                   
005800                                                                          
005900 77  SW-DELIM                    PIC X       VALUE 'Y'.                   
006000     88 DELIM-NO                             VALUE 'N'.                   
006100     88 DELIM-YES                            VALUE 'Y'.                   
006200                                                                          
006300*    -- WORK AREAS FOR CONSTRUCTING THE OUTPUT DSNAME                     
006400 01  W-PART1                     PIC X(40)   VALUE SPACES.                
006500 01  W-PART2                     PIC X(40)   VALUE SPACES.                
006600 01  W-FILEMARKER                PIC X(40)   VALUE SPACES.                
006700                                                                          
006800*    -- FILE NAME CONSTRUCTED FROM FILE PATTERN AND FILE MARKER           
006900 01  OUTPUT-FILE-NAME            PIC X(44).                               
007000                                                                          
007100 01  DYN-PARM.                                                            
007200     03  DYN-INIT                PIC X       VALUE 'I'.                   
007300     03  DYN-OPEN                PIC X       VALUE 'O'.                   
007400     03  DYN-WRITE               PIC X       VALUE ' '.                   
007500     03  DYN-CLOSE               PIC X       VALUE 'C'.                   
007600                                                                          
007700 01   -COPY WDYNAREA                                                      
007800                                                                          
007900 01  RC-DISPLAY                  PIC Z(8)9.                               
008000 01  REASON-DISPLAY              PIC Z(8)9.                               
008100                                                                          
008200 01  SMALL-LETTERS              PIC X(31)    VALUE                        
008300     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
008400 01  CAPS-LETTERS               PIC X(31)    VALUE                        
008500     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
008600                                                                          
008700 01  MQ-CALLS.                                                            
008800*                                                                         
008900     03  MQCONN                  PIC X(8)    VALUE 'CSQBCONN'.            
009000     03  MQDISC                  PIC X(8)    VALUE 'CSQBDISC'.            
009100     03  MQOPEN                  PIC X(8)    VALUE 'CSQBOPEN'.            
009200     03  MQCLOSE                 PIC X(8)    VALUE 'CSQBCLOS'.            
009300     03  MQGET                   PIC X(8)    VALUE 'CSQBGET '.            
009400     03  MQCRTMH                 PIC X(8)    VALUE 'CSQBCTMH'.            
009500     03  MQINQMP                 PIC X(8)    VALUE 'CSQBIQMP'.            
009600                                                                          
009700 01  GENERAL-SUBPROGRAMS.                                                 
009800*                                                                         
009900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010000     03  WZ01ATAB                PIC X(8)    VALUE 'WZ01ATAB'.            
010100     03  WDYNALC                 PIC X(8)    VALUE 'WDYNALC'.             
010200     03  WFILWRT                 PIC X(8)    VALUE 'WFILWRT'.             
010300                                                                          
010400*    --- PARAMETERS TO ABEND                                              
010500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010800                                                                          
010900 01  ERROR-TEXT.                                                          
011000     03  FILLER                  PIC X(12)  VALUE 'ERROR-TEXT: '.         
011100     03  ERROR-TEXT-STR          PIC X(80)  VALUE SPACE.                  
011200                                                                          
011300 01  DISPLAY-TEXT                PIC X(80)  VALUE SPACE.                  
011400                                                                          
011500*    -- FIELDS USED WHEN SPLITTING THE DATA BASED ON CR/LF                
011600 01  WLEN                        PIC S9(11)  BINARY VALUE ZERO.           
011700 01  WSTART                      PIC S9(11)  BINARY VALUE ZERO.           
011800                                                                          
011900 01  WDATA                       PIC X(104857600) VALUE SPACE.            
012000 01  LINE-AREA                   PIC X(9999) VALUE SPACE.                 
012100 01  WLEN-LINE                   PIC 9(11)   VALUE ZERO.                  
012200                                                                          
012300*    -- PARAMETERS TO WZ01ATAB                                            
012400 01  -COPY WZ01ATAB                                                       
012500                                                                          
012600 01  MQ-AREA-START               PIC X(24)   VALUE  'MQ-AREA'.            
012700                                                                          
012800 01  MQ-QMGR                     PIC X(48)   VALUE SPACE.                 
012900 01  MQ-QNAME                    PIC X(48)   VALUE SPACE.                 
013000 01  MQ-COMPCODE                 PIC S9(9)   BINARY.                      
013100 01  MQ-REASON                   PIC S9(9)   BINARY.                      
013200 01  MQ-HCONN                    PIC S9(9)   BINARY VALUE ZERO.           
013300 01  MQ-OPENOPTIONS              PIC S9(9)   BINARY VALUE ZERO.           
013400 01  MQ-HOBJ                     PIC S9(9)   BINARY VALUE ZERO.           
013500 01  MQ-MSGLENGTH                PIC S9(9)   BINARY.                      
013600 01  MQ-DATALENGTH               PIC S9(9)   BINARY.                      
013700                                                                          
013800 01  MQ-INTEGRATION-ID           PIC X(100)  VALUE SPACE.                 
013900 01  MQ-CONTRACT-ID              PIC X(100)  VALUE SPACE.                 
014000 01  MQ-TOPIC-STRING             PIC X(100)  VALUE SPACE.                 
014100 01  MQ-RECORD-DELIMITER         PIC X(100)  VALUE SPACE.                 
014200 01  MQ-TYPE                     PIC X(100)  VALUE SPACE.                 
014300 01  MQ-VCOM-DELIMITER           PIC X(100)  VALUE SPACE.                 
014400 01  MQ-FILE-MARKER              PIC X(100)  VALUE SPACE.                 
014500 01  MQ-HEADER-NAME              PIC X(100)  VALUE SPACE.                 
014600 01  MQ-PROPHANDLE               PIC S9(18)  BINARY.                      
014700 01  MQ-PROPNAME-TO-SET          PIC X(50)   VALUE SPACE.                 
014800 01  MQ-PROPTYPE                 PIC S9(9)   BINARY.                      
014900 01  MQ-PROPVALUELENGTH          PIC S9(9)   BINARY.                      
015000 01  MQ-PROPVALUE                PIC X(100)  VALUE SPACE.                 
015100 01  MQ-PROPDATALENGTH           PIC S9(9)   BINARY.                      
015200 01  MQ-PROPNAME-TO-RETRIEVE     PIC X(100)  VALUE SPACE.                 
015300 01  MQ-PROPNAME-RETRIEVED       PIC X(100)  VALUE SPACE.                 
015400                                                                          
015500 01  MQ-MESSAGE-DESCRIPTOR.                                               
015600*    -COPY CMQMD2V                                                        
015700                                                                          
015800 01  MQ-GET-MESSAGE-OPTIONS.                                              
015900*    -COPY CMQGMOV                                                        
016000                                                                          
016100 01  MQ-OBJECT-DESCRIPTOR.                                                
016200*    -COPY CMQODV                                                         
016300                                                                          
016400 01  MQ-CONSTANTS.                                                        
016500*    -COPY CMQV                                                           
016600                                                                          
016700 01  MQ-CREATE-HANDLE-OPTIONS.                                            
016800*    -COPY CMQCMHOV                                                       
016900                                                                          
017000 01  MQ-INQ-PROP-OPTIONS.                                                 
017100*    -COPY CMQIMPOV                                                       
017200                                                                          
017300 01  MQ-PROPNAME.                                                         
017400*    -COPY CMQCHRVV                                                       
017500                                                                          
017600 01  MQ-PROPDESC.                                                         
017700*    -COPY CMQPDV                                                         
017800                                                                          
017900 01  UT-AREA-START               PIC X(16)   VALUE  'UT-AREA'.            
018000 01  UT-RECLEN                   PIC  9(4)   VALUE ZERO.                  
018100 01  UT-AREA                     PIC X(9999) VALUE SPACE.                 
018200 01  MAX-RESULT-LEN              PIC  9(5)   VALUE ZERO.                  
018300                                                                          
018400 LINKAGE SECTION.                                                         
018500                                                                          
018600 01  PARM-AREA.                                                           
018700     03  PARM-LENGTH             PIC S9(4)   BINARY.                      
018800     03  PARM-RTENV              PIC X(4).                                
018900     03  PARM-DATA               PIC X(96).                               
019000                                                                          
019100 PROCEDURE DIVISION USING PARM-AREA.                                      
019200 MAIN SECTION.                                                            
019300                                                                          
019400     PERFORM A-INIT                                                       
019500                                                                          
019600     PERFORM S04-MQ-CONN-OPEN                                             
019700     PERFORM S04-CREATE-MESSAGE-HANDLE                                    
019800     PERFORM S04-MQ-GET                                                   
019900     PERFORM S04-MQ-INQ-HEADERS                                           
020000     PERFORM S01-ALLOC-OPEN-OUTPUT-FILE                                   
020100                                                                          
020200     PERFORM UNTIL WSTART > WLEN                                          
020300       PERFORM B-PROCESS-MESSAGE                                          
020400       PERFORM S01-WRITE-OUTPUT-RECORD                                    
020500     END-PERFORM                                                          
020600                                                                          
020700     PERFORM S01-CLOSE-OUTPUT-FILE                                        
020800     PERFORM S04-MQ-CLOSE                                                 
020900     PERFORM S04-MQ-DISC                                                  
021000     PERFORM Z-FINIT                                                      
021100                                                                          
021200     MOVE ZERO                   TO RETURN-CODE                           
021300     GOBACK                                                               
021400     .                                                                    
021500 A-INIT SECTION.                                                          
021600                                                                          
021700* -- OUTPUT FILE IS OPENED VIA WDYNALC                                    
021800                                                                          
021900     DISPLAY 'PARM-LENGTH        : ' PARM-LENGTH                          
022000     IF PARM-LENGTH > 4                                                   
022100       PERFORM AA-EXTRACT-PARM                                            
022200     ELSE                                                                 
022300       MOVE 'ADDRESS,FILE PARAMETERS MISSING'                             
022400                                 TO ERROR-TEXT-STR                        
022500       PERFORM S99-ABEND                                                  
022600     END-IF                                                               
022700                                                                          
022800     MOVE WS-ADDISPABS           TO ATAB-ADDISPABS                        
022900                                                                          
023000     INSPECT ATAB-ADDISPABS CONVERTING                                    
023100             SMALL-LETTERS TO CAPS-LETTERS                                
023200     CALL WZ01ATAB            USING ATAB-WZ01ATAB                         
023300                                                                          
023400     IF RETURN-CODE > ZERO                                                
023500*      -- ABSTRACT ADDRESS NOT FOUND IN ATAB                              
023600       MOVE SPACE                TO ERROR-TEXT-STR                        
023700       STRING ATAB-ADDISPABS DELIMITED BY SPACE                           
023800              ' NOT FOUND IN WZ01ATAB' DELIMITED BY SIZE                  
023900                               INTO ERROR-TEXT-STR                        
024000       PERFORM S99-ABEND                                                  
024100     ELSE                                                                 
024200       PERFORM AB-EXTRACT-MQ-PARAMETERS                                   
024300     END-IF                                                               
024400     .                                                                    
024500                                                                          
024600 AA-EXTRACT-PARM SECTION.                                                 
024700                                                                          
024800     MOVE 0                      TO LL                                    
024900     UNSTRING PARM-DATA DELIMITED BY ALL ';'                              
025000         INTO WS-ADDISPABS                                                
025100              WS-FILE-PATTERN                                             
025200              WS-RECFM                                                    
025300              WS-LEN   COUNT IN LL                                        
025400              WS-QMGR                                                     
025500                                                                          
025600     IF WS-RECFM NOT = 'FB' AND NOT = 'VB'                                
025700       MOVE 'OUTPUT FILE RECORD FORMAT MUST BE FB OR VB'                  
025800                                 TO ERROR-TEXT-STR                        
025900       PERFORM S99-ABEND                                                  
026000     END-IF                                                               
026100                                                                          
026200     IF LL = 0 OR WS-LEN (1:LL) NOT NUMERIC                               
026300       MOVE 'OUTPUT FILE RECORD LENGTH MISSING OR NOT NUMERIC'            
026400                                 TO ERROR-TEXT-STR                        
026500       PERFORM S99-ABEND                                                  
026600     ELSE                                                                 
026700       MOVE WS-LEN (1:LL)        TO WS-LRECL-NUM                          
026800                                    MAX-RESULT-LEN                        
026900       IF WS-RECFM = 'VB'                                                 
027000         COMPUTE MAX-RESULT-LEN   = MAX-RESULT-LEN - 4                    
027100       END-IF                                                             
027200       MOVE MAX-RESULT-LEN       TO WLEN-LINE                             
027300     END-IF                                                               
027400                                                                          
027500     DISPLAY '************* PARM DATA **************'                     
027600     DISPLAY 'ENVIRONMENT        : ' PARM-RTENV                           
027700     DISPLAY 'ADDRESS            : ' WS-ADDISPABS                         
027800     DISPLAY 'FILE PATTERN       : ' WS-FILE-PATTERN                      
027900     DISPLAY 'RECFM              : ' WS-RECFM                             
028000     DISPLAY 'LRECL              : ' WS-LRECL-NUM                         
028100     DISPLAY 'QMGR               : ' WS-QMGR                              
028200     DISPLAY '********** END OF PARM DATA **********'                     
028300                                                                          
028400     .                                                                    
028500                                                                          
028600 AB-EXTRACT-MQ-PARAMETERS SECTION.                                        
028700                                                                          
028800*    -- QMGR NAME IS DETERMINED FROM ENVIRONMENT SPECIFIED                
028900*    -- ON EXEC-CARD PARM                                                 
029000                                                                          
029100     IF PARM-RTENV  = 'QASE'                                              
029200       MOVE 'MC0P'               TO MQ-QMGR                               
029300     ELSE                                                                 
029400       IF PARM-RTENV = 'ACPT'                                             
029500         MOVE 'MC0Q'             TO MQ-QMGR                               
029600       ELSE                                                               
029700         MOVE 'MC0T'             TO MQ-QMGR                               
029800       END-IF                                                             
029900     END-IF                                                               
030000                                                                          
030100*    -- SEE IF DEFAULT QMGR NEEDS TO BE OVERRIDED WITH THE ONE            
030200*    -- IN PARM                                                           
030300                                                                          
030400     IF WS-QMGR = 'NULL'                                                  
030500       CONTINUE                                                           
030600     ELSE                                                                 
030700       MOVE WS-QMGR              TO MQ-QMGR                               
030800     END-IF                                                               
030900                                                                          
031000     DISPLAY 'QUEUE MANAGER      : ' MQ-QMGR                              
031100*    -- QUEUE NAME                                                        
031200     MOVE ZERO                   TO TALLY                                 
031300     MOVE SPACE                  TO MQ-QNAME                              
031400     INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                           
031500             FOR CHARACTERS BEFORE INITIAL 'LQ:'                          
031600     IF TALLY < LENGTH OF ATAB-ADDISPINT-RECV                             
031700       UNSTRING ATAB-ADDISPINT-RECV(TALLY + 4:)                           
031800         DELIMITED BY ';'                                                 
031900                               INTO MQ-QNAME                              
032000     ELSE                                                                 
032100       MOVE SPACE                TO ERROR-TEXT-STR                        
032200       MOVE 'LOCAL QUEUE NAME IS MISSING IN WZ01ATAB'                     
032300                                 TO ERROR-TEXT-STR                        
032400       PERFORM S99-ABEND                                                  
032500     END-IF                                                               
032600     DISPLAY 'QUEUE NAME         : ' MQ-QNAME                             
032700                                                                          
032800*    -- META DATA: MESSAGE CONVERSION                                     
032900     MOVE ZERO                   TO TALLY                                 
033000     MOVE 'Y'                    TO SW-MQ-CONV                            
033100     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
033200             FOR CHARACTERS BEFORE INITIAL 'CONV:'                        
033300     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
033400       UNSTRING ATAB-ADDISPMETA(TALLY + 6:)  DELIMITED BY ';'             
033500                               INTO SW-MQ-CONV                            
033600     END-IF                                                               
033700     DISPLAY IDPGM ' CONVERSION         : ' SW-MQ-CONV                    
033800                                                                          
033900*      -- META DATA: RECORD DELIMITER                                     
034000*    When receiving files, it's only important to see if there is         
034100*    a delimiter or not. If there is no delim, we should not split        
034200*    the message. Instead entire message is written onto file in          
034300*    one or multiple records, based on lrecl of file.                     
034400*    Program that processes the file need to handle it accordingly        
034500*    Typical when we are receving xml files where there need not          
034600*    be line delimiters.                                                  
034700     MOVE ZERO                   TO TALLY                                 
034800     INITIALIZE MQ-RECORD-DELIMITER                                       
034900     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
035000             FOR CHARACTERS BEFORE INITIAL 'D:'                           
035100     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
035200       UNSTRING ATAB-ADDISPMETA(TALLY + 3:) DELIMITED BY ';'              
035300                               INTO MQ-RECORD-DELIMITER                   
035400       DISPLAY IDPGM ' RECORD DELIMITER   : '                             
035500               MQ-RECORD-DELIMITER                                        
035600     END-IF                                                               
035700     IF MQ-RECORD-DELIMITER = 'NONE'                                      
035800       SET DELIM-NO              TO TRUE                                  
035900     ELSE                                                                 
036000       SET DELIM-YES             TO TRUE                                  
036100     END-IF                                                               
036200                                                                          
036300*    -- META DATA: FILE MARKER                                            
036400     MOVE ZERO                   TO TALLY                                 
036500     MOVE SPACE                  TO MQ-FILE-MARKER                        
036600     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
036700             FOR CHARACTERS BEFORE INITIAL 'FM:'                          
036800     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
036900       UNSTRING ATAB-ADDISPMETA(TALLY + 4:)  DELIMITED BY ';'             
037000                               INTO MQ-FILE-MARKER                        
037100     END-IF                                                               
037200     DISPLAY 'FILE MARKER        : ' MQ-FILE-MARKER                       
037300                                                                          
037400*    -- META DATA: HEADER RECORD                                          
037500     MOVE ZERO                   TO TALLY                                 
037600     MOVE SPACES                 TO WS-HEADER-RECORD                      
037700     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
037800             FOR CHARACTERS BEFORE INITIAL 'HR:'                          
037900     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
038000       UNSTRING ATAB-ADDISPMETA(TALLY + 4:)  DELIMITED BY ';'             
038100                               INTO WS-HEADER-RECORD                      
038200     END-IF                                                               
038300     DISPLAY 'HEADER RECORD      : ' WS-HEADER-RECORD                     
038400                                                                          
038500     MOVE 0                      TO LL                                    
038600     UNSTRING WS-HEADER-RECORD DELIMITED BY ':' OR ALL SPACE              
038700         INTO MQ-HEADER-NAME                                              
038800              WS-LEN    COUNT IN LL                                       
038900                                                                          
039000     IF WS-HEADER-RECORD > SPACES                                         
039100       IF LL = 0 OR WS-LEN(1:LL) NOT NUMERIC                              
039200                                                                          
039300         MOVE 'LENGTH FOR HEADER RECORD MISSING'                          
039400                                 TO ERROR-TEXT-STR                        
039500         PERFORM S99-ABEND                                                
039600       ELSE                                                               
039700         MOVE WS-LEN (1:LL)      TO WS-HEADER-LEN                         
039800       END-IF                                                             
039900     END-IF                                                               
040000     .                                                                    
040100                                                                          
040200 B-PROCESS-MESSAGE SECTION.                                               
040300                                                                          
040400     IF MSG-CONV-YES AND DELIM-YES                                        
040500       PERFORM BA-NORMAL-MESSAGE                                          
040600     ELSE                                                                 
040700       PERFORM BB-BINARY-OR-NODELIM-MESSAGE                               
040800     END-IF                                                               
040900     .                                                                    
041000                                                                          
041100 BA-NORMAL-MESSAGE SECTION.                                               
041200                                                                          
041300     INITIALIZE LINE-AREA                                                 
041400                WLEN-LINE                                                 
041500                                                                          
041600     UNSTRING WDATA (1:WLEN)                                              
041700       DELIMITED BY CRLF-EBCDIC OR                                        
041800                    CRLF-ASCII  OR                                        
041900                    CR          OR                                        
042000                    LF-EBCDIC   OR                                        
042100                    LF-ASCII                                              
042200       INTO LINE-AREA                                                     
042300       COUNT IN WLEN-LINE                                                 
042400       WITH POINTER WSTART                                                
042500                                                                          
042600     IF (WLEN-LINE + WS-HEADER-LEN) > MAX-RESULT-LEN                      
042700       MOVE SPACE                TO ERROR-TEXT-STR                        
042800       STRING 'DATA TRUNCATION. RECEIVED LENGTH > ' WLEN-LINE             
042900              '  MAX ALLOWED = ' MAX-RESULT-LEN                           
043000              DELIMITED BY SIZE                                           
043100                               INTO ERROR-TEXT-STR                        
043200       PERFORM S99-ABEND                                                  
043300     END-IF                                                               
043400                                                                          
043500     IF WLEN-LINE > 0                                                     
043600       INITIALIZE UT-AREA                                                 
043700       COMPUTE UT-RECLEN = WLEN-LINE + WS-HEADER-LEN                      
043800       IF WS-HEADER-LEN > 0                                               
043900         STRING WS-HEADER-VAL (1:WS-HEADER-LEN) DELIMITED BY SIZE         
044000                LINE-AREA (1:WLEN-LINE)         DELIMITED BY SIZE         
044100                               INTO UT-AREA                               
044200       ELSE                                                               
044300         MOVE LINE-AREA (1:WLEN-LINE)                                     
044400                                 TO UT-AREA                               
044500       END-IF                                                             
044600     END-IF                                                               
044700                                                                          
044800     .                                                                    
044900                                                                          
045000 BB-BINARY-OR-NODELIM-MESSAGE SECTION.                                    
045100                                                                          
045200     IF WSTART + WLEN-LINE > WLEN + 1                                     
045300       COMPUTE WLEN-LINE = WLEN - WSTART + 1                              
045400     END-IF                                                               
045500                                                                          
045600     COMPUTE UT-RECLEN = WLEN-LINE                                        
045700     MOVE WDATA (WSTART:WLEN-LINE)                                        
045800                                 TO UT-AREA                               
045900     ADD WLEN-LINE               TO WSTART                                
046000     .                                                                    
046100                                                                          
046200 Z-FINIT SECTION.                                                         
046300                                                                          
046400     CONTINUE                                                             
046500     .                                                                    
046600                                                                          
046700 S01-ALLOC-OPEN-OUTPUT-FILE SECTION.                                      
046800                                                                          
046900*--  INITIERA DCB I WFILWRT SÅ PROGRAMMET VET                             
047000*--  VILKEN FIL DEN SKA ÖPPNA OCH SKRIVA SEDAN                            
047100     CALL WFILWRT             USING OUTFILE                               
047200                                    DYN-INIT                              
047300                                                                          
047400     IF DYN-INIT = 'F'                                                    
047500       MOVE 'INIT CALL TO WFILWRT FAILED'                                 
047600                                 TO ERROR-TEXT-STR                        
047700       PERFORM S99-ABEND                                                  
047800     END-IF                                                               
047900                                                                          
048000*--  BERÄKNA FILENS NAMN                                                  
048100     UNSTRING WS-FILE-PATTERN DELIMITED BY '*'                            
048200         INTO  W-PART1 W-PART2                                            
048300     MOVE SPACE                  TO OUTPUT-FILE-NAME                      
048400     STRING W-PART1 DELIMITED BY SPACE                                    
048500            W-FILEMARKER DELIMITED BY SPACE                               
048600            W-PART2 DELIMITED BY SPACE                                    
048700       INTO OUTPUT-FILE-NAME                                              
048800                                                                          
048900     MOVE FUNCTION UPPER-CASE (OUTPUT-FILE-NAME)                          
049000                                 TO OUTPUT-FILE-NAME                      
049100     DISPLAY 'ALLOCATING OUTPUT FILE: ' OUTPUT-FILE-NAME                  
049200                                                                          
049300     MOVE 'C'                    TO DYN-FREE                              
049400     MOVE OUTPUT-FILE-NAME       TO DYN-DSNAME                            
049500     MOVE '+1'                   TO DYN-GENMBR                            
049600     MOVE WS-RECFM               TO DYN-RECFM                             
049700     MOVE 'N'                    TO DYN-DISP1                             
049800     MOVE 'C'                    TO DYN-DISP2                             
049900     MOVE 'D'                    TO DYN-DISP3                             
050000     MOVE 'PSEB'                 TO DYN-DATACLASS                         
050100     MOVE 'NOBACKUP'             TO DYN-MGMCLASS                          
050200     MOVE WS-LRECL-NUM           TO DYN-LRECL                             
050300     MOVE +0                     TO DYN-BLKSIZE                           
050400     MOVE 'R'                    TO DYN-RLSE                              
050500                                                                          
050600     CALL WDYNALC             USING OUTFILE                               
050700                                    DYN-AREA                              
050800     IF DYN-KDSVAR-OK                                                     
050900       DISPLAY 'OUTPUT FILE ALLOCATED - OK'                               
051000     ELSE                                                                 
051100       MOVE 'OUTPUT FILE COULD NOT BE ALLOCATED'                          
051200                                 TO ERROR-TEXT-STR                        
051300       PERFORM S99-ABEND                                                  
051400     END-IF                                                               
051500                                                                          
051600*    -- ÖPPNA FILEN FÖR SKRIVNING                                         
051700     CALL WFILWRT             USING OUTFILE                               
051800                                    DYN-OPEN                              
051900                                                                          
052000     IF DYN-OPEN = 'F'                                                    
052100       MOVE SPACE                TO ERROR-TEXT-STR                        
052200       STRING 'OUTPUT FILE ' DELIMITED BY SIZE                            
052300              DYN-DSNAME     DELIMITED BY SPACE                           
052400              DYN-GENMBR     DELIMITED BY SIZE                            
052500              ' COULD NOT BE OPENED.' DELIMITED BY SIZE                   
052600                               INTO ERROR-TEXT-STR                        
052700       PERFORM S99-ABEND                                                  
052800     ELSE                                                                 
052900       DISPLAY 'OUTPUT FILE OPENED  OK'                                   
053000     END-IF                                                               
053100     .                                                                    
053200                                                                          
053300 S01-WRITE-OUTPUT-RECORD SECTION.                                         
053400                                                                          
053500     MOVE UT-RECLEN              TO DYN-LRECL                             
053600                                                                          
053700     CALL WFILWRT             USING OUTFILE                               
053800                                    DYN-WRITE                             
053900                                    DYN-RECFM                             
054000                                    DYN-LRECL                             
054100                                    UT-AREA                               
054200                                                                          
054300     IF DYN-WRITE  = 'F'                                                  
054400       MOVE  'ERROR WHEN WRITING DATA TO OUTPUT FILE'                     
054500                                 TO ERROR-TEXT-STR                        
054600       PERFORM S99-ABEND                                                  
054700     ELSE                                                                 
054800       ADD 1                     TO W-RECORD-COUNT                        
054900     END-IF                                                               
055000     .                                                                    
055100                                                                          
055200 S01-CLOSE-OUTPUT-FILE   SECTION.                                         
055300                                                                          
055400     CALL WFILWRT             USING OUTFILE                               
055500                                    DYN-CLOSE                             
055600     IF DYN-CLOSE = 'F'                                                   
055700       MOVE  'ERROR WHEN CLOSING OUTPUT FILE'                             
055800                                 TO ERROR-TEXT-STR                        
055900       PERFORM S99-ABEND                                                  
056000     ELSE                                                                 
056100       DISPLAY W-RECORD-COUNT ' RECORDS WRITTEN TO OUTPUT FILE'           
056200     END-IF                                                               
056300     .                                                                    
056400                                                                          
056500 S04-MQ-CONN-OPEN  SECTION.                                               
056600                                                                          
056700     CALL  MQCONN             USING MQ-QMGR                               
056800                                    MQ-HCONN                              
056900                                    MQ-COMPCODE                           
057000                                    MQ-REASON                             
057100                                                                          
057200     IF MQ-COMPCODE NOT = MQCC-OK AND                                     
057300        MQ-REASON   NOT = MQRC-ALREADY-CONNECTED                          
057400       MOVE SPACE                TO ERROR-TEXT-STR                        
057500       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
057600       MOVE MQ-REASON            TO REASON-DISPLAY                        
057700       STRING 'RC FROM MQCONN ' RC-DISPLAY                                
057800              ' REASON: ' REASON-DISPLAY                                  
057900              ' ' MQ-QMGR                                                 
058000         DELIMITED BY SIZE                                                
058100                               INTO ERROR-TEXT-STR                        
058200       PERFORM S99-ABEND                                                  
058300     END-IF                                                               
058400                                                                          
058500     DISPLAY 'MQCONN SUCCESSFUL    '                                      
058600                                                                          
058700     MOVE MQ-QNAME               TO MQOD-OBJECTNAME                       
058800                                                                          
058900     COMPUTE MQ-OPENOPTIONS = MQOO-FAIL-IF-QUIESCING +                    
059000                              MQOO-INPUT-SHARED                           
059100                                                                          
059200     CALL  MQOPEN             USING MQ-HCONN                              
059300                                    MQOD                                  
059400                                    MQ-OPENOPTIONS                        
059500                                    MQ-HOBJ                               
059600                                    MQ-COMPCODE                           
059700                                    MQ-REASON                             
059800                                                                          
059900     IF MQ-COMPCODE NOT = MQCC-OK                                         
060000       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
060100       MOVE MQ-REASON            TO REASON-DISPLAY                        
060200       MOVE SPACE                TO ERROR-TEXT-STR                        
060300       STRING 'RC FROM MQOPEN ' RC-DISPLAY                                
060400              ' REASON: ' REASON-DISPLAY                                  
060500              ' ' MQ-QNAME                                                
060600          DELIMITED BY SIZE                                               
060700                               INTO ERROR-TEXT-STR                        
060800       PERFORM S99-ABEND                                                  
060900     END-IF                                                               
061000     DISPLAY 'MQOPEN SUCCESSFUL    '                                      
061100*    -- INITIATE A NEW FRESH MESSAGE                                      
061200     INITIALIZE WLEN                                                      
061300                WDATA                                                     
061400     .                                                                    
061500                                                                          
061600 S04-CREATE-MESSAGE-HANDLE SECTION.                                       
061700*    -- CREATE A HANDLE FOR HEADER PROPERTIES                             
061800     MOVE MQCMHO-VALIDATE        TO MQCMHO-OPTIONS                        
061900     CALL MQCRTMH             USING MQ-HCONN                              
062000                                    MQ-CREATE-HANDLE-OPTIONS              
062100                                    MQ-PROPHANDLE                         
062200                                    MQ-COMPCODE                           
062300                                    MQ-REASON                             
062400                                                                          
062500     IF MQ-COMPCODE NOT = MQCC-OK                                         
062600       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
062700       MOVE MQ-REASON            TO REASON-DISPLAY                        
062800                                                                          
062900       MOVE SPACE                TO ERROR-TEXT-STR                        
063000       STRING 'RC FROM MQCRTMH' RC-DISPLAY                                
063100              ' REASON: ' REASON-DISPLAY                                  
063200              ' ' MQ-QNAME                                                
063300          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
063400                                                                          
063500       PERFORM S99-ABEND                                                  
063600     END-IF                                                               
063700     .                                                                    
063800                                                                          
063900 S04-MQ-GET  SECTION.                                                     
064000*    -- GET THE MESSAGE                                                   
064100                                                                          
064200     IF MSG-CONV-YES                                                      
064300       COMPUTE MQGMO-OPTIONS      = MQGMO-FAIL-IF-QUIESCING               
064400                                  + MQGMO-NO-WAIT                         
064500                                  + MQGMO-CONVERT                         
064600                                  + MQGMO-PROPERTIES-IN-HANDLE            
064700     ELSE                                                                 
064800       COMPUTE MQGMO-OPTIONS      = MQGMO-FAIL-IF-QUIESCING               
064900                                  + MQGMO-NO-WAIT                         
065000                                  + MQGMO-PROPERTIES-IN-HANDLE            
065100     END-IF                                                               
065200                                                                          
065300     MOVE MQ-PROPHANDLE          TO MQGMO-MSGHANDLE                       
065400                                                                          
065500     MOVE MQMI-NONE              TO MQMD-MSGID                            
065600     MOVE MQMI-NONE              TO MQMD-CORRELID                         
065700     MOVE MQFMT-STRING           TO MQMD-FORMAT                           
065800     MOVE MQENC-NATIVE           TO MQMD-ENCODING                         
065900     MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID                   
066000                                                                          
066100     MOVE MQGMO-VERSION-4        TO MQGMO-VERSION                         
066200                                                                          
066300     MOVE LENGTH OF WDATA        TO MQ-MSGLENGTH                          
066400                                                                          
066500     CALL  MQGET              USING MQ-HCONN                              
066600                                    MQ-HOBJ                               
066700                                    MQMD                                  
066800                                    MQGMO                                 
066900                                    MQ-MSGLENGTH                          
067000                                    WDATA                                 
067100                                    MQ-DATALENGTH                         
067200                                    MQ-COMPCODE                           
067300                                    MQ-REASON                             
067400                                                                          
067500     IF MQ-COMPCODE NOT = MQCC-OK AND                                     
067600        MQ-REASON   NOT = MQRC-NO-MSG-AVAILABLE                           
067700       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
067800       MOVE MQ-REASON            TO REASON-DISPLAY                        
067900       MOVE SPACE                TO ERROR-TEXT-STR                        
068000       STRING 'RC FROM MQGET '   RC-DISPLAY                               
068100              ' REASON: ' REASON-DISPLAY                                  
068200          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
068300       PERFORM S99-ABEND                                                  
068400     ELSE                                                                 
068500       IF MQ-DATALENGTH > LENGTH OF WDATA                                 
068600         MOVE MQ-DATALENGTH      TO RC-DISPLAY                            
068700         MOVE SPACE              TO ERROR-TEXT-STR                        
068800         STRING 'MQGET ERROR. MSG TOO LONG'                               
068900                '. MSG LENGTH: ' RC-DISPLAY                               
069000                '. MAX ALLOWED: ' LENGTH OF WDATA                         
069100            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
069200                                                                          
069300         PERFORM S99-ABEND                                                
069400       ELSE                                                               
069500         MOVE MQ-DATALENGTH      TO WLEN                                  
069600         MOVE 1                  TO WSTART                                
069700       END-IF                                                             
069800     END-IF                                                               
069900     DISPLAY 'MQGET   SUCCESSFUL   '                                      
070000     .                                                                    
070100                                                                          
070200 S04-MQ-INQ-HEADERS SECTION.                                              
070300*    -- THEN GET THE MANDATORY PROPERTIES IF THEY HAVE VALUES             
070400                                                                          
070500     MOVE SWEDISH-EBCDIC         TO MQIMPO-REQUESTEDCCSID                 
070600     MOVE MQTYPE-STRING          TO MQ-PROPTYPE                           
070700                                                                          
070800     COMPUTE MQIMPO-OPTIONS = MQIMPO-CONVERT-VALUE                        
070900                            + MQIMPO-INQ-FIRST                            
071000*                           + MQIMPO-CONVERT-TYPE                         
071100                                                                          
071200     MOVE '%'                    TO MQ-PROPNAME-TO-RETRIEVE               
071300     MOVE 1                      TO MQCHARV-VSLENGTH                      
071400     SET MQCHARV-VSPTR           TO ADDRESS                               
071500                                 OF MQ-PROPNAME-TO-RETRIEVE               
071600     MOVE LENGTH OF MQ-PROPNAME-TO-RETRIEVE                               
071700                                 TO MQCHARV-VSBUFSIZE                     
071800                                                                          
071900     MOVE SPACES                 TO MQ-PROPNAME-RETRIEVED                 
072000                                    MQ-PROPVALUE                          
072100                                                                          
072200     SET MQIMPO-RETURNEDNAME-VSPTR                                        
072300                                 TO ADDRESS                               
072400                                 OF MQ-PROPNAME-RETRIEVED                 
072500     MOVE LENGTH OF MQ-PROPNAME-RETRIEVED                                 
072600                                 TO MQIMPO-RETURNEDNAME-VSBUFSIZE         
072700                                                                          
072800                                                                          
072900     MOVE LENGTH OF MQ-PROPVALUE TO MQ-PROPVALUELENGTH                    
073000                                                                          
073100     CALL MQINQMP             USING MQ-HCONN                              
073200                                    MQ-PROPHANDLE                         
073300                                    MQ-INQ-PROP-OPTIONS                   
073400                                    MQ-PROPNAME                           
073500                                    MQ-PROPDESC                           
073600                                    MQ-PROPTYPE                           
073700                                    MQ-PROPVALUELENGTH                    
073800                                    MQ-PROPVALUE                          
073900                                    MQ-PROPDATALENGTH                     
074000                                    MQ-COMPCODE                           
074100                                    MQ-REASON                             
074200                                                                          
074300     IF MQ-COMPCODE = MQCC-OK                                             
074400       STRING MQ-PROPNAME-RETRIEVED DELIMITED BY SPACE                    
074500              ' = '                 DELIMITED BY SIZE                     
074600              MQ-PROPVALUE          DELIMITED BY SIZE                     
074700                               INTO DISPLAY-TEXT                          
074800       DISPLAY DISPLAY-TEXT                                               
074900                                                                          
075000       IF MQ-PROPNAME-RETRIEVED = MQ-FILE-MARKER                          
075100         MOVE MQ-PROPVALUE       TO W-FILEMARKER                          
075200       END-IF                                                             
075300                                                                          
075400       IF MQ-PROPNAME-RETRIEVED = MQ-HEADER-NAME                          
075500         MOVE MQ-PROPVALUE       TO WS-HEADER-VAL                         
075600       END-IF                                                             
075700                                                                          
075800     ELSE                                                                 
075900       IF MQ-REASON = MQRC-PROPERTY-NOT-AVAILABLE                         
076000         DISPLAY 'PROPERTY(1) NOT AVAILABLE'                              
076100       ELSE                                                               
076200         MOVE MQ-COMPCODE        TO RC-DISPLAY                            
076300         MOVE MQ-REASON          TO REASON-DISPLAY                        
076400         MOVE SPACE              TO ERROR-TEXT-STR                        
076500         STRING 'RC FROM MQINQMP (1)' RC-DISPLAY                          
076600                ' REASON: ' REASON-DISPLAY                                
076700            DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
076800         PERFORM S99-ABEND                                                
076900       END-IF                                                             
077000     END-IF                                                               
077100                                                                          
077200     PERFORM                                                              
077300       UNTIL MQ-REASON = MQRC-PROPERTY-NOT-AVAILABLE                      
077400                                                                          
077500       MOVE SWEDISH-EBCDIC       TO MQIMPO-REQUESTEDCCSID                 
077600       MOVE MQTYPE-STRING        TO MQ-PROPTYPE                           
077700                                                                          
077800       COMPUTE MQIMPO-OPTIONS = MQIMPO-CONVERT-VALUE                      
077900                              + MQIMPO-INQ-NEXT                           
078000       MOVE '%'                  TO MQ-PROPNAME-TO-RETRIEVE               
078100       MOVE 1                    TO MQCHARV-VSLENGTH                      
078200                                                                          
078300       SET MQCHARV-VSPTR         TO ADDRESS                               
078400                                 OF MQ-PROPNAME-TO-RETRIEVE               
078500                                                                          
078600       MOVE LENGTH OF MQ-PROPNAME-TO-RETRIEVE                             
078700                                 TO MQCHARV-VSBUFSIZE                     
078800                                                                          
078900       SET MQIMPO-RETURNEDNAME-VSPTR                                      
079000                                 TO ADDRESS                               
079100                                 OF MQ-PROPNAME-RETRIEVED                 
079200       MOVE LENGTH OF MQ-PROPNAME-RETRIEVED                               
079300                                 TO MQIMPO-RETURNEDNAME-VSBUFSIZE         
079400                                                                          
079500       MOVE LENGTH OF MQ-PROPVALUE                                        
079600                                 TO MQ-PROPVALUELENGTH                    
079700                                                                          
079800       MOVE SPACES               TO MQ-PROPNAME-RETRIEVED                 
079900                                    MQ-PROPVALUE                          
080000                                                                          
080100       CALL MQINQMP           USING MQ-HCONN                              
080200                                    MQ-PROPHANDLE                         
080300                                    MQ-INQ-PROP-OPTIONS                   
080400                                    MQ-PROPNAME                           
080500                                    MQ-PROPDESC                           
080600                                    MQ-PROPTYPE                           
080700                                    MQ-PROPVALUELENGTH                    
080800                                    MQ-PROPVALUE                          
080900                                    MQ-PROPDATALENGTH                     
081000                                    MQ-COMPCODE                           
081100                                    MQ-REASON                             
081200                                                                          
081300       IF MQ-COMPCODE = MQCC-OK                                           
081400         STRING MQ-PROPNAME-RETRIEVED DELIMITED BY SPACE                  
081500                ' = '                 DELIMITED BY SIZE                   
081600                MQ-PROPVALUE          DELIMITED BY SIZE                   
081700                               INTO DISPLAY-TEXT                          
081800         DISPLAY DISPLAY-TEXT                                             
081900                                                                          
082000         IF MQ-PROPNAME-RETRIEVED = MQ-FILE-MARKER                        
082100           MOVE MQ-PROPVALUE     TO W-FILEMARKER                          
082200         END-IF                                                           
082300                                                                          
082400         IF MQ-PROPNAME-RETRIEVED = MQ-HEADER-NAME                        
082500           MOVE MQ-PROPVALUE     TO WS-HEADER-VAL                         
082600         END-IF                                                           
082700                                                                          
082800       ELSE                                                               
082900         IF MQ-REASON = MQRC-PROPERTY-NOT-AVAILABLE                       
083000           CONTINUE                                                       
083100         ELSE                                                             
083200           MOVE MQ-COMPCODE      TO RC-DISPLAY                            
083300           MOVE MQ-REASON        TO REASON-DISPLAY                        
083400           MOVE SPACE            TO ERROR-TEXT-STR                        
083500           STRING 'RC FROM MQINQMP ' RC-DISPLAY                           
083600                  ' REASON: ' REASON-DISPLAY                              
083700              DELIMITED BY SIZE INTO ERROR-TEXT-STR                       
083800           PERFORM S99-ABEND                                              
083900         END-IF                                                           
084000       END-IF                                                             
084100     END-PERFORM                                                          
084200     DISPLAY 'INQ PROPERTIES SUCCESSFUL'                                  
084300                                                                          
084400     .                                                                    
084500 S04-MQ-CLOSE  SECTION.                                                   
084600                                                                          
084700*    -- FINALLY, CLOSE THE QUEUE                                          
084800     CALL  MQCLOSE            USING MQ-HCONN                              
084900                                    MQ-HOBJ                               
085000                                    MQCO-NONE                             
085100                                    MQ-COMPCODE                           
085200                                    MQ-REASON                             
085300                                                                          
085400     IF MQ-COMPCODE NOT = MQCC-OK                                         
085500       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
085600       MOVE MQ-REASON            TO REASON-DISPLAY                        
085700       MOVE SPACE                TO ERROR-TEXT-STR                        
085800       STRING 'RC FROM MQCLOSE ' RC-DISPLAY                               
085900              ' REASON: ' REASON-DISPLAY                                  
086000          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
086100       PERFORM S99-ABEND                                                  
086200     END-IF                                                               
086300                                                                          
086400     DISPLAY 'MQCLOSE SUCCESSFUL   '                                      
086500     .                                                                    
086600 S04-MQ-DISC  SECTION.                                                    
086700                                                                          
086800*    -- BREAK CONTACT WITH MQ QUEUE MANAGER                               
086900                                                                          
087000     CALL  MQDISC             USING MQ-HCONN                              
087100                                    MQ-COMPCODE                           
087200                                    MQ-REASON                             
087300                                                                          
087400     IF MQ-COMPCODE NOT = MQCC-OK                                         
087500       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
087600       MOVE MQ-REASON            TO REASON-DISPLAY                        
087700       MOVE SPACE                TO ERROR-TEXT-STR                        
087800       STRING 'RC FROM MQDISC '  RC-DISPLAY                               
087900              ' REASON: ' REASON-DISPLAY                                  
088000          DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
088100       PERFORM S99-ABEND                                                  
088200     END-IF                                                               
088300                                                                          
088400     DISPLAY 'MQDISC SUCCESSFUL    '                                      
088500     .                                                                    
088600 S99-ABEND SECTION.                                                       
088700                                                                          
088800     DISPLAY IDPGM ' ' ERROR-TEXT                                         
088900     CALL ABEND               USING RKOD-ABEND-NO-DUMP                    
089000     .                                                                    
