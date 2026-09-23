000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ01CALL.                                                
000300 AUTHOR.         KJELL ANDRE.                                             
000400 DATE-WRITTEN.   01/12/07                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*      GENERAL SUBPROGRAM FOR SENDING A SYNCHRONOUS MESSAGE               
000900*      AND RETURNING A RESPONSE TO THE CALLER.                            
001000*      IT IS PART OF THE NEW DISPATCHER FRAMEWORK USED IN                 
001100*      THE CAR PARTS SYSTEMS. CURRENTLY IT HANDLES COMMUNICATION          
001200*      VIA VCOM ONLY                                                      
001300*                                                                         
001400*      CALL SYNTAX:                                                       
001500*        CALL WZ01CALL USING CALL-CONTROL-AREA                            
001600*                            CALL-KVDLEN-IN                               
001700*                            MY-MESSAGE-DATA                              
001800*                            CALL-KVDLEN-OUT                              
001900*                            THE-RESPONSE-DATA                            
002000*                                                                         
002100*      THE PARAMETERS USED IN THE CALLS ARE DEFINED AND                   
002200*      EXPLAINED IN MORE DETAIL IN COPYTEXT WZ01CALL.                     
002300*                                                                         
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900 77  IDPGM                       PIC X(8)    VALUE 'WZ01CALL'.            
003000 77  YES                         PIC X       VALUE 'J'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200                                                                          
003300 77  WS-KDCOMTYPE                PIC X(10)   VALUE SPACES.                
003400                                                                          
003500 77  OK-SWITCH                   PIC X       VALUE 'J'.                   
003600   88 ALL-OK                                 VALUE 'J'.                   
003700   88 SOME-ERROR                             VALUE 'N'.                   
003800                                                                          
003900*    -- EMULATION OF EOF - IF LENGTH OF LAST RESULT IS < 9990             
004000*    -- THEN THIS WAS THE LAST RECORD                                     
004100 77  RECEIVE-EOF-SWITCH          PIC S9(9)   BINARY VALUE ZERO.           
004200   88 NO-MORE-TO-RECEIVE                     VALUE ZERO THRU 9997.        
004300   88 MORE-TO-RECEIVE                        VALUE 9998 THRU 9999.        
004400                                                                          
004500 01  RC-DISPLAY                  PIC 9(4).                                
004600                                                                          
004700 01  FILLER                      PIC X(16)   VALUE 'ERROR-TEXT:'.         
004800 01  ERROR-TEXT                  PIC X(80).                               
004900                                                                          
005000 01  MAX-RESULT-LEN              PIC S9(9)   BINARY.                      
005100                                                                          
005200*    -- VALUE FROM PREVIOUS CALL SAVED HERE                               
005300 01  PREV-ADDISPABS              PIC X(50)   VALUE 'NO-VALUE'.            
005400                                                                          
005500*    -- FIELDS USED WHEN CUTTING UP THE DATA INTO                         
005600*    -- PIECES OF TRANMSITTABLE SIZE (10K FOR VCOM)                       
005700 01  WLEN                        PIC S9(9)   BINARY.                      
005800 01  WSTART                      PIC S9(9)   BINARY.                      
005900 01  WFROM-POS                   PIC S9(9)   BINARY.                      
006000                                                                          
006100 01  WSEGMPREFIX                 PIC X(8).                                
006200 01  WDATA                       PIC X(10000).                            
006300                                                                          
006400 01  DYNAMIC-SUBPROGRAMS.                                                 
006500     03 ABEND                    PIC X(8)    VALUE 'ABEND   '.            
006600     03 WZ01ATAB                 PIC X(8)    VALUE 'WZ01ATAB'.            
006700     03 AIBTDLI                  PIC X(8)    VALUE 'AIBTDLI'.             
006800     03 CSCONS                   PIC X(8)    VALUE 'CSCONS  '.            
006900     03 CSSEND                   PIC X(8)    VALUE 'CSSEND  '.            
007000     03 CSRECV                   PIC X(8)    VALUE 'CSRECV  '.            
007100     03 CSRLSE                   PIC X(8)    VALUE 'CSRLSE  '.            
007200     03 BAQCSTUB                 PIC X(8)    VALUE 'BAQCSTUB'.            
007300     03 BAQCTERM                 PIC X(8)    VALUE 'BAQCTERM'.            
007400                                                                          
007500                                                                          
007600*    -- PARAMETERS TO ABEND                                               
007700                                                                          
007800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   BINARY VALUE +16.            
007900                                                                          
008000*    -- PARAMETERS TO WZ01ATAB                                            
008100                                                                          
008200*01  -COPY WZ01ATAB                                                       
008300                                                                          
008400 01  VCOM-AREA-START             PIC X(16)   VALUE                        
008500                                 'VCOM-AREA-START '.                      
008600*01  -COPY W0028 -PRE VCOM-                                               
008700                                                                          
008800*    -- 9990 = CA 9999 - 8  (8 EXTRA BYTES NEEDED FOR PREFIX)             
008900 01  MAX-VCOM-DATA-LENGTH        PIC S9(9)   BINARY VALUE +9990.          
009000                                                                          
009100****************************************************************          
009200 01 BAQ-REQUEST-PTR                          USAGE POINTER.               
009300 01 BAQ-REQUEST-LEN              PIC S9(9)   COMP-5 SYNC.                 
009400 01 BAQ-RESPONSE-PTR                         USAGE POINTER.               
009500 01 BAQ-RESPONSE-LEN             PIC S9(9)   COMP-5 SYNC.                 
009600                                                                          
009700*                                                                         
009800*   -COPY BAQRINFO                                                        
009900*                                                                         
010000*   -COPY WAPIINFO                                                        
010100*                                                                         
010200 01 API-REQUEST              PIC X(10000000).                             
010300                                                                          
010400*01 API-RESPONSE             PIC X(104857600).                            
010500 01 API-RESPONSE             PIC X(10000).                                
010600                                                                          
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800                                                                          
010900 01  KEYS-TILL-DLI.                                                       
011000     03  W-WDGXKEY-X.                                                     
011100         05  W-IDHTYP            PIC X(04)   VALUE '0103'.                
011200         05  FILLER              PIC X(26)   VALUE LOW-VALUES.            
011300     03  W-ADDISPABS-X.                                                   
011400         05  W-ADDISPABS         PIC X(50)   VALUE SPACES.                
011500                                                                          
011600*    --- STATUS-KOD FROM IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FOUND                       VALUE '  '.                  
011900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012000                                                                          
012100 01  GOOD-STATUS-CODES.                                                   
012200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX                        
012300                                 PIC XX.                                  
012400                                                                          
012500 01  SSA1                        PIC X(128).                              
012600 01  SSA2                        PIC X(128).                              
012700                                                                          
012800*    -- IMS FUNCTION CODES                                                
012900*    -COPY W0003.                                                         
013000                                                                          
013100 01  DLI-IO-AREA                 PIC X(1000).                             
013200*01  -COPY WDGX0104 -RED DLI-IO-AREA.                                     
013300                                                                          
013400 01  AIB-AREA-START              PIC X(16)   VALUE                        
013500                                 'AIB-AREA-START  '.                      
013600*    -COPY W0031.                                                         
013700                                                                          
013800 LINKAGE SECTION.                                                         
013900                                                                          
014000*01 -COPY WZ01CAL1.                                                       
014100                                                                          
014200                                                                          
014300 01  CALL-KVDLEN-IN             PIC S9(9) BINARY.                         
014400                                                                          
014500 01  CALL-DATA-IN               PIC X(5242880).                           
014600                                                                          
014700 01  CALL-KVDLEN-OUT            PIC S9(9) BINARY.                         
014800                                                                          
014900*01  CALL-DATA-OUT              PIC X(4000000).                           
015000 01  CALL-DATA-OUT              PIC X(9999).                              
015100                                                                          
015200*01  -COPY W0008  -PRE WDR5-                                              
015300     05 FILLER                   PIC X.                                   
015400                                                                          
015500     EJECT                                                                
015600 PROCEDURE DIVISION USING                                                 
015700                      CALL-CONTROL-AREA                                   
015800                      CALL-KVDLEN-IN    CALL-DATA-IN                      
015900                      CALL-KVDLEN-OUT   CALL-DATA-OUT                     
016000                     .                                                    
016100 MAIN SECTION.                                                            
016200                                                                          
016300     PERFORM A-INIT                                                       
016400*    IF WS-KDCOMTYPE = 'VCOM'                                             
016500*      CONTINUE                                                           
016600*    ELSE                                                                 
016700*      DISPLAY 'CALL ' FUNCTION CURRENT-DATE ' ' CALL-ADDISPABS           
016800*              ' ' WS-KDCOMTYPE ' ' OK-SWITCH                             
016900*    END-IF                                                               
017000     IF CALL-ADDISPABS = SPACE                                            
017100*      -- FINAL RELEASE                                                   
017200       EVALUATE WS-KDCOMTYPE                                              
017300         WHEN 'VCOM'                                                      
017400           PERFORM B4-VCOM-RELEASE                                        
017500       END-EVALUATE                                                       
017600     ELSE                                                                 
017700       IF  PREV-ADDISPABS NOT = CALL-ADDISPABS                            
017800*          -- RELEASE PREVIOUS CONNECTION                                 
017900         EVALUATE WS-KDCOMTYPE                                            
018000           WHEN 'VCOM'                                                    
018100             PERFORM B4-VCOM-RELEASE                                      
018200             PERFORM B1-VCOM-CONNECT                                      
018300         END-EVALUATE                                                     
018400       END-IF                                                             
018500       IF ALL-OK                                                          
018600         EVALUATE WS-KDCOMTYPE                                            
018700           WHEN 'VCOM'                                                    
018800             PERFORM B2-VCOM-SEND                                         
018900             PERFORM B3-VCOM-RECV                                         
019000           WHEN 'IMSP2P'                                                  
019100             IF CALL-ADDISPABS (1:3) = 'API'                              
019200               PERFORM G1-API-INIT                                        
019300               PERFORM G2-API-CALL                                        
019400             ELSE                                                         
019500               MOVE 10 TO CALL-KDRC                                       
019600               SET SOME-ERROR      TO TRUE                                
019700             END-IF                                                       
019800           WHEN 'API'                                                     
019900             PERFORM G1-API-INIT                                          
020000             PERFORM G2-API-CALL                                          
020100           WHEN OTHER                                                     
020200             CALL ABEND USING RKOD-ABEND-NO-DUMP                          
020300         END-EVALUATE                                                     
020400       END-IF                                                             
020500     END-IF                                                               
020600                                                                          
020700     IF CALL-KDRC = 0                                                     
020800       MOVE ZERO TO RETURN-CODE                                           
020900     ELSE                                                                 
021000       MOVE 12 TO RETURN-CODE                                             
021100     END-IF                                                               
021200                                                                          
021300     GOBACK                                                               
021400     .                                                                    
021500                                                                          
021600     EJECT                                                                
021700 A-INIT SECTION.                                                          
021800                                                                          
021900     SET ALL-OK TO TRUE                                                   
022000     MOVE ZERO                   TO CALL-KDRC                             
022100                                                                          
022200     IF CALL-ADDISPABS NOT = SPACE                                        
022300*      -- SAVE MAX ACCEPTED RESULT LENGTH FOR FUTURE USE                  
022400       MOVE CALL-KVDLEN-OUT  TO MAX-RESULT-LEN                            
022500                                                                          
022600       PERFORM AA-SEARCH-ATAB                                             
022700     END-IF                                                               
022800     .                                                                    
022900                                                                          
023000     EJECT                                                                
023100 AA-SEARCH-ATAB SECTION.                                                  
023200                                                                          
023300     MOVE FUNCTION UPPER-CASE (CALL-ADDISPABS)                            
023400                                 TO ATAB-ADDISPABS                        
023500                                    W-ADDISPABS                           
023600     CALL WZ01ATAB USING ATAB-WZ01ATAB                                    
023700                                                                          
023800     IF (RETURN-CODE > ZERO) OR (ATAB-ADDISPABS (1:3) = 'API')            
023900*      -- Address not found in WZ01ATAB or found but with                 
024000*      -- KDCOMTYP = API, then read the address database                  
024100       PERFORM IMS-GU-ADDRESS-INFO                                        
024200       IF SEGMENT-FOUND                                                   
024300         MOVE 0104-KDCOMTYPE     TO WS-KDCOMTYPE                          
024400       ELSE                                                               
024500         MOVE 10                 TO CALL-KDRC                             
024600         SET SOME-ERROR          TO TRUE                                  
024700       END-IF                                                             
024800     ELSE                                                                 
024900       MOVE ATAB-KDCOMTYPE       TO WS-KDCOMTYPE                          
025000     END-IF                                                               
025100     .                                                                    
025200                                                                          
025300 B1-VCOM-CONNECT SECTION.                                                 
025400                                                                          
025500     MOVE 20             TO VCOM-TIMEOUT                                  
025600     MOVE SPACE          TO VCOM-PARTNER                                  
025700     MOVE SPACE          TO VCOM-SENDERTAG                                
025800                                                                          
025900     MOVE ZERO TO TALLY                                                   
026000     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
026100             FOR CHARACTERS BEFORE INITIAL 'PAR:'                         
026200     MOVE SPACE TO VCOM-PARTNER                                           
026300     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
026400       UNSTRING ATAB-ADDISPINT(TALLY + 5:)  DELIMITED BY ';'              
026500       INTO VCOM-PARTNER                                                  
026600     END-IF                                                               
026700     MOVE ZERO TO TALLY                                                   
026800     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
026900             FOR CHARACTERS BEFORE INITIAL 'STAG:'                        
027000     MOVE SPACE TO VCOM-SENDERTAG                                         
027100     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
027200       UNSTRING ATAB-ADDISPINT(TALLY + 6:)  DELIMITED BY ';'              
027300       INTO VCOM-SENDERTAG                                                
027400     END-IF                                                               
027500                                                                          
027600     MOVE 'INIT41  '     TO VCOM-INITIATOR                                
027700                                                                          
027800     CALL CSCONS USING VCOM-RC                                            
027900                       VCOM-CONVID                                        
028000                       VCOM-SECUR                                         
028100                       VCOM-TIMEOUT                                       
028200                       VCOM-SENDERTAG                                     
028300                       VCOM-PARTNER                                       
028400                       VCOM-INITIATOR                                     
028500                                                                          
028600     IF VCOM-RC NOT = ZERO                                                
028700       MOVE VCOM-RC TO RC-DISPLAY                                         
028800       STRING 'RC FROM CSCONS ' RC-DISPLAY                                
028900              ' ' VCOM-CONVID                                             
029000              ' ' VCOM-PARTNER                                            
029100              ' ' VCOM-SENDERTAG                                          
029200          DELIMITED BY SIZE                                               
029300          INTO ERROR-TEXT                                                 
029400       MOVE 12 TO CALL-KDRC                                               
029500       SET SOME-ERROR TO TRUE                                             
029600     ELSE                                                                 
029700       MOVE CALL-ADDISPABS TO PREV-ADDISPABS                              
029800     END-IF                                                               
029900                                                                          
030000     .                                                                    
030100                                                                          
030200     EJECT                                                                
030300 B2-VCOM-SEND    SECTION.                                                 
030400                                                                          
030500*    -- IF THE DATA IS LONGER THAN CA 10000 BYTES, IT IS SPLIT            
030600*    -- INTO 10K SEGMENTS. THE FIRST IS SENT AS-IS, BUT THE               
030700*    -- FOLLOWING SEGMENTS ARE PREFIXED BY A "CONTINUATION                
030800*    -- PREFIX - 'WZ01CONT' SO THE RECEIVER CAN IDENTIFY THEM.            
030900                                                                          
031000     MOVE 1 TO WSTART                                                     
031100     MOVE MAX-VCOM-DATA-LENGTH TO WLEN                                    
031200     MOVE SPACE         TO WSEGMPREFIX                                    
031300     PERFORM UNTIL WSTART > CALL-KVDLEN-IN                                
031400                                                                          
031500*      -- DON'T MOVE MORE THAN WHAT REMAINS OF THE DATA                   
031600       IF WSTART + WLEN > CALL-KVDLEN-IN + 1                              
031700         COMPUTE WLEN = CALL-KVDLEN-IN - WSTART + 1                       
031800       END-IF                                                             
031900                                                                          
032000       MOVE CALL-DATA-IN(WSTART:WLEN) TO WDATA                            
032100       PERFORM B2A-SEND-SEGMENT                                           
032200                                                                          
032300       MOVE 'WZ01CONT' TO WSEGMPREFIX                                     
032400       ADD WLEN TO  WSTART                                                
032500     END-PERFORM                                                          
032600     .                                                                    
032700                                                                          
032800                                                                          
032900     SKIP3                                                                
033000 B2A-SEND-SEGMENT  SECTION.                                               
033100                                                                          
033200     MOVE SPACE TO VCOM-DATA                                              
033300     STRING                                                               
033400       WSEGMPREFIX  DELIMITED BY SPACE                                    
033500       WDATA(WSTART:WLEN) DELIMITED BY SIZE                               
033600      INTO VCOM-DATA                                                      
033700                                                                          
033800     IF WSEGMPREFIX = SPACE                                               
033900       MOVE WLEN TO VCOM-ACTLENGTH                                        
034000     ELSE                                                                 
034100       COMPUTE VCOM-ACTLENGTH = WLEN + 8                                  
034200     END-IF                                                               
034300                                                                          
034400     CALL CSSEND USING VCOM-RC                                            
034500                       VCOM-CONVID                                        
034600                       VCOM-ACTLENGTH                                     
034700                       VCOM-DATA                                          
034800                                                                          
034900     IF VCOM-RC NOT = ZERO                                                
035000       MOVE VCOM-RC TO RC-DISPLAY                                         
035100       STRING 'RC FROM CSSEND ' RC-DISPLAY                                
035200              ' ' VCOM-CONVID                                             
035300              ' ' VCOM-PARTNER                                            
035400              ' ' VCOM-SENDERTAG                                          
035500          DELIMITED BY SIZE                                               
035600          INTO ERROR-TEXT                                                 
035700       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
035800     END-IF                                                               
035900     .                                                                    
036000                                                                          
036100     EJECT                                                                
036200 B3-VCOM-RECV    SECTION.                                                 
036300                                                                          
036400*    -- THE DATA MAY BE SPLIT INTO SEGEMENT OF ABOUT 10K EACH.            
036500*    -- THE FIRST SEGMENT IS PURE DATA, BUT THE                           
036600*    -- FOLLOWING SEGMENTS ARE PREFIXED BY A "CONTINUATION                
036700*    -- PREFIX - 'WZ01CONT'                                               
036800                                                                          
036900*    -- TOTAL LENGTH OF DATA IS INITIALLY ZERO                            
037000     MOVE ZERO TO CALL-KVDLEN-OUT                                         
037100*    -- PUT DATA AT BEGINNING OF RECEVING DATA FIELD                      
037200     MOVE 1    TO WSTART                                                  
037300*    -- TAKE WHOLE RECORD FIRST TIME                                      
037400     MOVE 1    TO WFROM-POS                                               
037500                                                                          
037600*    -- FETCH AND PROCESS FIRST SEGMENT                                   
037700     PERFORM B3A-RECV-SEGMENT                                             
037800     PERFORM B3B-MOVE-DATA                                                
037900*    -- FETCH AND PROCESS MORE SEGMENTS IF THEY EXIST                     
038000     PERFORM UNTIL VCOM-RC NOT = ZERO                                     
038100             OR NO-MORE-TO-RECEIVE                                        
038200                                                                          
038300       PERFORM B3A-RECV-SEGMENT                                           
038400*      -- SKIP FIRST 8 POSITIONS FROM NOW ON                              
038500       MOVE 9 TO WFROM-POS                                                
038600       PERFORM B3B-MOVE-DATA                                              
038700                                                                          
038800     END-PERFORM                                                          
038900                                                                          
039000     IF WSTART > MAX-RESULT-LEN + 1                                       
039100*      -- TRUNCATION, SOME DATA NOT PROCESSED                             
039200       MOVE 21   TO CALL-KDRC                                             
039300     END-IF                                                               
039400     .                                                                    
039500                                                                          
039600                                                                          
039700     SKIP3                                                                
039800 B3A-RECV-SEGMENT  SECTION.                                               
039900                                                                          
040000     MOVE 20                   TO VCOM-TIMEOUT                            
040100     MOVE LENGTH OF VCOM-DATA  TO VCOM-MAXLENGTH                          
040200                                                                          
040300     CALL CSRECV USING VCOM-RC                                            
040400                       VCOM-CONVID                                        
040500                       VCOM-TIMEOUT                                       
040600                       VCOM-MAXLENGTH                                     
040700                       VCOM-ACTLENGTH                                     
040800                       VCOM-DATA                                          
040900                                                                          
041000     IF  VCOM-RC NOT = ZERO AND 35                                        
041100       MOVE VCOM-RC TO RC-DISPLAY                                         
041200       STRING 'RC FROM CSRECV ' RC-DISPLAY                                
041300              ' ' VCOM-CONVID                                             
041400              ' ' VCOM-PARTNER                                            
041500              ' ' VCOM-SENDERTAG                                          
041600          DELIMITED BY SIZE                                               
041700          INTO ERROR-TEXT                                                 
041800       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
041900     END-IF                                                               
042000                                                                          
042100     IF VCOM-RC = 35                                                      
042200*      -- NO DATA, DISCONNECT FROM OTHER SIDE                             
042300       MOVE SPACE TO WDATA                                                
042400       MOVE ZERO  TO WLEN                                                 
042500*      -- TRIGGER A NEW CONNECT NEXT TIME                                 
042600       MOVE 'NO-VALUE' TO PREV-ADDISPABS                                  
042700     ELSE                                                                 
042800*      -- TAKE CARE OF THE RECEIVED DATA                                  
042900       MOVE VCOM-DATA(1:VCOM-ACTLENGTH) TO WDATA                          
043000       MOVE VCOM-ACTLENGTH              TO WLEN                           
043100       MOVE VCOM-ACTLENGTH              TO RECEIVE-EOF-SWITCH             
043200     END-IF                                                               
043300     .                                                                    
043400                                                                          
043500     EJECT                                                                
043600 B3B-MOVE-DATA SECTION.                                                   
043700                                                                          
043800*    -- MOVE DATA FROM ONE SEGMENT TO THE RIGHT POSITION                  
043900*    -- IN THE OUTPUT FIELD. DO THIS ONLY IF THERE IS DATA                
044000*    -- TO MOVE, AND IT DOES NOT OVERFLOW THE OUTPUT FIELD                
044100                                                                          
044200     IF WLEN > WFROM-POS                                                  
044300     AND WSTART + WLEN - WFROM-POS <= MAX-RESULT-LEN + 1                  
044400       COMPUTE WLEN = WLEN + 1 - WFROM-POS                                
044500       MOVE WDATA(WFROM-POS:WLEN) TO CALL-DATA-OUT(WSTART:WLEN)           
044600       ADD  WLEN                  TO CALL-KVDLEN-OUT                      
044700     END-IF                                                               
044800*    -- PREPARE POINTER FOR NEXT TIME                                     
044900     ADD  WLEN                 TO WSTART                                  
045000     .                                                                    
045100                                                                          
045200     EJECT                                                                
045300 B4-VCOM-RELEASE SECTION.                                                 
045400                                                                          
045500     MOVE ZERO                        TO VCOM-RC                          
045600                                                                          
045700*    -- RELEASE ONLY IF PREVIOUS CONNECTION EXISTS                        
045800     IF PREV-ADDISPABS NOT = 'NO-VALUE'                                   
045900       CALL CSRLSE USING VCOM-RC                                          
046000                         VCOM-CONVID                                      
046100                                                                          
046200       IF VCOM-RC NOT = ZERO                                              
046300         MOVE VCOM-RC TO RC-DISPLAY                                       
046400         STRING 'RC FROM CSRLSE ' RC-DISPLAY                              
046500                ' ' VCOM-CONVID                                           
046600                ' ' VCOM-PARTNER                                          
046700                ' ' VCOM-SENDERTAG                                        
046800            DELIMITED BY SIZE                                             
046900            INTO ERROR-TEXT                                               
047000         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
047100       END-IF                                                             
047200                                                                          
047300       MOVE 'NO-VALUE' TO PREV-ADDISPABS                                  
047400     END-IF                                                               
047500     .                                                                    
047600                                                                          
047700 G1-API-INIT SECTION.                                                     
047800                                                                          
047900* Use pointer and length to specify the location of                       
048000* the request and response data structures.                               
048100* These assignments are required.                                         
048200*    INITIALIZE CALL-DATA-OUT                                             
048300                                                                          
048400     SET BAQ-REQUEST-PTR         TO ADDRESS OF CALL-DATA-IN               
048500     MOVE CALL-KVDLEN-IN         TO BAQ-REQUEST-LEN                       
048600     SET BAQ-RESPONSE-PTR        TO ADDRESS OF CALL-DATA-OUT              
048700     MOVE CALL-KVDLEN-OUT        TO BAQ-RESPONSE-LEN                      
048800                                                                          
048900     MOVE 0104-IDAPI             TO IDAPI                                 
049000     COMPUTE IDAPI-LEN            = FUNCTION LENGTH (                     
049100                                    FUNCTION TRIM (IDAPI))                
049200     MOVE 0104-IDPATH-API        TO IDPATH-API                            
049300     COMPUTE IDPATH-API-LEN       = FUNCTION LENGTH (                     
049400                                    FUNCTION TRIM(IDPATH-API))            
049500     MOVE 0104-IDPTYP-API        TO IDPTYP-API                            
049600     COMPUTE IDPTYP-API-LEN       = FUNCTION LENGTH (                     
049700                                    FUNCTION TRIM(IDPTYP-API))            
049800     .                                                                    
049900                                                                          
050000 G2-API-CALL SECTION.                                                     
050100                                                                          
050200     CALL BAQCSTUB            USING WAPIINFO                              
050300                                    BAQ-REQUEST-INFO                      
050400                                    BAQ-REQUEST-PTR                       
050500                                    BAQ-REQUEST-LEN                       
050600                                    BAQ-RESPONSE-INFO                     
050700                                    BAQ-RESPONSE-PTR                      
050800                                    BAQ-RESPONSE-LEN                      
050900                                                                          
051000*    MOVE BAQ-RETURN-CODE        TO CALL-KDRC-API                         
051100*    MOVE BAQ-STATUS-CODE        TO CALL-KDSTATUS-API                     
051200*    MOVE BAQ-RESPONSE-LEN       TO CALL-KVDLEN-OUT                       
051300     IF BAQ-SUCCESS                                                       
051400*      DISPLAY 'SUCCESS '                                                 
051500       COMPUTE CALL-KVDLEN-OUT = BAQ-RESPONSE-LEN                         
051600     ELSE                                                                 
051700*      DISPLAY 'ERROR   '                                                 
051800       MOVE 4    TO CALL-KDRC                                             
051900       IF BAQ-STATUS-MESSAGE-LEN > 0                                      
052000         MOVE BAQ-STATUS-MESSAGE (1:BAQ-STATUS-MESSAGE-LEN)               
052100                                 TO CALL-DATA-OUT                         
052200                                      (1:BAQ-STATUS-MESSAGE-LEN)          
052300         COMPUTE CALL-KVDLEN-OUT = BAQ-STATUS-MESSAGE-LEN                 
052400       END-IF                                                             
052500     END-IF                                                               
052600     CONTINUE                                                             
052700     .                                                                    
052800                                                                          
052900 IMS-GU-ADDRESS-INFO SECTION.                                             
053000                                                                          
053100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
053200              DELIMITED BY SIZE INTO SSA1                                 
053300     STRING 'WDGX0104(ADDISPAB =' W-ADDISPABS-X ')'                       
053400              DELIMITED BY SIZE INTO SSA2                                 
053500                                                                          
053600     MOVE 128                    TO AIB-LEN                               
053700     MOVE SPACES                 TO AIB-SUB-FUNCTION                      
053800     MOVE 'ATAB'                 TO AIB-PCB-NAME                          
053900     MOVE LENGTH OF DLI-IO-AREA  TO AIB-IOAREA-LENGTH                     
054000                                                                          
054100     MOVE SPACES                 TO GOOD-STATUS-CODES                     
054200                                                                          
054300     CALL AIBTDLI             USING GU                                    
054400                                    AIB-AREA                              
054500                                    DLI-IO-AREA                           
054600                                    SSA1                                  
054700                                    SSA2                                  
054800                                                                          
054900     SET ADDRESS OF WDR5-PCB     TO AIB-PCB-PTR                           
055000     MOVE WDR5-STATUS-CODE       TO STATUS-WS                             
055100     PERFORM IMS-STATUS-CHECK                                             
055200     .                                                                    
055300                                                                          
055400 IMS-STATUS-CHECK SECTION.                                                
055500                                                                          
055600     IF STATUS-WS = LOW-VALUE                                             
055700       STRING ' CAN NOT FIND PCB WITH NAME: '                             
055800              WS-KDCOMTYPE ' IN THE PSB.'                                 
055900             DELIMITED BY SIZE INTO ERROR-TEXT                            
056000       DISPLAY IDPGM ' ' ERROR-TEXT                                       
056100       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
056200     ELSE                                                                 
056300       SET STATUS-IX             TO 1                                     
056400       SEARCH GOOD-STATUS                                                 
056500         AT END                                                           
056600           STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS             
056700             DELIMITED BY SIZE INTO ERROR-TEXT                            
056800           DISPLAY IDPGM ' ' ERROR-TEXT                                   
056900           CALL ABEND USING RKOD-ABEND-NO-DUMP                            
057000         WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                         
057100           CONTINUE                                                       
057200       END-SEARCH                                                         
057300     END-IF                                                               
057400     .                                                                    
057500                                                                          
