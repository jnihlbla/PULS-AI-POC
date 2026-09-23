000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ043100.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   26/02/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PROGRAM THAT HANDLES ASYNC MQ DISTRIBUTIONS                      
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSACTION: WZ0431X                                             
001200*        REQUEST    : WZ04PROP                                            
001300*                                                                         
001400*    OUTDATA.                                                             
001500*                                                                         
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800 CONFIGURATION SECTION.                                                   
001900*SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
002000                                                                          
002100 DATA DIVISION.                                                           
002200                                                                          
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500 77 IDPGM                        PIC X(08) VALUE 'WZ043100'.              
002600                                                                          
002700 77 YES                          PIC X     VALUE 'J'.                     
002800 77 NOO                          PIC X     VALUE 'N'.                     
002900                                                                          
003000 77  WS-TEXT                     PIC X(100)   VALUE SPACES.               
003100 77  WS-TEMP                     PIC X(100)   VALUE SPACES.               
003200                                                                          
003300 77  MAX-PIX                     PIC 9(2)  VALUE 0.                       
003400 01  WS-PROP-TABLE.                                                       
003500     03  FILLER OCCURS 20 TIMES INDEXED BY WS-PIX.                        
003600         05  WS-PROP-NAME        PIC X(100)   VALUE SPACES.               
003700         05  WS-PROP-VALUE       PIC X(100)   VALUE SPACES.               
003800                                                                          
003900 77 WS-NUM-DISPLAY               PIC 9(9).                                
004000 77 WS-RECV-KVDLEN               PIC S9(9) VALUE ZERO COMP.               
004100 77 KDRC-DISPLAY                 PIC Z(5).                                
004200                                                                          
004300 77 WS-SEND-AREA                 PIC X(1000).                             
004400                                                                          
004500 01  ERROR-TEXT.                                                          
004600     03  FILLER                  PIC X(12)  VALUE 'ERROR-TEXT: '.         
004700     03  ERROR-TEXT-STR          PIC X(80)  VALUE SPACE.                  
004800                                                                          
004900 01 MESSAGE-TEXT-GRP.                                                     
005000    03 MESSAGE-TEXT              PIC X(100).                              
005100                                                                          
005200*    --- PARAMETERS TO ABEND                                              
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
005700                                                                          
005800 77 WS-MY-OWN-ADRESS             PIC X(50) VALUE                          
005900                                      'CARPARTS.PULS.MQASYNC'.            
006000                                                                          
006100 77 CONTINUE-SW                  PIC X     VALUE 'J'.                     
006200    88 CONTINUE-YES                        VALUE 'J'.                     
006300    88 CONTINUE-NO                         VALUE 'N'.                     
006400                                                                          
006500                                                                          
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01 GENERAL-SUBPROGRAMS.                                                  
006800    03 WZ01RECV                  PIC X(8)  VALUE 'WZ01RECV'.              
006900    03 WZ11OUTQ                  PIC X(8)  VALUE 'WZ11OUTQ'.              
007000    03 VIMSID                    PIC X(8)  VALUE 'VIMSID  '.              
007100    03 CBLTDLI                   PIC X(8)  VALUE 'CBLTDLI '.              
007200    03 AIBTDLI                   PIC X(8)  VALUE 'AIBTDLI '.              
007300    03 FELLOG                    PIC X(8)  VALUE 'FELLOG  '.              
007400    03 ABEND                     PIC X(8)  VALUE 'ABEND   '.              
007500                                                                          
007600 COPY WZ01RECV.                                                           
007700                                                                          
007800 COPY WZ04PROP.                                                           
007900                                                                          
008000 01 WS-ADDISPABS                 PIC X(50).                               
008100                                                                          
008200 01 OUTQ-AREA.                                                            
008300    03  OUTQ-CONTROL-AREA.                                                
008400        05  OUTQ-KDFUNC          PIC X(10).                               
008500        05  OUTQ-KDRC            PIC S9(9) COMP.                          
008600        05  OUTQ-IDCOM           PIC S9(9) COMP.                          
008700    03  OUTQ-OPEN-AREA.                                                   
008800        05  OUTQ-ADDISPABS       PIC X(50).                               
008900    03  OUTQ-PROPERTY-AREA.                                               
009000        05  OUTQ-PROPERTY-NAME   PIC X(100).                              
009100        05  OUTQ-PROPERTY-VALUE  PIC X(100).                              
009200    03  OUTQ-ADDITIONAL-INFO.                                             
009300        05  OUTQ-PHYSICALID      PIC X(100).                              
009400    03  OUTQ-KVDLEN              PIC S9(9) BINARY.                        
009500    03  OUTQ-DATA.                                                        
009600        05 FILLER OCCURS 1 TO 104857600 TIMES                             
009700           DEPENDING ON OUTQ-KVDLEN PIC X.                                
009800                                                                          
009900*    --- PARAMETERS TO VIMSID                                             
010000 01 WS-VIMSID                    PIC X(8)  VALUE SPACE.                   
010100 01 FILLER REDEFINES WS-VIMSID.                                           
010200    03 IMS-REGION                PIC X(3).                                
010300       88 TEST-REGION                      VALUE 'IMD' 'IMP' 'IMY'        
010400                                                 'IMB'.                   
010500       88 ENV-DEVE                         VALUE 'IMP'.                   
010600       88 ENV-IGRT                         VALUE 'IMY'.                   
010700       88 ENV-XDEV                         VALUE 'IMD'.                   
010800       88 ENV-ACPT                         VALUE 'IMB'.                   
010900       88 ENV-PROD                         VALUE 'IMG'.                   
011000    03 FILLER                    PIC X(5).                                
011100                                                                          
011200*                                                                         
011300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011400*                                                                         
011500 01 FILLER                       PIC X(16) VALUE 'IMS-WS'.                
011600                                                                          
011700 01  KEYS-TILL-DLI.                                                       
011800     03  W-WDGXKEY-X.                                                     
011900         05  W-IDHTYP            PIC X(04)   VALUE '0103'.                
012000         05  FILLER              PIC X(26)   VALUE LOW-VALUES.            
012100     03  W-ADDISPABS-X.                                                   
012200         05  W-ADDISPABS         PIC X(50)   VALUE SPACES.                
012300                                                                          
012400*    --- STATUS CODES FROM IMS                                            
012500 01 STATUS-WS                    PIC XX.                                  
012600    88 SEGMENT-FOUND                       VALUE '  '.                    
012700    88 SEGMENT-FOUND-EXISTS                VALUE 'II'.                    
012800    88 SEGMENT-MISSING                     VALUE 'GE'.                    
012900                                                                          
013000 01 GOOD-STATUS-CODES.                                                    
013100    03 GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX                          
013200                                 PIC XX.                                  
013300                                                                          
013400 01 SSA1                         PIC X(128).                              
013500 01 SSA2                         PIC X(128).                              
013600                                                                          
013700*    --- IMS FUNCTION CODES                                               
013800*01 -COPY W0003.                                                          
013900                                                                          
014000 01 AIB-AREA-START               PIC X(16)   VALUE                        
014100                                 'AIB-AREA-START  '.                      
014200    COPY W0031.                                                           
014300                                                                          
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500                                                                          
014600 01 DLI-IO-WDGX0104.                                                      
014700*   03  -COPY WDGX0104                                                    
014800                                                                          
014900 01 IN-AREA                      PIC X(5000000).                          
015000                                                                          
015100 LINKAGE SECTION.                                                         
015200*01  -COPY W0009   -PRE MSG-.                                             
015300                                                                          
015400*01  -COPY W0008  -PRE ATAB-                                              
015500     05  FILLER                  PIC X.                                   
015600                                                                          
015700 PROCEDURE DIVISION  USING MSG-PCB.                                       
015800 MAIN SECTION.                                                            
015900                                                                          
016000     PERFORM S03-RECEIVE-OPEN                                             
016100                                                                          
016200     PERFORM A-INIT                                                       
016300                                                                          
016400     IF RECV-KDRC = 0                                                     
016500       PERFORM S03-RECEIVE-PROP                                           
016600       IF PROP-IDPTYP = '¤PROP'                                           
016700         PERFORM B-HANDLE-PROPERTIES                                      
016800         IF CONTINUE-YES                                                  
016900           PERFORM S03-RECEIVE-DATA                                       
017000           IF RECV-KDRC = 0                                               
017100             PERFORM C-PROCESS-DATA                                       
017200           END-IF                                                         
017300         END-IF                                                           
017400       ELSE                                                               
017500         MOVE 'F'                TO RECV-KDKOMSTA                         
017600         MOVE 'Missing or Invalid Property Information'                   
017700                                 TO MESSAGE-TEXT                          
017800       END-IF                                                             
017900     END-IF                                                               
018000                                                                          
018100     PERFORM S03-RECEIVE-CLOSE                                            
018200     PERFORM Z-FINIT                                                      
018300                                                                          
018400     MOVE ZERO                   TO RETURN-CODE                           
018500     GOBACK                                                               
018600     .                                                                    
018700                                                                          
018800 A-INIT SECTION.                                                          
018900                                                                          
019000     INITIALIZE RECV-CLOSE-AREA                                           
019100     MOVE SPACES                 TO WS-PROP-TABLE                         
019200                                                                          
019300     CALL VIMSID              USING WS-VIMSID                             
019400                                                                          
019500D    IF TEST-REGION                                                       
019600D      DISPLAY 'START WZ043100 IN ' WS-VIMSID                             
019700D    END-IF                                                               
019800     .                                                                    
019900                                                                          
020000 B-HANDLE-PROPERTIES SECTION.                                             
020100                                                                          
020200     PERFORM                                                              
020300     VARYING PROP-IX FROM 1 BY 1                                          
020400       UNTIL PROP-IX > PROP-KVANTAL                                       
020500       EVALUATE PROP-IDPROPTYPE (PROP-IX)                                 
020600         WHEN 'ADDRESS'                                                   
020700         WHEN 'ADDISPABS'                                                 
020800           MOVE PROP-BEPROPVALUE (PROP-IX)                                
020900                                 TO OUTQ-ADDISPABS                        
021000                                    WS-ADDISPABS                          
021100         WHEN 'MQMPROP'                                                   
021200           SET WS-PIX         UP BY +1                                    
021300           MOVE PROP-IDPROPNAME  (PROP-IX)                                
021400                                 TO WS-PROP-NAME  (WS-PIX)                
021500           MOVE PROP-BEPROPVALUE (PROP-IX)                                
021600                                 TO WS-PROP-VALUE (WS-PIX)                
021700       END-EVALUATE                                                       
021800     END-PERFORM                                                          
021900     SET MAX-PIX                 TO WS-PIX                                
022000     IF WS-ADDISPABS = SPACE OR LOW-VALUES                                
022100       SET CONTINUE-NO           TO TRUE                                  
022200       MOVE 'F'                  TO RECV-KDKOMSTA                         
022300       MOVE 'Invalid value for property ADDISPABS'                        
022400                                 TO MESSAGE-TEXT                          
022500     ELSE                                                                 
022600       MOVE WS-ADDISPABS         TO W-ADDISPABS                           
022700       PERFORM IMS-GU-ADDRESS-INFO                                        
022800       IF SEGMENT-FOUND                                                   
022900         IF 0104-FLCONN = 'J' OR 'Y'                                      
023000           CONTINUE                                                       
023100         ELSE                                                             
023200           SET CONTINUE-NO       TO TRUE                                  
023300           MOVE 'Inactive connection. Will not be processed'              
023400                                 TO MESSAGE-TEXT                          
023500         END-IF                                                           
023600       ELSE                                                               
023700         SET CONTINUE-NO         TO TRUE                                  
023800         MOVE 'F'                TO RECV-KDKOMSTA                         
023900         MOVE 'Config for property ADDISPABS is missing'                  
024000                                 TO MESSAGE-TEXT                          
024100       END-IF                                                             
024200     END-IF                                                               
024300     .                                                                    
024400                                                                          
024500 C-PROCESS-DATA SECTION.                                                  
024600                                                                          
024700     PERFORM S04-OUTQ-OPEN                                                
024800     IF MAX-PIX > 0                                                       
024900       PERFORM CB-SEND-MSGPROP                                            
025000     END-IF                                                               
025100                                                                          
025200     PERFORM                                                              
025300       UNTIL RECV-KDRC > 0                                                
025400       PERFORM CC-PROCESS-MESSAGE                                         
025500       PERFORM S03-RECEIVE-DATA                                           
025600     END-PERFORM                                                          
025700                                                                          
025800     PERFORM S04-OUTQ-SEND                                                
025900     PERFORM S04-OUTQ-CLOSE                                               
026000                                                                          
026100     .                                                                    
026200                                                                          
026300  CB-SEND-MSGPROP SECTION.                                                
026400                                                                          
026500     PERFORM                                                              
026600     VARYING WS-PIX FROM 1 BY 1                                           
026700       UNTIL WS-PIX > MAX-PIX                                             
026800       IF WS-PROP-NAME  (WS-PIX) = SPACES OR                              
026900          WS-PROP-VALUE (WS-PIX) = SPACES                                 
027000         CONTINUE                                                         
027100       ELSE                                                               
027200         MOVE FUNCTION TRIM (WS-PROP-NAME  (WS-PIX))                      
027300                                 TO OUTQ-PROPERTY-NAME                    
027400         MOVE FUNCTION TRIM (WS-PROP-VALUE (WS-PIX))                      
027500                                 TO OUTQ-PROPERTY-VALUE                   
027600         PERFORM S04-SET-PROP                                             
027700       END-IF                                                             
027800     END-PERFORM                                                          
027900     .                                                                    
028000                                                                          
028100  CC-PROCESS-MESSAGE  SECTION.                                            
028200                                                                          
028300     MOVE RECV-KVDLEN            TO OUTQ-KVDLEN                           
028400     MOVE IN-AREA (1:RECV-KVDLEN)                                         
028500                                 TO OUTQ-DATA (1:OUTQ-KVDLEN)             
028600     PERFORM S04-PUT-MESSAGE                                              
028700     .                                                                    
028800                                                                          
028900 Z-FINIT SECTION.                                                         
029000     CONTINUE                                                             
029100     .                                                                    
029200                                                                          
029300 S03-RECEIVE-OPEN SECTION.                                                
029400                                                                          
029500     MOVE 'OPEN'                 TO RECV-KDFUNC                           
029600     MOVE WS-MY-OWN-ADRESS       TO RECV-ADDISPABS                        
029700     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
029800                                    RECV-OPEN-AREA                        
029900                                                                          
030000     IF RECV-KDRC > 0                                                     
030100       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
030200       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
030300             DELIMITED BY SIZE INTO ERROR-TEXT                            
030400       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
030500     END-IF                                                               
030600     .                                                                    
030700                                                                          
030800 S03-RECEIVE-PROP SECTION.                                                
030900                                                                          
031000*    -- TRUNCATION IS ALLOWED TO BE ABLE TO HANDLE TRAILING               
031100*    -- BLANKS IN HEADER RECORD. SHORTER RECORDS ARE ALSO                 
031200*    -- ACCEPTED AND MISSING FIELDS ARE ASSUMED TO BE = SPACE.            
031300                                                                          
031400     MOVE 'GET'                  TO RECV-KDFUNC                           
031500*    INITIALIZE PROP-WZ04PROP TO VALUE                                    
031600     MOVE 10                     TO PROP-KVANTAL                          
031700     MOVE LENGTH OF PROP-WZ04PROP                                         
031800                                 TO RECV-KVDLEN                           
031900     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
032000                                    RECV-KVDLEN                           
032100                                    PROP-WZ04PROP                         
032200                                                                          
032300     IF RECV-KDRC > 1 AND NOT = 21                                        
032400       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
032500       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
032600             DELIMITED BY SIZE INTO ERROR-TEXT                            
032700       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
032800     ELSE                                                                 
032900       MOVE RECV-KVDLEN          TO WS-RECV-KVDLEN                        
033000     END-IF                                                               
033100     .                                                                    
033200                                                                          
033300 S03-RECEIVE-DATA SECTION.                                                
033400                                                                          
033500     MOVE 'GET'                  TO RECV-KDFUNC                           
033600     MOVE LENGTH OF IN-AREA      TO RECV-KVDLEN                           
033700     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
033800                                    RECV-KVDLEN                           
033900                                    IN-AREA                               
034000                                                                          
034100     IF RECV-KDRC > 1                                                     
034200       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
034300       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
034400             DELIMITED BY SIZE INTO ERROR-TEXT                            
034500       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
034600     ELSE                                                                 
034700       MOVE RECV-KVDLEN          TO WS-RECV-KVDLEN                        
034800     END-IF                                                               
034900     .                                                                    
035000                                                                          
035100 S03-RECEIVE-CLOSE SECTION.                                               
035200                                                                          
035300     IF RECV-KDKOMSTA = 'F'                                               
035400       CONTINUE                                                           
035500     ELSE                                                                 
035600       MOVE 'A'                  TO RECV-KDKOMSTA                         
035700     END-IF                                                               
035800     MOVE MESSAGE-TEXT-GRP       TO RECV-MESSAGE                          
035900     COMPUTE RECV-MESSAGE-KVDLEN = FUNCTION LENGTH (                      
036000                                     FUNCTION TRIM (RECV-MESSAGE))        
036100                                                                          
036200     MOVE 'CLOSE'                TO RECV-KDFUNC                           
036300     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
036400                                    RECV-CLOSE-AREA                       
036500                                                                          
036600     IF RECV-KDRC > 0                                                     
036700       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
036800       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
036900             DELIMITED BY SIZE INTO ERROR-TEXT                            
037000       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
037100     END-IF                                                               
037200     .                                                                    
037300                                                                          
037400                                                                          
037500 S04-OUTQ-OPEN  SECTION.                                                  
037600                                                                          
037700     MOVE 'OPENONL'              TO OUTQ-KDFUNC                           
037800     MOVE ZERO                   TO OUTQ-KVDLEN                           
037900     CALL WZ11OUTQ            USING OUTQ-AREA                             
038000                                                                          
038100     IF OUTQ-KDRC > 0                                                     
038200       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
038300       STRING 'WZ11OUTQ OPEN ERROR RC = ' KDRC-DISPLAY                    
038400         DELIMITED BY SIZE     INTO ERROR-TEXT                            
038500       CALL ABEND                                                         
038600     END-IF                                                               
038700     .                                                                    
038800                                                                          
038900 S04-SET-PROP SECTION.                                                    
039000                                                                          
039100     MOVE 'SETPROP'              TO OUTQ-KDFUNC                           
039200     MOVE ZERO                   TO OUTQ-KVDLEN                           
039300     CALL WZ11OUTQ            USING OUTQ-AREA                             
039400                                                                          
039500     IF OUTQ-KDRC > 0                                                     
039600       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
039700       STRING 'WZ11OUTQ INQPROP ERROR RC = ' KDRC-DISPLAY                 
039800         DELIMITED BY SIZE     INTO ERROR-TEXT                            
039900       CALL ABEND                                                         
040000     ELSE                                                                 
040100       DISPLAY OUTQ-PROPERTY-NAME '=' OUTQ-PROPERTY-VALUE                 
040200     END-IF                                                               
040300                                                                          
040400     .                                                                    
040500 S04-PUT-MESSAGE  SECTION.                                                
040600*    -- PUT THE MESSAGE                                                   
040700     MOVE 'PUT'                  TO OUTQ-KDFUNC                           
040800                                                                          
040900     CALL WZ11OUTQ            USING OUTQ-AREA                             
041000     IF OUTQ-KDRC > 0                                                     
041100       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
041200       STRING 'WZ11OUTQ PUT ERROR RC = ' KDRC-DISPLAY                     
041300         DELIMITED BY SIZE     INTO ERROR-TEXT                            
041400       CALL ABEND                                                         
041500     END-IF                                                               
041600     .                                                                    
041700                                                                          
041800 S04-OUTQ-SEND SECTION.                                                   
041900     MOVE 'SEND'                 TO OUTQ-KDFUNC                           
042000     MOVE ZERO                   TO OUTQ-KVDLEN                           
042100     CALL WZ11OUTQ            USING OUTQ-AREA                             
042200                                                                          
042300     IF OUTQ-KDRC > 0                                                     
042400       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
042500       STRING 'WZ11OUTQ CLOSE ERROR RC = ' KDRC-DISPLAY                   
042600         DELIMITED BY SIZE     INTO ERROR-TEXT                            
042700       CALL ABEND                                                         
042800     END-IF                                                               
042900                                                                          
043000     .                                                                    
043100 S04-OUTQ-CLOSE  SECTION.                                                 
043200     MOVE 'CLOSE'                TO OUTQ-KDFUNC                           
043300     MOVE ZERO                   TO OUTQ-KVDLEN                           
043400     CALL WZ11OUTQ            USING OUTQ-AREA                             
043500                                                                          
043600     IF OUTQ-KDRC > 0                                                     
043700       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
043800       STRING 'WZ11OUTQ CLOSE ERROR RC = ' KDRC-DISPLAY                   
043900         DELIMITED BY SIZE     INTO ERROR-TEXT                            
044000       CALL ABEND                                                         
044100     END-IF                                                               
044200                                                                          
044300     .                                                                    
044400                                                                          
044500 IMS-GU-ADDRESS-INFO SECTION.                                             
044600                                                                          
044700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
044800              DELIMITED BY SIZE INTO SSA1                                 
044900     STRING 'WDGX0104(ADDISPAB =' W-ADDISPABS-X ')'                       
045000              DELIMITED BY SIZE INTO SSA2                                 
045100                                                                          
045200     MOVE 128                    TO AIB-LEN                               
045300     MOVE SPACES                 TO AIB-SUB-FUNCTION                      
045400     MOVE 'ATAB'                 TO AIB-PCB-NAME                          
045500     MOVE LENGTH OF DLI-IO-WDGX0104                                       
045600                                 TO AIB-IOAREA-LENGTH                     
045700                                                                          
045800     MOVE '  GE'                 TO GOOD-STATUS-CODES                     
045900                                                                          
046000     CALL AIBTDLI             USING GU                                    
046100                                    AIB-AREA                              
046200                                    DLI-IO-WDGX0104                       
046300                                    SSA1                                  
046400                                    SSA2                                  
046500                                                                          
046600     SET ADDRESS OF ATAB-PCB     TO AIB-PCB-PTR                           
046700     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
046800     PERFORM IMS-STATUS-CHECK                                             
046900     .                                                                    
047000                                                                          
047100 IMS-STATUS-CHECK SECTION.                                                
047200                                                                          
047300     IF STATUS-WS = LOW-VALUE                                             
047400       STRING ' CANNOT FIND PCB WITH NAME: ATAB IN THE PSB'               
047500             DELIMITED BY SIZE INTO ERROR-TEXT                            
047600       DISPLAY IDPGM ' ' ERROR-TEXT                                       
047700       CALL FELLOG                                                        
047800     ELSE                                                                 
047900       SET STATUS-IX             TO 1                                     
048000       SEARCH GOOD-STATUS                                                 
048100         AT END                                                           
048200           STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS             
048300             DELIMITED BY SIZE INTO ERROR-TEXT                            
048400           DISPLAY IDPGM ' ' ERROR-TEXT                                   
048500           CALL FELLOG                                                    
048600         WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                         
048700           CONTINUE                                                       
048800       END-SEARCH                                                         
048900     END-IF                                                               
049000     .                                                                    
