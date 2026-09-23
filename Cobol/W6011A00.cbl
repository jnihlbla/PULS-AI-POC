000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011A00.                                                
000300 AUTHOR.         DILEEPKUMAR THAKKALA.                                    
000400 DATE-WRITTEN.   26/03/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       PULS.CARPARTS.GOODS.CARRIER                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        GOODS RECEIVING BACKGROUND PROCESSING                            
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W6011AX                                             
001500*        REQUEST:     XML FROM FLS                                        
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        TO DISPATCHER                                                    
001900*        RESPONSE:    W6I11501                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W6011A00'.            
003400                                                                          
003500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700 77  KDRC-DISPLAY                PIC Z(5).                                
003800 77  WS-LINE                     PIC X(251).                              
003900 77  ERROR-POINT                 PIC 9(9)    VALUE ZERO.                  
004000                                                                          
004100 01  WS-TEXT-LEN                 PIC 9(5)    VALUE ZERO.                  
004200 01  WS-DATA-LEN                 PIC 9(5)    VALUE ZERO.                  
004300 01  WS-POS                      PIC 9(5)    VALUE 1.                     
004400 01  WS-RECV-XML                 PIC X(5000) VALUE SPACES.                
004500                                                                          
004600 77  WS-ADRESS-FLS               PIC X(50)   VALUE                        
004700                                       'APIOUT.FLS.GCERRORNOTIFY'.        
004800                                                                          
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005400     88  KEYS-OK                             VALUE 'J'.                   
005500     88  KEYS-WRONG                          VALUE 'N'.                   
005600     EJECT                                                                
005700 77  EXCEPTION-SW                PIC X       VALUE 'N'.                   
005800     88  EXCEPTION-YES                       VALUE 'J'.                   
005900     88  EXCEPTION-NO                        VALUE 'N'.                   
006000 01  EVENT-SW                    PIC X(50)   VALUE SPACES.                
006100     88  EVENT-GH-ARRIVAL            VALUE 'ARRIVED AT GATEHOUSE'.        
006200     88  EVENT-YARD-ARRIVAL          VALUE 'YARD ARRIVAL'.                
006300 01  ERROR-SW                    PIC X       VALUE SPACES.                
006400     88  ERROR-NONE                  VALUE ' '.                           
006500     88  ERROR-FEL-ASN               VALUE 'F'.                           
006600     88  ERROR-MISSING-ASN           VALUE 'M'.                           
006700 77  WS-XML-TEXT                 PIC X(300)  VALUE SPACES.                
006800 77  WS-XML-BUFF                 PIC X(300)  VALUE SPACES.                
006900 01  EL                          PIC S9(4) COMP.                          
007000 01  ELEMENT-GROUP.                                                       
007100     03  WS-CURR-ELEMENT         PIC X(50) OCCURS 20 TIMES                
007200                                           VALUE SPACES.                  
007300 01  IX                          PIC 99.                                  
007400 01  IX-MAX                      PIC 99    VALUE 10.                      
007500 01  MESSAGE-TEXT-GRP.                                                    
007600     03  MESSAGE-TEXT            PIC X(200)  OCCURS 10 TIMES              
007700                                             VALUE SPACES.                
007800 01  WS-SEND-AREA                PIC X(200)  VALUE SPACES.                
007900 01  WS-FORMATTED-TIMESTAMP      PIC X(23).                               
008000                                                                          
008100 01  WS-VARIABLES.                                                        
008200     03  WS-GCID                 PIC X(100)  VALUE SPACES.                
008300     03  WS-EVENT                PIC X(50)   VALUE SPACES.                
008400     03  WS-LOC-TYPE             PIC X(12)   VALUE SPACES.                
008500     03  WS-LOC                  PIC X(05)   VALUE SPACES.                
008600     03  WS-BOOKING-REF          PIC X(12)   VALUE SPACES.                
008700     03  WS-SUPPLIER             PIC X(05)   VALUE SPACES.                
008800     03  WS-ASN                  PIC X(08)   VALUE SPACES.                
008900     03  WS-RECEIVER             PIC X(05)   VALUE SPACES.                
009000     03  WS-TIAVIDAT-YYYY        PIC X(04)   VALUE SPACES.                
009100     03  WS-TIAVIDAT             PIC  9(6).                               
009200     03  WS-TIAVIDAT-X REDEFINES WS-TIAVIDAT.                             
009300         05  WS-TIAVIDAT-YY      PIC X(2).                                
009400         05  WS-TIAVIDAT-MM      PIC X(2).                                
009500         05  WS-TIAVIDAT-DD      PIC X(2).                                
009600*                                                                         
009700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009800 01  GENERAL-SUBPROGRAMS.                                                 
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010300     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
010400     03  W6011500                PIC X(8)    VALUE 'W6011500'.            
010500     SKIP3                                                                
010600*    --- PARAMETERS TO ABEND                                              
010700                                                                          
010800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011100     SKIP3                                                                
011200 01  MESSAGE-CODES.                                                       
011300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
011600     SKIP3                                                                
011700*01  -COPY WMSGAREA                                                       
011800*                                                                         
011900 01  P-TO-P-SW.                                                           
012000     03  PTOP-LL                 PIC S9(4)   VALUE +0 COMP SYNC.          
012100     03  PTOP-Z1                 PIC X       VALUE LOW-VALUE.             
012200     03  PTOP-Z2                 PIC X       VALUE LOW-VALUE.             
012300     03  PTOP-TRANSKOD           PIC X(7)    VALUE 'W6T115X'.             
012400     03  FILLER                  PIC X       VALUE SPACE.                 
012500     03  FILLER                  PIC X(4)    VALUE '611A'.                
012600     03  PTOP-KDMFSFOR           PIC X       VALUE '1'.                   
012700*    03  -COPY W6I11501 -PRE 6115-                                        
012800 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
012900     SKIP3                                                                
013000*01  -COPY WZ01RECV                                                       
013100 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
013200 01  RECV-AREA                   PIC X(5000) VALUE SPACES.                
013300                                                                          
013400*01  -COPY WZ01SEND                                                       
013500 01  HDR-AREA.                                                            
013600*    03  -COPY WZ01REQU                                                   
013700*    03  -COPY WZ04HDR                                                    
013800                                                                          
013900*01  -COPY WAPIINFO                                                       
014000                                                                          
014100 01  FILLER                      PIC X(16)   VALUE 'WGC-ERR-DATA'.        
014200 01  WGC-ERR-DATA.                                                        
014300*03  -COPY WGC00Q01 -PRE WGC-                                             
014400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  KEYS-FOR-DLI.                                                        
014900     SKIP2                                                                
015000*--------W6D1ASEQ                                                         
015100     03  W-W6D1ASEQ-MIN-X.                                                
015200         05  W-INL-IDDC-MIN        PIC X(2)   VALUE SPACE.                
015300         05  W-INL-IDLEVNR-MIN     PIC X(5)   VALUE SPACE.                
015400         05  W-INL-IDFS-MIN        PIC X(8)   VALUE SPACE.                
015500         05  W-INL-TIAVIDAT-MIN    PIC S9(7)  VALUE ZERO COMP-3.          
015600         05  FILLER                PIC X(12)  VALUE LOW-VALUES.           
015700                                                                          
015800     03  W-W6D1ASEQ-MAX-X.                                                
015900         05  W-INL-IDDC-MAX        PIC X(2)   VALUE SPACE.                
016000         05  W-INL-IDLEVNR-MAX     PIC X(5)   VALUE SPACE.                
016100         05  W-INL-IDFS-MAX        PIC X(8)   VALUE SPACE.                
016200         05  W-INL-TIAVIDAT-MAX    PIC S9(7)  VALUE ZERO COMP-3.          
016300         05  FILLER                PIC X(12)  VALUE HIGH-VALUES.          
016400                                                                          
016500     03  W-WDGXKEY-0103-X.                                                
016600         05  W-IDHTYP-0103         PIC X(4)    VALUE '0103'.              
016700         05  FILLER                PIC X(26)   VALUE LOW-VALUE.           
016800     03  W-KY0104-X.                                                      
016900         05  W-ADDISPABS           PIC X(50)    VALUE SPACES.             
017000                                                                          
017100*    --- STATUS-KOD FRÅN IMS                                              
017200 01  STATUS-WS                   PIC XX.                                  
017300     88  SEGMENT-FOUND                       VALUE '  '.                  
017400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017600     SKIP2                                                                
017700 01  GOOD-STATUSCODES.                                                    
017800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(128).                              
018100 01  SSA2                        PIC X(128).                              
018200     EJECT                                                                
018300*    --- IMS FUNCTION CODES                                               
018400*01  -COPY W0003                                                          
018500     EJECT                                                                
018600*    ---  DLI INPUT-OUTPUT AREA                                           
018700 01  FILLER                      PIC X(16)   VALUE 'W6D101-AREA'.         
018800 01  DLI-IO-W6D101.                                                       
018900*    03  -COPY W6D101                                                     
019000                                                                          
019100 01  DLI-IO-WDGX0104.                                                     
019200*    03  -COPY WDGX0104                                                   
019300                                                                          
019400 LINKAGE SECTION.                                                         
019500*01  -COPY W0009   -PRE MSG-                                              
019600*01  -COPY W0009   -PRE ALT-                                              
019700 01  DISTRDOC-PCB                PIC X.                                   
019800 01  GC-ERROR-PCB                PIC X.                                   
019900*01  -COPY W0008   -PRE ATAB-                                             
020000     05  FILLER                  PIC X.                                   
020100*01  -COPY W0008   -PRE W6D1ASEQ-                                         
020200     05  FILLER                  PIC X.                                   
020300                                                                          
020400 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB                               
020500                           DISTRDOC-PCB GC-ERROR-PCB                      
020600                           ATAB-PCB W6D1ASEQ-PCB.                         
020700 MAIN SECTION.                                                            
020800                                                                          
020900     PERFORM S01-RECEIVE-OPEN                                             
021000     PERFORM S02-RECEIVE-FULL-MESSAGE                                     
021100                                                                          
021200     PERFORM A-INIT                                                       
021300     PERFORM B-CHECK-KEYS                                                 
021400                                                                          
021500     IF KEYS-OK                                                           
021600       PERFORM C-PARSE-XML                                                
021700       PERFORM D-PROCESS-GCID                                             
021800     END-IF                                                               
021900                                                                          
022000     PERFORM S03-RECEIVE-CLOSE                                            
022100                                                                          
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INIT SECTION.                                                          
022700     CONTINUE                                                             
022800     .                                                                    
022900     EJECT                                                                
023000 B-CHECK-KEYS SECTION.                                                    
023100                                                                          
023200     MOVE JA  TO KEYS-SW                                                  
023300     .                                                                    
023400     EJECT                                                                
023500 C-PARSE-XML SECTION.                                                     
023600                                                                          
023700     XML PARSE WS-RECV-XML (1 : WS-DATA-LEN)                              
023800       PROCESSING PROCEDURE CA-HANDLE-PARSE                               
023900       ON EXCEPTION                                                       
024000         PERFORM CB-HANDLE-EXCEPTION                                      
024100     END-XML                                                              
024200     .                                                                    
024300 CA-HANDLE-PARSE SECTION.                                                 
024400                                                                          
024500     COMPUTE WS-TEXT-LEN = FUNCTION LENGTH (XML-TEXT)                     
024600     MOVE SPACES                 TO WS-XML-TEXT                           
024700     MOVE XML-TEXT               TO WS-XML-TEXT                           
024800                                                                          
024900     EVALUATE XML-EVENT                                                   
025000                                                                          
025100       WHEN 'START-OF-ELEMENT'                                            
025200           ADD 1                 TO EL                                    
025300           MOVE 1                TO WS-POS                                
025400           MOVE FUNCTION UPPER-CASE (WS-XML-TEXT)                         
025500                                 TO WS-CURR-ELEMENT(EL)                   
025600           MOVE SPACES           TO WS-XML-BUFF                           
025700       WHEN 'CONTENT-CHARACTERS'                                          
025800         PERFORM CAA-CONTENT-CHARS                                        
025900                                                                          
026000       WHEN 'START-OF-DOCUMENT'                                           
026100         MOVE 0                  TO EL                                    
026200       WHEN 'END-OF-ELEMENT'                                              
026300         MOVE SPACES             TO WS-CURR-ELEMENT (EL)                  
026400                                    WS-XML-BUFF                           
026500         SUBTRACT 1            FROM EL                                    
026600       WHEN OTHER                                                         
026700           CONTINUE                                                       
026800                                                                          
026900     END-EVALUATE                                                         
027000     .                                                                    
027100                                                                          
027200 CAA-CONTENT-CHARS SECTION.                                               
027300                                                                          
027400     IF XML-INFORMATION = 2                                               
027500*      There is more data to come for the same element.                   
027600       PERFORM CAAA-HANDLE-XML-TEXT                                       
027700     ELSE                                                                 
027800       PERFORM CAAA-HANDLE-XML-TEXT                                       
027900       EVALUATE WS-CURR-ELEMENT(EL)                                       
028000                                                                          
028100         WHEN 'NS2:GCID'                                                  
028200         WHEN 'GCID'                                                      
028300           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
028400                                      TO WS-GCID                          
028500                                                                          
028600         WHEN 'NS2:EVENT'                                                 
028700         WHEN 'EVENT'                                                     
028800           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
028900                                      TO WS-EVENT                         
029000                                                                          
029100         WHEN 'NS2:LOCATIONTYPE'                                          
029200         WHEN 'LOCATIONTYPE'                                              
029300           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
029400                                      TO WS-LOC-TYPE                      
029500                                                                          
029600         WHEN 'NS2:LOCATION'                                              
029700         WHEN 'LOCATION'                                                  
029800           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
029900                                      TO WS-LOC                           
030000                                                                          
030100         WHEN 'NS2:BOOKINGREFERENCE'                                      
030200         WHEN 'BOOKINGREFERENCE'                                          
030300                                                                          
030400           MOVE WS-XML-BUFF (1 : WS-POS - 1)                              
030500                                      TO WS-BOOKING-REF                   
030600                                                                          
030700       END-EVALUATE                                                       
030800       MOVE SPACES                    TO WS-XML-BUFF                      
030900     END-IF                                                               
031000     .                                                                    
031100                                                                          
031200 CAAA-HANDLE-XML-TEXT SECTION.                                            
031300                                                                          
031400     STRING WS-XML-TEXT (1 : WS-TEXT-LEN) DELIMITED BY SIZE               
031500                                    INTO WS-XML-BUFF                      
031600                            WITH POINTER WS-POS                           
031700     .                                                                    
031800                                                                          
031900 CB-HANDLE-EXCEPTION SECTION.                                             
032000                                                                          
032100     SET EXCEPTION-YES           TO TRUE                                  
032200     MOVE SPACES                 TO WS-XML-TEXT                           
032300     MOVE XML-TEXT               TO WS-XML-TEXT                           
032400     ADD 1 TO LENGTH OF XML-TEXT GIVING ERROR-POINT                       
032500     MOVE 'PROCESSING ...'                                                
032600                                 TO WS-LINE                               
032700     DISPLAY WS-LINE                                                      
032800                                                                          
032900     MOVE '!!! AN EXCEPTION OCCURED WHILE PARSING INPUT XML !!!'          
033000                                 TO WS-LINE                               
033100     DISPLAY WS-LINE                                                      
033200     MOVE SPACES                 TO WS-LINE                               
033300                                                                          
033400     STRING 'EXCEPTION AT POSITION ' ERROR-POINT                          
033500             DELIMITED BY SIZE INTO WS-LINE                               
033600     DISPLAY WS-LINE                                                      
033700     MOVE SPACES                 TO WS-LINE                               
033800                                                                          
033900     STRING 'EXCEPTION CODE (XML-CODE) = '                                
034000             FUNCTION HEX-OF (XML-CODE)                                   
034100             DELIMITED BY SIZE INTO WS-LINE                               
034200     DISPLAY WS-LINE                                                      
034300     MOVE SPACES                 TO WS-LINE                               
034400     CALL ABEND USING RKOD-ABEND-WITH-DUMP                                
034500     .                                                                    
034600                                                                          
034700 D-PROCESS-GCID SECTION.                                                  
034800                                                                          
034900     UNSTRING WS-GCID DELIMITED BY '-'   INTO WS-SUPPLIER                 
035000                                              WS-TIAVIDAT-YYYY            
035100                                              WS-ASN                      
035200                                              WS-RECEIVER                 
035300                                                                          
035400     MOVE FUNCTION UPPER-CASE(WS-EVENT)    TO EVENT-SW                    
035500     SET ERROR-NONE                        TO TRUE                        
035600                                                                          
035700     IF (EVENT-GH-ARRIVAL OR EVENT-YARD-ARRIVAL) AND                      
035800        FUNCTION UPPER-CASE(WS-RECEIVER) = 'BP2TW'                        
035900                                                                          
036000       PERFORM DA-READ-W6D1                                               
036100       IF SEGMENT-FOUND                                                   
036200         IF INL-FLFEL = 'N'                                               
036300           IF EVENT-YARD-ARRIVAL                                          
036400             PERFORM F-CALL-W60115                                        
036500           END-IF                                                         
036600         ELSE                                                             
036700           SET ERROR-FEL-ASN               TO TRUE                        
036800           PERFORM DB-HANDLE-ERROR                                        
036900         END-IF                                                           
037000       ELSE                                                               
037100         SET ERROR-MISSING-ASN             TO TRUE                        
037200         PERFORM DB-HANDLE-ERROR                                          
037300       END-IF                                                             
037400     END-IF                                                               
037500     .                                                                    
037600                                                                          
037700 DA-READ-W6D1 SECTION.                                                    
037800                                                                          
037900     MOVE LOW-VALUE            TO W-W6D1ASEQ-MIN-X                        
038000     MOVE HIGH-VALUE           TO W-W6D1ASEQ-MAX-X                        
038100                                                                          
038200     IF WS-RECEIVER = 'BP2TW' OR '1441'                                   
038300                                                                          
038400        MOVE '11'              TO W-INL-IDDC-MIN                          
038500                                  W-INL-IDDC-MAX                          
038600     END-IF                                                               
038700                                                                          
038800     MOVE WS-SUPPLIER          TO W-INL-IDLEVNR-MIN                       
038900                                  W-INL-IDLEVNR-MAX                       
039000                                                                          
039100     MOVE WS-ASN               TO W-INL-IDFS-MIN                          
039200                                  W-INL-IDFS-MAX                          
039300                                                                          
039400     MOVE WS-TIAVIDAT-YYYY(3:) TO WS-TIAVIDAT-YY                          
039500     MOVE '01'                 TO WS-TIAVIDAT-MM                          
039600     MOVE '01'                 TO WS-TIAVIDAT-DD                          
039700     MOVE WS-TIAVIDAT          TO W-INL-TIAVIDAT-MIN                      
039800                                                                          
039900     MOVE '12'                 TO WS-TIAVIDAT-MM                          
040000     MOVE '31'                 TO WS-TIAVIDAT-DD                          
040100     MOVE WS-TIAVIDAT          TO W-INL-TIAVIDAT-MAX                      
040200                                                                          
040300     PERFORM IMS-GU-W6D101-ASEQ                                           
040400     .                                                                    
040500                                                                          
040600 DB-HANDLE-ERROR SECTION.                                                 
040700                                                                          
040800     INITIALIZE MESSAGE-TEXT-GRP                                          
040900     IF EVENT-YARD-ARRIVAL                                                
041000       IF ERROR-FEL-ASN                                                   
041100         STRING 'Yard arrival event received for GC ID : '                
041200                  WS-GCID '.'                                             
041300                       DELIMITED BY SIZE INTO MESSAGE-TEXT (1)            
041400         STRING 'Event cannot be processed.'                              
041500                       DELIMITED BY SIZE INTO MESSAGE-TEXT (2)            
041600         STRING 'Correct errors and activate the preadvice '              
041700                  'note in PULS instead.'                                 
041800                       DELIMITED BY SIZE INTO MESSAGE-TEXT (3)            
041900       ELSE                                                               
042000         STRING 'Yard arrival event received for GC ID : '                
042100                       DELIMITED BY SIZE                                  
042200                WS-GCID DELIMITED BY SPACE                                
042300                  '.'  DELIMITED BY SIZE                                  
042400                                         INTO MESSAGE-TEXT (1)            
042500         STRING 'Event cannot be processed.'                              
042600                       DELIMITED BY SIZE INTO MESSAGE-TEXT (2)            
042700         STRING 'Advice note is not available for receiving.'             
042800                       DELIMITED BY SIZE INTO MESSAGE-TEXT (3)            
042900         STRING 'Is either missing or already processed.'                 
043000                       DELIMITED BY SIZE INTO MESSAGE-TEXT (4)            
043100         STRING 'Check in PULS'                                           
043200                       DELIMITED BY SIZE INTO MESSAGE-TEXT (5)            
043300       END-IF                                                             
043400                                                                          
043500       MOVE 'GCEVENT-ERROR'                TO HDR-IDOUTTYPE               
043600       MOVE WS-GCID                        TO HDR-IDOUTREC                
043700       PERFORM S20-SEND-ERROR                                             
043800     END-IF                                                               
043900                                                                          
044000     IF EVENT-GH-ARRIVAL                                                  
044100*      Trigger notification to FLS if we have FLFEL = 'J' or if           
044200*      GC is missing or already received.                                 
044300       MOVE WS-ADRESS-FLS         TO W-ADDISPABS                          
044400       PERFORM IMS-GU-WDGX0104                                            
044500       IF SEGMENT-FOUND                                                   
044600         IF 0104-FLCONN = YES OR JA                                       
044700           PERFORM DBA-MOVE-ERROR-DTLS                                    
044800           PERFORM S31-SEND-OPEN                                          
044900           PERFORM S32-SEND-PUT-HEADER                                    
045000           PERFORM S33-SEND-PUT-DATA                                      
045100           PERFORM S34-SEND-CLOSE                                         
045200         END-IF                                                           
045300       END-IF                                                             
045400     END-IF                                                               
045500     .                                                                    
045600                                                                          
045700 DBA-MOVE-ERROR-DTLS SECTION.                                             
045800                                                                          
045900     MOVE 0104-IDUSERKEY          TO WGC-USER-KEY                         
046000     COMPUTE WGC-USER-KEY-LENGTH =                                        
046100          FUNCTION LENGTH(FUNCTION TRIM (WGC-USER-KEY))                   
046200                                                                          
046300     UNSTRING WS-GCID DELIMITED BY '-'                                    
046400                                INTO WGC-MFG                              
046500                                     WGC-ISSUEDATEYEAR                    
046600                                     WGC-DELIVERYNOTENUMBER               
046700                                     WGC-SHIPTO                           
046800     COMPUTE WGC-MFG-LENGTH =                                             
046900          FUNCTION LENGTH(FUNCTION TRIM (WGC-MFG))                        
047000                                                                          
047100     COMPUTE WGC-DELIVERYNOTENUMBER-LENGTH =                              
047200          FUNCTION LENGTH(FUNCTION TRIM (WGC-DELIVERYNOTENUMBER))         
047300                                                                          
047400     COMPUTE WGC-ISSUEDATEYEAR-LENGTH =                                   
047500          FUNCTION LENGTH(FUNCTION TRIM (WGC-ISSUEDATEYEAR))              
047600                                                                          
047700     COMPUTE WGC-SHIPTO-LENGTH =                                          
047800          FUNCTION LENGTH(FUNCTION TRIM (WGC-SHIPTO))                     
047900                                                                          
048000     MOVE 1                        TO WGC-ERRORS2-NUM                     
048100     IF ERROR-FEL-ASN                                                     
048200       MOVE 'E01'                  TO WGC-ERRORCODE(1)                    
048300       COMPUTE WGC-ERRORCODE-LENGTH(1) =                                  
048400            FUNCTION LENGTH(FUNCTION TRIM(WGC-ERRORCODE(1)))              
048500                                                                          
048600       MOVE 'Error in ASN processing. Update required in PULS'            
048700                               TO WGC-ERRORMESSAGE(1)                     
048800     ELSE                                                                 
048900       MOVE 'E02'                  TO WGC-ERRORCODE(1)                    
049000       COMPUTE WGC-ERRORCODE-LENGTH(1) =                                  
049100            FUNCTION LENGTH(FUNCTION TRIM(WGC-ERRORCODE(1)))              
049200                                                                          
049300       MOVE 'ASN is missing or previously received! Check in PULS'        
049400                               TO WGC-ERRORMESSAGE(1)                     
049500     END-IF                                                               
049600     COMPUTE WGC-ERRORMESSAGE-LENGTH(1) =                                 
049700            FUNCTION LENGTH(FUNCTION TRIM(WGC-ERRORMESSAGE(1)))           
049800     .                                                                    
049900                                                                          
050000 F-CALL-W60115 SECTION.                                                   
050100                                                                          
050200     COMPUTE PTOP-LL = LENGTH OF 6115-MID-W6I11501 + 17                   
050300                                                                          
050400     MOVE ALL '+'           TO 6115-MID-W6I11501                          
050500                                                                          
050600     MOVE INL-IDDC          TO 6115-MID-IDDC-UT                           
050700     MOVE INL-IDLEVNR       TO 6115-MID-IDLEVNR-UT                        
050800                               6115-MID-IDLEVNR-RAD(1)                    
050900                                                                          
051000     MOVE SPACES            TO WS-TIAVIDAT-X                              
051100     MOVE INL-TIAVIDAT      TO WS-TIAVIDAT                                
051200     MOVE WS-TIAVIDAT-X     TO 6115-MID-TIAVIDAT-UT                       
051300                               6115-MID-TIAVIDAT-RAD(1)                   
051400                                                                          
051500     MOVE INL-IDFS          TO 6115-MID-IDFS-UT                           
051600                               6115-MID-IDFS-RAD(1)                       
051700     MOVE 'S  '             to 6115-MID-KDCMDVAL-RAD(1)                   
051800     MOVE WS-BOOKING-REF    TO 6115-MID-IDLBBET-UT                        
051900                               6115-MID-IDLBBET-UPD                       
052000     MOVE 'GM3'             TO 6115-MID-ADINLOMR-PRT-IN                   
052100*    MOVE ALL '+'           TO 6115-MID-ADINLOMR-PRT-IN                   
052200     MOVE 'N'               TO 6115-MID-FLKLAR-UPD                        
052300     MOVE ALL '+'           TO 6115-MID-ADINLOMR-LPL-UPD                  
052400                                                                          
052500     PERFORM IMS-INSERT-MSG-ALT                                           
052600     .                                                                    
052700                                                                          
052800*    --- WZ01RECV   SECTIONS                                              
052900 S01-RECEIVE-OPEN SECTION.                                                
053000                                                                          
053100     MOVE 'OPEN'                      TO RECV-KDFUNC                      
053200     MOVE 'PULS.CARPARTS.GOODS.CARRIER'      TO RECV-ADDISPABS            
053300                                                                          
053400     CALL WZ01RECV USING RECV-CONTROL-AREA                                
053500                         RECV-OPEN-AREA                                   
053600                                                                          
053700     IF RECV-KDRC > 0 AND NOT = 20                                        
053800       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
053900       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
054000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
054200     END-IF                                                               
054300     .                                                                    
054400     SKIP3                                                                
054500 S02-RECEIVE-FULL-MESSAGE SECTION.                                        
054600                                                                          
054700     MOVE 0                          TO WS-DATA-LEN                       
054800     MOVE 1                          TO WS-POS                            
054900     PERFORM S02A-RECEIVE-MESSAGE                                         
055000     PERFORM                                                              
055100       UNTIL RECV-KDRC > 0                                                
055200       COMPUTE WS-DATA-LEN = WS-DATA-LEN + RECV-KVDLEN                    
055300       MOVE RECV-AREA (1:RECV-KVDLEN)                                     
055400                                     TO WS-RECV-XML                       
055500                                         (WS-POS : RECV-KVDLEN)           
055600       COMPUTE WS-POS = WS-POS + RECV-KVDLEN                              
055700                                                                          
055800       PERFORM S02A-RECEIVE-MESSAGE                                       
055900     END-PERFORM                                                          
056000     .                                                                    
056100                                                                          
056200 S02A-RECEIVE-MESSAGE SECTION.                                            
056300                                                                          
056400     MOVE 'GET'                      TO RECV-KDFUNC                       
056500     MOVE LENGTH OF RECV-AREA        TO RECV-KVDLEN                       
056600                                                                          
056700     CALL WZ01RECV USING RECV-CONTROL-AREA                                
056800                         RECV-KVDLEN                                      
056900                         RECV-AREA                                        
057000                                                                          
057100     IF RECV-KDRC > 1                                                     
057200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
057300       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
057400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057600     END-IF                                                               
057700     .                                                                    
057800                                                                          
057900 S03-RECEIVE-CLOSE SECTION.                                               
058000                                                                          
058100     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
058200     CALL WZ01RECV USING RECV-CONTROL-AREA                                
058300                                                                          
058400     IF RECV-KDRC > 0                                                     
058500       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
058600       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
058700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
058800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058900     END-IF                                                               
059000     .                                                                    
059100                                                                          
059200                                                                          
059300 S20-SEND-ERROR SECTION.                                                  
059400     PERFORM S21-SEND-OPEN                                                
059500     PERFORM S22-PUT-HEADER                                               
059600     PERFORM                                                              
059700     VARYING IX FROM 1 BY 1                                               
059800       UNTIL IX > IX-MAX                                                  
059900       MOVE MESSAGE-TEXT (IX)    TO WS-SEND-AREA                          
060000       PERFORM S23-PUT-LINE                                               
060100     END-PERFORM                                                          
060200     PERFORM S24-SEND-CLOSE                                               
060300     .                                                                    
060400                                                                          
060500 S21-SEND-OPEN SECTION.                                                   
060600     MOVE 'CARPARTS.DAP.DISTRDOC'                                         
060700                                 TO SEND-ADDISPABS                        
060800     MOVE 'OPEN'                 TO SEND-KDFUNC                           
060900     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
061000                                    SEND-OPEN-AREA                        
061100     IF SEND-KDRC > ZERO                                                  
061200       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
061300       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
061400             DELIMITED BY SIZE INTO ERROR-TEXT                            
061500       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
061600     END-IF                                                               
061700     .                                                                    
061800                                                                          
061900 S22-PUT-HEADER SECTION.                                                  
062000     MOVE 1                      TO REQU-IDMSGVER                         
062100     MOVE 'R'                    TO REQU-KDPGMACT                         
062200     MOVE IDPGM                  TO REQU-IDUSER                           
062300     MOVE FUNCTION                                                        
062400       FORMATTED-CURRENT-DATE('YYYYMMDDThhmmss.sssssss')                  
062500                                 TO WS-FORMATTED-TIMESTAMP                
062600     MOVE WS-FORMATTED-TIMESTAMP (14:2)                                   
062700                                 TO HDR-IDLIST (1:2)                      
062800     MOVE WS-FORMATTED-TIMESTAMP (17:7)                                   
062900                                 TO HDR-IDLIST (3:8)                      
063000     MOVE 'PUT'                  TO SEND-KDFUNC                           
063100     MOVE LENGTH OF HDR-AREA     TO SEND-KVDLEN                           
063200     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
063300                                    SEND-KVDLEN                           
063400                                    HDR-AREA                              
063500     IF SEND-KDRC > ZERO                                                  
063600       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
063700       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
063800             DELIMITED BY SIZE INTO ERROR-TEXT                            
063900       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
064000     END-IF                                                               
064100     .                                                                    
064200                                                                          
064300 S23-PUT-LINE SECTION.                                                    
064400     MOVE 'PUT'                  TO SEND-KDFUNC                           
064500     MOVE LENGTH OF WS-SEND-AREA TO SEND-KVDLEN                           
064600     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
064700                                    SEND-KVDLEN                           
064800                                    WS-SEND-AREA                          
064900     IF SEND-KDRC > ZERO                                                  
065000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
065100       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
065200             DELIMITED BY SIZE INTO ERROR-TEXT                            
065300       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
065400     END-IF                                                               
065500     .                                                                    
065600                                                                          
065700 S24-SEND-CLOSE SECTION.                                                  
065800     MOVE 'CLOSE'                TO SEND-KDFUNC                           
065900     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
066000     IF SEND-KDRC > ZERO                                                  
066100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
066200       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
066300             DELIMITED BY SIZE INTO ERROR-TEXT                            
066400       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
066500     END-IF                                                               
066600     .                                                                    
066700                                                                          
066800 S31-SEND-OPEN SECTION.                                                   
066900                                                                          
067000     MOVE WS-ADRESS-FLS                   TO SEND-ADDISPABS               
067100     MOVE 'OPEN'                          TO SEND-KDFUNC                  
067200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
067300                         SEND-OPEN-AREA                                   
067400     IF SEND-KDRC > ZERO                                                  
067500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
067600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
067700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
067800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
067900     END-IF                                                               
068000     .                                                                    
068100                                                                          
068200 S32-SEND-PUT-HEADER SECTION.                                             
068300     MOVE 0104-IDAPI             TO IDAPI                                 
068400     COMPUTE IDAPI-LEN            = FUNCTION LENGTH (                     
068500                                    FUNCTION TRIM (IDAPI))                
068600     MOVE 0104-IDPATH-API        TO IDPATH-API                            
068700     COMPUTE IDPATH-API-LEN       = FUNCTION LENGTH (                     
068800                                    FUNCTION TRIM (IDPATH-API))           
068900     MOVE 0104-IDPTYP-API        TO IDPTYP-API                            
069000     COMPUTE IDPTYP-API-LEN       = FUNCTION LENGTH (                     
069100                                    FUNCTION TRIM (IDPTYP-API))           
069200                                                                          
069300     MOVE 'PUT'                  TO SEND-KDFUNC                           
069400     MOVE LENGTH OF WAPIINFO     TO SEND-KVDLEN                           
069500     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
069600                                    SEND-KVDLEN                           
069700                                    WAPIINFO                              
069800     IF SEND-KDRC > ZERO                                                  
069900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
070000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
070100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
070200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
070300     END-IF                                                               
070400     .                                                                    
070500                                                                          
070600 S33-SEND-PUT-DATA SECTION.                                               
070700     MOVE 'PUT'                           TO SEND-KDFUNC                  
070800     MOVE LENGTH OF WGC-ERR-DATA          TO SEND-KVDLEN                  
070900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
071000                         SEND-KVDLEN                                      
071100                         WGC-ERR-DATA                                     
071200     IF SEND-KDRC > ZERO                                                  
071300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
071400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
071500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
071600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
071700     END-IF                                                               
071800     .                                                                    
071900                                                                          
072000 S34-SEND-CLOSE SECTION.                                                  
072100     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
072200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
072300     IF SEND-KDRC > ZERO                                                  
072400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
072500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
072600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
072700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
072800     END-IF                                                               
072900     .                                                                    
073000                                                                          
073100 IMS-INSERT-MSG-ALT SECTION.                                              
073200                                                                          
073300     MOVE LOW-VALUE TO PTOP-Z1                                            
073400                       PTOP-Z2                                            
073500                                                                          
073600     MOVE SPACE TO GOOD-STATUSCODES                                       
073700     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
073800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
073900     PERFORM IMS-STATUSCHECK                                              
074000     .                                                                    
074100                                                                          
074200 IMS-GU-W6D101-ASEQ SECTION.                                              
074300                                                                          
074400     STRING 'W6D101  (W6D1ASEQ>=' W-W6D1ASEQ-MIN-X                        
074500                    '&W6D1ASEQ<=' W-W6D1ASEQ-MAX-X ')'                    
074600             DELIMITED BY SIZE INTO SSA1                                  
074700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
074800     CALL CBLTDLI             USING GU                                    
074900                                    W6D1ASEQ-PCB                          
075000                                    DLI-IO-W6D101                         
075100                                    SSA1                                  
075200     MOVE W6D1ASEQ-STATUS-CODE   TO STATUS-WS                             
075300     PERFORM IMS-STATUSCHECK                                              
075400     .                                                                    
075500                                                                          
075600 IMS-GU-WDGX0104  SECTION.                                                
075700                                                                          
075800     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
075900             DELIMITED BY SIZE INTO SSA1                                  
076000     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
076100             DELIMITED BY SIZE INTO SSA2                                  
076200     MOVE '  '                   TO GOOD-STATUSCODES                      
076300     CALL CBLTDLI             USING GU                                    
076400                                    ATAB-PCB                              
076500                                    DLI-IO-WDGX0104                       
076600                                    SSA1 SSA2                             
076700     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
076800     PERFORM IMS-STATUSCHECK                                              
076900     .                                                                    
077000*----------------------------------------------------------------*        
077100 IMS-STATUSCHECK SECTION.                                                 
077200                                                                          
077300     SET STATUS-IX TO 1                                                   
077400     SEARCH GOOD-STATUS                                                   
077500       AT END                                                             
077600         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
077700           DELIMITED BY SIZE INTO ERROR-TEXT                              
077800         CALL FELLOG                                                      
077900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
078000         CONTINUE                                                         
078100     END-SEARCH                                                           
078200     .                                                                    
