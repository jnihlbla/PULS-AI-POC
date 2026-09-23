000100*COMPOPT VPOSIX=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ044000.                                                
000400 AUTHOR.         RAHUL REDDY.                                             
000500 DATE-WRITTEN.   21/11/22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        PROGRAM THAT HANDLES API CALLS.                                  
001000*        USED FOR ASYNC COMMUNICATIONS, WHERE PULS TRANSACTIONS           
001100*        DONT HAVE TO WAIT FOR API RESPONSE.                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: WZ0440X                                             
001500*        MID:         WAPIINFO + API-REQUEST                              
001600*                                                                         
001700*    OUTDATA.                                                             
001800*                                                                         
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100 CONFIGURATION SECTION.                                                   
002200*SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
002300                                                                          
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900 77 IDPGM                        PIC X(08) VALUE 'WZ044000'.              
003000                                                                          
003100 77 ERROR-TEXT                   PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77 YES                          PIC X     VALUE 'J'.                     
003400 77 NOO                          PIC X     VALUE 'N'.                     
003500 77 IX                           PIC 9(2).                                
003600                                                                          
003700 77 RETRY-MAX                    PIC 9(2)  VALUE 3.                       
003800 77 RETRY-CNT                    PIC 9(2)  VALUE 0.                       
003900                                                                          
004000 77 WS-NUM-DISPLAY               PIC 9(9).                                
004100 77 WS-RECV-KVDLEN               PIC S9(9) VALUE ZERO COMP.               
004200 77 KDRC-DISPLAY                 PIC Z(5).                                
004300                                                                          
004400 77 WS-SEND-AREA                 PIC X(1000).                             
004500                                                                          
004600 77 WS-CR-PARCEL-PATH            PIC X(250) VALUE                         
004700     '%2Fparceltransport-transport%2Ftransport'.                          
004800 77 WS-CR-PARCEL-METHOD          PIC X(6)  VALUE 'POST'.                  
004900                                                                          
005000*    --- MAIL CONTENT                                                     
005100*    --- For Create Parcel transport errors.                              
005200 01 WS-CR-PARCEL-CONT1.                                                   
005300    03 FILLER                    PIC X(80) VALUE 'Due to an error         
005400-      'in the communication between PULS and Parcel transport ser        
005500-      'vice,'.                                                           
005600    03 FILLER                    PIC X(80) VALUE 'below parcel inf        
005700-      'o was not sent succesfully.'.                                     
005800                                                                          
005900    03 FILLER                    PIC X(80) VALUE SPACES.                  
006000                                                                          
006100    03 FILLER.                                                            
006200       05 FILLER                 PIC X(9)  VALUE 'Parcel Id'.             
006300       05 FILLER                 PIC X(3)  VALUE ' : '.                   
006400       05 WS-CR-PARCEL-ID        PIC X(68) VALUE SPACES.                  
006500                                                                          
006600    03 FILLER                    PIC X(80) VALUE SPACES.                  
006700                                                                          
006800    03 FILLER                    PIC X(80) VALUE 'Response is :'.         
006900                                                                          
007000    03 WS-CR-PARCEL-MESSAGE      PIC X(1040) VALUE SPACES. *>13*80        
007100                                                                          
007200 01 FILLER REDEFINES WS-CR-PARCEL-CONT1.                                  
007300    03 WS-CR-PARCEL1-LINE        PIC X(80) OCCURS 19.                     
007400 77 MAX-CR-PARCEL1-LINES         PIC 9(2)  VALUE 19.                      
007500                                                                          
007600                                                                          
007700*    --- For SYNQ errors                                                  
007800 01 WS-SYNQ-CONTENT1.                                                     
007900    03 FILLER                    PIC X(80) VALUE 'Due to an error         
008000-      'in the communication between PULS and SYNQ,'.                     
008100                                                                          
008200    03 FILLER                    PIC X(80) VALUE 'below Picking Or        
008300-      'der needs to be created manually in SYNQ'.                        
008400                                                                          
008500    03 FILLER                    PIC X(80) VALUE SPACES.                  
008600    03 FILLER.                                                            
008700       05 FILLER                 PIC X(19) VALUE 'orderId'.               
008800       05 FILLER                 PIC X     VALUE ':'.                     
008900       05 WSY1-ORDERID           PIC X(60) VALUE SPACES.                  
009000    03 FILLER.                                                            
009100       05 FILLER                 PIC X(19) VALUE 'owner'.                 
009200       05 FILLER                 PIC X     VALUE ':'.                     
009300       05 WSY1-OWNER             PIC X(60) VALUE SPACES.                  
009400    03 FILLER.                                                            
009500       05 FILLER                 PIC X(19) VALUE 'orderType'.             
009600       05 FILLER                 PIC X     VALUE ':'.                     
009700       05 WSY1-ORDERTYPE         PIC X(60) VALUE SPACES.                  
009800    03 FILLER.                                                            
009900       05 FILLER                 PIC X(19) VALUE 'dispatchDate'.          
010000       05 FILLER                 PIC X     VALUE ':'.                     
010100       05 WSY1-DISPATCHDATE      PIC X(60) VALUE SPACES.                  
010200    03 FILLER.                                                            
010300       05 FILLER                 PIC X(19) VALUE 'priority'.              
010400       05 FILLER                 PIC X     VALUE ':'.                     
010500       05 WSY1-PRIORITY          PIC 9.                                   
010600       05 FILLER                 PIC X(59) VALUE SPACES.                  
010700    03 FILLER.                                                            
010800       05 FILLER                 PIC X(19) VALUE                          
010900                                             'consolidationLoc'.          
011000       05 FILLER                 PIC X     VALUE ':'.                     
011100       05 WSY1-CONSOLLOC         PIC X(60) VALUE SPACES.                  
011200    03 FILLER.                                                            
011300       05 FILLER                 PIC X(80) VALUE 'instruction'.           
011400    03 FILLER.                                                            
011500       05 FILLER                 PIC X(19) VALUE '   type'.               
011600       05 FILLER                 PIC X     VALUE ':'.                     
011700       05 WSY1-TYPE              PIC X(60) VALUE SPACES.                  
011800    03 FILLER.                                                            
011900       05 FILLER                 PIC X(19) VALUE '   text'.               
012000       05 FILLER                 PIC X     VALUE ':'.                     
012100       05 WSY1-TEXT              PIC X(60) VALUE SPACES.                  
012200    03 FILLER.                                                            
012300       05 FILLER                 PIC X(19) VALUE                          
012400                                           'orderLineNumber'.             
012500       05 FILLER                 PIC X     VALUE ':'.                     
012600       05 WSY1-ORDLINENUM        PIC 9(3)  VALUE ZERO.                    
012700       05 FILLER                 PIC X(57) VALUE SPACES.                  
012800    03 FILLER.                                                            
012900       05 FILLER                 PIC X(19) VALUE 'productId'.             
013000       05 FILLER                 PIC X     VALUE ':'.                     
013100       05 WSY1-PRODUCTID         PIC X(60) VALUE SPACES.                  
013200    03 FILLER.                                                            
013300       05 FILLER                 PIC X(19) VALUE                          
013400                                           'quantityOrdered'.             
013500       05 FILLER                 PIC X     VALUE ':'.                     
013600       05 WSY1-QTYORDERED        PIC 9(5)  VALUE ZERO.                    
013700       05 FILLER                 PIC X(55) VALUE SPACES.                  
013800    03 FILLER.                                                            
013900       05 FILLER                 PIC X(80) VALUE                          
014000                                           'attributeValue'.              
014100    03 FILLER.                                                            
014200       05 FILLER                 PIC X(19) VALUE '   name'.               
014300       05 FILLER                 PIC X     VALUE ':'.                     
014400       05 WSY1-NAME              PIC X(60) VALUE SPACES.                  
014500    03 FILLER.                                                            
014600       05 FILLER                 PIC X(19) VALUE '   value'.              
014700       05 FILLER                 PIC X     VALUE ':'.                     
014800       05 WSY1-VALUE             PIC X(60) VALUE SPACES.                  
014900 01 FILLER REDEFINES WS-SYNQ-CONTENT1.                                    
015000    03 WS-SY1-LINE OCCURS 18     PIC X(80).                               
015100 77 MAX-SY1-LINES                PIC 9(2)  VALUE 18.                      
015200                                                                          
015210 01 WS-FORMATTED-TIMESTAMP       PIC X(23).                               
015300                                                                          
015400*    --- PARAMETERS TO ABEND                                              
015500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
015700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
015900                                                                          
016000 77 WS-MY-OWN-ADRESS             PIC X(50) VALUE                          
016100                                      'CARPARTS.API.DISTR'.               
016200                                                                          
016300 77 CONTINUE-SW                  PIC X     VALUE 'J'.                     
016400    88 CONTINUE-YES                        VALUE 'J'.                     
016500    88 CONTINUE-NO                         VALUE 'N'.                     
016600                                                                          
016700 77 WZ11OUTM-SW                  PIC X     VALUE ' '.                     
016800    88 SW-OUTM-ERR                         VALUE 'E'.                     
016900    88 SW-OUTM-OPEN                        VALUE 'O'.                     
017000                                                                          
017100 77 API-CALL-SW                  PIC X     VALUE 'J'.                     
017200    88 API-CALL-OK                         VALUE 'J'.                     
017300    88 API-CALL-NOT-OK                     VALUE 'N'.                     
017400                                                                          
017500 77 ZCEE-CALL-SW                 PIC X     VALUE 'N'.                     
017600    88 ZCEE-CALL-DONE                      VALUE 'J'.                     
017700    88 ZCEE-CALL-NOT-DONE                  VALUE 'N'.                     
017800                                                                          
017900 77 SW-MAIL-OPEN                 PIC X     VALUE 'N'.                     
018000    88 MAIL-OPEN-YES                       VALUE 'J'.                     
018100    88 MAIL-OPEN-NO                        VALUE 'N'.                     
018200                                                                          
018300 77 SW-RETRY                     PIC X     VALUE 'N'.                     
018400    88 RETRY-YES                           VALUE 'J'.                     
018500    88 RETRY-NO                            VALUE 'N'.                     
018600                                                                          
018700 01 WS-OUTDATA-L                 PIC S9(9) COMP.                          
018800 01 WS-OUTDATA                   PIC X(3000).                             
018900                                                                          
019000 01 WS-TEXT                      PIC X(10000).                            
019100 01 WS-SECURITY-KEY              PIC X(32) VALUE SPACES.                  
019200 01 WS-BAQCTERM-RC               PIC X(03) VALUE SPACES.                  
019300                                                                          
019400****************************************************************          
019500 01 BAQ-REQUEST-PTR                          USAGE POINTER.               
019600 01 BAQ-REQUEST-LEN              PIC S9(9)   COMP-5 SYNC.                 
019700 01 BAQ-RESPONSE-PTR                         USAGE POINTER.               
019800 01 BAQ-RESPONSE-LEN             PIC S9(9)   COMP-5 SYNC.                 
019900                                                                          
020000*                                                                         
020100*   -COPY BAQRINFO                                                        
020200*                                                                         
020300*   -COPY WAPIINFO                                                        
020400*                                                                         
020500 01 API-REQUEST                  PIC X(10000000).                         
020600*01 WSY00Q01 -COPY WSY00Q01 -RED API-REQUEST                              
020700*01 WTM00Q01 -COPY WTM00Q01 -RED API-REQUEST                              
020800                                                                          
020900*01 API-RESPONSE                 PIC X(104857600).                        
021000 01 API-RESPONSE                 PIC X(10000).                            
021100                                                                          
021200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
021300 01 GENERAL-SUBPROGRAMS.                                                  
021400    03 WZ11OUTM                  PIC X(8)  VALUE 'WZ11OUTM'.              
021500    03 WZ01SEND                  PIC X(8)  VALUE 'WZ01SEND'.              
021600    03 BAQCSTUB                  PIC X(8)  VALUE 'BAQCSTUB'.              
021700    03 BAQCTERM                  PIC X(8)  VALUE 'BAQCTERM'.              
021800    03 WZ01RECV                  PIC X(8)  VALUE 'WZ01RECV'.              
021900    03 VIMSID                    PIC X(8)  VALUE 'VIMSID  '.              
022000    03 CBLTDLI                   PIC X(8)  VALUE 'CBLTDLI '.              
022100    03 FELLOG                    PIC X(8)  VALUE 'FELLOG  '.              
022200    03 ABEND                     PIC X(8)  VALUE 'ABEND   '.              
022300    03 W009WAIT                  PIC X(8)  VALUE 'W009WAIT'.              
022400                                                                          
022500*    --- PARAMETERS TO W009WAIT                                           
022600 77  WAIT-TIME                   PIC S9(9)   COMP VALUE +0.               
022700                                                                          
022800*01  -COPY WZ11OUTM                                                       
022900                                                                          
023000*01  -COPY WZ01RECV                                                       
023100                                                                          
023200*01  -COPY WZ01SEND                                                       
023300                                                                          
023400 01  HDR-AREA.                                                            
023500*    03  -COPY WZ01REQU                                                   
023600*    03  -COPY WZ04HDR                                                    
023700                                                                          
023800*    --- PARAMETERS TO VIMSID                                             
023900 01 WS-VIMSID                    PIC X(8)  VALUE SPACE.                   
024000 01 FILLER REDEFINES WS-VIMSID.                                           
024100    03 IMS-REGION                PIC X(3).                                
024200       88 TEST-REGION                      VALUE 'IMD' 'IMP' 'IMY'        
024300                                                 'IMB'.                   
024400       88 ENV-DEVE                         VALUE 'IMP'.                   
024500       88 ENV-IGRT                         VALUE 'IMY'.                   
024600       88 ENV-XDEV                         VALUE 'IMD'.                   
024700       88 ENV-ACPT                         VALUE 'IMB'.                   
024800       88 ENV-PROD                         VALUE 'IMG'.                   
024900    03 FILLER                    PIC X(5).                                
025000                                                                          
025100*                                                                         
025200 01 FILLER                       PIC X(16) VALUE 'REQU-AREA'.             
025300*01 -COPY WZ0440I1                                                        
025400                                                                          
025500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
025600*                                                                         
025700 01 FILLER                       PIC X(16) VALUE 'IMS-WS'.                
025800                                                                          
025900*    --- STATUS CODES FROM IMS                                            
026000 01 STATUS-WS                    PIC XX.                                  
026100    88 SEGMENT-FOUND                       VALUE '  '.                    
026200    88 SEGMENT-FOUND-EXISTS                VALUE 'II'.                    
026300    88 SEGMENT-MISSING                     VALUE 'GE'.                    
026400                                                                          
026500 01 GOOD-STATUSCODES.                                                     
026600    03 GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX                          
026700                                 PIC XX.                                  
026800                                                                          
026900 01 SSA1                         PIC X(64).                               
027000                                                                          
027100*    --- IMS FUNCTION CODES                                               
027200*01  -COPY W0003.                                                         
027300                                                                          
027400*    ---  DLI INPUT-OUTPUT AREA                                           
027500                                                                          
027600 LINKAGE SECTION.                                                         
027700*01  -COPY W0009   -PRE MSG-.                                             
027800 01 DISTRDOC-PCB                 PIC X.                                   
027900                                                                          
028000 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
028100 MAIN SECTION.                                                            
028200                                                                          
028300     PERFORM S03-RECEIVE-OPEN                                             
028400     PERFORM S03-RECEIVE-APIINFO                                          
028500     IF RECV-KDRC = 0                                                     
028600       PERFORM A-INIT                                                     
028700       PERFORM S03-RECEIVE-APIDATA                                        
028800       PERFORM C-CALL-API                                                 
028900     END-IF                                                               
029000     PERFORM S03-RECEIVE-CLOSE                                            
029100     PERFORM Z-FINIT                                                      
029200                                                                          
029300     MOVE ZERO                   TO RETURN-CODE                           
029400     GOBACK                                                               
029500     .                                                                    
029600                                                                          
029700 A-INIT SECTION.                                                          
029800                                                                          
029900     INITIALIZE API-REQUEST                                               
030000                API-RESPONSE                                              
030100                RECV-CLOSE-AREA                                           
030200                                                                          
030300     CALL VIMSID              USING WS-VIMSID                             
030400                                                                          
030500D    IF TEST-REGION                                                       
030600D      DISPLAY 'START WZ044000 IN ' WS-VIMSID                             
030700D    END-IF                                                               
030800     .                                                                    
030900                                                                          
031000 C-CALL-API SECTION.                                                      
031100                                                                          
031200                                                                          
031300*SET THE SWITCH SO IF WE MADE A CALL TO ZCEE, THEN WE TERMINATE AT        
031400*THE END.                                                                 
031500     SET ZCEE-CALL-DONE          TO TRUE                                  
031600                                                                          
031700     SET RETRY-YES               TO TRUE                                  
031800                                                                          
031900     PERFORM                                                              
032000       UNTIL RETRY-NO OR RETRY-CNT > RETRY-MAX                            
032100       PERFORM S01-CALL-ZCEE                                              
032200     END-PERFORM                                                          
032300                                                                          
032400     IF BAQ-SUCCESS                                                       
032500D      PERFORM S81-TRACE1                                                 
032600       MOVE BAQ-STATUS-CODE      TO RECV-KDRC-HTTP                        
032700       MOVE 'A'                  TO RECV-KDKOMSTA                         
032800       MOVE SPACES               TO RECV-KDRC-PULS                        
032900       MOVE ZERO                 TO RECV-MESSAGE-KVDLEN                   
033000       MOVE SPACES               TO RECV-MESSAGE                          
033100*      IF BAQ-RESPONSE-LEN > 1024                                         
033200*        MOVE API-RESPONSE (1:1024)                                       
033300*                                TO RECV-MESSAGE                          
033400*        MOVE 1024               TO RECV-MESSAGE-KVDLEN                   
033500*      ELSE                                                               
033600*        IF BAQ-RESPONSE-LEN > 0                                          
033700*          MOVE API-RESPONSE (1:BAQ-RESPONSE-LEN)                         
033800*                                TO RECV-MESSAGE                          
033900*          MOVE BAQ-RESPONSE-LEN TO RECV-MESSAGE-KVDLEN                   
034000*        ELSE                                                             
034100*          MOVE SPACES           TO RECV-MESSAGE                          
034200*          MOVE ZERO             TO RECV-MESSAGE-KVDLEN                   
034300*        END-IF                                                           
034400*      END-IF                                                             
034500*      CONTINUE                                                           
034600     ELSE                                                                 
034700       MOVE BAQ-STATUS-CODE      TO RECV-KDRC-HTTP                        
034800       MOVE 'F'                  TO RECV-KDKOMSTA                         
034900       EVALUATE TRUE                                                      
035000         WHEN BAQ-ERROR-IN-API                                            
035100           MOVE 'E01-API'        TO RECV-KDRC-PULS                        
035200         WHEN BAQ-ERROR-IN-ZCEE                                           
035300           MOVE 'E02-ZCEE'       TO RECV-KDRC-PULS                        
035400         WHEN BAQ-ERROR-IN-STUB                                           
035500           MOVE 'E03-STUB'       TO RECV-KDRC-PULS                        
035600         WHEN BAQ-ERROR-NO-RESPONSE                                       
035700           MOVE 'E04-NORESP'     TO RECV-KDRC-PULS                        
035800       END-EVALUATE                                                       
035900       INSPECT BAQ-STATUS-MESSAGE REPLACING ALL X'00'                     
036000                                             BY X'40'                     
036100       MOVE FUNCTION TRIM (FUNCTION DISPLAY-OF (                          
036200            FUNCTION NATIONAL-OF (BAQ-STATUS-MESSAGE, 1047)               
036300                                  , 278))                                 
036400                                   TO RECV-MESSAGE                        
036500       COMPUTE RECV-MESSAGE-KVDLEN = FUNCTION BYTE-LENGTH(                
036600                      FUNCTION TRIM (RECV-MESSAGE))                       
036700       PERFORM CA-HANDLE-NOTIFICATIONS                                    
036800     END-IF                                                               
036900     .                                                                    
037000                                                                          
037100 Z-FINIT SECTION.                                                         
037200     IF ZCEE-CALL-DONE                                                    
037300       PERFORM S02-TERM-ZCEE                                              
037400     END-IF                                                               
037500     IF MAIL-OPEN-YES                                                     
037600       PERFORM S91-SEND-MAIL-CLOSE                                        
037700     END-IF                                                               
037800     .                                                                    
037900                                                                          
038000 S01-CALL-ZCEE SECTION.                                                   
038100                                                                          
038200     INITIALIZE BAQ-STATUS-MESSAGE                                        
038300                                                                          
038400* Use pointer and length to specify the location of                       
038500* the request and response data structures.                               
038600* These assignments are required.                                         
038700                                                                          
038800     SET BAQ-REQUEST-PTR         TO ADDRESS OF API-REQUEST                
038900     MOVE LENGTH OF API-REQUEST  TO BAQ-REQUEST-LEN                       
039000     SET BAQ-RESPONSE-PTR        TO ADDRESS OF API-RESPONSE               
039100     MOVE LENGTH OF API-RESPONSE TO BAQ-RESPONSE-LEN                      
039200                                                                          
039300*CALL API VIA z/OS CONNECT                                                
039400     CALL BAQCSTUB            USING WAPIINFO                              
039500                                    BAQ-REQUEST-INFO                      
039600                                    BAQ-REQUEST-PTR                       
039700                                    BAQ-REQUEST-LEN                       
039800                                    BAQ-RESPONSE-INFO                     
039900                                    BAQ-RESPONSE-PTR                      
040000                                    BAQ-RESPONSE-LEN                      
040100                                                                          
040200     SET RETRY-NO                TO TRUE                                  
040300     IF BAQ-SUCCESS                                                       
040400       CONTINUE                                                           
040500     ELSE                                                                 
040600       EVALUATE TRUE                                                      
040700*        WHEN BAQ-ERROR-IN-API                                            
040800*          chk for BAQ-STATUS-CODE                                        
040900*        WHEN BAQ-ERROR-IN-ZCEE                                           
041000         WHEN BAQ-ERROR-IN-STUB                                           
041100           INSPECT BAQ-STATUS-MESSAGE TALLYING TALLY                      
041200             FOR ALL 'RSN=ATTLS detection failed'                         
041300           IF TALLY > 0                                                   
041400             SET RETRY-YES       TO TRUE                                  
041500           END-IF                                                         
041600       END-EVALUATE                                                       
041700     END-IF                                                               
041800                                                                          
041900     IF RETRY-YES                                                         
042000       ADD +1                    TO RETRY-CNT                             
042100       COMPUTE WAIT-TIME = RETRY-CNT * 100                                
042200       CALL W009WAIT          USING WAIT-TIME                             
042300     END-IF                                                               
042400                                                                          
042500     .                                                                    
042600                                                                          
042700 CA-HANDLE-NOTIFICATIONS SECTION.                                         
042800                                                                          
042900     EVALUATE IDAPI                                                       
043000       WHEN 'PULS-SYNQ_1.0.0'                                             
043100         PERFORM CAA-HANDLE-SYNQ-NOTIFY                                   
043200       WHEN 'PULS-TMS-PARCELTRANSPORT_3.0'                                
043300         PERFORM CAB-HANDLE-CR-PARCEL-NOTIFY                              
043400       WHEN OTHER                                                         
043500         PERFORM S81-TRACE1                                               
043600     END-EVALUATE                                                         
043700     .                                                                    
043800                                                                          
043900 CAA-HANDLE-SYNQ-NOTIFY SECTION.                                          
044000                                                                          
044100     IF IDPATH-API = '%2Fresources%2Forders%2Fbatch' AND                  
044200        IDPTYP-API = 'POST'                                               
044300       MOVE ZERO                 TO TALLY                                 
044400       INSPECT BAQ-STATUS-MESSAGE TALLYING TALLY                          
044500               FOR CHARACTERS BEFORE INITIAL                              
044600               'Duplicate order'                                          
044700       IF TALLY >= LENGTH OF BAQ-STATUS-MESSAGE AND                       
044800          orderType OF WSY00Q01 (1) = 'PCK'                               
044900         PERFORM S21-SEND-OPEN                                            
045000                                                                          
045100         MOVE priority (1)                                                
045200                                 TO WSY1-PRIORITY                         
045300         MOVE 'SYNQNOTIFY'       TO HDR-IDOUTTYPE                         
045400         STRING 'PCK-ORD-PRIO-'                                           
045500                WSY1-PRIORITY                                             
045600                       DELIMITED BY SIZE                                  
045700                               INTO HDR-IDOUTREC                          
045800         MOVE FUNCTION CURRENT-DATE(3:12)                                 
045900                                 TO HDR-IDLIST                            
046000         PERFORM S22-PUT-HEADER                                           
046100                                                                          
046200         MOVE orderId (1) (1:orderId-length (1))                          
046300                                 TO WSY1-ORDERID                          
046400         MOVE Xowner (1) (1:Xowner-length(1))                             
046500                                 TO WSY1-OWNER                            
046600         MOVE orderType OF WSY00Q01 (1) (1:orderType-length(1))           
046700                                 TO WSY1-ORDERTYPE                        
046800         MOVE dispatchDate (1) (1:dispatchDate-length(1))                 
046900                                 TO WSY1-DISPATCHDATE                     
047000         MOVE consolidationLoc2 (1)                                       
047100                (1:consolidationLoc2-length(1))                           
047200                                 TO WSY1-CONSOLLOC                        
047300         MOVE Xtype (1, 1) (1:Xtype-length(1, 1))                         
047400                                 TO WSY1-TYPE                             
047500         MOVE Xtext (1, 1) (1:Xtext-length(1, 1))                         
047600                                 TO WSY1-TEXT                             
047700         MOVE orderLineNumber (1, 1)                                      
047800                                 TO WSY1-ORDLINENUM                       
047900         MOVE productId (1, 1) (1:productId-length(1, 1))                 
048000                                 TO WSY1-PRODUCTID                        
048100         MOVE quantityOrdered (1, 1)                                      
048200                                 TO WSY1-QTYORDERED                       
048300         MOVE name OF WSY00Q01 (1, 1, 1)                                  
048400               (1:name-length OF WSY00Q01 (1, 1, 1))                      
048500                                 TO WSY1-NAME                             
048600         MOVE Xvalue2 (1, 1, 1) (1:Xvalue2-length(1, 1, 1))               
048700                                 TO WSY1-VALUE                            
048800         PERFORM                                                          
048900         VARYING IX FROM 1 BY 1                                           
049000           UNTIL IX > MAX-SY1-LINES                                       
049100           MOVE WS-SY1-LINE (IX) TO WS-SEND-AREA                          
049200           MOVE 80               TO SEND-KVDLEN                           
049300           PERFORM S25-PUT-LINE                                           
049400         END-PERFORM                                                      
049500                                                                          
049600         PERFORM S29-SEND-CLOSE                                           
049700       END-IF                                                             
049800     END-IF                                                               
049900     .                                                                    
050000                                                                          
050100 CAB-HANDLE-CR-PARCEL-NOTIFY SECTION.                                     
050200                                                                          
050300     IF IDPATH-API = WS-CR-PARCEL-PATH AND                                
050400        IDPTYP-API = WS-CR-PARCEL-METHOD                                  
050500       MOVE ZERO                 TO TALLY                                 
050600       INSPECT RECV-MESSAGE(1:RECV-MESSAGE-KVDLEN) TALLYING TALLY         
050700               FOR CHARACTERS BEFORE INITIAL 'BAQ'                        
050800       IF TALLY >= RECV-MESSAGE-KVDLEN                                    
050900         MOVE RECV-MESSAGE (1:RECV-MESSAGE-KVDLEN)                        
051000                                 TO WS-CR-PARCEL-MESSAGE                  
051100         MOVE PARCELIDENTIFIER (1:PARCELIDENTIFIER-LENGTH)                
051200                                 TO WS-CR-PARCEL-ID                       
051300                                                                          
051400         PERFORM S21-SEND-OPEN                                            
051500         MOVE 'CR-PARCEL'        TO HDR-IDOUTTYPE                         
051600         MOVE 'GENERAL'          TO HDR-IDOUTREC                          
051700         MOVE FUNCTION                                                    
051710         FORMATTED-CURRENT-DATE('YYYYMMDDThhmmss.sssssss')                
051711                                 TO WS-FORMATTED-TIMESTAMP                
051720         MOVE WS-FORMATTED-TIMESTAMP (14:2)                               
051800                                 TO HDR-IDLIST (1:2)                      
051810         MOVE WS-FORMATTED-TIMESTAMP (17:7)                               
051830                                 TO HDR-IDLIST (3:8)                      
051900         PERFORM S22-PUT-HEADER                                           
052000                                                                          
052100         PERFORM                                                          
052200         VARYING IX FROM 1 BY 1                                           
052300           UNTIL IX > MAX-CR-PARCEL1-LINES                                
052400           MOVE WS-CR-PARCEL1-LINE (IX)                                   
052500                                 TO WS-SEND-AREA                          
052600           MOVE 80               TO SEND-KVDLEN                           
052700           PERFORM S25-PUT-LINE                                           
052800         END-PERFORM                                                      
052900                                                                          
053000         PERFORM S29-SEND-CLOSE                                           
053100       ELSE                                                               
053200         PERFORM S81-TRACE1                                               
053300       END-IF                                                             
053400     ELSE                                                                 
053500       PERFORM S81-TRACE1                                                 
053600     END-IF                                                               
053700     .                                                                    
053800                                                                          
053900 S02-TERM-ZCEE SECTION.                                                   
054000*CLOSE AND CLEAR CACHED CONNECTION                                        
054100     INITIALIZE BAQ-RESPONSE-INFO                                         
054200     CALL BAQCTERM            USING BAQ-RESPONSE-INFO                     
054300                                                                          
054400     IF BAQ-SUCCESS                                                       
054500D      PERFORM S81-TRACE2                                                 
054600       CONTINUE                                                           
054700     ELSE                                                                 
054800       EVALUATE TRUE                                                      
054900         WHEN BAQ-ERROR-IN-API                                            
055000           MOVE 'T01'            TO WS-BAQCTERM-RC                        
055100         WHEN BAQ-ERROR-IN-ZCEE                                           
055200           MOVE 'T02'            TO WS-BAQCTERM-RC                        
055300         WHEN BAQ-ERROR-IN-STUB                                           
055400           MOVE 'T03'            TO WS-BAQCTERM-RC                        
055500       END-EVALUATE                                                       
055600D      PERFORM S81-TRACE2                                                 
055700     END-IF                                                               
055800     .                                                                    
055900                                                                          
056000 S03-RECEIVE-OPEN SECTION.                                                
056100                                                                          
056200     MOVE 'OPEN'                 TO RECV-KDFUNC                           
056300     MOVE WS-MY-OWN-ADRESS       TO RECV-ADDISPABS                        
056400     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
056500                                    RECV-OPEN-AREA                        
056600                                                                          
056700     IF RECV-KDRC > 0                                                     
056800       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
056900       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
057000             DELIMITED BY SIZE INTO ERROR-TEXT                            
057100       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
057200     END-IF                                                               
057300     .                                                                    
057400                                                                          
057500 S03-RECEIVE-APIINFO SECTION.                                             
057600                                                                          
057700*    -- TRUNCATION IS ALLOWED TO BE ABLE TO HANDLE TRAILING               
057800*    -- BLANKS IN HEADER RECORD. SHORTER RECORDS ARE ALSO                 
057900*    -- ACCEPTED AND MISSING FIELDS ARE ASSUMED TO BE = SPACE.            
058000                                                                          
058100     MOVE 'GET'                  TO RECV-KDFUNC                           
058200     INITIALIZE WAPIINFO                                                  
058300     MOVE LENGTH OF WAPIINFO     TO RECV-KVDLEN                           
058400     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
058500                                    RECV-KVDLEN                           
058600                                    WAPIINFO                              
058700                                                                          
058800     IF RECV-KDRC > 1 AND NOT = 21                                        
058900       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
059000       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
059100             DELIMITED BY SIZE INTO ERROR-TEXT                            
059200       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
059300     ELSE                                                                 
059400       MOVE RECV-KVDLEN          TO WS-RECV-KVDLEN                        
059500     END-IF                                                               
059600     .                                                                    
059700                                                                          
059800 S03-RECEIVE-APIDATA SECTION.                                             
059900                                                                          
060000     MOVE 'GET'                  TO RECV-KDFUNC                           
060100     MOVE LENGTH OF API-REQUEST  TO RECV-KVDLEN                           
060200     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
060300                                    RECV-KVDLEN                           
060400                                    API-REQUEST                           
060500                                                                          
060600     IF RECV-KDRC > 1                                                     
060700       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
060800       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
060900             DELIMITED BY SIZE INTO ERROR-TEXT                            
061000       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
061100     ELSE                                                                 
061200       MOVE RECV-KVDLEN          TO WS-RECV-KVDLEN                        
061300     END-IF                                                               
061400     .                                                                    
061500                                                                          
061600 S03-RECEIVE-CLOSE SECTION.                                               
061700                                                                          
061800     MOVE 'CLOSE'                TO RECV-KDFUNC                           
061900     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
062000                                    RECV-CLOSE-AREA                       
062100                                                                          
062200     IF RECV-KDRC > 0                                                     
062300       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
062400       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
062500             DELIMITED BY SIZE INTO ERROR-TEXT                            
062600       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
062700     END-IF                                                               
062800     .                                                                    
062900                                                                          
063000 S21-SEND-OPEN SECTION.                                                   
063100     MOVE 'CARPARTS.DAP.DISTRDOC'                                         
063200                                 TO SEND-ADDISPABS                        
063300     MOVE 'OPEN'                 TO SEND-KDFUNC                           
063400     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
063500                                    SEND-OPEN-AREA                        
063600     IF SEND-KDRC > ZERO                                                  
063700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
063800       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
063900             DELIMITED BY SIZE INTO ERROR-TEXT                            
064000       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
064100     END-IF                                                               
064200     .                                                                    
064300                                                                          
064400 S22-PUT-HEADER SECTION.                                                  
064500     MOVE 1                      TO REQU-IDMSGVER                         
064600     MOVE 'R'                    TO REQU-KDPGMACT                         
064700     MOVE IDPGM                  TO REQU-IDUSER                           
064800     MOVE 'PUT'                  TO SEND-KDFUNC                           
064900     MOVE LENGTH OF HDR-AREA     TO SEND-KVDLEN                           
065000     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
065100                                     SEND-KVDLEN                          
065200                                     HDR-AREA                             
065300     IF SEND-KDRC > ZERO                                                  
065400       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
065500       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
065600             DELIMITED BY SIZE INTO ERROR-TEXT                            
065700       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
065800     END-IF                                                               
065900     .                                                                    
066000                                                                          
066100 S25-PUT-LINE SECTION.                                                    
066200     MOVE 'PUT'                  TO SEND-KDFUNC                           
066300     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
066400                                    SEND-KVDLEN                           
066500                                    WS-SEND-AREA                          
066600     IF SEND-KDRC > ZERO                                                  
066700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
066800       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
066900             DELIMITED BY SIZE INTO ERROR-TEXT                            
067000       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
067100     END-IF                                                               
067200     .                                                                    
067300                                                                          
067400 S29-SEND-CLOSE SECTION.                                                  
067500     MOVE 'CLOSE'                TO SEND-KDFUNC                           
067600     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
067700     IF SEND-KDRC > ZERO                                                  
067800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
067900       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
068000             DELIMITED BY SIZE INTO ERROR-TEXT                            
068100       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
068200     END-IF                                                               
068300     .                                                                    
068400                                                                          
068500 S81-TRACE1 SECTION.                                                      
068600     STRING 'PGM ERROR CODE   = ' ERROR-TEXT                              
068700                       DELIMITED BY SIZE                                  
068800                               INTO OUTM-TEOUTDATA                        
068900     MOVE 80                     TO OUTM-TEOUTDATA-L                      
069000     PERFORM S91-SEND-MAIL-LINE2                                          
069100     MOVE BAQ-STATUS-CODE        TO WS-NUM-DISPLAY                        
069200     STRING 'BAQ STATUS CODE  = ' WS-NUM-DISPLAY                          
069300                       DELIMITED BY SIZE                                  
069400                               INTO OUTM-TEOUTDATA                        
069500     MOVE 80                     TO OUTM-TEOUTDATA-L                      
069600     PERFORM S91-SEND-MAIL-LINE2                                          
069700     STRING 'BAQ STATUS MSG   = ' BAQ-STATUS-MESSAGE (1:905)              
069800                       DELIMITED BY SIZE                                  
069900                               INTO OUTM-TEOUTDATA                        
070000     MOVE 924                    TO OUTM-TEOUTDATA-L                      
070100     PERFORM S91-SEND-MAIL-LINE2                                          
070200     STRING 'API RESPONSE     = ' API-RESPONSE (1:200)                    
070300                       DELIMITED BY SIZE                                  
070400                               INTO OUTM-TEOUTDATA                        
070500     MOVE 220                    TO OUTM-TEOUTDATA-L                      
070600     PERFORM S91-SEND-MAIL-LINE2                                          
070700     .                                                                    
070800                                                                          
070900 S81-TRACE2 SECTION.                                                      
071000     MOVE 'TERM CONNECTION : '   TO OUTM-TEOUTDATA                        
071100     MOVE 80                     TO OUTM-TEOUTDATA-L                      
071200     PERFORM S91-SEND-MAIL-LINE2                                          
071300     STRING 'PGM ERROR CODE   = ' WS-BAQCTERM-RC                          
071400                       DELIMITED BY SIZE                                  
071500                               INTO OUTM-TEOUTDATA                        
071600     MOVE 80                     TO OUTM-TEOUTDATA-L                      
071700     PERFORM S91-SEND-MAIL-LINE2                                          
071800     MOVE BAQ-STATUS-CODE        TO WS-NUM-DISPLAY                        
071900     STRING 'BAQ STATUS CODE  = ' WS-NUM-DISPLAY                          
072000                       DELIMITED BY SIZE                                  
072100                               INTO OUTM-TEOUTDATA                        
072200     MOVE 80                     TO OUTM-TEOUTDATA-L                      
072300     PERFORM S91-SEND-MAIL-LINE2                                          
072400     STRING 'BAQ STATUS MSG   = ' BAQ-STATUS-MESSAGE                      
072500                       DELIMITED BY SIZE                                  
072600                               INTO OUTM-TEOUTDATA                        
072700     MOVE 200                    TO OUTM-TEOUTDATA-L                      
072800     PERFORM S91-SEND-MAIL-LINE2                                          
072900     .                                                                    
073000                                                                          
073100 S91-SEND-MAIL-OPEN SECTION.                                              
073200                                                                          
073300     MOVE SPACE                  TO OUTM-WZ11OUT                          
073400                                                                          
073500     MOVE 1                      TO OUTM-IDCALL                           
073600     MOVE 'OPEN'                 TO OUTM-KDFUNC                           
073700     MOVE ZERO                   TO OUTM-KDRC                             
073800                                                                          
073900     MOVE 'RAHUL.REDDY@VOLVOCARS.COM'                                     
074000                                 TO OUTM-IDOUTDEST                        
074100     MOVE '.TXT'                 TO OUTM-IDPFDEF                          
074200     MOVE SPACES                 TO OUTM-FLCARRCNTL                       
074300     STRING 'NO.REPLY.' IMS-REGION '@VOLVOCARS.COM'                       
074400                       DELIMITED BY SIZE                                  
074500                               INTO OUTM-IDMAIL-SENDER                    
074600     MOVE IDAPI                  TO OUTM-IDMAILTTL                        
074700                                                                          
074800*    -- INITIALIZE ADDL INFO FIELDS. THESE ARE NOT USED                   
074900*    -- AS RESTART IS NOT AVAILABLE HERE.                                 
075000     MOVE SPACES                 TO OUTM-IDOUTTYPE                        
075100     MOVE SPACES                 TO OUTM-IDOUTREC                         
075200     MOVE SPACES                 TO OUTM-IDLIST                           
075300     MOVE ZEROES                 TO OUTM-TIREGDAT                         
075400     MOVE ZEROES                 TO OUTM-TIKLOCK                          
075500                                                                          
075600     CALL WZ11OUTM            USING OUTM-WZ11OUT                          
075700                                                                          
075800     IF OUTM-KDRC > 0                                                     
075900       SET SW-OUTM-ERR           TO TRUE                                  
076000     ELSE                                                                 
076100       SET SW-OUTM-OPEN          TO TRUE                                  
076200     END-IF                                                               
076300     .                                                                    
076400                                                                          
076500 S91-SEND-MAIL-LINE SECTION.                                              
076600                                                                          
076700     MOVE 'PUT'                  TO OUTM-KDFUNC                           
076800     MOVE LENGTH OF WAPIINFO                                              
076900                                 TO OUTM-TEOUTDATA-L                      
077000     MOVE WAPIINFO               TO OUTM-TEOUTDATA                        
077100                                                                          
077200     CALL WZ11OUTM            USING OUTM-WZ11OUT                          
077300                                                                          
077400     MOVE SPACES                 TO OUTM-TEOUTDATA                        
077500     .                                                                    
077600                                                                          
077700 S91-SEND-MAIL-LINE2 SECTION.                                             
077800                                                                          
077900     IF MAIL-OPEN-NO                                                      
078000       MOVE OUTM-TEOUTDATA-L     TO WS-OUTDATA-L                          
078100       MOVE OUTM-TEOUTDATA       TO WS-OUTDATA                            
078200       PERFORM S91-SEND-MAIL-OPEN                                         
078300       PERFORM S91-SEND-MAIL-LINE                                         
078400       SET MAIL-OPEN-YES         TO TRUE                                  
078500       MOVE WS-OUTDATA-L         TO OUTM-TEOUTDATA-L                      
078600       MOVE WS-OUTDATA           TO OUTM-TEOUTDATA                        
078700     END-IF                                                               
078800                                                                          
078900     MOVE 'PUT'                  TO OUTM-KDFUNC                           
079000                                                                          
079100     CALL WZ11OUTM            USING OUTM-WZ11OUT                          
079200                                                                          
079300     MOVE SPACES                 TO OUTM-TEOUTDATA                        
079400     .                                                                    
079500                                                                          
079600 S91-SEND-MAIL-CLOSE SECTION.                                             
079700                                                                          
079800     MOVE 'CLOSE'                TO OUTM-KDFUNC                           
079900     CALL WZ11OUTM            USING OUTM-WZ11OUT                          
080000                                                                          
080100     .                                                                    
080200                                                                          
