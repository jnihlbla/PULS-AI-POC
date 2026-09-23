000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ043000.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   21/03/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PROGRAM THAT SENDS EVENTS VIA API                                
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSACTION: WZ0430X                                             
001200*        MID:         WZI43001                                            
001300*                     WMSGKOM (Optional)                                  
001400*                                                                         
001500*    OUTDATA.                                                             
001600*        MOD:         WMSGKOM (Optional)                                  
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 CONFIGURATION SECTION.                                                   
002000*SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
002100                                                                          
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500 77 IDPGM                    PIC X(08) VALUE 'WZ043000'.                  
002600                                                                          
002700 77 ERROR-TEXT               PIC X(80) VALUE SPACE.                       
002800                                                                          
002900 77 YES                      PIC X     VALUE 'J'.                         
003000 77 NOO                      PIC X     VALUE 'N'.                         
003100                                                                          
003200 77 WS-SECTION               PIC X(50) VALUE SPACES.                      
003300                                                                          
003400 77 WS-ECOM-ADDRESS          PIC X(50) VALUE                              
003500                             'APIOUT.ECOM.ORDEREVENT'.                    
003600 77 WS-COMPOUND-ADDRESS      PIC X(50) VALUE                              
003700                             'APIOUT.COMPOUND.ORDEREVENT'.                
003800 77 WS-TACD-ADDRESS          PIC X(50) VALUE                              
003900                             'APIOUT.TACDIS.ORDEREVENT'.                  
004000 77 WS-LYNK-ADDRESS          PIC X(50) VALUE                              
004100                             'APIOUT.LYNK.ORDEREVENT'.                    
004200 77 KDRC-DISPLAY             PIC Z(5).                                    
004300 77 WS-RC                    PIC 9(3).                                    
004400                                                                          
004500 77 CONTINUE-SW              PIC X     VALUE 'J'.                         
004600    88 CONTINUE-YES                    VALUE 'J'.                         
004700    88 CONTINUE-NO                     VALUE 'N'.                         
004800                                                                          
004900 77 DISPATCHER-SW            PIC X     VALUE 'Z'.                         
005000    88 CLASSIC-DISP                    VALUE 'C'.                         
005100    88 WZ-DISP                         VALUE 'Z'.                         
005200                                                                          
005300 77 EVENTREC-SW              PIC X(3)  VALUE SPACES.                      
005400    88 REC-LYNK                        VALUE 'LYN'.                       
005500    88 REC-POLE                        VALUE 'POL'.                       
005600    88 REC-ECOM                        VALUE 'ECO'.                       
005700    88 REC-TACD                        VALUE 'TAD'.                       
005800    88 REC-ACC                         VALUE 'ACC'.                       
005900    88 REC-APA                         VALUE 'APA'.                       
006000    88 REC-APB-COMPOUND                VALUE 'APB'.                       
006100    88 REC-APC                         VALUE 'APC'.                       
006200    88 REC-APD                         VALUE 'APD'.                       
006300    88 REC-APE                         VALUE 'APE'.                       
006400    88 REC-APF                         VALUE 'APF'.                       
006500    88 REC-APG                         VALUE 'APG'.                       
006600    88 REC-APH                         VALUE 'APH'.                       
006700    88 REC-API                         VALUE 'API'.                       
006800    88 REC-APJ                         VALUE 'APJ'.                       
006900                                                                          
007000*   -COPY WAPIINFO                                                        
007100                                                                          
007200 01 API-REQUEST              PIC X(10000).                                
007300 01 API-REQUEST-LEN          PIC S9(9) BINARY.                            
007400                                                                          
007500 01 ECOM-EVENT-REQUEST.                                                   
007600*   03 -COPY WEVEE0I1 -PRE ecom-                                          
007700                                                                          
007800 01 TACD-EVENT-REQUEST.                                                   
007900*   03 -COPY WEVET0I1 -PRE tacd-                                          
008000                                                                          
008100 01 VCOP-EVENT-REQUEST.                                                   
008200*   03 -COPY WEVECOI1 -PRE vcop-                                          
008300                                                                          
008400 01 LYNK-SAP-EVENT-REQUEST.                                               
008500*   03 -COPY WEVEL0I1 -PRE lynk-sap-                                      
008600                                                                          
008700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008800 01 GENERAL-SUBPROGRAMS.                                                  
008900    03 VIMSID                PIC X(8)  VALUE 'VIMSID  '.                  
009000    03 CBLTDLI               PIC X(8)  VALUE 'CBLTDLI '.                  
009100    03 FELLOG                PIC X(8)  VALUE 'FELLOG  '.                  
009200    03 WZ01RECV              PIC X(8)  VALUE 'WZ01RECV'.                  
009300    03 WZ01SEND              PIC X(8)  VALUE 'WZ01SEND'.                  
009400                                                                          
009500*01  -COPY WZ01RECV                                                       
009600                                                                          
009700*01  -COPY WZ01SEND                                                       
009800                                                                          
009900*    --- PARAMETERS TO VIMSID                                             
010000 01 WS-VIMSID                PIC X(8)  VALUE SPACE.                       
010100 01 FILLER REDEFINES WS-VIMSID.                                           
010200    03 IMS-REGION            PIC X(3).                                    
010300       88 TEST-REGION                  VALUE 'IMD' 'IMP' 'IMY'            
010400                                             'IMB'.                       
010500       88 ENV-DEVE                     VALUE 'IMP'.                       
010600       88 ENV-IGRT                     VALUE 'IMY'.                       
010700       88 ENV-XDEV                     VALUE 'IMD'.                       
010800       88 ENV-ACPT                     VALUE 'IMB'.                       
010900       88 ENV-PROD                     VALUE 'IMG'.                       
011000    03 FILLER                PIC X(5).                                    
011100*                                                                         
011200 01 FILLER                       PIC X(16) VALUE 'REQU-AREA'.             
011300 01 REQU-AREA                    PIC X(700).                              
011400                                                                          
011500 01 DATA-AREA.                                                            
011600*   03 -COPY WZ0430I1                                                     
011700*      05 -COPY WAPIORD  -PRE ORD-  -RED REQU-EVENT-DATA                  
011800*      05 -COPY WAPIDISC -PRE DISC- -RED REQU-EVENT-DATA                  
011900                                                                          
012000 01  FILLER                      PIC X(16) VALUE 'MSG/MOD-AREA'.          
012100*01  -COPY WMSGAREA                                                       
012200                                                                          
012300 01  FILLER                      PIC X(16) VALUE 'KOM-IO-AREA'.           
012400 01 KOM-IO-AREA.                                                          
012500*   03  -COPY WMSGKOM                                                     
012600                                                                          
012700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012800                                                                          
012900 01 FILLER                       PIC X(16) VALUE 'IMS-WS'.                
013000                                                                          
013100 01 KEYS-FOR-DLI.                                                         
013200    03 W-WDGXKEY-0103-X.                                                  
013300       05  W-IDHTYP-0103         PIC X(4)  VALUE '0103'.                  
013400       05  FILLER                PIC X(26) VALUE LOW-VALUES.              
013500    03 W-KY0104-X.                                                        
013600       05  W-ADDISPABS           PIC X(50) VALUE SPACES.                  
013700                                                                          
013800*    --- STATUS CODES FROM IMS                                            
013900 01 STATUS-WS                    PIC XX.                                  
014000    88 SEGMENT-FOUND                   VALUE '  '.                        
014100    88 SEGMENT-MISSING                 VALUE 'GE'.                        
014200                                                                          
014300 01 GOOD-STATUSCODES.                                                     
014400    03 GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX                          
014500                                 PIC XX.                                  
014600                                                                          
014700 01 SSA1                         PIC X(128).                              
014800 01 SSA2                         PIC X(128).                              
014900                                                                          
015000*    --- IMS FUNCTION CODES                                               
015100*01  -COPY W0003.                                                         
015200                                                                          
015300*    ---  DLI INPUT-OUTPUT AREA                                           
015400 01 FILLER                       PIC X(16) VALUE 'DLI-IO-WDR501'.         
015500 01 DLI-IO-WDR501.                                                        
015600*   03  -COPY WDGX01                                                      
015700                                                                          
015800 01 FILLER                       PIC X(16) VALUE 'DLI-IO-0104'.           
015900 01 DLI-IO-WDGX0104.                                                      
016000*   03  -COPY WDGX0104                                                    
016100                                                                          
016200 LINKAGE SECTION.                                                         
016300*01  -COPY W0009   -PRE MSG-.                                             
016400                                                                          
016500*01  -COPY W0009   -PRE DISP-.                                            
016600                                                                          
016700*01  -COPY W0009   -PRE APIASYNC-.                                        
016800                                                                          
016900*01  -COPY W0008   -PRE ATAB-                                             
017000    05  FILLER               PIC X.                                       
017100                                                                          
017200 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB APIASYNC-PCB ATAB-PCB.        
017300 MAIN SECTION.                                                            
017400                                                                          
017500     PERFORM S01-RECV-OPEN                                                
017600     PERFORM S02-RECV-MESSAGE                                             
017700                                                                          
017800     IF RECV-KDRC = 0                                                     
017900       PERFORM A-INIT                                                     
018000       EVALUATE TRUE                                                      
018100         WHEN REC-LYNK                                                    
018200           PERFORM B-PROCESS-LYNK-SAP-EVENT                               
018300         WHEN REC-POLE                                                    
018400*          Polestar events are also sent to Ecom now!!!                   
018500           PERFORM D-PROCESS-ECOM-EVENT                                   
018600         WHEN REC-ECOM                                                    
018700           PERFORM D-PROCESS-ECOM-EVENT                                   
018800         WHEN REC-ACC                                                     
018900*          ACC events are also sent to Ecom                               
019000           PERFORM D-PROCESS-ECOM-EVENT                                   
019100         WHEN REC-APA                                                     
019200*          APA (Importers via ecom/serv layer) events sent to Ecom        
019300           PERFORM D-PROCESS-ECOM-EVENT                                   
019400         WHEN REC-APD                                                     
019500*          APD (INTERNAL ORDER)events sent to Ecom                        
019600           PERFORM D-PROCESS-ECOM-EVENT                                   
019700         WHEN REC-TACD                                                    
019800           PERFORM F-PROCESS-TACD-EVENT                                   
019900         WHEN REC-APB-COMPOUND                                            
020000           PERFORM G-PROCESS-APB-COMPOUND-EVENT                           
020100       END-EVALUATE                                                       
020200     END-IF                                                               
020300     PERFORM Z-FINIT                                                      
020400     MOVE ZERO                   TO RETURN-CODE                           
020500     GOBACK                                                               
020600     .                                                                    
020700                                                                          
020800 A-INIT SECTION.                                                          
020900                                                                          
021000     INITIALIZE API-REQUEST                                               
021100                RECV-CLOSE-AREA                                           
021200                W-ADDISPABS                                               
021300                                                                          
021400     CALL VIMSID              USING WS-VIMSID                             
021500                                                                          
021600     IF RECV-KVDLEN = LENGTH OF DATA-AREA                                 
021700       MOVE REQU-AREA            TO DATA-AREA                             
021800       SET WZ-DISP               TO TRUE                                  
021900     ELSE                                                                 
022000       MOVE REQU-AREA (6:)       TO DATA-AREA                             
022100       SET CLASSIC-DISP          TO TRUE                                  
022200       PERFORM S02-RECV-MESSAGE                                           
022300       MOVE REQU-AREA            TO KOM-IO-AREA (13:)                     
022400       MOVE +54                  TO MSG-KOM-KVLL                          
022500       MOVE 'W0T693X '           TO MSG-KOM-KDTRANS                       
022600     END-IF                                                               
022700                                                                          
022800     MOVE REQU-IDEVENTREC        TO EVENTREC-SW                           
022900     .                                                                    
023000                                                                          
023100 B-PROCESS-LYNK-SAP-EVENT SECTION.                                        
023200                                                                          
023300     MOVE 1                      TO lynk-sap-eventName-num                
023400     MOVE REQU-IDEVENT           TO lynk-sap-eventName2                   
023500     COMPUTE lynk-sap-eventName2-length = FUNCTION BYTE-LENGTH (          
023600                           FUNCTION TRIM (lynk-sap-eventName2))           
023700                                                                          
023800     MOVE 1                      TO lynk-sap-eventType-num                
023900     MOVE REQU-IDEVENTTYP        TO lynk-sap-eventType2                   
024000     COMPUTE lynk-sap-eventType2-length = FUNCTION BYTE-LENGTH (          
024100                           FUNCTION TRIM (lynk-sap-eventType2))           
024200                                                                          
024300     MOVE 1                      TO lynk-sap-timestamp-num                
024400     MOVE REQU-TIMESTAMP         TO lynk-sap-timestamp2                   
024500     COMPUTE lynk-sap-timestamp2-length = FUNCTION BYTE-LENGTH (          
024600                           FUNCTION TRIM (lynk-sap-timestamp2))           
024700                                                                          
024800     EVALUATE REQU-IDCPYTXT                                               
024900       WHEN 'WAPIORD'                                                     
025000         PERFORM BA-PROCESS-LYNK-SAP-ORDER                                
025100       WHEN 'WAPIDISC'                                                    
025200         SET CONTINUE-NO         TO TRUE                                  
025300         MOVE 'Not sent. Copytext not configured'                         
025400                                 TO RECV-MESSAGE                          
025500       WHEN OTHER                                                         
025600         SET CONTINUE-NO         TO TRUE                                  
025700         MOVE 'CPY'              TO MSG-KOM-IDMFSMED                      
025800         MOVE 'Not sent. Unknown copytext'                                
025900                                 TO RECV-MESSAGE                          
026000     END-EVALUATE                                                         
026100                                                                          
026200     IF CONTINUE-YES                                                      
026300       PERFORM BC-CALL-API                                                
026400     END-IF                                                               
026500     .                                                                    
026600                                                                          
026700 BA-PROCESS-LYNK-SAP-ORDER SECTION.                                       
026800                                                                          
026900     MOVE 1                      TO lynk-sap-eventData2-num               
027000                                                                          
027100     MOVE 1                      TO lynk-sap-orderId-num                  
027200     MOVE ORD-IDAPIORDREF        TO lynk-sap-orderId2                     
027300     COMPUTE lynk-sap-orderId2-length = FUNCTION BYTE-LENGTH (            
027400                           FUNCTION TRIM (lynk-sap-orderId2))             
027500                                                                          
027600     MOVE 1                      TO lynk-sap-eventMessage-num             
027700     MOVE ORD-TEMFSINF           TO lynk-sap-eventMessage2                
027800     COMPUTE lynk-sap-eventMessage2-length =                              
027900                           FUNCTION BYTE-LENGTH (                         
028000                           FUNCTION TRIM (lynk-sap-eventMessage2))        
028100                                                                          
028200     MOVE 1                      TO lynk-sap-eventCode-num                
028300     MOVE ORD-IDMSG              TO lynk-sap-eventCode2                   
028400     COMPUTE lynk-sap-eventCode2-length = FUNCTION BYTE-LENGTH (          
028500                           FUNCTION TRIM (lynk-sap-eventCode2))           
028600     .                                                                    
028700                                                                          
028800 BC-CALL-API SECTION.                                                     
028900                                                                          
029000     MOVE WS-LYNK-ADDRESS        TO W-ADDISPABS                           
029100     PERFORM IMS-GU-WDGX0104                                              
029200     IF SEGMENT-FOUND                                                     
029300       IF 0104-FLCONN = 'Y'                                               
029400         MOVE 0104-IDAPI         TO IDAPI                                 
029500         COMPUTE IDAPI-LEN        = FUNCTION LENGTH (                     
029600                                    FUNCTION TRIM (IDAPI))                
029700         MOVE 0104-IDPATH-API    TO IDPATH-API                            
029800         COMPUTE IDPATH-API-LEN   = FUNCTION LENGTH (                     
029900                                    FUNCTION TRIM (IDPATH-API))           
030000         MOVE 0104-IDPTYP-API    TO IDPTYP-API                            
030100         COMPUTE IDPTYP-API-LEN   = FUNCTION LENGTH (                     
030200                                    FUNCTION TRIM (IDPTYP-API))           
030300         MOVE 0104-IDPROXY       TO lynk-sap-proxy-key                    
030400         COMPUTE lynk-sap-proxy-key-length                                
030500                                  = FUNCTION BYTE-LENGTH (                
030600                                    FUNCTION TRIM                         
030700                                      (lynk-sap-proxy-key))               
030800       ELSE                                                               
030900         SET CONTINUE-NO         TO TRUE                                  
031000         MOVE 'FLCONN = N. Not sent'                                      
031100                                 TO RECV-MESSAGE                          
031200       END-IF                                                             
031300     ELSE                                                                 
031400       SET CONTINUE-NO           TO TRUE                                  
031500       MOVE 'F'                  TO RECV-KDKOMSTA                         
031600       MOVE 'Error - Not sent. No entry found in ATAB DB'                 
031700                                 TO RECV-MESSAGE                          
031800       MOVE 'ADR'                TO MSG-KOM-IDMFSMED                      
031900     END-IF                                                               
032000                                                                          
032100     MOVE LYNK-SAP-EVENT-REQUEST TO API-REQUEST                           
032200     COMPUTE API-REQUEST-LEN      = FUNCTION LENGTH (                     
032300                                      LYNK-SAP-EVENT-REQUEST)             
032400     IF CONTINUE-YES                                                      
032500       PERFORM S11-SEND-OPEN                                              
032600       PERFORM S12-SEND-API-HEADER                                        
032700       PERFORM S13-SEND-API-DATA                                          
032800       PERFORM S14-SEND-CLOSE                                             
032900     END-IF                                                               
033000     .                                                                    
033100                                                                          
033200 D-PROCESS-ECOM-EVENT SECTION.                                            
033300                                                                          
033400     MOVE 1                      TO ecom-eventName-num                    
033500     MOVE REQU-IDEVENT           TO ecom-eventName2                       
033600     COMPUTE ecom-eventName2-length = FUNCTION BYTE-LENGTH (              
033700                           FUNCTION TRIM (ecom-eventName2))               
033800                                                                          
033900     MOVE 1                      TO ecom-eventType2-num                   
034000     MOVE REQU-IDEVENTTYP        TO ecom-eventType3                       
034100     COMPUTE ecom-eventType3-length = FUNCTION BYTE-LENGTH (              
034200                           FUNCTION TRIM (ecom-eventType3))               
034300                                                                          
034400     MOVE 1                      TO ecom-timestamp-num                    
034500     MOVE REQU-TIMESTAMP         TO ecom-timestamp2                       
034600     COMPUTE ecom-timestamp2-length = FUNCTION BYTE-LENGTH (              
034700                           FUNCTION TRIM (ecom-timestamp2))               
034800                                                                          
034900     EVALUATE REQU-IDCPYTXT                                               
035000       WHEN 'WAPIORD'                                                     
035100         PERFORM DA-PROCESS-ECOM-ORDER                                    
035200       WHEN 'WAPIDISC'                                                    
035300         SET CONTINUE-NO         TO TRUE                                  
035400         MOVE 'Not sent. Copytext not configured'                         
035500                                 TO RECV-MESSAGE                          
035600       WHEN OTHER                                                         
035700         SET CONTINUE-NO         TO TRUE                                  
035800         MOVE 'CPY'              TO MSG-KOM-IDMFSMED                      
035900         MOVE 'Not sent. Unknown copytext'                                
036000                                 TO RECV-MESSAGE                          
036100     END-EVALUATE                                                         
036200                                                                          
036300     IF CONTINUE-YES                                                      
036400       PERFORM DC-CALL-API                                                
036500     END-IF                                                               
036600     .                                                                    
036700                                                                          
036800 DA-PROCESS-ECOM-ORDER SECTION.                                           
036900                                                                          
037000     MOVE ORD-IDAPIORDREF        TO ecom-orderReference                   
037100     COMPUTE ecom-orderReference-length                                   
037200                                  = FUNCTION BYTE-LENGTH (                
037300                                    FUNCTION TRIM (                       
037400                                     ecom-orderReference))                
037500                                                                          
037600     MOVE ORD-IDMSG              TO ecom-eventType                        
037700     COMPUTE ecom-eventType-length                                        
037800                                  = FUNCTION BYTE-LENGTH (                
037900                                    FUNCTION TRIM (                       
038000                                     ecom-eventType))                     
038100                                                                          
038200     MOVE 1                      TO ecom-eventData2-num                   
038300                                                                          
038400     MOVE 1                      TO ecom-orderId-num                      
038500     MOVE ORD-IDAPIORDREF        TO ecom-orderId2                         
038600     COMPUTE ecom-orderId2-length = FUNCTION BYTE-LENGTH (                
038700                           FUNCTION TRIM (ecom-orderId2))                 
038800                                                                          
038900     MOVE 1                      TO ecom-eventMessage-num                 
039000     MOVE ORD-TEMFSINF           TO ecom-eventMessage2                    
039100     COMPUTE ecom-eventMessage2-length = FUNCTION BYTE-LENGTH (           
039200                           FUNCTION TRIM (ecom-eventMessage2))            
039300                                                                          
039400     MOVE 1                      TO ecom-eventCode-num                    
039500     MOVE ORD-IDMSG              TO ecom-eventCode2                       
039600     COMPUTE ecom-eventCode2-length = FUNCTION BYTE-LENGTH (              
039700                           FUNCTION TRIM (ecom-eventCode2))               
039800     .                                                                    
039900                                                                          
040000 DC-CALL-API SECTION.                                                     
040100                                                                          
040200     MOVE WS-ECOM-ADDRESS        TO W-ADDISPABS                           
040300     PERFORM IMS-GU-WDGX0104                                              
040400     IF SEGMENT-FOUND                                                     
040500       IF 0104-FLCONN = 'Y'                                               
040600         MOVE 0104-IDAPI         TO IDAPI                                 
040700         COMPUTE IDAPI-LEN        = FUNCTION LENGTH (                     
040800                                    FUNCTION TRIM (IDAPI))                
040900         MOVE 0104-IDPATH-API    TO IDPATH-API                            
041000         COMPUTE IDPATH-API-LEN   = FUNCTION LENGTH (                     
041100                                    FUNCTION TRIM (IDPATH-API))           
041200         MOVE 0104-IDPTYP-API    TO IDPTYP-API                            
041300         COMPUTE IDPTYP-API-LEN   = FUNCTION LENGTH (                     
041400                                    FUNCTION TRIM (IDPTYP-API))           
041500         MOVE 0104-IDUSERKEY     TO ecom-user-key                         
041600         COMPUTE ecom-user-key-length                                     
041700                                  = FUNCTION BYTE-LENGTH (                
041800                                    FUNCTION TRIM (ecom-user-key))        
041900       ELSE                                                               
042000         SET CONTINUE-NO         TO TRUE                                  
042100         MOVE 'FLCONN = N. Not sent'                                      
042200                                 TO RECV-MESSAGE                          
042300       END-IF                                                             
042400     ELSE                                                                 
042500       SET CONTINUE-NO           TO TRUE                                  
042600       MOVE 'F'                  TO RECV-KDKOMSTA                         
042700       MOVE 'Error - Not sent. No entry found in ATAB DB'                 
042800                                 TO RECV-MESSAGE                          
042900       MOVE 'ADR'                TO MSG-KOM-IDMFSMED                      
043000     END-IF                                                               
043100                                                                          
043200     MOVE ECOM-EVENT-REQUEST     TO API-REQUEST                           
043300     COMPUTE API-REQUEST-LEN      = FUNCTION LENGTH (                     
043400                                      ECOM-EVENT-REQUEST)                 
043500                                                                          
043600     IF CONTINUE-YES                                                      
043700       PERFORM S11-SEND-OPEN                                              
043800       PERFORM S12-SEND-API-HEADER                                        
043900       PERFORM S13-SEND-API-DATA                                          
044000       PERFORM S14-SEND-CLOSE                                             
044100     END-IF                                                               
044200     .                                                                    
044300                                                                          
044400 F-PROCESS-TACD-EVENT SECTION.                                            
044500                                                                          
044600     MOVE 1                      TO tacd-eventName-num                    
044700     MOVE REQU-IDEVENT           TO tacd-eventName2                       
044800     COMPUTE tacd-eventName2-length = FUNCTION BYTE-LENGTH (              
044900                           FUNCTION TRIM (tacd-eventName2))               
045000                                                                          
045100     MOVE 1                      TO tacd-eventType-num                    
045200     MOVE REQU-IDEVENTTYP        TO tacd-eventType2                       
045300     COMPUTE tacd-eventType2-length = FUNCTION BYTE-LENGTH (              
045400                           FUNCTION TRIM (tacd-eventType2))               
045500                                                                          
045600     MOVE 1                      TO tacd-timestamp-num                    
045700     MOVE REQU-TIMESTAMP         TO tacd-timestamp2                       
045800     COMPUTE tacd-timestamp2-length = FUNCTION BYTE-LENGTH (              
045900                           FUNCTION TRIM (tacd-timestamp2))               
046000                                                                          
046100     EVALUATE REQU-IDCPYTXT                                               
046200       WHEN 'WAPIORD'                                                     
046300         PERFORM FA-PROCESS-TACD-ORDER                                    
046400       WHEN 'WAPIDISC'                                                    
046500         SET CONTINUE-NO         TO TRUE                                  
046600         MOVE 'Not sent. Copytext not configured'                         
046700                                 TO RECV-MESSAGE                          
046800       WHEN OTHER                                                         
046900         SET CONTINUE-NO         TO TRUE                                  
047000         MOVE 'CPY'              TO MSG-KOM-IDMFSMED                      
047100         MOVE 'Not sent. Unknown copytext'                                
047200                                 TO RECV-MESSAGE                          
047300     END-EVALUATE                                                         
047400                                                                          
047500     IF CONTINUE-YES                                                      
047600       PERFORM FC-CALL-API                                                
047700     END-IF                                                               
047800     .                                                                    
047900                                                                          
048000 FA-PROCESS-TACD-ORDER SECTION.                                           
048100                                                                          
048200     MOVE 1                      TO tacd-eventData2-num                   
048300                                                                          
048400     MOVE 1                      TO tacd-orderId-num                      
048500     MOVE ORD-IDAPIORDREF        TO tacd-orderId2                         
048600     COMPUTE tacd-orderId2-length = FUNCTION BYTE-LENGTH (                
048700                           FUNCTION TRIM (tacd-orderId2))                 
048800                                                                          
048900     MOVE 1                      TO tacd-eventMessage-num                 
049000     MOVE ORD-TEMFSINF           TO tacd-eventMessage2                    
049100     COMPUTE tacd-eventMessage2-length = FUNCTION BYTE-LENGTH (           
049200                           FUNCTION TRIM (tacd-eventMessage2))            
049300                                                                          
049400     MOVE 1                      TO tacd-eventCode-num                    
049500     MOVE ORD-IDMSG              TO tacd-eventCode2                       
049600     COMPUTE tacd-eventCode2-length = FUNCTION BYTE-LENGTH (              
049700                           FUNCTION TRIM (tacd-eventCode2))               
049800     .                                                                    
049900                                                                          
050000 FC-CALL-API SECTION.                                                     
050100                                                                          
050200     MOVE WS-TACD-ADDRESS        TO W-ADDISPABS                           
050300     PERFORM IMS-GU-WDGX0104                                              
050400     IF SEGMENT-FOUND                                                     
050500       IF 0104-FLCONN = 'Y'                                               
050600         MOVE 0104-IDAPI         TO IDAPI                                 
050700         COMPUTE IDAPI-LEN        = FUNCTION LENGTH (                     
050800                                    FUNCTION TRIM (IDAPI))                
050900         MOVE 0104-IDPATH-API    TO IDPATH-API                            
051000         COMPUTE IDPATH-API-LEN   = FUNCTION LENGTH (                     
051100                                    FUNCTION TRIM (IDPATH-API))           
051200         MOVE 0104-IDPTYP-API    TO IDPTYP-API                            
051300         COMPUTE IDPTYP-API-LEN   = FUNCTION LENGTH (                     
051400                                    FUNCTION TRIM (IDPTYP-API))           
051500         MOVE 0104-IDPROXY       TO tacd-proxy-key                        
051600         COMPUTE tacd-proxy-key-length                                    
051700                                  = FUNCTION BYTE-LENGTH (                
051800                                   FUNCTION TRIM (tacd-proxy-key))        
051900       ELSE                                                               
052000         SET CONTINUE-NO         TO TRUE                                  
052100         MOVE 'FLCONN = N. Not sent'                                      
052200                                 TO RECV-MESSAGE                          
052300       END-IF                                                             
052400     ELSE                                                                 
052500       SET CONTINUE-NO           TO TRUE                                  
052600       MOVE 'F'                  TO RECV-KDKOMSTA                         
052700       MOVE 'Error - Not sent. No entry found in ATAB DB'                 
052800                                 TO RECV-MESSAGE                          
052900       MOVE 'ADR'                TO MSG-KOM-IDMFSMED                      
053000     END-IF                                                               
053100                                                                          
053200     MOVE TACD-EVENT-REQUEST     TO API-REQUEST                           
053300     COMPUTE API-REQUEST-LEN      = FUNCTION LENGTH (                     
053400                                      TACD-EVENT-REQUEST)                 
053500                                                                          
053600     IF CONTINUE-YES                                                      
053700       PERFORM S11-SEND-OPEN                                              
053800       PERFORM S12-SEND-API-HEADER                                        
053900       PERFORM S13-SEND-API-DATA                                          
054000       PERFORM S14-SEND-CLOSE                                             
054100     END-IF                                                               
054200     .                                                                    
054300                                                                          
054400 G-PROCESS-APB-COMPOUND-EVENT SECTION.                                    
054500                                                                          
054600     MOVE 1                      TO vcop-eventName-num                    
054700     MOVE REQU-IDEVENT           TO vcop-eventName2                       
054800     COMPUTE vcop-eventName2-length = FUNCTION BYTE-LENGTH (              
054900                           FUNCTION TRIM (vcop-eventName2))               
055000                                                                          
055100     MOVE 1                      TO vcop-eventType-num                    
055200     MOVE REQU-IDEVENTTYP        TO vcop-eventType2                       
055300     COMPUTE vcop-eventType2-length = FUNCTION BYTE-LENGTH (              
055400                           FUNCTION TRIM (vcop-eventType2))               
055500                                                                          
055600     MOVE 1                      TO vcop-timestamp-num                    
055700     MOVE REQU-TIMESTAMP         TO vcop-timestamp2                       
055800     COMPUTE vcop-timestamp2-length = FUNCTION BYTE-LENGTH (              
055900                           FUNCTION TRIM (vcop-timestamp2))               
056000                                                                          
056100     EVALUATE REQU-IDCPYTXT                                               
056200       WHEN 'WAPIORD'                                                     
056300         PERFORM GA-PROCESS-APB-COMPOUND-ORDER                            
056400       WHEN 'WAPIDISC'                                                    
056500         SET CONTINUE-NO         TO TRUE                                  
056600         MOVE 'Not sent. Copytext not configured'                         
056700                                 TO RECV-MESSAGE                          
056800       WHEN OTHER                                                         
056900         SET CONTINUE-NO         TO TRUE                                  
057000         MOVE 'CPY'              TO MSG-KOM-IDMFSMED                      
057100         MOVE 'Not sent. Unknown copytext'                                
057200                                 TO RECV-MESSAGE                          
057300     END-EVALUATE                                                         
057400                                                                          
057500     IF CONTINUE-YES                                                      
057600       PERFORM GC-CALL-API                                                
057700     END-IF                                                               
057800     .                                                                    
057900                                                                          
058000 GA-PROCESS-APB-COMPOUND-ORDER SECTION.                                   
058100                                                                          
058200     MOVE 1                      TO vcop-eventData2-num                   
058300                                                                          
058400     MOVE 1                      TO vcop-orderId-num                      
058500     MOVE ORD-IDAPIORDREF        TO vcop-orderId2                         
058600     COMPUTE vcop-orderId2-length = FUNCTION BYTE-LENGTH (                
058700                           FUNCTION TRIM (vcop-orderId2))                 
058800                                                                          
058900     MOVE 1                      TO vcop-eventMessage-num                 
059000     MOVE ORD-TEMFSINF           TO vcop-eventMessage2                    
059100     COMPUTE vcop-eventMessage2-length = FUNCTION BYTE-LENGTH (           
059200                           FUNCTION TRIM (vcop-eventMessage2))            
059300                                                                          
059400     MOVE 1                      TO vcop-eventCode-num                    
059500     MOVE ORD-IDMSG              TO vcop-eventCode2                       
059600     COMPUTE vcop-eventCode2-length = FUNCTION BYTE-LENGTH (              
059700                           FUNCTION TRIM (vcop-eventCode2))               
059800     .                                                                    
059900                                                                          
060000 GC-CALL-API SECTION.                                                     
060100                                                                          
060200     MOVE WS-COMPOUND-ADDRESS    TO W-ADDISPABS                           
060300     PERFORM IMS-GU-WDGX0104                                              
060400     IF SEGMENT-FOUND                                                     
060500       IF 0104-FLCONN = 'Y'                                               
060600         MOVE 0104-IDAPI         TO IDAPI                                 
060700         COMPUTE IDAPI-LEN        = FUNCTION LENGTH (                     
060800                                    FUNCTION TRIM (IDAPI))                
060900         MOVE 0104-IDPATH-API    TO IDPATH-API                            
061000         COMPUTE IDPATH-API-LEN   = FUNCTION LENGTH (                     
061100                                    FUNCTION TRIM (IDPATH-API))           
061200         MOVE 0104-IDPTYP-API    TO IDPTYP-API                            
061300         COMPUTE IDPTYP-API-LEN   = FUNCTION LENGTH (                     
061400                                    FUNCTION TRIM (IDPTYP-API))           
061500         MOVE 0104-IDUSERKEY     TO vcop-user-key                         
061600         COMPUTE vcop-user-key-length                                     
061700                                  = FUNCTION BYTE-LENGTH (                
061800                                   FUNCTION TRIM (vcop-user-key))         
061900       ELSE                                                               
062000         SET CONTINUE-NO         TO TRUE                                  
062100         MOVE 'FLCONN = N. Not sent'                                      
062200                                 TO RECV-MESSAGE                          
062300       END-IF                                                             
062400     ELSE                                                                 
062500       SET CONTINUE-NO           TO TRUE                                  
062600       MOVE 'F'                  TO RECV-KDKOMSTA                         
062700       MOVE 'Error - Not sent. No entry found in ATAB DB'                 
062800                                 TO RECV-MESSAGE                          
062900       MOVE 'ADR'                TO MSG-KOM-IDMFSMED                      
063000     END-IF                                                               
063100                                                                          
063200     MOVE VCOP-EVENT-REQUEST     TO API-REQUEST                           
063300     COMPUTE API-REQUEST-LEN      = FUNCTION LENGTH (                     
063400                                      VCOP-EVENT-REQUEST)                 
063500                                                                          
063600     IF CONTINUE-YES                                                      
063700       PERFORM S11-SEND-OPEN                                              
063800       PERFORM S12-SEND-API-HEADER                                        
063900       PERFORM S13-SEND-API-DATA                                          
064000       PERFORM S14-SEND-CLOSE                                             
064100     END-IF                                                               
064200     .                                                                    
064300                                                                          
064400 Z-FINIT SECTION.                                                         
064500                                                                          
064600     IF RECV-KDKOMSTA = 'F'                                               
064700       CONTINUE                                                           
064800     ELSE                                                                 
064900       MOVE 'A'                  TO RECV-KDKOMSTA                         
065000     END-IF                                                               
065100     IF MSG-KOM-IDMFSMED = SPACE                                          
065200       MOVE '101'                TO MSG-KOM-IDMFSMED                      
065300     END-IF                                                               
065400     IF CLASSIC-DISP                                                      
065500       PERFORM IMS-INSERT-DISP-MSG                                        
065600     END-IF                                                               
065700     COMPUTE RECV-MESSAGE-KVDLEN = FUNCTION LENGTH (                      
065800                                     FUNCTION TRIM (RECV-MESSAGE))        
065900     PERFORM S03-RECV-CLOSE                                               
066000     .                                                                    
066100                                                                          
066200*    --- DISPATCHER SECTIONS                                              
066300 S01-RECV-OPEN SECTION.                                                   
066400     MOVE 'S01-RECV-OPEN'        TO WS-SECTION                            
066500                                                                          
066600     MOVE 'OPEN'                 TO RECV-KDFUNC                           
066700     MOVE 'CARPARTS.PULS.ORDEREVENTS'                                     
066800                                 TO RECV-ADDISPABS                        
066900                                                                          
067000     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
067100                                    RECV-OPEN-AREA                        
067200                                                                          
067300     IF RECV-KDRC > 0                                                     
067400       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
067500       STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
067600             DELIMITED BY SIZE INTO ERROR-TEXT                            
067700       CALL FELLOG                                                        
067800     END-IF                                                               
067900     .                                                                    
068000                                                                          
068100 S02-RECV-MESSAGE SECTION.                                                
068200     MOVE 'S02-RECV-MESSAGE'     TO WS-SECTION                            
068300                                                                          
068400     MOVE 'GET'                  TO RECV-KDFUNC                           
068500     MOVE LENGTH OF REQU-AREA    TO RECV-KVDLEN                           
068600     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
068700                                    RECV-KVDLEN                           
068800                                    REQU-AREA                             
068900     IF RECV-KDRC > 1                                                     
069000       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
069100       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
069200             DELIMITED BY SIZE INTO ERROR-TEXT                            
069300       CALL FELLOG                                                        
069400     END-IF                                                               
069500     .                                                                    
069600                                                                          
069700 S03-RECV-CLOSE SECTION.                                                  
069800     MOVE 'S03-RECV-CLOSE'       TO WS-SECTION                            
069900                                                                          
070000     MOVE 'CLOSE'                TO RECV-KDFUNC                           
070100     CALL WZ01RECV            USING RECV-CONTROL-AREA                     
070200                                    RECV-CLOSE-AREA                       
070300                                                                          
070400     IF RECV-KDRC > 0                                                     
070500       MOVE RECV-KDRC            TO KDRC-DISPLAY                          
070600       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
070700             DELIMITED BY SIZE INTO ERROR-TEXT                            
070800       CALL FELLOG                                                        
070900     END-IF                                                               
071000     .                                                                    
071100                                                                          
071200 S11-SEND-OPEN SECTION.                                                   
071300                                                                          
071400     MOVE 'S11-SEND-OPEN'        TO WS-SECTION                            
071500                                                                          
071600     MOVE W-ADDISPABS            TO SEND-ADDISPABS                        
071700     MOVE 'OPEN'                 TO SEND-KDFUNC                           
071800     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
071900                                    SEND-OPEN-AREA                        
072000     IF SEND-KDRC > ZERO                                                  
072100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
072200       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
072300             DELIMITED BY SIZE INTO ERROR-TEXT                            
072400       CALL FELLOG                                                        
072500     END-IF                                                               
072600     .                                                                    
072700                                                                          
072800 S12-SEND-API-HEADER SECTION.                                             
072900                                                                          
073000     MOVE 'S12-SEND-API-HEADER'  TO WS-SECTION                            
073100                                                                          
073200     MOVE 'PUT'                  TO SEND-KDFUNC                           
073300     MOVE LENGTH OF WAPIINFO     TO SEND-KVDLEN                           
073400     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
073500                                    SEND-KVDLEN                           
073600                                    WAPIINFO                              
073700     IF SEND-KDRC > ZERO                                                  
073800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
073900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
074000             DELIMITED BY SIZE INTO ERROR-TEXT                            
074100       CALL FELLOG                                                        
074200     END-IF                                                               
074300     .                                                                    
074400                                                                          
074500 S13-SEND-API-DATA SECTION.                                               
074600     MOVE 'S13-SEND-API-DATA '   TO WS-SECTION                            
074700                                                                          
074800     MOVE 'PUT'                  TO SEND-KDFUNC                           
074900     MOVE API-REQUEST-LEN        TO SEND-KVDLEN                           
075000     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
075100                                    SEND-KVDLEN                           
075200                                    API-REQUEST                           
075300                                                                          
075400     IF SEND-KDRC > ZERO                                                  
075500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
075600       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
075700       DELIMITED BY SIZE       INTO ERROR-TEXT                            
075800       CALL FELLOG                                                        
075900     END-IF                                                               
076000     .                                                                    
076100                                                                          
076200 S14-SEND-CLOSE SECTION.                                                  
076300     MOVE 'S14-SEND-CLOSE'       TO WS-SECTION                            
076400                                                                          
076500     MOVE 'CLOSE'                TO SEND-KDFUNC                           
076600     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
076700                                                                          
076800     IF SEND-KDRC > 0                                                     
076900       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
077000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
077100             DELIMITED BY SIZE INTO ERROR-TEXT                            
077200       CALL FELLOG                                                        
077300     END-IF                                                               
077400     .                                                                    
077500                                                                          
077600* --- IMS SECTIONS ---                                                    
077700 IMS-INSERT-DISP-MSG SECTION.                                             
077800                                                                          
077900     MOVE SPACE                  TO GOOD-STATUSCODES                      
078000     CALL CBLTDLI             USING ISRT                                  
078100                                    DISP-PCB                              
078200                                    KOM-IO-AREA                           
078300     MOVE DISP-STATUS-CODE       TO STATUS-WS                             
078400     PERFORM IMS-STATUSCHECK                                              
078500     .                                                                    
078600                                                                          
078700 IMS-GU-WDGX0104  SECTION.                                                
078800                                                                          
078900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
079000             DELIMITED BY SIZE INTO SSA1                                  
079100     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
079200             DELIMITED BY SIZE INTO SSA2                                  
079300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
079400     CALL CBLTDLI             USING GU                                    
079500                                    ATAB-PCB                              
079600                                    DLI-IO-WDGX0104                       
079700                                    SSA1                                  
079800                                    SSA2                                  
079900     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
080000     PERFORM IMS-STATUSCHECK                                              
080100     .                                                                    
080200                                                                          
080300 IMS-STATUSCHECK SECTION.                                                 
080400                                                                          
080500     SET STATUS-IX               TO 1                                     
080600     SEARCH GOOD-STATUS                                                   
080700       AT END                                                             
080800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
080900             DELIMITED BY SIZE INTO ERROR-TEXT                            
081000         CALL FELLOG                                                      
081100       WHEN GOOD-STATUS(STATUS-IX) = STATUS-WS                            
081200         CONTINUE                                                         
081300     END-SEARCH                                                           
081400     .                                                                    
