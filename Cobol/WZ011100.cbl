000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ011100.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   24/08/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.DISPTABMAINT                               
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAM TO HANDLE ENTRIES IN DISPATCHER TABLE                    
001100*                                    (WDR5/0103)                          
001200*                                                                         
001300*        THE PROGRAM UPDATES   WDR5                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: WZA111                                              
001700*        REQUEST:     WZ0111I1                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        RESPONSE:    WZ0111O1                                            
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 CONFIGURATION SECTION.                                                   
002400 SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
002500 INPUT-OUTPUT SECTION.                                                    
002600 FILE-CONTROL.                                                            
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'WZ011100'.            
003100                                                                          
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003300 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003400 77  KDRC-DISPLAY                PIC Z(5).                                
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- PARAMETERS TO VIMSID                                             
004000 01  W-VIMSID                    PIC X(8)  VALUE SPACE.                   
004100 01  FILLER REDEFINES W-VIMSID.                                           
004200     03 IMS-REGION               PIC X(3).                                
004300        88 ENV-TEST                     VALUE 'IMD' 'IMP' 'IMY'           
004400                                             'IMB'.                       
004500        88 ENV-DEVE                     VALUE 'IMP'.                      
004600        88 ENV-IGRT                     VALUE 'IMY'.                      
004700        88 ENV-XDEV                     VALUE 'IMD'.                      
004800        88 ENV-ACPT                     VALUE 'IMB'.                      
004900        88 ENV-PROD                     VALUE 'IMG' 'IMR'.                
005000    03  FILLER                   PIC X(5).                                
005100                                                                          
005200                                                                          
005300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005400     88  KEYS-OK                             VALUE 'J'.                   
005500     88  KEYS-WRONG                          VALUE 'N'.                   
005600                                                                          
005700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005800     88  INDATA-OK                           VALUE 'J'.                   
005900     88  INDATA-WRONG                        VALUE 'N'.                   
006000                                                                          
006100 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
006200     88  UPDATE-OK                           VALUE 'J'.                   
006300     88  UPDATE-WRONG                        VALUE 'N'.                   
006400                                                                          
006500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007100     03  VIMSID                  PIC X(8)    VALUE 'VIMSID'.              
007200                                                                          
007300*    --- PARAMETERS TO ABEND                                              
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800                                                                          
007900 01  MESSAGE-CODES.                                                       
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008100     03  ERR-IS-INVALID          PIC X(3)    VALUE '023'.                 
008200     03  ERR-IS-MISSING          PIC X(3)    VALUE '025'.                 
008300     03  ERR-ALREADY-EXIST       PIC X(3)    VALUE '047'.                 
008400                                                                          
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008700                                                                          
008800*01  -COPY WZ01SUB                                                        
008900                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009100                                                                          
009200 01  REQU-AREA.                                                           
009300*    03  -COPY WZ01REQ2                                                   
009400*    03  -COPY WZ0111I1                                                   
009500                                                                          
009600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009700                                                                          
009800 01  RESP-AREA.                                                           
009900*    03  -COPY WZ01RES2                                                   
010000*    03  -COPY WZ0111O1                                                   
010100                                                                          
010200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010300*                                                                         
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500                                                                          
010600 01  KEYS-FOR-DLI.                                                        
010700     03  W-WDGXKEY-0103-X.                                                
010800         05  W-IDHTYP-0103       PIC X(4)    VALUE '0103'.                
010900         05  FILLER              PIC X(26)   VALUE LOW-VALUES.            
011000     03  W-KY0104-X.                                                      
011100         05  W-ADDISPABS         PIC X(50)   VALUE SPACES.                
011200                                                                          
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FOUND                       VALUE '  '.                  
011600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011800                                                                          
011900 01  GOOD-STATUSCODES.                                                    
012000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100                                                                          
012200 01  SSA1                        PIC X(128).                              
012300 01  SSA2                        PIC X(128).                              
012400                                                                          
012500*    --- IMS FUNCTION CODES                                               
012600*01  -COPY W0003                                                          
012700                                                                          
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
012900 01  DLI-IO-WDGX01.                                                       
013000*    03  -COPY WDGX01                                                     
013100                                                                          
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX0104'.                    
013300 01  DLI-IO-WDGX0104.                                                     
013400*    03  -COPY WDGX0104                                                   
013500                                                                          
013600 LINKAGE SECTION.                                                         
013700*01  -COPY W0009  -PRE MSG-                                               
013800                                                                          
013900*01  -COPY W0008  -PRE ATAB-                                              
014000     05  FILLER                  PIC X.                                   
014100                                                                          
014200 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB.                              
014300 MAIN SECTION.                                                            
014400                                                                          
014500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014600     IF SUB-KDRC = 0                                                      
014700       PERFORM A-INIT                                                     
014800       PERFORM B-CHECK-KEYS                                               
014900       IF KEYS-OK                                                         
015000         IF REQU-UPDATE                                                   
015100           PERFORM G-CHECK-INPUT                                          
015200           IF INDATA-OK                                                   
015300             PERFORM H-UPDATE                                             
015400           END-IF                                                         
015500         END-IF                                                           
015600         IF REQU-QUERY OR                                                 
015700            (REQU-UPDATE AND UPDATE-OK)                                   
015800           PERFORM F-READ-SHOW-INFO                                       
015900         END-IF                                                           
016000       END-IF                                                             
016100       PERFORM S02-RETURN-RESPONSE                                        
016200     END-IF                                                               
016300                                                                          
016400     PERFORM Z-FINIT                                                      
016500     MOVE ZERO                   TO RETURN-CODE                           
016600     GOBACK                                                               
016700     .                                                                    
016800                                                                          
016900 A-INIT SECTION.                                                          
017000                                                                          
017100     CONTINUE                                                             
017200     .                                                                    
017300                                                                          
017400 B-CHECK-KEYS SECTION.                                                    
017500                                                                          
017600     MOVE YES                    TO KEYS-SW                               
017700                                                                          
017800     IF REQU-ADDISPABS = SPACES OR LOW-VALUES                             
017900       MOVE NOO                  TO KEYS-SW                               
018000       MOVE 'ADDISPABS'          TO RESP-IDELMT-ERROR                     
018100     ELSE                                                                 
018200       MOVE FUNCTION UPPER-CASE (REQU-ADDISPABS)                          
018300                                 TO W-ADDISPABS                           
018400     END-IF                                                               
018500                                                                          
018600     IF KEYS-WRONG                                                        
018700       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
018800     END-IF                                                               
018900     .                                                                    
019000                                                                          
019100 G-CHECK-INPUT SECTION.                                                   
019200                                                                          
019300     MOVE YES                    TO INDATA-SW                             
019400                                                                          
019500     PERFORM IMS-GHU-WDGX0104                                             
019600     IF REQU-KDCMD-INSERT                                                 
019700       IF SEGMENT-FOUND                                                   
019800         MOVE ERR-ALREADY-EXIST  TO RESP-IDMSG-ERROR                      
019900         MOVE 'ADDISPABS'        TO RESP-IDELMT-ERROR                     
020000         MOVE NOO                TO INDATA-SW                             
020100       END-IF                                                             
020200     END-IF                                                               
020300                                                                          
020400     IF REQU-KDCMD-REPLACE OR REQU-KDCMD-DELETE                           
020500       IF SEGMENT-MISSING                                                 
020600         MOVE ERR-IS-MISSING     TO RESP-IDMSG-ERROR                      
020700         MOVE 'ADDISPABS'        TO RESP-IDELMT-ERROR                     
020800         MOVE NOO                TO INDATA-SW                             
020900       END-IF                                                             
021000     END-IF                                                               
021100                                                                          
021200     IF REQU-KDCMD-INSERT OR REQU-KDCMD-REPLACE                           
021300       PERFORM GA-CHECK-ATTRIBUTES                                        
021400     END-IF                                                               
021500     .                                                                    
021600                                                                          
021700 GA-CHECK-ATTRIBUTES SECTION.                                             
021800                                                                          
021900     IF INDATA-OK                                                         
022000       IF REQU-KDCOMTYPE = 'API' OR 'MQ'                                  
022100         MOVE REQU-KDCOMTYPE     TO 0104-KDCOMTYPE                        
022200       ELSE                                                               
022300         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
022400         MOVE 'KDCOMTYPE'        TO RESP-IDELMT-ERROR                     
022500         MOVE NOO                TO INDATA-SW                             
022600       END-IF                                                             
022700     END-IF                                                               
022800                                                                          
022900     IF INDATA-OK                                                         
023000       IF REQU-FLCOMMDIR = 'I' OR 'O'                                     
023100         MOVE REQU-FLCOMMDIR     TO 0104-FLCOMMDIR                        
023200       ELSE                                                               
023300         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
023400         MOVE 'FLCOMMDIR'        TO RESP-IDELMT-ERROR                     
023500         MOVE NOO                TO INDATA-SW                             
023600       END-IF                                                             
023700     END-IF                                                               
023800                                                                          
023900     IF INDATA-OK                                                         
024000       IF REQU-FLCONN = 'Y' OR 'N' OR SPACES                              
024100         IF REQU-FLCONN = SPACES                                          
024200           MOVE 'Y'              TO REQU-FLCONN                           
024300         END-IF                                                           
024400         MOVE REQU-FLCONN        TO 0104-FLCONN                           
024500       ELSE                                                               
024600         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
024700         MOVE 'FLCONN'           TO RESP-IDELMT-ERROR                     
024800         MOVE NOO                TO INDATA-SW                             
024900       END-IF                                                             
025000     END-IF                                                               
025100                                                                          
025200     IF INDATA-OK                                                         
025300       IF REQU-KVDAGAR-RESEND IS NUMERIC                                  
025400         MOVE REQU-KVDAGAR-RESEND                                         
025500                                 TO 0104-KVDAGAR-RESEND                   
025600       ELSE                                                               
025700         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
025800         MOVE 'KVDAGAR-RESEND'   TO RESP-IDELMT-ERROR                     
025900         MOVE NOO                TO INDATA-SW                             
026000       END-IF                                                             
026100     END-IF                                                               
026200                                                                          
026300     IF INDATA-OK AND REQU-KDCOMTYPE = 'API'                              
026400       PERFORM GAA-CHECK-API-METADATA                                     
026500     END-IF                                                               
026600                                                                          
026700     IF INDATA-OK AND REQU-KDCOMTYPE = 'MQ'                               
026800       PERFORM GAB-CHECK-MQ-METADATA                                      
026900     END-IF                                                               
027000     .                                                                    
027100                                                                          
027200 GAA-CHECK-API-METADATA SECTION.                                          
027300                                                                          
027400     IF INDATA-OK                                                         
027500       IF REQU-IDAPI OF REQU-API-METADATA = SPACES OR LOW-VALUES          
027600         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
027700         MOVE 'IDAPI'            TO RESP-IDELMT-ERROR                     
027800         MOVE NOO                TO INDATA-SW                             
027900       ELSE                                                               
028000         MOVE REQU-IDAPI OF REQU-API-METADATA                             
028100                                 TO 0104-IDAPI                            
028200       END-IF                                                             
028300     END-IF                                                               
028400                                                                          
028500     IF INDATA-OK                                                         
028600       IF REQU-IDUSERKEY OF REQU-API-METADATA = LOW-VALUES                
028700         MOVE SPACES             TO REQU-IDUSERKEY                        
028800                                    OF REQU-API-METADATA                  
028900       END-IF                                                             
029000       MOVE REQU-IDUSERKEY OF REQU-API-METADATA                           
029100                                 TO 0104-IDUSERKEY                        
029200     END-IF                                                               
029300                                                                          
029400     IF INDATA-OK                                                         
029500       IF REQU-IDPATH-API = SPACES OR LOW-VALUES                          
029600         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
029700         MOVE 'IDPATH-API'       TO RESP-IDELMT-ERROR                     
029800         MOVE NOO                TO INDATA-SW                             
029900       END-IF                                                             
030000       MOVE REQU-IDPATH-API      TO 0104-IDPATH-API                       
030100     END-IF                                                               
030200                                                                          
030300     IF INDATA-OK                                                         
030400       IF REQU-IDPTYP-API = SPACES OR LOW-VALUES                          
030500         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
030600         MOVE 'IDPTYP-API'       TO RESP-IDELMT-ERROR                     
030700         MOVE NOO                TO INDATA-SW                             
030800       END-IF                                                             
030900       MOVE REQU-IDPTYP-API      TO 0104-IDPTYP-API                       
031000     END-IF                                                               
031100                                                                          
031200     IF INDATA-OK                                                         
031300       IF REQU-IDPROXY = LOW-VALUES                                       
031400         MOVE SPACES             TO REQU-IDPROXY                          
031500       END-IF                                                             
031600       MOVE REQU-IDPROXY         TO 0104-IDPROXY                          
031700     END-IF                                                               
031800                                                                          
031900     IF INDATA-OK                                                         
032000       IF REQU-IDAPPKEY = LOW-VALUES                                      
032100         MOVE SPACES             TO REQU-IDAPPKEY                         
032200       END-IF                                                             
032300       MOVE REQU-IDAPPKEY        TO 0104-IDAPPKEY                         
032400     END-IF                                                               
032500     .                                                                    
032600                                                                          
032700 GAB-CHECK-MQ-METADATA SECTION.                                           
032800                                                                          
032900     IF INDATA-OK                                                         
033000       IF REQU-IDINTEGRATION = SPACES OR LOW-VALUES                       
033100         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
033200         MOVE 'IDINTEGRATION'    TO RESP-IDELMT-ERROR                     
033300         MOVE NOO                TO INDATA-SW                             
033400       END-IF                                                             
033500       MOVE REQU-IDINTEGRATION   TO 0104-IDINTEGRATION                    
033600     END-IF                                                               
033700                                                                          
033800     IF INDATA-OK                                                         
033900       IF REQU-IDCONTRACT-MQ = SPACES OR LOW-VALUES                       
034000         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
034100         MOVE 'IDCONTRACT-MQ'    TO RESP-IDELMT-ERROR                     
034200         MOVE NOO                TO INDATA-SW                             
034300       END-IF                                                             
034400       MOVE REQU-IDCONTRACT-MQ   TO 0104-IDCONTRACT-MQ                    
034500     END-IF                                                               
034600                                                                          
034700     IF INDATA-OK                                                         
034800       IF REQU-IDMQOBJ = SPACES OR LOW-VALUES                             
034900         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
035000         MOVE 'IDMQOBJ'          TO RESP-IDELMT-ERROR                     
035100         MOVE NOO                TO INDATA-SW                             
035200       END-IF                                                             
035300       MOVE REQU-IDMQOBJ         TO 0104-IDMQOBJ                          
035400     END-IF                                                               
035500                                                                          
035600     IF INDATA-OK                                                         
035700       IF REQU-KDMQOBJ = 'QUEUE' OR 'TOPIC' OR 'SUBSCRIPTION'             
035800         CONTINUE                                                         
035900       ELSE                                                               
036000         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
036100         MOVE 'KDMQOBJ'          TO RESP-IDELMT-ERROR                     
036200         MOVE NOO                TO INDATA-SW                             
036300       END-IF                                                             
036400       MOVE REQU-KDMQOBJ         TO 0104-KDMQOBJ                          
036500     END-IF                                                               
036600                                                                          
036700     IF INDATA-OK                                                         
036800       IF REQU-KDMQFMT = SPACES OR 'MQSTR' OR 'STRING'                    
036900         IF REQU-KDMQFMT = 'STRING'                                       
037000           MOVE 'MQSTR'          TO REQU-KDMQFMT                          
037100         END-IF                                                           
037200       ELSE                                                               
037300         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
037400         MOVE 'KDMQFMT'          TO RESP-IDELMT-ERROR                     
037500         MOVE NOO                TO INDATA-SW                             
037600       END-IF                                                             
037700       MOVE REQU-KDMQFMT         TO 0104-KDMQFMT                          
037800     END-IF                                                               
037900                                                                          
038000     IF INDATA-OK                                                         
038100       IF REQU-KDDELIM-REC = LOW-VALUES                                   
038200         MOVE SPACES             TO REQU-KDDELIM-REC                      
038300       END-IF                                                             
038400       MOVE REQU-KDDELIM-REC     TO 0104-KDDELIM-REC                      
038500     END-IF                                                               
038600                                                                          
038700     IF INDATA-OK                                                         
038800       IF REQU-KDMSGLEN = 'B' OR 'KB' OR 'MB'                             
038900         CONTINUE                                                         
039000       ELSE                                                               
039100         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
039200         MOVE 'KDMSGLEN'         TO RESP-IDELMT-ERROR                     
039300         MOVE NOO                TO INDATA-SW                             
039400       END-IF                                                             
039500       MOVE REQU-KDMSGLEN        TO 0104-KDMSGLEN                         
039600     END-IF                                                               
039700                                                                          
039800     IF INDATA-OK                                                         
039900       IF REQU-KVMSGLEN-MAX IS NUMERIC AND                                
040000          REQU-KVMSGLEN-MAX > 0                                           
040100         CONTINUE                                                         
040200       ELSE                                                               
040300         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
040400         MOVE 'KVMSGLEN-MAX'     TO RESP-IDELMT-ERROR                     
040500         MOVE NOO                TO INDATA-SW                             
040600       END-IF                                                             
040700       MOVE REQU-KVMSGLEN-MAX    TO 0104-KVMSGLEN-MAX                     
040800     END-IF                                                               
040900                                                                          
041000     IF INDATA-OK                                                         
041100       IF REQU-KDMSGLEN = 'MB' AND                                        
041200          REQU-KVMSGLEN-MAX > 100                                         
041300         MOVE 'KVMSGLEN-MAX'     TO RESP-IDELMT-ERROR                     
041400         MOVE NOO                TO INDATA-SW                             
041500       END-IF                                                             
041600     END-IF                                                               
041700                                                                          
041800     IF INDATA-OK                                                         
041900       IF REQU-FLMSGSPLIT = 'Y' OR 'N' OR SPACES                          
042000         IF REQU-FLMSGSPLIT = SPACES                                      
042100           MOVE 'N'              TO REQU-FLMSGSPLIT                       
042200         END-IF                                                           
042300       ELSE                                                               
042400         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
042500         MOVE 'FLMSGSPLIT'       TO RESP-IDELMT-ERROR                     
042600         MOVE NOO                TO INDATA-SW                             
042700       END-IF                                                             
042800       MOVE REQU-FLMSGSPLIT      TO 0104-FLMSGSPLIT                       
042900     END-IF                                                               
043000                                                                          
043100     IF INDATA-OK                                                         
043200       IF REQU-FLMSGGROUP = 'Y' OR 'N' OR SPACES                          
043300         IF REQU-FLMSGGROUP = SPACES                                      
043400           MOVE 'N'              TO REQU-FLMSGGROUP                       
043500         END-IF                                                           
043600       ELSE                                                               
043700         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
043800         MOVE 'FLMSGGROUP'       TO RESP-IDELMT-ERROR                     
043900         MOVE NOO                TO INDATA-SW                             
044000       END-IF                                                             
044100       MOVE REQU-FLMSGGROUP      TO 0104-FLMSGGROUP                       
044200     END-IF                                                               
044300                                                                          
044400     IF INDATA-OK                                                         
044500       CALL VIMSID            USING W-VIMSID                              
044600       IF REQU-IDQMGR = LOW-VALUES                                        
044700         MOVE SPACES             TO REQU-IDQMGR                           
044800       END-IF                                                             
044900       EVALUATE TRUE ALSO REQU-IDQMGR                                     
045000         WHEN ENV-PROD ALSO 'MC0P'                                        
045100         WHEN ENV-ACPT ALSO 'MC0Q'                                        
045200         WHEN ENV-TEST ALSO 'MC0T'                                        
045300         WHEN ENV-TEST ALSO 'MC3Q'                                        
045400         WHEN ENV-TEST ALSO 'MC3T'                                        
045500         WHEN ENV-TEST ALSO 'MC4T'                                        
045600           CONTINUE                                                       
045700         WHEN ENV-PROD ALSO SPACES                                        
045800           MOVE 'MC0P'           TO REQU-IDQMGR                           
045900         WHEN ENV-ACPT ALSO SPACES                                        
046000           MOVE 'MC0Q'           TO REQU-IDQMGR                           
046100         WHEN ENV-TEST ALSO SPACES                                        
046200           MOVE 'MC0T'           TO REQU-IDQMGR                           
046300         WHEN OTHER                                                       
046400           MOVE ERR-IS-INVALID   TO RESP-IDMSG-ERROR                      
046500           MOVE 'IDQMGR'         TO RESP-IDELMT-ERROR                     
046600           MOVE NOO              TO INDATA-SW                             
046700       END-EVALUATE                                                       
046800       MOVE REQU-IDQMGR          TO 0104-IDQMGR                           
046900     END-IF                                                               
047000                                                                          
047100     IF INDATA-OK                                                         
047200       IF REQU-KDCCSID = 278 OR 37 OR 1208 OR 1047 OR 0                   
047300         IF REQU-KDCCSID = ZERO                                           
047400           MOVE 278              TO REQU-KDCCSID                          
047500         END-IF                                                           
047600       ELSE                                                               
047700         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
047800         MOVE 'KDCCSID'          TO RESP-IDELMT-ERROR                     
047900         MOVE NOO                TO INDATA-SW                             
048000       END-IF                                                             
048100       MOVE REQU-KDCCSID         TO 0104-KDCCSID                          
048200     END-IF                                                               
048300                                                                          
048400     IF INDATA-OK                                                         
048500       IF REQU-KDEOF = LOW-VALUES                                         
048600         MOVE SPACES             TO REQU-KDEOF                            
048700       END-IF                                                             
048800       MOVE REQU-KDEOF           TO 0104-KDEOF                            
048900     END-IF                                                               
049000                                                                          
049100     IF INDATA-OK                                                         
049200       IF REQU-KDLOG-LEVEL = 'T' OR 'D' OR 'E' OR 'W' OR 'I' OR           
049300                             SPACES OR LOW-VALUES                         
049400         IF REQU-KDLOG-LEVEL = SPACES OR LOW-VALUES                       
049500           MOVE 'I'              TO REQU-KDLOG-LEVEL                      
049600         END-IF                                                           
049700       ELSE                                                               
049800         MOVE ERR-IS-INVALID     TO RESP-IDMSG-ERROR                      
049900         MOVE 'KDLOG-LEVEL'      TO RESP-IDELMT-ERROR                     
050000         MOVE NOO                TO INDATA-SW                             
050100       END-IF                                                             
050200       MOVE REQU-KDLOG-LEVEL     TO 0104-KDLOG-LEVEL                      
050300     END-IF                                                               
050400                                                                          
050500     .                                                                    
050600                                                                          
050700 H-UPDATE SECTION.                                                        
050800                                                                          
050900     IF REQU-KDCMD-INSERT                                                 
051000       MOVE W-ADDISPABS          TO 0104-ADDISPABS                        
051100       PERFORM IMS-ISRT-WDGX0104                                          
051200     END-IF                                                               
051300     IF REQU-KDCMD-REPLACE                                                
051400       PERFORM IMS-REPL-WDGX0104                                          
051500     END-IF                                                               
051600     .                                                                    
051700                                                                          
051800 F-READ-SHOW-INFO SECTION.                                                
051900                                                                          
052000     PERFORM IMS-GU-WDGX0104                                              
052100     IF SEGMENT-MISSING                                                   
052200        MOVE '025'               TO RESP-IDMSG-INFO                       
052300     ELSE                                                                 
052400       MOVE 0104-ADDISPABS       TO RESP-ADDISPABS                        
052500       MOVE 0104-KDCOMTYPE       TO RESP-KDCOMTYPE                        
052600       MOVE 0104-FLCOMMDIR       TO RESP-FLCOMMDIR                        
052700       MOVE 0104-FLCONN          TO RESP-FLCONN                           
052800       MOVE 0104-KVDAGAR-RESEND  TO RESP-KVDAGAR-RESEND                   
052900                                                                          
053000       IF 0104-KDCOMTYPE = 'API'                                          
053100         MOVE LOW-VALUES         TO RESP-MQ-METADATA                      
053200         MOVE 0104-IDAPI         TO RESP-IDAPI                            
053300         MOVE 0104-IDUSERKEY     TO RESP-IDUSERKEY                        
053400         MOVE 0104-IDPATH-API    TO RESP-IDPATH-API                       
053500         MOVE 0104-IDPTYP-API    TO RESP-IDPTYP-API                       
053600         MOVE 0104-IDPROXY       TO RESP-IDPROXY                          
053700         MOVE 0104-IDAPPKEY      TO RESP-IDAPPKEY                         
053800       END-IF                                                             
053900                                                                          
054000       IF 0104-KDCOMTYPE = 'MQ'                                           
054100         MOVE LOW-VALUES         TO RESP-API-METADATA                     
054200         MOVE 0104-IDINTEGRATION TO RESP-IDINTEGRATION                    
054300         MOVE 0104-IDCONTRACT-MQ TO RESP-IDCONTRACT-MQ                    
054400         MOVE 0104-IDMQOBJ       TO RESP-IDMQOBJ                          
054500         MOVE 0104-KDMQOBJ       TO RESP-KDMQOBJ                          
054600         MOVE 0104-KDMQFMT       TO RESP-KDMQFMT                          
054700         MOVE 0104-KDDELIM-REC   TO RESP-KDDELIM-REC                      
054800         MOVE 0104-KVMSGLEN-MAX  TO RESP-KVMSGLEN-MAX                     
054900         MOVE 0104-KDMSGLEN      TO RESP-KDMSGLEN                         
055000         MOVE 0104-FLMSGSPLIT    TO RESP-FLMSGSPLIT                       
055100         MOVE 0104-FLMSGGROUP    TO RESP-FLMSGGROUP                       
055200         MOVE 0104-IDQMGR        TO RESP-IDQMGR                           
055300         MOVE 0104-KDCCSID       TO RESP-KDCCSID                          
055400         MOVE 0104-KDEOF         TO RESP-KDEOF                            
055500         MOVE 0104-KDLOG-LEVEL   TO RESP-KDLOG-LEVEL                      
055600       END-IF                                                             
055700     END-IF                                                               
055800     .                                                                    
055900                                                                          
056000                                                                          
056100 Z-FINIT SECTION.                                                         
056200                                                                          
056300     CONTINUE                                                             
056400     .                                                                    
056500                                                                          
056600*    --- DISPATCHER SECTIONS                                              
056700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
056800                                                                          
056900     MOVE 'GETARG'               TO SUB-KDFUNC                            
057000     MOVE 'CARPARTS.PULS.DISPTABMAINT'                                    
057100                                 TO SUB-ADDISPABS                         
057200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
057300                                                                          
057400     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
057500                                    SUB-KVDLEN                            
057600                                    REQU-AREA                             
057700                                                                          
057800     IF SUB-KDRC > 0                                                      
057900       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
058000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
058100             DELIMITED BY SIZE INTO ERROR-TEXT                            
058200       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
058300     END-IF                                                               
058400     .                                                                    
058500                                                                          
058600 S02-RETURN-RESPONSE SECTION.                                             
058700                                                                          
058800     MOVE 'RETURN'               TO SUB-KDFUNC                            
058900     MOVE LENGTH OF RESP-AREA    TO SUB-KVDLEN                            
059000                                                                          
059100                                                                          
059200     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
059300                                    SUB-KVDLEN                            
059400                                    RESP-AREA                             
059500                                                                          
059600     IF SUB-KDRC > 0                                                      
059700       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
059800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
059900             DELIMITED BY SIZE INTO ERROR-TEXT                            
060000       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
060100     END-IF                                                               
060200     .                                                                    
060300                                                                          
060400 IMS-GU-WDGX0104 SECTION.                                                 
060500                                                                          
060600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
060700             DELIMITED BY SIZE INTO SSA1                                  
060800     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
060900             DELIMITED BY SIZE INTO SSA2                                  
061000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
061100     CALL CBLTDLI             USING GU                                    
061200                                    ATAB-PCB                              
061300                                    DLI-IO-WDGX0104                       
061400                                    SSA1                                  
061500                                    SSA2                                  
061600     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
061700     PERFORM IMS-STATUSCHECK                                              
061800     .                                                                    
061900 IMS-GHU-WDGX0104 SECTION.                                                
062000                                                                          
062100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
062200             DELIMITED BY SIZE INTO SSA1                                  
062300     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
062400             DELIMITED BY SIZE INTO SSA2                                  
062500     MOVE '  GE'                 TO GOOD-STATUSCODES                      
062600     CALL CBLTDLI             USING GHU                                   
062700                                    ATAB-PCB                              
062800                                    DLI-IO-WDGX0104                       
062900                                    SSA1                                  
063000                                    SSA2                                  
063100     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
063200     PERFORM IMS-STATUSCHECK                                              
063300     .                                                                    
063400 IMS-ISRT-WDGX0104 SECTION.                                               
063500                                                                          
063600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
063700             DELIMITED BY SIZE INTO SSA1                                  
063800     MOVE 'WDGX0104 '            TO SSA2                                  
063900     MOVE SPACES                 TO GOOD-STATUSCODES                      
064000     CALL CBLTDLI             USING ISRT                                  
064100                                    ATAB-PCB                              
064200                                    DLI-IO-WDGX0104                       
064300                                    SSA1                                  
064400                                    SSA2                                  
064500     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
064600     PERFORM IMS-STATUSCHECK                                              
064700     .                                                                    
064800 IMS-REPL-WDGX0104 SECTION.                                               
064900                                                                          
065000     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
065100             DELIMITED BY SIZE INTO SSA1                                  
065200     MOVE SPACES                 TO GOOD-STATUSCODES                      
065300     CALL CBLTDLI             USING REPL                                  
065400                                    ATAB-PCB                              
065500                                    DLI-IO-WDGX0104                       
065600                                    SSA1                                  
065700     MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
065800     PERFORM IMS-STATUSCHECK                                              
065900     .                                                                    
066000 IMS-STATUSCHECK SECTION.                                                 
066100                                                                          
066200     SET STATUS-IX               TO 1                                     
066300     SEARCH GOOD-STATUS                                                   
066400       AT END                                                             
066500         STRING 'INVALID STATUS FROM IMS:' STATUS-WS                      
066600             DELIMITED BY SIZE INTO ERROR-TEXT                            
066700         CALL ABEND           USING RKOD-ABEND-WITH-DUMP                  
066800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
066900         CONTINUE                                                         
067000     END-SEARCH                                                           
067100     .                                                                    
