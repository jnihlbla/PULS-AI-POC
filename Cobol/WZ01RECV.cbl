000100**ROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ01RECV.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   01/11/20                                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*      GENERAL SUBPROGRAM FOR RECEIVING ASYNCHRONOUS MESSAGES.            
001000*      IT IS PART OF THE NEW DISPATCHER FRAMEWORK USED IN                 
001100*      THE CAR PARTS SYSTEMS. CURRENTLY IT HANDLES COMMUNICATION          
001200*      VIA VCOM OR IMS PROGRAM-TO-PROGRAM SWICHING.                       
001300*      MQ IS NOW ALSO SUPPORTED.                                          
001400*                                                                         
001500*      CALL SYNTAX:                                                       
001600*      (OPEN)                                                             
001700*        CALL WZ01RECV USING RECV-CONTROL-AREA                            
001800*                            RECV-OPEN-AREA                               
001900*                                                                         
002000*      (RECV)                                                             
002100*        CALL WZ01RECV USING RECV-CONTROL-AREA                            
002200*                            RECV-KVDLEN                                  
002300*                            MY-DATA-FIELD                                
002400*                                                                         
002500*      (CLOSE)                                                            
002600*        CALL WZ01RECV USING RECV-CONTROL-AREA                            
002700*                                                                         
002800*      THE PARAMETERS USED IN THE CALLS ARE ALL DEFINED IN                
002900*      COPYTEXT WZ01RECV AND EXPLAINED IN MORE DETAIL IN THIS             
003000*      COPYTEXT.                                                          
003100*                                                                         
003200                                                                          
003300 DATA DIVISION.                                                           
003400                                                                          
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'WZ01RECV'.            
003800 77  YES                         PIC X       VALUE 'J'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
004000 77  CR                          PIC X       VALUE X'0D'.                 
004100 77  LF-EBCDIC                   PIC X       VALUE X'25'.                 
004200 77  LF-ASCII                    PIC X       VALUE X'0A'.                 
004300*77  CRLF-CHARS                  PIC XXX     VALUE X'0D250A'.             
004400 77  SWEDISH-EBCDIC              PIC S9(9)   VALUE 278  BINARY.           
004500                                                                          
004600 77  CONTINUE-PREFIX             PIC X(8)    VALUE '*CONTINU'.            
004700 77  RETADDRESS-PREFIX           PIC X(8)    VALUE '*RETADDR'.            
004800 77  RESTARTKEY-PREFIX           PIC X(8)    VALUE '*DISPATC'.            
004900                                                                          
005000 77  OK-SWITCH                   PIC X       VALUE 'J'.                   
005100 88  ALL-OK                                  VALUE 'J'.                   
005200 88  SOME-ERROR                              VALUE 'N'.                   
005300                                                                          
005400 01  TSTAMP                      PIC X(14).                               
005500                                                                          
005600 01  W-VIMSID                    PIC X(8)    VALUE SPACE.                 
005700                                                                          
005800 01  RC-DISPLAY                  PIC Z(8)9.                               
005900 01  REASON-DISPLAY              PIC Z(8)9.                               
006000 01  W-DISP1                     PIC Z(8)9.                               
006100 01  W-DISP2                     PIC Z(8)9.                               
006200                                                                          
006300 01  FILLER                      PIC X(16)   VALUE 'ERROR-TEXT:'.         
006400 01  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
006500 01  ERROR-TEXT2                 PIC X(80)   VALUE SPACE.                 
006600 01  ERROR-TEXT-TNT              PIC X(100).                              
006700                                                                          
006800*    -- FIELD USED WHEN TIMEOUT SPECIFIED IN ATAB FOR VCOM                
006900 01  WTIMEOUT                    PIC 9(5).                                
007000                                                                          
007100*    -- FIELDS USED WHEN MERGES DATA THAT HAS BEEN CUT UP INTO            
007200*    -- PIECES OF TRANSMITTABLE SIZE (10K FOR VCOM, 8K FOR IMS)           
007300 01  FILLER                      PIC X(16)   VALUE '-WDATA-'.             
007400 01  WDLEN-X.                                                             
007500   03  WDLEN                     PIC S9(4)   BINARY.                      
007600 01  WLEN                        PIC S9(9)   BINARY.                      
007700 01  WSTART                      PIC S9(9)   BINARY.                      
007800 01  WFROM-POS                   PIC S9(9)   BINARY.                      
007900 01  WDATA                       PIC X(32000).                            
008000                                                                          
008100 01  FILLER                      PIC X(16)   VALUE '-WSEGDATA-'.          
008200 01  WSEGLEN                     PIC S9(9)   BINARY.                      
008300 01  WSEGPOS                     PIC S9(9)   BINARY.                      
008400 01  WSEGPOS-PREVIOUS            PIC S9(9)   BINARY.                      
008500 01  WSEGDATA                    PIC X(1000000).                          
008600                                                                          
008700*    -- MAX ALLOWED LENGTH SPECIFIED IN THE GET CALL                      
008800 01  MAX-RESULT-LEN              PIC S9(9)   BINARY.                      
008900                                                                          
009000 01  DYNAMIC-SUBPROGRAMS.                                                 
009100   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
009200   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
009300   03  WZ01ATAB                  PIC X(8)    VALUE 'WZ01ATAB'.            
009400   03  DSCONR                    PIC X(8)    VALUE 'DSCONR  '.            
009500   03  DSRECV                    PIC X(8)    VALUE 'DSRECV  '.            
009600   03  DSRLSE                    PIC X(8)    VALUE 'DSRLSE  '.            
009700   03  AIBTDLI                   PIC X(8)    VALUE 'AIBTDLI '.            
009800   03  VIMSID                    PIC X(8)    VALUE 'VIMSID  '.            
009900   03  W009WTOP                  PIC X(8)    VALUE 'W009WTOP'.            
010000*  03  --- MQ CALLS ARE STATIC ----                                       
010100                                                                          
010200                                                                          
010300*    -- PARAMETERS TO ABEND                                               
010400                                                                          
010500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   BINARY VALUE +16.            
010600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   BINARY VALUE +1000.          
010700 77  RKOD-ABEND-DB2              PIC S9(4)   BINARY VALUE +998.           
010800                                                                          
010900*    -- PARAMETERS TO WZ01ATAB                                            
011000                                                                          
011100*01  -COPY WZ01ATAB                                                       
011200                                                                          
011300 77  SW-EBCDIC               PIC 9(5)  VALUE 278.                         
011400 77  UTF8                    PIC 9(5)  VALUE 1208.                        
011500                                                                          
011600     EJECT                                                                
011700 01  VCOM-AREA-START             PIC X(16)   VALUE                        
011800                                 'VCOM-AREA-START '.                      
011900*01  -COPY W0028 -PRE VCOM-                                               
012000                                                                          
012100                                                                          
012200     EJECT                                                                
012300 01  MQ-AREA-START               PIC X(16)   VALUE                        
012400                                 'MQ-AREA-START   '.                      
012500                                                                          
012600 01  MQ-QMGR                     PIC X(48) VALUE SPACE.                   
012700 01  MQ-QNAME                    PIC X(48) VALUE SPACE.                   
012800 01  MQ-COMPCODE                 PIC S9(9) BINARY.                        
012900 01  MQ-REASON                   PIC S9(9) BINARY.                        
013000 01  MQ-HCONN                    PIC S9(9) BINARY VALUE ZERO.             
013100 01  MQ-OPENOPTIONS              PIC S9(9) BINARY VALUE ZERO.             
013200 01  MQ-HOBJ                     PIC S9(9) BINARY VALUE ZERO.             
013300 01  MQ-MSGLENGTH                PIC S9(9) BINARY.                        
013400 01  MQ-DATALENGTH               PIC S9(9) BINARY.                        
013500 01  TRANSLATED-LENGTH           PIC S9(9) BINARY.                        
013600 01  MQ-CONV                     PIC X(3)  VALUE SPACE.                   
013700 01  MQ-TIMEOUT                  PIC 9(5)  VALUE ZERO.                    
013800                                                                          
013900 01  MQ-INTEGRATION-ID           PIC X(100) VALUE SPACE.                  
014000 01  MQ-CONTRACT-ID              PIC X(100) VALUE SPACE.                  
014100 01  MQ-MSGHANDLE                PIC S9(18) BINARY.                       
014200 01  MQ-PROPNAME-TO-RETRIEVE     PIC X(50).                               
014300 01  MQ-PROPTYPE                 PIC S9(9)  BINARY.                       
014400 01  MQ-PROPVALUELENGTH          PIC S9(9)  BINARY.                       
014500 01  MQ-PROPVALUE                PIC X(100).                              
014600 01  MQ-PROPDATALENGTH           PIC S9(9)  BINARY.                       
014700                                                                          
014800                                                                          
014900 01  MQ-TRIGGER-MESSAGE-DESCRIPTOR.                                       
015000*    -COPY CMQTMC2V -PRE TMC2-                                            
015100                                                                          
015200 01  MQ-MESSAGE-DESCRIPTOR.                                               
015300*    -COPY CMQMDV                                                         
015400                                                                          
015500 01  MQ-GET-MESSAGE-OPTIONS.                                              
015600*    -COPY CMQGMOV                                                        
015700                                                                          
015800 01  MQ-OBJECT-DESCRIPTOR.                                                
015900*    -COPY CMQODV                                                         
016000                                                                          
016100 01  MQ-CONSTANTS.                                                        
016200*    -COPY CMQV                                                           
016300                                                                          
016400 01  MQ-CREATE-HANDLE-OPTIONS.                                            
016500*    -COPY CMQCMHOV                                                       
016600                                                                          
016700 01  MQ-INQUIRE-PROP-OPTIONS.                                             
016800*    -COPY CMQIMPOV                                                       
016900                                                                          
017000 01  MQ-PROPNAME.                                                         
017100*    -COPY CMQCHRVV                                                       
017200                                                                          
017300 01  MQ-INQ-PROPNAME.                                                     
017400*    -COPY CMQCHRVV -PRE INQ-                                             
017500                                                                          
017600 01  MQ-PROPDESC.                                                         
017700*    -COPY CMQPDV                                                         
017800                                                                          
017900 77  MQ-TNT-MSG-LEN              PIC S9(9) BINARY VALUE ZERO.             
018000                                                                          
018100 01  MQ-TNT-MSG.                                                          
018200     03  FILLER                  PIC X(38) VALUE                          
018300                '<?xml version="1.0" encoding="UTF-8"?>'.                 
018400     03  MQ-TNT-MSG-DATA         PIC X(10000) VALUE SPACES.               
018500                                                                          
018600 01  log.                                                                 
018700     03  messageId               PIC X(48).                               
018800     03  timestamp               PIC X(24).                               
018900     03  messageSize             PIC X(09).                               
019000     03  integrationId           PIC X(10).                               
019100     03  contractId              PIC X(10).                               
019200     03  transactionId           PIC X(48).                               
019300*    03  logicalIds.                                                      
019400*        05  logicalId                       OCCURS 5.                    
019500*            07  logicalIdType   PIC X(50).                               
019600*            07  logicalIdValue  PIC X(50).                               
019700*    03  physicalId              PIC X(100).                              
019800*    03  hostName                PIC X(30).                               
019900*    03  hostIp                  PIC X(30).                               
020000     03  applicationId           PIC X(06).                               
020100*    03  objectId                PIC X(60).                               
020200     03  level                   PIC X(10).                               
020300     03  logtext                 PIC X(5000).                             
020400                                                                          
020500 01  MQ-COPYBOOKS-TNT.                                                    
020600     COPY CMQODV   REPLACING LEADING == MQOD == BY == TTOD ==.            
020700     COPY CMQMD2V  REPLACING LEADING == MQMD == BY == TTMD ==.            
020800     COPY CMQPMOV  REPLACING LEADING == MQPMO == BY == TTPMO ==.          
020900 01  TNT-FIELDS.                                                          
021000     03  TNT-OPENOPTIONS         PIC S9(9) BINARY VALUE ZERO.             
021100     03  TNT-HOBJ                PIC S9(9) BINARY VALUE ZERO.             
021200     03  TNT-INTEGRATION-ID      PIC X(10)   VALUE SPACE.                 
021300     03  TNT-CONTRACT-ID         PIC X(10)   VALUE SPACE.                 
021400                                                                          
021500     EJECT                                                                
021600 01  IDCOM-AREA-START             PIC X(16)   VALUE                       
021700                                 'IDCOM-AREA-START'.                      
021800                                                                          
021900*    -- NUMBER OF USED ENTRIES IN IDCOM-TABLE AND MAX VALUE               
022000 01  IDCOM-IX-MAX-USED           PIC S9(9)   BINARY VALUE +0.             
022100 01  IDCOM-TABLE-LENGTH          PIC S9(9)   BINARY VALUE +2.             
022200*    -- WORK INDEX                                                        
022300 01  IDCOM-IX                    PIC S9(9)   BINARY.                      
022400                                                                          
022500*    -- AREA FOR SAVING THE DATA FOR EACH IDCOM VALUE                     
022600 01  IDCOM-TABLE.                                                         
022700   03  IDCOM-ENTRY OCCURS 2.                                              
022800     05  IDCOM-KDCOMTYPE         PIC X(10).                               
022900     05  IDCOM-OPEN              PIC X(1).                                
023000*    -- DATA FOR VCOM-TRANSMISSIONS:                                      
023100     05  IDCOM-VCOM-DISTID       PIC X(34).                               
023200*    -- BUFFER FOR SAVING BLOCKED DATA AND DATA                           
023300*    -- READ IN PREVIOUS CALL                                             
023400     05  IDCOM-CURRENT-LEN       PIC S9(9)  BINARY.                       
023500     05  IDCOM-CURRENT-POS       PIC S9(9)  BINARY.                       
023600     05  IDCOM-CURRENT-DATA      PIC X(1000000).                          
023700*    -- DATA FOR RESTART-TABLE:                                           
023800     05  IDCOM-KVDAGAR-RESEND    PIC X(2).                                
023900     05  IDCOM-RESTART-KEYS.                                              
024000       07 IDCOM-ADDISPABS        PIC X(50)  VALUE SPACES.                 
024100       07 IDCOM-TIDATETIME-DB2   PIC X(26)  VALUE SPACES.                 
024200       07 IDCOM-IDLOPNR          PIC S9(4) COMP VALUE 0.                  
024300     05  IDCOM-MQ-HCONN          PIC S9(9) BINARY.                        
024400     05  IDCOM-MQ-HOBJ           PIC S9(9) BINARY.                        
024500                                                                          
024600     EJECT                                                                
024700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
024800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
024900                                                                          
025000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
025100 01  DB2-WS.                                                              
025200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
025300         88  CURSOR-OK                       VALUE 000.                   
025400         88  LINES-FOUND                     VALUE 000.                   
025500         88  LINES-MISSING                   VALUE 100.                   
025600         88  RESOURCE-WRONG                  VALUE 904.                   
025700     03  GOOD-SQLCODECODES.                                               
025800         05  GOOD-SQLCODE OCCURS 5                                        
025900             INDEXED BY SQLCODE-IX PIC 9(3).                              
026000     EJECT                                                                
026100 01  FILLER                      PIC X(16)  VALUE 'TZ1DIDA-AREA'.         
026200                                                                          
026300*01  -COPY TZ1DIDA -PRE DIDA-                                             
026400     EJECT                                                                
026500                                                                          
026600     EXEC SQL INCLUDE TZ1DIDA END-EXEC.                                   
026700     EJECT                                                                
026800*    -- IMS FUNKTIONSKODER                                                
026900*    -COPY W0003                                                          
027000                                                                          
027100 01  STATUS-WS                   PIC XX.                                  
027200     88 SEGMENT-EXISTS           VALUE SPACE.                             
027300                                                                          
027400 01  VALID-STATUS-CODES.                                                  
027500   03  VALID-STATUS OCCURS 5 INDEXED BY STATUS-IX                         
027600                                 PIC XX.                                  
027700                                                                          
027800     EJECT                                                                
027900 01  AIB-AREA-START              PIC X(16)   VALUE                        
028000                                 'AIB-AREA-START  '.                      
028100                                                                          
028200*    -COPY W0031                                                          
028300                                                                          
028400 01  IMSMSG-AREA-START            PIC X(16)   VALUE                       
028500                                 'IMSMSG-AREA-STAR'.                      
028600                                                                          
028700*    -- AREA FOR SAVING THE DATA RECEIVED AS IMS MSG                      
028800 01 IMSMSG-IO-AREA.                                                       
028900   03  IMSMSG-KVLL               PIC S9(4)   BINARY.                      
029000   03  IMSMSG-KDZZ               PIC XX.                                  
029100   03  IMSMSG-IO-DATA            PIC X(32000).                            
029200                                                                          
029300 LINKAGE SECTION.                                                         
029400                                                                          
029500*01 -COPY WZ01REC1                                                        
029600                                                                          
029700*01 -COPY WZ01REC2                                                        
029800                                                                          
029900*01 -COPY WZ01REC3                                                        
030000                                                                          
030100 01  RECV-KVDLEN                PIC S9(9) BINARY.                         
030200                                                                          
030300 01  RECV-DATA                  PIC X(1500000).                           
030400                                                                          
030500*    -COPY W0009  -PRE ALT-                                               
030600                                                                          
030700 PROCEDURE DIVISION USING                                                 
030800                      RECV-CONTROL-AREA                                   
030900                      RECV-KVDLEN                                         
031000*                      ... OR RECV-OPEN-AREA                              
031100*                      ... OR RECV-CLOSE-AREA                             
031200                      RECV-DATA                                           
031300                     .                                                    
031400* -- NOTE: ARGUMENTS ABOVE DIFFER DEPENDING ON CALL TYPE                  
031500* --       SEE A-INIT                                                     
031600 MAIN SECTION.                                                            
031700                                                                          
031800     PERFORM A-INIT                                                       
031900                                                                          
032000     IF ALL-OK                                                            
032100       EVALUATE IDCOM-KDCOMTYPE(IDCOM-IX) ALSO RECV-KDFUNC                
032200         WHEN  ANY   ALSO  'OPEN'                                         
032300           IF IDCOM-KDCOMTYPE(IDCOM-IX) = 'MIXED'                         
032400                                       OR 'IMSP2P'                        
032500*            -- TRY READING DATA VIA IO-PCB                               
032600             PERFORM C1-IMSP2P-INIT                                       
032700                                                                          
032800*            -- CHECK IF MID CONTAINS EXPEDITER NAME                      
032900*            -- OR STARTS WITH 'VCOM' (RESTART FROM EPTY SCREEN)          
033000*            -- THEN THE REAL DATA SHOULD BE FETCHED VIA VCOM             
033100             IF  IDCOM-KDCOMTYPE(IDCOM-IX) = 'MIXED'                      
033200             AND RECV-KDRC = 0 AND WLEN > 8                               
033300             AND ( WDATA(9:8) = VCOM-EXPEDITER                            
033400                OR WDATA(9:4) = 'VCOM'         )                          
033500               MOVE 'VCOM'   TO IDCOM-KDCOMTYPE(IDCOM-IX)                 
033600             END-IF                                                       
033700                                                                          
033800*            -- CHECK IF MID IS AN MQ TRIGGER MSG                         
033900*            -- OR STARTS WITH 'MQ' (RESTART FROM EPTY SCREEN)            
034000             IF IDCOM-KDCOMTYPE(IDCOM-IX) = 'MIXED'                       
034100             AND RECV-KDRC = 0 AND WLEN > 8                               
034200             AND ( WDATA(1:3) = 'TMC'                                     
034300                OR WDATA(9:2) = 'MQ'   )                                  
034400               MOVE 'MQ'     TO IDCOM-KDCOMTYPE(IDCOM-IX)                 
034500             END-IF                                                       
034600                                                                          
034700*            -- CHECK IF MID IS A DISPATCHER MSG                          
034800*            -- Applicable only if the first segment itself is            
034900*            -- with *DISPATC prefix, in which case, the actual           
035000*            -- message is to be read from dispatcher table.              
035100*            -- Only one segment is expected (with restart keys)          
035200             IF (IDCOM-KDCOMTYPE(IDCOM-IX) = 'MIXED' OR 'IMSP2P')         
035300             AND RECV-KDRC = 0 AND WLEN > 8                               
035400             AND ( WDATA(1:8) = RESTARTKEY-PREFIX )                       
035500               MOVE 'DISP'   TO IDCOM-KDCOMTYPE(IDCOM-IX)                 
035600             END-IF                                                       
035700                                                                          
035800*            -- IF NONE OF THE ABOVE AND STILL MIXED FORMAT               
035900*            -- ONLY DATA VIA IMS MID REMAINS                             
036000             IF  IDCOM-KDCOMTYPE(IDCOM-IX) = 'MIXED'                      
036100               MOVE 'IMSP2P' TO IDCOM-KDCOMTYPE(IDCOM-IX)                 
036200             END-IF                                                       
036300           END-IF                                                         
036400                                                                          
036500           IF IDCOM-KDCOMTYPE(IDCOM-IX) =  'VCOM'                         
036600             PERFORM B1-VCOM-CONNECT                                      
036700           END-IF                                                         
036800                                                                          
036900           IF IDCOM-KDCOMTYPE(IDCOM-IX) =  'MQ'                           
037000             PERFORM D1-MQ-CONNECT-OPEN                                   
037100           END-IF                                                         
037200                                                                          
037300           IF IDCOM-KDCOMTYPE(IDCOM-IX) =  'DISP'                         
037400             PERFORM E1-DISP-OPEN                                         
037500           END-IF                                                         
037600                                                                          
037700                                                                          
037800         WHEN  'VCOM'    ALSO  'GET'                                      
037900           PERFORM B2-VCOM-RECV                                           
038000                                                                          
038100         WHEN  'VCOM'    ALSO  'CLOSE'                                    
038200           PERFORM B3-VCOM-RELEASE                                        
038300                                                                          
038400                                                                          
038500         WHEN  'IMSP2P'  ALSO  'GET'                                      
038600           PERFORM C2-IMSP2P-GET-FROM-IMS                                 
038700                                                                          
038800         WHEN  'IMSP2P'  ALSO  'CLOSE'                                    
038900           PERFORM C3-IMSP2P-CLOSE                                        
039000                                                                          
039100                                                                          
039200         WHEN  'MQ'      ALSO  'GET'                                      
039300           PERFORM D2-MQ-GET-MSG-LINE                                     
039400                                                                          
039500         WHEN  'MQ'      ALSO  'CLOSE'                                    
039600           PERFORM D3-MQ-CLOSE                                            
039700                                                                          
039800*        - TWO SPECIAL FUNCTIONS FOR (MQ CLOSEQ+DISCONNECT=CLOSE)         
039900         WHEN  'MQ'      ALSO  'CLOSEQ'                                   
040000           PERFORM D4-MQ-CLOSEQ                                           
040100         WHEN  'MQ'      ALSO  'DISCONNECT'                               
040200           PERFORM D5-MQ-DISCONNECT                                       
040300                                                                          
040400                                                                          
040500         WHEN  'DISP'    ALSO  'GET'                                      
040600           PERFORM E2-DISP-GET                                            
040700                                                                          
040800         WHEN  'DISP'    ALSO  'CLOSE'                                    
040900           PERFORM E3-DISP-CLOSE                                          
041000                                                                          
041100                                                                          
041200         WHEN OTHER                                                       
041300           MOVE SPACE TO ERROR-TEXT  ERROR-TEXT2                          
041400           MOVE 'INVALID COMBINATION IN EVALUATE'                         
041500                TO ERROR-TEXT                                             
041600           IF RECV-KDFUNC NOT = 'OPEN' AND 'GET' AND 'CLOSE'              
041700             STRING 'INVALID FUNCTION CODE ' RECV-KDFUNC                  
041800             DELIMITED BY SIZE INTO ERROR-TEXT                            
041900           END-IF                                                         
042000           IF IDCOM-KDCOMTYPE(IDCOM-IX) NOT =  'VCOM'                     
042100           AND 'IMSP2P' AND 'MQ'                                          
042200             STRING 'INVALID COMTYPE IN ATAB '                            
042300                    IDCOM-KDCOMTYPE(IDCOM-IX)                             
042400             DELIMITED BY SIZE INTO ERROR-TEXT                            
042500           END-IF                                                         
042600           CALL ABEND USING RKOD-ABEND-NO-DUMP                            
042700                                                                          
042800       END-EVALUATE                                                       
042900     END-IF                                                               
043000                                                                          
043100     IF RECV-KDRC = 0                                                     
043200       MOVE ZERO TO RETURN-CODE                                           
043300     ELSE                                                                 
043400       MOVE 12 TO RETURN-CODE                                             
043500     END-IF                                                               
043600                                                                          
043700     GOBACK                                                               
043800     .                                                                    
043900                                                                          
044000 A-INIT SECTION.                                                          
044100                                                                          
044200     SET ALL-OK TO TRUE                                                   
044300     MOVE ZERO TO RECV-KDRC                                               
044400                                                                          
044500* -- ARGUMENTS DIFFERS DEPENDING ON CALL TYPE                             
044600* --   OPEN:  1) CONTROL-AREA   2) OPEN-AREA                              
044700* --   RECV:  1) CONTROL-AREA   2) KVDLEN     3) DATA                     
044800* --   CLOSE: 1) CONTROL-AREA   2) CLOSE-AREA (Optional)                  
044900* -- FIX SECOND ARGUMENT ON OPEN / CLOSE CALL:                            
045000                                                                          
045100     IF RECV-KDFUNC = 'OPEN'                                              
045200       SET ADDRESS OF RECV-OPEN-AREA TO ADDRESS OF RECV-KVDLEN            
045300       PERFORM AA-SEARCH-ATAB                                             
045400     END-IF                                                               
045500                                                                          
045600     IF RECV-KDFUNC = 'CLOSE'                                             
045700       SET ADDRESS OF RECV-CLOSE-AREA TO ADDRESS OF RECV-KVDLEN           
045800     END-IF                                                               
045900                                                                          
046000     IF ALL-OK                                                            
046100       PERFORM AB-LOOK-UP-IN-IDCOM-TAB                                    
046200     END-IF                                                               
046300                                                                          
046400*    -- SAVE MAX ALLOWED LENGTH OF RESULT                                 
046500*    -- (IT WILL BE OVERLAYED BY ACTUAL LENGTH)                           
046600     IF RECV-KDFUNC = 'GET'                                               
046700       MOVE RECV-KVDLEN TO MAX-RESULT-LEN                                 
046800     END-IF                                                               
046900     .                                                                    
047000                                                                          
047100 AA-SEARCH-ATAB SECTION.                                                  
047200                                                                          
047300     MOVE FUNCTION UPPER-CASE(RECV-ADDISPABS)                             
047400                                 TO ATAB-ADDISPABS                        
047500     CALL WZ01ATAB USING ATAB-WZ01ATAB                                    
047600                                                                          
047700     IF RETURN-CODE > ZERO                                                
047800*      -- ADDRESS NOT FOUND IN ATAB                                       
047900       MOVE 10 TO RECV-KDRC                                               
048000       SET SOME-ERROR TO TRUE                                             
048100     END-IF                                                               
048200     .                                                                    
048300                                                                          
048400     EJECT                                                                
048500 AB-LOOK-UP-IN-IDCOM-TAB  SECTION.                                        
048600                                                                          
048700     IF RECV-KDFUNC = 'OPEN'                                              
048800*      -- SEARCH FOR A FREE ENTRY IN IDCOM-TABLE                          
048900       MOVE 1                    TO IDCOM-IX                              
049000       PERFORM                                                            
049100         UNTIL (IDCOM-IX > IDCOM-TABLE-LENGTH)                            
049200            OR (IDCOM-OPEN(IDCOM-IX) NOT = YES)                           
049300          ADD 1                  TO IDCOM-IX                              
049400       END-PERFORM                                                        
049500       IF IDCOM-IX > IDCOM-IX-MAX-USED                                    
049600         MOVE IDCOM-IX TO IDCOM-IX-MAX-USED                               
049700       END-IF                                                             
049800                                                                          
049900       IF IDCOM-IX-MAX-USED > IDCOM-TABLE-LENGTH                          
050000         MOVE   'IDCOM TABLE OVERFLOW'                                    
050100                TO ERROR-TEXT                                             
050200         DISPLAY IDPGM ' ' ERROR-TEXT                                     
050300         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
050400       ELSE                                                               
050500         MOVE IDCOM-IX-MAX-USED TO IDCOM-IX                               
050600         MOVE ATAB-KDCOMTYPE    TO IDCOM-KDCOMTYPE(IDCOM-IX)              
050700         MOVE ATAB-KVDAGAR-RESEND                                         
050800                                TO IDCOM-KVDAGAR-RESEND(IDCOM-IX)         
050900                                                                          
051000*        -- CHECK IF EXPEDITER IS SPECIFIED                               
051100         MOVE ZERO TO TALLY                                               
051200         INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                       
051300                 FOR ALL 'EXP:'                                           
051400         IF TALLY > 0                                                     
051500*          -- YES, EXTRACT THE EXPEDITER NAME                             
051600           MOVE ZERO TO TALLY                                             
051700           MOVE SPACE TO VCOM-EXPEDITER                                   
051800           INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                     
051900                   FOR CHARACTERS BEFORE INITIAL 'EXP:'                   
052000           UNSTRING ATAB-ADDISPINT-RECV(TALLY + 5:)                       
052100           DELIMITED BY ';'                                               
052200           INTO VCOM-EXPEDITER                                            
052300                                                                          
052400*          -- EVEN IF COMTYPE IS VCOM, THE PROGRAM MUST                   
052500*          -- TRY TO READ FROM THE IO-PCB. "MIXED"                        
052600*          -- WILL TRIGGER THIS.                                          
052700           MOVE 'MIXED'         TO IDCOM-KDCOMTYPE(IDCOM-IX)              
052800         END-IF                                                           
052900                                                                          
053000*        -- CHECK IF MQ QUEUE NAME IS SPECIFIED                           
053100         MOVE ZERO TO TALLY                                               
053200         INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                       
053300                 FOR ALL 'LQ:'                                            
053400         IF TALLY > 0                                                     
053500*          -- QMGR NAME IS NO LONGER FETCHED FROM ATAB                    
053600*          -- INSTEAD IT IS DETERMINED FROM THE IMS SYSTEM                
053700           CALL VIMSID USING W-VIMSID                                     
053800           IF W-VIMSID(1:3) = 'IMG'                                       
053900             MOVE 'MC0P'     TO MQ-QMGR                                   
054000           ELSE                                                           
054100             IF W-VIMSID(1:3) = 'IMB'                                     
054200               MOVE 'MC0Q' TO MQ-QMGR                                     
054300             ELSE                                                         
054400               MOVE 'MC0T' TO MQ-QMGR                                     
054500             END-IF                                                       
054600           END-IF                                                         
054700                                                                          
054800*          -- EXTRACT THE QUEUE NAME                                      
054900           MOVE ZERO TO TALLY                                             
055000           MOVE SPACE TO MQ-QNAME                                         
055100           INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                     
055200                   FOR CHARACTERS BEFORE INITIAL 'LQ:'                    
055300           UNSTRING ATAB-ADDISPINT-RECV(TALLY + 4:)                       
055400           DELIMITED BY ';'                                               
055500           INTO MQ-QNAME                                                  
055600                                                                          
055700*          -- CHECK IF CONV PARAMETER                                     
055800           MOVE SPACE TO MQ-CONV                                          
055900           MOVE ZERO TO TALLY                                             
056000           INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                     
056100                   FOR ALL 'CONV:'                                        
056200           IF TALLY > 0                                                   
056300*            -- YES, EXTRACT THE CONV VALUE                               
056400             MOVE ZERO TO TALLY                                           
056500             INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                   
056600                     FOR CHARACTERS BEFORE INITIAL 'CONV:'                
056700             UNSTRING ATAB-ADDISPINT-RECV(TALLY + 6:)                     
056800             DELIMITED BY ';'                                             
056900             INTO MQ-CONV                                                 
057000           END-IF                                                         
057100                                                                          
057200*          -- CHECK IF TIME-OUT PARAMETER                                 
057300           MOVE ZERO TO MQ-TIMEOUT                                        
057400           MOVE ZERO TO TALLY                                             
057500           INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                     
057600                   FOR ALL 'TOUT:'                                        
057700           IF TALLY > 0                                                   
057800*            -- YES, EXTRACT THE TIMEOUT VALUE                            
057900             MOVE ZERO TO TALLY                                           
058000             MOVE ZERO TO WTIMEOUT                                        
058100             INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                   
058200                     FOR CHARACTERS BEFORE INITIAL 'TOUT:'                
058300             UNSTRING ATAB-ADDISPINT-RECV(TALLY + 6:)                     
058400                      DELIMITED BY ';' INTO WTIMEOUT                      
058500             MOVE WTIMEOUT TO MQ-TIMEOUT                                  
058600           END-IF                                                         
058700                                                                          
058800*          -- EVEN IF COMTYPE IS MQ, THE PROGRAM MUST                     
058900*          -- TRY TO READ FROM THE IO-PCB. "MIXED"                        
059000*          -- WILL TRIGGER THIS.                                          
059100           MOVE 'MIXED'         TO IDCOM-KDCOMTYPE(IDCOM-IX)              
059200         END-IF                                                           
059300                                                                          
059400         MOVE YES               TO IDCOM-OPEN(IDCOM-IX)                   
059500*        -- USE INDEX AS ID FOR THIS TRANSMISSION                         
059600         MOVE IDCOM-IX          TO RECV-IDCOM                             
059700       END-IF                                                             
059800     ELSE                                                                 
059900*      -- TAKE IDCOM INDEX FROM ARGUMENT                                  
060000       MOVE RECV-IDCOM TO IDCOM-IX                                        
060100       IF IDCOM-IX > IDCOM-IX-MAX-USED OR < 1                             
060200       OR IDCOM-OPEN(IDCOM-IX) = NOO                                      
060300         MOVE 11 TO RECV-KDRC                                             
060400         SET SOME-ERROR TO TRUE                                           
060500       END-IF                                                             
060600     END-IF                                                               
060700     .                                                                    
060800                                                                          
060900     EJECT                                                                
061000 B1-VCOM-CONNECT SECTION.                                                 
061100                                                                          
061200*    -- DEFAULT VALUE FOR TIMEOUT                                         
061300     MOVE 10             TO VCOM-TIMEOUT                                  
061400*    -- CHECK IF TIMEOUT IS SPECIFIED IN ATAB                             
061500     MOVE ZERO TO TALLY                                                   
061600     INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                           
061700             FOR ALL 'TOUT:'                                              
061800     IF TALLY > 0                                                         
061900*      -- YES, EXTRACT THE TIMEOUT VALUE                                  
062000       MOVE ZERO TO TALLY                                                 
062100       MOVE ZERO TO WTIMEOUT                                              
062200       INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                         
062300               FOR CHARACTERS BEFORE INITIAL 'TOUT:'                      
062400       UNSTRING ATAB-ADDISPINT-RECV(TALLY + 6:)                           
062500                DELIMITED BY ';'  INTO WTIMEOUT                           
062600       MOVE WTIMEOUT TO VCOM-TIMEOUT                                      
062700     END-IF                                                               
062800                                                                          
062900     CALL DSCONR USING VCOM-RC                                            
063000                       VCOM-DISTID                                        
063100                       VCOM-SECUR                                         
063200                       VCOM-TIMEOUT                                       
063300                       VCOM-SENDERTAG                                     
063400                       VCOM-EXPEDITER                                     
063500                       VCOM-RECTYPE                                       
063600                                                                          
063700     IF VCOM-RC NOT = ZERO                                                
063800     AND VCOM-RC NOT = 41                                                 
063900       MOVE VCOM-RC TO RC-DISPLAY                                         
064000       MOVE SPACE TO ERROR-TEXT ERROR-TEXT2                               
064100       STRING 'RC FROM DSCONR ' RC-DISPLAY                                
064200              ' ' VCOM-DISTID                                             
064300              ' ' VCOM-EXPEDITER                                          
064400              ' ' VCOM-SENDERTAG                                          
064500          DELIMITED BY SIZE                                               
064600          INTO ERROR-TEXT                                                 
064700       DISPLAY IDPGM ' ' ERROR-TEXT                                       
064800       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
064900     END-IF                                                               
065000                                                                          
065100     IF VCOM-RC = 41                                                      
065200*      -- NO DATA TO RECEIVE                                              
065300       MOVE 20 TO RECV-KDRC                                               
065400       MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                   
065500       MOVE SPACE TO ERROR-TEXT2                                          
065600       MOVE 'RC 41 FROM DSCONR. NO DATA TO RECEIVE' TO ERROR-TEXT         
065700     ELSE                                                                 
065800       MOVE VCOM-DISTID TO IDCOM-VCOM-DISTID(IDCOM-IX)                    
065900*      -- READ AND SAVE FIRST DATA SEGMENT                                
066000       PERFORM B2A-RECV-SEGMENT                                           
066100       MOVE WLEN  TO IDCOM-CURRENT-LEN (IDCOM-IX)                         
066200       MOVE WDATA TO IDCOM-CURRENT-DATA(IDCOM-IX)                         
066300     END-IF                                                               
066400                                                                          
066500*    -- RETRIEVE ANY SENDER-TAG VALUE AND PUT IN "EXTRA"                  
066600*    -- ADDRESS FIELD                                                     
066700     MOVE VCOM-SENDERTAG TO RECV-ADDISPXTRA                               
066800     .                                                                    
066900                                                                          
067000     EJECT                                                                
067100 B2-VCOM-RECV    SECTION.                                                 
067200                                                                          
067300*    -- THE DATA MAY BE SPLIT INTO SEGEMENTS OF ABOUT 10K EACH.           
067400*    -- THE FIRST SEGMENT IS PURE DATA, BUT THE                           
067500*    -- FOLLOWING SEGMENTS ARE PREFIXED BY A "CONTINUATION                
067600*    -- PREFIX" - '*CONTINU'. THE FIRST RECORD OF DATA MAY ALSO           
067700*    -- INCLUDE A LAST '*RETADDR' SEGMENT CONTAINING INFO ABOUT           
067800*    -- RETURN ADDRESS.                                                   
067900*    -- THE TOTAL SEQUENCE OF POSSIBLE SEGMENTS LOOKS LIKE THIS:          
068000*    --   TRANS+DATA [, *CONTINU+DATA]... [,*RETADDR+ADDRESS]             
068100*    --   TRANS+DATA [, *CONTINU+DATA]...                                 
068200*    --   ...                                                             
068300                                                                          
068400     MOVE IDCOM-VCOM-DISTID(IDCOM-IX) TO VCOM-DISTID                      
068500                                                                          
068600*    --  FETCH DATA FROM PREVIOUS CALL                                    
068700     MOVE IDCOM-CURRENT-LEN (IDCOM-IX)  TO WLEN                           
068800     MOVE IDCOM-CURRENT-DATA(IDCOM-IX)  TO WDATA                          
068900                                                                          
069000*    -- TOTAL LENGTH OF DATA IS INITIALLY ZERO                            
069100     MOVE ZERO TO RECV-KVDLEN                                             
069200                                                                          
069300     IF WLEN <= ZERO                                                      
069400*      -- EOF                                                             
069500       MOVE 1 TO RECV-KDRC                                                
069600     ELSE                                                                 
069700                                                                          
069800*      -- PUT DATA AT BEGINNING OF RECEVING DATA FIELD                    
069900       MOVE 1    TO WSTART                                                
070000*      -- TAKE THE WHOLE RECORD FIRST TIME                                
070100       MOVE 1    TO WFROM-POS                                             
070200                                                                          
070300*      -- FIRST MOVE DATA FROM PREVIOUS CALL                              
070400       PERFORM S01-MOVE-DATA                                              
070500                                                                          
070600*      -- ADD CONTINUATION SEGMENTS (IF THERE ARE ANY)                    
070700       PERFORM B2A-RECV-SEGMENT                                           
070800       PERFORM UNTIL VCOM-RC NOT = ZERO                                   
070900               OR WDATA(1:8) NOT = CONTINUE-PREFIX                        
071000                                                                          
071100*        -- SKIP FIRST 8 POSITIONS IN FROM NOW ON                         
071200         MOVE 9 TO WFROM-POS                                              
071300         PERFORM S01-MOVE-DATA                                            
071400                                                                          
071500         PERFORM B2A-RECV-SEGMENT                                         
071600       END-PERFORM                                                        
071700                                                                          
071800       IF WSTART > MAX-RESULT-LEN + 1                                     
071900*        -- TRUNCATION, SOME DATA NOT PROCESSED                           
072000         MOVE SPACE TO ERROR-TEXT ERROR-TEXT2                             
072100         MOVE 21   TO RECV-KDRC                                           
072200         SUBTRACT 1 FROM WSTART GIVING W-DISP1                            
072300         MOVE MAX-RESULT-LEN TO W-DISP2                                   
072400         STRING 'DATA TRUNCATION. RECEIVED LENGTH: ' W-DISP1              
072500                ' MAX ALLOWED: ' W-DISP2                                  
072600           DELIMITED BY SIZE                                              
072700           INTO ERROR-TEXT                                                
072800         DISPLAY IDPGM ' ' ERROR-TEXT                                     
072900         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
073000       END-IF                                                             
073100                                                                          
073200*      -- TAKE CARE OF RETURN DATA SEGMENT AT END OF 1:ST RECORD          
073300       IF WDATA(1:8) = RETADDRESS-PREFIX                                  
073400         MOVE WDATA(9:) TO RECV-ADDISPABS-RETURN                          
073500         PERFORM B2A-RECV-SEGMENT                                         
073600       END-IF                                                             
073700                                                                          
073800*      -- SAVE UNUSED SEGMENT FOR NEXT CALL                               
073900*      -- REMEMBER END-OF-DATA AS A ZERO LENGTH                           
074000       IF VCOM-RC NOT = ZERO                                              
074100         MOVE ZERO  TO IDCOM-CURRENT-LEN (IDCOM-IX)                       
074200         MOVE SPACE TO IDCOM-CURRENT-DATA(IDCOM-IX)                       
074300       ELSE                                                               
074400         MOVE WLEN  TO IDCOM-CURRENT-LEN (IDCOM-IX)                       
074500         MOVE WDATA TO IDCOM-CURRENT-DATA(IDCOM-IX)                       
074600       END-IF                                                             
074700                                                                          
074800     END-IF                                                               
074900     .                                                                    
075000                                                                          
075100     SKIP3                                                                
075200 B2A-RECV-SEGMENT  SECTION.                                               
075300                                                                          
075400     MOVE LENGTH OF VCOM-DATA TO VCOM-MAXLENGTH                           
075500                                                                          
075600     CALL DSRECV USING VCOM-RC                                            
075700                       VCOM-DISTID                                        
075800                       VCOM-MAXLENGTH                                     
075900                       VCOM-ACTLENGTH                                     
076000                       VCOM-DATA                                          
076100                                                                          
076200     IF  VCOM-RC NOT = ZERO                                               
076300     AND VCOM-RC NOT = 45                                                 
076400       MOVE VCOM-RC TO RC-DISPLAY                                         
076500       STRING 'RC FROM DSRECV ' RC-DISPLAY                                
076600              ' ' VCOM-DISTID                                             
076700              ' ' VCOM-EXPEDITER                                          
076800              ' ' VCOM-SENDERTAG                                          
076900          DELIMITED BY SIZE                                               
077000          INTO ERROR-TEXT                                                 
077100       DISPLAY IDPGM ' ' ERROR-TEXT                                       
077200       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
077300     END-IF                                                               
077400                                                                          
077500     IF VCOM-RC = 45                                                      
077600       MOVE SPACE TO WDATA                                                
077700       MOVE ZERO  TO WLEN                                                 
077800     ELSE                                                                 
077900       MOVE VCOM-DATA(1:VCOM-ACTLENGTH) TO WDATA                          
078000       MOVE VCOM-ACTLENGTH              TO WLEN                           
078100     END-IF                                                               
078200     .                                                                    
078300                                                                          
078400     EJECT                                                                
078500 B3-VCOM-RELEASE SECTION.                                                 
078600                                                                          
078700     MOVE IDCOM-VCOM-DISTID(IDCOM-IX) TO VCOM-DISTID                      
078800     MOVE ZERO                        TO VCOM-RC                          
078900     MOVE +1                          TO VCOM-RVALUE                      
079000                                                                          
079100     CALL DSRLSE USING VCOM-RC                                            
079200                       VCOM-DISTID                                        
079300                       VCOM-RVALUE                                        
079400                                                                          
079500     IF VCOM-RC NOT = ZERO                                                
079600       MOVE VCOM-RC TO RC-DISPLAY                                         
079700       STRING 'RC FROM DSRECV ' RC-DISPLAY                                
079800              ' ' VCOM-DISTID                                             
079900              ' ' VCOM-EXPEDITER                                          
080000              ' ' VCOM-SENDERTAG                                          
080100          DELIMITED BY SIZE                                               
080200          INTO ERROR-TEXT                                                 
080300       DISPLAY IDPGM ' ' ERROR-TEXT                                       
080400       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
080500     END-IF                                                               
080600                                                                          
080700     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
080800     .                                                                    
080900                                                                          
081000     EJECT                                                                
081100 C1-IMSP2P-INIT  SECTION.                                                 
081200                                                                          
081300                                                                          
081400*    -- READ AND SAVE FIRST DATA SEGMENT                                  
081500     PERFORM IMS-GET-FIRST-SEGMENT                                        
081600     MOVE WSEGLEN  TO IDCOM-CURRENT-LEN (IDCOM-IX)                        
081700     MOVE WSEGPOS  TO IDCOM-CURRENT-POS (IDCOM-IX)                        
081800     MOVE WSEGDATA TO IDCOM-CURRENT-DATA(IDCOM-IX)                        
081900                                                                          
082000     IF WSEGLEN = 0                                                       
082100*      -- NO DATA TO RECEIVE                                              
082200       MOVE 20 TO RECV-KDRC                                               
082300       MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                   
082400     ELSE                                                                 
082500       MOVE FUNCTION UPPER-CASE (WSEGDATA (1:8))                          
082600                                 TO RECV-KDTRANS                          
082700     END-IF                                                               
082800     MOVE WSEGLEN  TO WLEN                                                
082900     MOVE WSEGDATA TO WDATA                                               
083000     .                                                                    
083100                                                                          
083200     EJECT                                                                
083300 C2-IMSP2P-GET-FROM-IMS  SECTION.                                         
083400                                                                          
083500*    -- THE DATA MAY BE SPLIT INTO SEGEMENT OF ABOUT 10K EACH.            
083600*    -- THE FIRST SEGMENT IS PURE DATA (WITH INITIAL TRANS CODE)          
083700*    -- BUT THE FOLLOWING SEGMENTS ARE PREFIXED BY A "CONTINUATION        
083800*    -- PREFIX" - '*CONTINU'. THE FIRST SET OF SEGMENTS MAY ALSO          
083900*    -- BE FOLLOWED BY A '*RETADDR' SEGMENT CONTAINING INFO               
084000*    -- ABOUT RETURN ADDRESS AND/OR A '*DISPATC' SEGMENT                  
084100*    -- CONTAINING INFO ABOUT KEYS TO THE DISPATCHER RESTART TABLE        
084200*    -- THE TOTAL SEQUENCE OF POSSIBLE SEGMENTS LOOKS LIKE THIS:          
084300*    --   TRANS+DATA [, *CONTINU+DATA]...                                 
084400*    --   [*RETADDR+ADDRESS]                                              
084500*    --   [*DISPATC+ADDRESS]                                              
084600*    --   TRANS+DATA [, *CONTINU+DATA]...                                 
084700*    --   ...                                                             
084800                                                                          
084900*    -- FETCH DATA SEGMENT SAVED FROM PREVIOUS CALL                       
085000     MOVE IDCOM-CURRENT-LEN (IDCOM-IX)  TO WSEGLEN                        
085100     MOVE IDCOM-CURRENT-POS (IDCOM-IX)  TO WSEGPOS                        
085200     MOVE IDCOM-CURRENT-DATA(IDCOM-IX)  TO WSEGDATA                       
085300     PERFORM C2A-NEXT-DATA-PART                                           
085400                                                                          
085500*    -- TOTAL LENGTH OF DATA IS INITIALLY ZERO                            
085600     MOVE ZERO TO RECV-KVDLEN                                             
085700                                                                          
085800     IF WLEN <= ZERO                                                      
085900*      -- EOF                                                             
086000       MOVE 1 TO RECV-KDRC                                                
086100     ELSE                                                                 
086200*      -- PUT DATA AT BEGINNING OF RECEVING DATA FIELD                    
086300       MOVE 1    TO WSTART                                                
086400*      -- SKIP TRANSACTION / PREFIX                                       
086500       MOVE 9    TO WFROM-POS                                             
086600                                                                          
086700*      -- FIRST MOVE DATA FROM PREVIOUS CALL                              
086800       PERFORM S01-MOVE-DATA                                              
086900                                                                          
087000*        -- ADD CONTINUATION SEGMENTS (IF THERE ARE ANY)                  
087100       PERFORM C2A-NEXT-DATA-PART                                         
087200       PERFORM UNTIL WLEN = ZERO                                          
087300               OR WDATA(1:8) NOT = CONTINUE-PREFIX                        
087400                                                                          
087500         PERFORM S01-MOVE-DATA                                            
087600                                                                          
087700         PERFORM C2A-NEXT-DATA-PART                                       
087800       END-PERFORM                                                        
087900                                                                          
088000       IF WSTART > MAX-RESULT-LEN + 1                                     
088100*        -- TRUNCATION, SOME DATA NOT PROCESSED                           
088200         MOVE SPACE TO ERROR-TEXT ERROR-TEXT2                             
088300         MOVE 21   TO RECV-KDRC                                           
088400         SUBTRACT 1 FROM WSTART GIVING W-DISP1                            
088500         MOVE MAX-RESULT-LEN TO W-DISP2                                   
088600         STRING 'DATA TRUNCATION. RECEIVED LENGTH: ' W-DISP1              
088700                ' MAX ALLOWED: ' W-DISP2                                  
088800           DELIMITED BY SIZE                                              
088900           INTO ERROR-TEXT                                                
089000         DISPLAY IDPGM ' ' ERROR-TEXT                                     
089100         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
089200       END-IF                                                             
089300                                                                          
089400*      -- TAKE CARE OF DISPATCHER RESEND KEY SEGMENT                      
089500*      -- SAVE IT FOR CLOSE CALL                                          
089600       IF WLEN > ZERO                                                     
089700       AND WDATA(1:8) = RESTARTKEY-PREFIX                                 
089800         MOVE WDATA(9:) TO IDCOM-RESTART-KEYS(IDCOM-IX)                   
089900         PERFORM C2A-NEXT-DATA-PART                                       
090000       END-IF                                                             
090100                                                                          
090200*      -- SIMILARILY, TAKE CARE OF RETURN DATA SEGMENT AT END             
090300*      -- OF 1:ST RECORD                                                  
090400       IF WLEN > ZERO                                                     
090500       AND WDATA(1:8) = RETADDRESS-PREFIX                                 
090600         MOVE WDATA(9:) TO RECV-ADDISPABS-RETURN                          
090700         PERFORM C2A-NEXT-DATA-PART                                       
090800       END-IF                                                             
090900                                                                          
091000*      -- SAVE UNUSED SEGMENT FOR NEXT CALL                               
091100*      -- REMEMBER END-OF-DATA AS ZERO DATA LENGTH                        
091200       IF WLEN <= ZERO                                                    
091300         MOVE ZERO     TO IDCOM-CURRENT-LEN (IDCOM-IX)                    
091400         MOVE ZERO     TO IDCOM-CURRENT-POS (IDCOM-IX)                    
091500         MOVE SPACE    TO IDCOM-CURRENT-DATA(IDCOM-IX)                    
091600       ELSE                                                               
091700         MOVE WSEGLEN  TO IDCOM-CURRENT-LEN (IDCOM-IX)                    
091800         MOVE WSEGPOS-PREVIOUS TO IDCOM-CURRENT-POS (IDCOM-IX)            
091900         MOVE WSEGDATA TO IDCOM-CURRENT-DATA(IDCOM-IX)                    
092000       END-IF                                                             
092100                                                                          
092200     END-IF                                                               
092300     .                                                                    
092400                                                                          
092500     EJECT                                                                
092600 C2A-NEXT-DATA-PART    SECTION.                                           
092700                                                                          
092800*    -- THE SEGMENT DATA MAY BE IN TWO DIFFERENT FORMATS.                 
092900*    -- 1. "NEW" BLOCKED FORMAT:                                          
093000*    --  THE IMS SEGMENT CONTAINS 1 OR MORE "DATA PARTS"                  
093100*    --  IN THE FORMAT: PREFIX (8 BYTES) +                                
093200*    --  DATA LENGTH (2 BYTES BINARY) + THE DATA ITSELF                   
093300*    --  THE PREFIX AND DATA IS EXTRACTED AND PLACED IN WDATA             
093400*    --  (THE DATA LENGTH BETWEEN THE PREFIX AND DATA IS REMOVED)         
093500*    --  AND THE LENGTH OF WHAT HAS BEEN EXTRACTED IS PUT IN WLEN         
093600*    -- 2. "TRADITIONAL" UNBLOCKED FORMAT                                 
093700*    --  THE IMS SEGMENT CONTAINS ONLY ONE "DATA PART"                    
093800*    --  IN THE REGULAR FORMAT - PREFIX/TRANS CODE (8 BYTES)              
093900*    --  + THE DATA ITSELF                                                
094000*    --  THE WHOLE SEGMENT IS EXTRACTED AND PLACED IN WDATA               
094100*    --  AND THE LENGTH OF WHAT HAS BEEN EXTRACTED IS PUT IN WLEN         
094200*    -- WSEGDATA CONTAINS THE WHOLE SEGMENT.                              
094300*    -- WSEGPOS POINTS TO THE START OF NEXT DATA PART.                    
094400                                                                          
094500     MOVE ZERO  TO WDLEN                                                  
094600     MOVE ZERO  TO WLEN                                                   
094700     MOVE SPACE TO WDATA                                                  
094800                                                                          
094900     IF WSEGPOS < WSEGLEN                                                 
095000*      -- ASSUME FORMAT 1 (WITH INTERNAL LENGTH FIELDS)                   
095100       MOVE WSEGDATA(WSEGPOS + 8:2) TO WDLEN-X                            
095200     ELSE                                                                 
095300       PERFORM IMS-GET-NEXT-SEGMENT                                       
095400       IF SEGMENT-EXISTS                                                  
095500*        -- ASSUME FORMAT 1                                               
095600         MOVE WSEGDATA(WSEGPOS + 8:2) TO WDLEN-X                          
095700       END-IF                                                             
095800     END-IF                                                               
095900                                                                          
096000*    -- CHECK FOR FORMAT 2 (SIMPLE UNBLOCKED)                             
096100     IF WDLEN > 16384 OR < 0                                              
096200       MOVE WSEGLEN  TO WLEN                                              
096300       MOVE WSEGDATA TO WDATA                                             
096400       MOVE WSEGPOS TO WSEGPOS-PREVIOUS                                   
096500       COMPUTE WSEGPOS = WSEGPOS + WSEGLEN                                
096600**     DISPLAY 'WZ01RECV, UNBLOCKED DATA: ' WSEGDATA(1:20)                
096700                                                                          
096800     ELSE                                                                 
096900       IF WDLEN > 0                                                       
097000*         -- DATA EXISTS, FORMAT 1 (BLOCKED)                              
097100**       DISPLAY 'WZ01RECV, BLOCKED DATA: ' WSEGDATA(1:20)                
097200         IF WSEGPOS + 9 + WDLEN > WSEGLEN                                 
097300         MOVE ' INVALID IMS SEGMENT DATA. SEE -WSEGDATA- IN W-S.'         
097400                TO ERROR-TEXT                                             
097500           DISPLAY IDPGM ' ' ERROR-TEXT                                   
097600           CALL FELLOG                                                    
097700         END-IF                                                           
097800         MOVE SPACE TO WDATA                                              
097900         STRING                                                           
098000           WSEGDATA(WSEGPOS:8)                                            
098100           WSEGDATA(WSEGPOS + 10:WDLEN)                                   
098200           DELIMITED BY SIZE                                              
098300           INTO WDATA                                                     
098400         COMPUTE  WLEN = WDLEN + 8                                        
098500*        -- ADD LENGTH OF DATA PART (WDLEN) +                             
098600*        -- SEGMENT PREFIX (8)                                            
098700         MOVE WSEGPOS TO WSEGPOS-PREVIOUS                                 
098800         COMPUTE WSEGPOS = WSEGPOS + WDLEN + 10                           
098900       END-IF                                                             
099000     END-IF                                                               
099100     .                                                                    
099200                                                                          
099300     EJECT                                                                
099400 C3-IMSP2P-CLOSE  SECTION.                                                
099500                                                                          
099600*    -- DELETE THE ROWS IN THE RESTART TABLE IF '0' DAYS                  
099700*    -- SAVING TIME HAS BEEN SPECIFIED AND THERE ACTUALLY                 
099800*    -- WAS A *DISPATC SEGMENT IN THE DATA                                
099900                                                                          
100000     IF IDCOM-TIDATETIME-DB2 (IDCOM-IX) > SPACES                          
100100       MOVE IDCOM-ADDISPABS (IDCOM-IX)                                    
100200                                 TO DIDA-ADDISPABS                        
100300       MOVE IDCOM-TIDATETIME-DB2 (IDCOM-IX)                               
100400                                 TO DIDA-TIDATETIME-DB2                   
100500       MOVE IDCOM-IDLOPNR (IDCOM-IX)                                      
100600                                 TO DIDA-IDLOPNR                          
100700       IF IDCOM-KVDAGAR-RESEND(IDCOM-IX) = '0 '                           
100800         PERFORM DB2-DELETE-TZ1DIDA                                       
100900       ELSE                                                               
101000         MOVE RECV-KDRC-HTTP     TO DIDA-KDRC-HTTP                        
101100         MOVE RECV-KDRC-PULS     TO DIDA-KDRC-PULS                        
101200         MOVE RECV-KDKOMSTA      TO DIDA-KDKOMSTA                         
101300         MOVE RECV-MESSAGE-KVDLEN                                         
101400                                 TO DIDA-MESSAGE-L                        
101500         MOVE RECV-MESSAGE       TO DIDA-MESSAGE-D                        
101600         PERFORM DB2-UPDATE-TZ1DIDA                                       
101700       END-IF                                                             
101800     END-IF                                                               
101900                                                                          
102000     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
102100     .                                                                    
102200                                                                          
102300                                                                          
102400     EJECT                                                                
102500 D1-MQ-CONNECT-OPEN SECTION.                                              
102600                                                                          
102700*    --- FETCH INFO ABOUT INT AND CONTRACT ID FROM META DATA              
102800     PERFORM S02-EXTRACT-MQ-PARAMETERS                                    
102900                                                                          
103000     CALL 'MQCONN' USING MQ-QMGR                                          
103100                         MQ-HCONN                                         
103200                         MQ-COMPCODE                                      
103300                         MQ-REASON                                        
103400                                                                          
103500     IF MQ-COMPCODE NOT = MQCC-OK                                         
103600     AND MQ-REASON NOT = MQRC-ALREADY-CONNECTED                           
103700       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
103800       MOVE MQ-REASON  TO REASON-DISPLAY                                  
103900       STRING 'RC FROM MQCONN ' RC-DISPLAY                                
104000              ' REASON: ' REASON-DISPLAY                                  
104100              ' ' MQ-QMGR                                                 
104200          DELIMITED BY SIZE                                               
104300          INTO ERROR-TEXT                                                 
104400       MOVE SPACE TO ERROR-TEXT2                                          
104500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
104600       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
104700     END-IF                                                               
104800                                                                          
104900     MOVE MQ-QNAME TO MQOD-OBJECTNAME                                     
105000*    -- SOME OPEN OPTIONS?                                                
105100     COMPUTE MQ-OPENOPTIONS = MQOO-INPUT-EXCLUSIVE +                      
105200                              MQOO-FAIL-IF-QUIESCING                      
105300                                                                          
105400     CALL 'MQOPEN' USING MQ-HCONN                                         
105500                         MQOD                                             
105600                         MQ-OPENOPTIONS                                   
105700                         MQ-HOBJ                                          
105800                         MQ-COMPCODE                                      
105900                         MQ-REASON                                        
106000                                                                          
106100     IF MQ-COMPCODE NOT = MQCC-OK                                         
106200       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
106300       MOVE MQ-REASON  TO REASON-DISPLAY                                  
106400       STRING 'RC FROM MQOPEN ' RC-DISPLAY                                
106500              ' REASON: ' REASON-DISPLAY                                  
106600              ' ' MQ-QNAME                                                
106700          DELIMITED BY SIZE                                               
106800          INTO ERROR-TEXT                                                 
106900       MOVE SPACE TO ERROR-TEXT2                                          
107000       DISPLAY IDPGM ' ' ERROR-TEXT                                       
107100       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
107200     END-IF                                                               
107300                                                                          
107400     MOVE MQ-HCONN TO IDCOM-MQ-HCONN(IDCOM-IX)                            
107500     MOVE MQ-HOBJ  TO IDCOM-MQ-HOBJ(IDCOM-IX)                             
107600                                                                          
107700*    -- FETCH AND SAVE THE MQ MESSAGE IN ADVANCE                          
107800     PERFORM D1A-MQ-GET-FROM-LOCAL-QUEUE                                  
107900     MOVE WSEGLEN  TO IDCOM-CURRENT-LEN (IDCOM-IX)                        
108000     MOVE WSEGPOS  TO IDCOM-CURRENT-POS (IDCOM-IX)                        
108100     MOVE WSEGDATA TO IDCOM-CURRENT-DATA(IDCOM-IX)                        
108200     .                                                                    
108300                                                                          
108400     SKIP3                                                                
108500 D1A-MQ-GET-FROM-LOCAL-QUEUE SECTION.                                     
108600                                                                          
108700     MOVE IDCOM-MQ-HCONN (IDCOM-IX) TO MQ-HCONN                           
108800     MOVE IDCOM-MQ-HOBJ  (IDCOM-IX) TO MQ-HOBJ                            
108900                                                                          
109000                                                                          
109100*    -- CREATE A HANDLE FOR HEADER PROPERTIES                             
109200     MOVE MQCMHO-VALIDATE TO MQCMHO-OPTIONS                               
109300     CALL 'MQCRTMH' USING MQ-HCONN                                        
109400                       MQ-CREATE-HANDLE-OPTIONS                           
109500                       MQ-MSGHANDLE                                       
109600                       MQ-COMPCODE                                        
109700                       MQ-REASON                                          
109800                                                                          
109900     IF MQ-COMPCODE NOT = MQCC-OK                                         
110000       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
110100       MOVE MQ-REASON  TO REASON-DISPLAY                                  
110200       STRING 'RC FROM MQCRTMH' RC-DISPLAY                                
110300              ' REASON: ' REASON-DISPLAY                                  
110400              ' ' MQ-QNAME                                                
110500          DELIMITED BY SIZE                                               
110600          INTO ERROR-TEXT                                                 
110700       MOVE SPACE TO ERROR-TEXT2                                          
110800       DISPLAY IDPGM ' ' ERROR-TEXT                                       
110900       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
111000     END-IF                                                               
111100                                                                          
111200                                                                          
111300*    -- SOME GET OPTIONS.                                                 
111400     COMPUTE MQGMO-OPTIONS =                                              
111500               MQGMO-ACCEPT-TRUNCATED-MSG +                               
111600               MQGMO-FAIL-IF-QUIESCING +                                  
111700               MQGMO-PROPERTIES-IN-HANDLE                                 
111800                                                                          
111900*    -- FOR REQUEST-REPLY A WAIT TIMEOUT INTERVAL MAY BE NEEDED           
112000*    -- THE VALUE IN ATAB IS IN SECONDS, WAITINTERVAL IS MILLISEC         
112100     IF MQ-TIMEOUT > ZERO                                                 
112200        ADD  MQGMO-WAIT   TO MQGMO-OPTIONS                                
112300        MULTIPLY MQ-TIMEOUT BY 1000 GIVING MQGMO-WAITINTERVAL             
112400     ELSE                                                                 
112500        ADD MQGMO-NO-WAIT TO MQGMO-OPTIONS                                
112600     END-IF                                                               
112700                                                                          
112800     IF MQ-CONV NOT = NOO                                                 
112900*      -- CONVERT TO EBCDIC UNLESS SPECIFIED NOT TO                       
113000       ADD MQGMO-CONVERT  TO MQGMO-OPTIONS                                
113100     END-IF                                                               
113200                                                                          
113300                                                                          
113400     MOVE MQMI-NONE             TO MQMD-MSGID                             
113500     MOVE MQCI-NONE             TO MQMD-CORRELID                          
113600                                                                          
113700     MOVE MQENC-NATIVE          TO MQMD-ENCODING                          
113800     MOVE MQCCSI-Q-MGR          TO MQMD-CODEDCHARSETID                    
113900                                                                          
114000     MOVE MQGMO-VERSION-4       TO MQGMO-VERSION                          
114100     MOVE MQ-MSGHANDLE          TO MQGMO-MSGHANDLE                        
114200                                                                          
114300*    -- FETCH THE MESSAGE                                                 
114400     MOVE LENGTH OF WSEGDATA  TO MQ-MSGLENGTH                             
114500     CALL 'MQGET' USING MQ-HCONN                                          
114600                       MQ-HOBJ                                            
114700                       MQMD                                               
114800                       MQ-GET-MESSAGE-OPTIONS                             
114900                       MQ-MSGLENGTH                                       
115000                       WSEGDATA                                           
115100                       MQ-DATALENGTH                                      
115200                       MQ-COMPCODE                                        
115300                       MQ-REASON                                          
115400                                                                          
115500     IF MQ-COMPCODE NOT = MQCC-OK                                         
115600     AND MQ-REASON NOT = MQRC-FORMAT-ERROR                                
115700       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
115800       MOVE MQ-REASON  TO REASON-DISPLAY                                  
115900       STRING 'RC FROM MQGET ' RC-DISPLAY                                 
116000              ' REASON: ' REASON-DISPLAY                                  
116100              ' ' MQ-QNAME                                                
116200          DELIMITED BY SIZE                                               
116300          INTO ERROR-TEXT                                                 
116400       MOVE SPACE TO ERROR-TEXT2                                          
116500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
116600       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
116700     ELSE                                                                 
116800       IF MQ-DATALENGTH > LENGTH OF WSEGDATA                              
116900         MOVE MQ-DATALENGTH TO RC-DISPLAY                                 
117000         STRING 'MQGET ERROR. MSG TOO LONG'                               
117100                '. MSG LENGTH: ' RC-DISPLAY                               
117200                '. MAX ALLOWED: ' LENGTH OF WSEGDATA                      
117300            DELIMITED BY SIZE                                             
117400            INTO ERROR-TEXT                                               
117500         MOVE SPACE TO ERROR-TEXT2                                        
117600         DISPLAY IDPGM ' ' ERROR-TEXT                                     
117700         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
117800       ELSE                                                               
117900         IF MQ-REASON = MQRC-FORMAT-ERROR                                 
118000         AND MQ-CONV NOT = NOO                                            
118100*          -- INVALID DATA FORMAT                                         
118200*          -- MQ CANNOT HANDLE CONVERSION, MUST BE DONE MANUALLY          
118300*          -- (KEEP UNICODE IF NO CONVERSION SPECIFIED)                   
118400           PERFORM D1AA-CONVERT-TO-EBCDIC                                 
118500         END-IF                                                           
118600                                                                          
118700*        -- SAVE THE MESSAGE FOR SUBSEQUENT GET CALLS                     
118800*        -- MESSAGE IS ALREADY IN WSEGDATA, SAVE ITS LENGTH               
118900*        -- (THE LENGTH MAY HAVE BEEN RECOMPUTED IN D1AA SECTION)         
119000         MOVE MQ-DATALENGTH               TO WSEGLEN                      
119100*        -- FIRST GET CALL SHOULD START FROM THE START OF BUFFER          
119200         MOVE 1                           TO WSEGPOS                      
119300                                                                          
119400       END-IF                                                             
119500     END-IF                                                               
119600                                                                          
119700                                                                          
119800*    -- FETCH SILLY PROPERTIES                                            
119900     COMPUTE MQIMPO-OPTIONS = MQIMPO-CONVERT-VALUE                        
120000                            + MQIMPO-CONVERT-TYPE                         
120100                            + MQIMPO-INQ-FIRST                            
120200                                                                          
120300     MOVE SWEDISH-EBCDIC   TO MQIMPO-REQUESTEDCCSID                       
120400     MOVE MQTYPE-STRING    TO MQ-PROPTYPE                                 
120500                                                                          
120600     MOVE 'IntegrationId'  TO MQ-PROPNAME-TO-RETRIEVE                     
120700     MOVE 13               TO INQ-MQCHARV-VSLENGTH                        
120800     SET INQ-MQCHARV-VSPTR TO ADDRESS OF MQ-PROPNAME-TO-RETRIEVE          
120900     MOVE LENGTH OF MQ-PROPNAME-TO-RETRIEVE                               
121000                           TO INQ-MQCHARV-VSBUFSIZE                       
121100     MOVE LENGTH OF MQ-PROPVALUE                                          
121200                           TO MQ-PROPVALUELENGTH                          
121300                                                                          
121400     CALL 'MQINQMP' USING MQ-HCONN                                        
121500                       MQ-MSGHANDLE                                       
121600                       MQ-INQUIRE-PROP-OPTIONS                            
121700                       MQ-INQ-PROPNAME                                    
121800                       MQ-PROPDESC                                        
121900                       MQ-PROPTYPE                                        
122000                       MQ-PROPVALUELENGTH                                 
122100                       MQ-PROPVALUE                                       
122200                       MQ-PROPDATALENGTH                                  
122300                       MQ-COMPCODE                                        
122400                       MQ-REASON                                          
122500                                                                          
122600     IF MQ-COMPCODE  = MQCC-OK                                            
122700       MOVE MQ-PROPVALUE TO MQ-INTEGRATION-ID                             
122800     ELSE                                                                 
122900       IF MQ-REASON = MQRC-PROPERTY-NOT-AVAILABLE                         
123000         CONTINUE                                                         
123100*         MOVE SPACE        TO MQ-INTEGRATION-ID                          
123200       ELSE                                                               
123300         MOVE MQ-COMPCODE TO RC-DISPLAY                                   
123400         MOVE MQ-REASON  TO REASON-DISPLAY                                
123500         STRING 'RC FROM MQINQMP' RC-DISPLAY                              
123600                ' REASON: ' REASON-DISPLAY                                
123700                ' ' MQ-QNAME                                              
123800            DELIMITED BY SIZE                                             
123900            INTO ERROR-TEXT                                               
124000         MOVE SPACE TO ERROR-TEXT2                                        
124100         DISPLAY IDPGM ' ' ERROR-TEXT                                     
124200         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
124300       END-IF                                                             
124400     END-IF                                                               
124500                                                                          
124600     MOVE 'ContractId'     TO MQ-PROPNAME-TO-RETRIEVE                     
124700     MOVE 10               TO INQ-MQCHARV-VSLENGTH                        
124800     SET INQ-MQCHARV-VSPTR TO ADDRESS OF MQ-PROPNAME-TO-RETRIEVE          
124900     MOVE LENGTH OF MQ-PROPNAME-TO-RETRIEVE                               
125000                           TO INQ-MQCHARV-VSBUFSIZE                       
125100     MOVE LENGTH OF MQ-PROPVALUE                                          
125200                           TO MQ-PROPVALUELENGTH                          
125300                                                                          
125400     CALL 'MQINQMP' USING MQ-HCONN                                        
125500                       MQ-MSGHANDLE                                       
125600                       MQ-INQUIRE-PROP-OPTIONS                            
125700                       MQ-INQ-PROPNAME                                    
125800                       MQ-PROPDESC                                        
125900                       MQ-PROPTYPE                                        
126000                       MQ-PROPVALUELENGTH                                 
126100                       MQ-PROPVALUE                                       
126200                       MQ-PROPDATALENGTH                                  
126300                       MQ-COMPCODE                                        
126400                       MQ-REASON                                          
126500                                                                          
126600     IF MQ-COMPCODE  = MQCC-OK                                            
126700       MOVE MQ-PROPVALUE TO MQ-CONTRACT-ID                                
126800     ELSE                                                                 
126900       IF MQ-REASON = MQRC-PROPERTY-NOT-AVAILABLE                         
127000         CONTINUE                                                         
127100*         MOVE SPACE        TO MQ-CONTRACT-ID                             
127200       ELSE                                                               
127300         MOVE MQ-COMPCODE TO RC-DISPLAY                                   
127400         MOVE MQ-REASON  TO REASON-DISPLAY                                
127500         STRING 'RC FROM MQINQMP' RC-DISPLAY                              
127600                ' REASON: ' REASON-DISPLAY                                
127700                ' ' MQ-QNAME                                              
127800            DELIMITED BY SIZE                                             
127900            INTO ERROR-TEXT                                               
128000         MOVE SPACE TO ERROR-TEXT2                                        
128100         DISPLAY IDPGM ' ' ERROR-TEXT                                     
128200         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
128300       END-IF                                                             
128400     END-IF                                                               
128500                                                                          
128600*    -- WRITE LOG DATA TO TNT QUEUE                                       
128700     MOVE MQ-INTEGRATION-ID      TO TNT-INTEGRATION-ID                    
128800     MOVE MQ-CONTRACT-ID         TO TNT-CONTRACT-ID                       
128900                                                                          
129000     PERFORM T-TRACK-AND-TRACE                                            
129100     .                                                                    
129200                                                                          
129300     EJECT                                                                
129400 D1AA-CONVERT-TO-EBCDIC SECTION.                                          
129500                                                                          
129600*    -- CONVERT TO UCS2 AND THEN TO SWEDISH EBCDIC                        
129700     COMPUTE TRANSLATED-LENGTH =                                          
129800        FUNCTION ULENGTH(WSEGDATA(1:MQ-DATALENGTH))                       
129900                                                                          
130000     MOVE FUNCTION DISPLAY-OF(                                            
130100             FUNCTION NATIONAL-OF(WSEGDATA(1:MQ-DATALENGTH),              
130200                                  MQMD-CODEDCHARSETID),                   
130300             SW-EBCDIC)                                                   
130400       TO WSEGDATA                                                        
130500     MOVE TRANSLATED-LENGTH TO MQ-DATALENGTH                              
130600     .                                                                    
130700                                                                          
130800     EJECT                                                                
130900 D2-MQ-GET-MSG-LINE  SECTION.                                             
131000                                                                          
131100*    -- THE WHOLE MESSAGE WAS READ IN THE OPEN CALL                       
131200*    -- IT CONTAINS 1 OR MORE "LINES" SEPARATED BY A CR(-LF)              
131300*    -- SEQUENCE (IN EBCDIC OR ASCII)                                     
131400*    --                                                                   
131500*    -- HERE, EXTRACT THE NEXT LINE OF DATA                               
131600*                                                                         
131700     MOVE IDCOM-CURRENT-LEN (IDCOM-IX)  TO WSEGLEN                        
131800     MOVE IDCOM-CURRENT-POS (IDCOM-IX)  TO WSEGPOS                        
131900     MOVE IDCOM-CURRENT-DATA(IDCOM-IX)  TO WSEGDATA                       
132000                                                                          
132100     MOVE ZERO  TO WDLEN                                                  
132200     MOVE SPACE TO WDATA                                                  
132300                                                                          
132400     IF WSEGPOS <= WSEGLEN                                                
132500       UNSTRING WSEGDATA(WSEGPOS:WSEGLEN - WSEGPOS + 1)                   
132600       DELIMITED BY CR OR LF-ASCII OR LF-EBCDIC                           
132700       INTO WDATA COUNT IN WDLEN                                          
132800                                                                          
132900       IF WDLEN  > MAX-RESULT-LEN                                         
133000*      -- TRUNCATION, SOME DATA NOT PROCESSED                             
133100         MOVE SPACE TO ERROR-TEXT ERROR-TEXT2                             
133200         MOVE 21     TO RECV-KDRC                                         
133300         MOVE WDLEN          TO W-DISP1                                   
133400         MOVE MAX-RESULT-LEN TO W-DISP2                                   
133500         STRING 'DATA TRUNCATION. RECEIVED LENGTH: ' W-DISP1              
133600                ' MAX ALLOWED: ' W-DISP2                                  
133700           DELIMITED BY SIZE                                              
133800           INTO ERROR-TEXT                                                
133900         DISPLAY IDPGM ' ' ERROR-TEXT                                     
134000         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
134100       END-IF                                                             
134200                                                                          
134300*      -- RETURN THE RESULT                                               
134400       IF WDLEN > 0                                                       
134500         MOVE WDATA(1:WDLEN) TO RECV-DATA(1:WDLEN)                        
134600       END-IF                                                             
134700       MOVE WDLEN        TO RECV-KVDLEN                                   
134800                                                                          
134900*      -- COMPUTE WSEGPOS FOR NEXT CALL                                   
135000       COMPUTE WSEGPOS = WSEGPOS + WDLEN + 1                              
135100       IF WSEGDATA(WSEGPOS:1) = CR OR LF-EBCDIC OR LF-ASCII               
135200*        -- ALSO SKIP ADDITIONAL CONTROL CHAR                             
135300         ADD 1 TO WSEGPOS                                                 
135400       END-IF                                                             
135500*      -- SAVE NEW STARTING POINT                                         
135600       MOVE WSEGPOS TO IDCOM-CURRENT-POS (IDCOM-IX)                       
135700                                                                          
135800     ELSE                                                                 
135900*      -- EOF                                                             
136000       MOVE 1        TO RECV-KDRC                                         
136100       MOVE ZERO     TO RECV-KVDLEN                                       
136200       MOVE ZERO     TO IDCOM-CURRENT-LEN (IDCOM-IX)                      
136300       MOVE ZERO     TO IDCOM-CURRENT-POS (IDCOM-IX)                      
136400       MOVE SPACE    TO IDCOM-CURRENT-DATA(IDCOM-IX)                      
136500     END-IF                                                               
136600     .                                                                    
136700                                                                          
136800                                                                          
136900     EJECT                                                                
137000 D3-MQ-CLOSE  SECTION.                                                    
137100                                                                          
137200*    --  CLOSE THE MQ QUEUE                                               
137300*    --  AND BREAK CONNECTION WITH THE QUEUE MANAGER                      
137400     PERFORM S05-MQ-CLOSEQ                                                
137500     PERFORM S06-MQ-DISCONNECT                                            
137600                                                                          
137700     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
137800     .                                                                    
137900                                                                          
138000     EJECT                                                                
138100 D4-MQ-CLOSEQ  SECTION.                                                   
138200                                                                          
138300*    --  CLOSE THE MQ QUEUE                                               
138400     PERFORM S05-MQ-CLOSEQ                                                
138500     .                                                                    
138600                                                                          
138700     EJECT                                                                
138800 D5-MQ-DISCONNECT  SECTION.                                               
138900                                                                          
139000*    --  BREAK CONNECTION WITH THE QUEUE MANAGER                          
139100     PERFORM S06-MQ-DISCONNECT                                            
139200                                                                          
139300     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
139400     .                                                                    
139500                                                                          
139600 E1-DISP-OPEN  SECTION.                                                   
139700                                                                          
139800*    If you are here, then the first and only segment from IMS            
139900*    is the *DISPATC one. Use that to open the dispatcher table           
140000*    cursor.                                                              
140100                                                                          
140200*    -- ASSUME FORMAT 1 (WITH INTERNAL LENGTH FIELDS)                     
140300     MOVE WDATA(9:2)             TO WDLEN-X                               
140400                                                                          
140500*    -- CHECK FOR FORMAT 2 (SIMPLE UNBLOCKED)                             
140600     IF WDLEN > 16384 OR < 0                                              
140700       MOVE WDATA(9:WSEGLEN)     TO IDCOM-RESTART-KEYS(IDCOM-IX)          
140800     ELSE                                                                 
140900       IF WDLEN > 0                                                       
141000*        -- DATA EXISTS, FORMAT 1 (BLOCKED)                               
141100         MOVE WDATA(11:WDLEN)    TO IDCOM-RESTART-KEYS(IDCOM-IX)          
141200       ELSE                                                               
141300         MOVE ' INVALID IMS SEGMENT DATA. '                               
141400                                 TO ERROR-TEXT                            
141500         DISPLAY IDPGM ' ' ERROR-TEXT                                     
141600         CALL FELLOG                                                      
141700       END-IF                                                             
141800     END-IF                                                               
141900                                                                          
142000     MOVE IDCOM-ADDISPABS (IDCOM-IX)                                      
142100                                 TO DIDA-ADDISPABS                        
142200     MOVE IDCOM-TIDATETIME-DB2 (IDCOM-IX)                                 
142300                                 TO DIDA-TIDATETIME-DB2                   
142400     PERFORM DB2-DCL-OPN-TZ1DIDA-CRS                                      
142500     IF LINES-FOUND                                                       
142600       CONTINUE                                                           
142700     ELSE                                                                 
142800       MOVE 20                   TO RECV-KDRC                             
142900     END-IF                                                               
143000     .                                                                    
143100                                                                          
143200 E2-DISP-GET   SECTION.                                                   
143300                                                                          
143400     PERFORM DB2-FETCH-TZ1DIDA-CRS                                        
143500     IF LINES-FOUND                                                       
143600       IF DIDA-DATA-LENGTH > MAX-RESULT-LEN                               
143700*        -- TRUNCATION, SOME DATA NOT PROCESSED                           
143800         MOVE SPACE              TO ERROR-TEXT                            
143900                                    ERROR-TEXT2                           
144000         MOVE 21                 TO RECV-KDRC                             
144100         MOVE DIDA-DATA-LENGTH   TO W-DISP1                               
144200         MOVE MAX-RESULT-LEN     TO W-DISP2                               
144300         STRING 'DATA TRUNCATION. RECEIVED LENGTH: ' W-DISP1              
144400                ' MAX ALLOWED: ' W-DISP2                                  
144500           DELIMITED BY SIZE   INTO ERROR-TEXT                            
144600                                                                          
144700         DISPLAY IDPGM ' ' ERROR-TEXT                                     
144800         CALL ABEND           USING RKOD-ABEND-NO-DUMP                    
144900       ELSE                                                               
145000         MOVE DIDA-DATA-DATA(1:DIDA-DATA-LENGTH)                          
145100                                 TO RECV-DATA (1:DIDA-DATA-LENGTH)        
145200         MOVE DIDA-DATA-LENGTH   TO RECV-KVDLEN                           
145300       END-IF                                                             
145400     ELSE                                                                 
145500       MOVE 1                    TO RECV-KDRC                             
145600     END-IF                                                               
145700     .                                                                    
145800                                                                          
145900 E3-DISP-CLOSE SECTION.                                                   
146000                                                                          
146100     PERFORM DB2-CLOSE-TZ1DIDA-CRS                                        
146200                                                                          
146300*    -- DELETE THE ROWS IN THE RESTART TABLE IF '0' DAYS                  
146400*    -- SAVING TIME HAS BEEN SPECIFIED AND THERE ACTUALLY                 
146500*    -- WAS A *DISPATC SEGMENT IN THE DATA                                
146600                                                                          
146700     IF IDCOM-TIDATETIME-DB2 (IDCOM-IX) > SPACES                          
146800       MOVE IDCOM-ADDISPABS (IDCOM-IX)                                    
146900                                 TO DIDA-ADDISPABS                        
147000       MOVE IDCOM-TIDATETIME-DB2 (IDCOM-IX)                               
147100                                 TO DIDA-TIDATETIME-DB2                   
147200       IF IDCOM-KVDAGAR-RESEND(IDCOM-IX) = '0 '                           
147300         PERFORM DB2-DELETE-TZ1DIDA                                       
147400       ELSE                                                               
147500         MOVE RECV-KDRC-HTTP     TO DIDA-KDRC-HTTP                        
147600         MOVE RECV-KDRC-PULS     TO DIDA-KDRC-PULS                        
147700         MOVE RECV-KDKOMSTA      TO DIDA-KDKOMSTA                         
147800         MOVE RECV-MESSAGE-KVDLEN                                         
147900                                 TO DIDA-MESSAGE-L                        
148000         MOVE RECV-MESSAGE       TO DIDA-MESSAGE-D                        
148100         PERFORM DB2-UPDATE-TZ1DIDA                                       
148200       END-IF                                                             
148300     END-IF                                                               
148400                                                                          
148500     MOVE NOO                    TO IDCOM-OPEN(IDCOM-IX)                  
148600     .                                                                    
148700                                                                          
148800 S01-MOVE-DATA SECTION.                                                   
148900                                                                          
149000*    -- MOVE DATA FROM ONE SEGMENT TO THE RIGHT POSITION                  
149100*    -- IN THE OUTPUT FIELD. DO THIS ONLY IF THERE IS ANY DATA            
149200*    -- TO MOVE, AND IT DOES NOT OVERFLOW THE OUTPUT FIELD.               
149300*    -- WFROM-POS IS EITHER 1 OR 9 DEPENDING ON IF THE FIRST              
149400*    -- 8 POSITIONS IN THE SEGMENT SHOULD BE SKIPPED OR NOT.              
149500                                                                          
149600     IF WLEN >= WFROM-POS                                                 
149700     AND WSTART + WLEN - WFROM-POS <= MAX-RESULT-LEN                      
149800       COMPUTE WLEN = WLEN + 1 - WFROM-POS                                
149900       MOVE WDATA(WFROM-POS:WLEN) TO RECV-DATA(WSTART:WLEN)               
150000       ADD  WLEN                  TO RECV-KVDLEN                          
150100*      -- PREPARE POINTER FOR NEXT TIME                                   
150200       ADD  WLEN                 TO WSTART                                
150300     ELSE                                                                 
150400*      -- ADJUST WSTART SO IT CAN BE COMPARED WITH MAX-RESULT-LEN         
150500*      -- AND TRIGGER TRUNCATION ERROR - KDRC 21                          
150600       COMPUTE WSTART = WSTART + WLEN - WFROM-POS + 1                     
150700     END-IF                                                               
150800     .                                                                    
150900                                                                          
151000                                                                          
151100 S02-EXTRACT-MQ-PARAMETERS SECTION.                                       
151200                                                                          
151300*    -- META DATA: INTEGRATION ID                                         
151400     MOVE ZERO                   TO TALLY                                 
151500     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
151600             FOR CHARACTERS BEFORE INITIAL 'I:'                           
151700     MOVE SPACE                  TO MQ-INTEGRATION-ID                     
151800     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
151900       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
152000                               INTO MQ-INTEGRATION-ID                     
152100     END-IF                                                               
152200                                                                          
152300*    -- META DATA: CONTRACT ID                                            
152400     MOVE ZERO                   TO TALLY                                 
152500     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
152600             FOR CHARACTERS BEFORE INITIAL 'C:'                           
152700     MOVE SPACE                  TO MQ-CONTRACT-ID                        
152800     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
152900       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
153000                               INTO MQ-CONTRACT-ID                        
153100     END-IF                                                               
153200     .                                                                    
153300                                                                          
153400 S05-MQ-CLOSEQ SECTION.                                                   
153500                                                                          
153600     MOVE IDCOM-MQ-HCONN (IDCOM-IX) TO MQ-HCONN                           
153700     MOVE IDCOM-MQ-HOBJ  (IDCOM-IX) TO MQ-HOBJ                            
153800                                                                          
153900     CALL 'MQCLOSE' USING MQ-HCONN                                        
154000                         MQ-HOBJ                                          
154100                         MQCO-NONE                                        
154200                         MQ-COMPCODE                                      
154300                         MQ-REASON                                        
154400                                                                          
154500     IF MQ-COMPCODE NOT = MQCC-OK                                         
154600       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
154700       MOVE MQ-REASON  TO REASON-DISPLAY                                  
154800       STRING 'RC FROM MQCLOSE ' RC-DISPLAY                               
154900              ' REASON: ' REASON-DISPLAY                                  
155000          DELIMITED BY SIZE                                               
155100          INTO ERROR-TEXT                                                 
155200       MOVE SPACE TO ERROR-TEXT2                                          
155300       DISPLAY IDPGM ' ' ERROR-TEXT                                       
155400       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
155500     END-IF                                                               
155600     .                                                                    
155700                                                                          
155800     EJECT                                                                
155900 S06-MQ-DISCONNECT SECTION.                                               
156000                                                                          
156100     MOVE IDCOM-MQ-HCONN (IDCOM-IX) TO MQ-HCONN                           
156200                                                                          
156300     CALL 'MQDISC'  USING MQ-HCONN                                        
156400                         MQ-COMPCODE                                      
156500                         MQ-REASON                                        
156600                                                                          
156700     IF MQ-COMPCODE NOT = MQCC-OK                                         
156800       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
156900       MOVE MQ-REASON  TO REASON-DISPLAY                                  
157000       STRING 'RC FROM MQDISC ' RC-DISPLAY                                
157100              ' REASON: ' REASON-DISPLAY                                  
157200          DELIMITED BY SIZE                                               
157300          INTO ERROR-TEXT                                                 
157400       MOVE SPACE TO ERROR-TEXT2                                          
157500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
157600       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
157700     END-IF                                                               
157800     .                                                                    
157900                                                                          
158000 T-TRACK-AND-TRACE SECTION.                                               
158100     MOVE FUNCTION HEX-OF (MQMD-MSGID)                                    
158200                                 TO messageId                             
158300     MOVE WSEGLEN                TO messageSize                           
158400     MOVE WSEGDATA(1:WSEGLEN)    TO logtext                               
158500     INSPECT logtext REPLACING ALL X'00' BY '_'                           
158600                                                                          
158700     MOVE FUNCTION                                                        
158800       FORMATTED-CURRENT-DATE('YYYY-MM-DDThh:mm:ss.sssZ')                 
158900                                 TO timestamp                             
159000                                                                          
159100     MOVE TNT-INTEGRATION-ID     TO integrationId                         
159200     MOVE TNT-CONTRACT-ID        TO contractId                            
159300     MOVE SPACES                 TO transactionId                         
159400                                                                          
159500     MOVE '000573'               TO applicationId                         
159600                                                                          
159700     MOVE 'INFO'                 TO level                                 
159800                                                                          
159900     XML GENERATE MQ-TNT-MSG-DATA                                         
160000       FROM log                                                           
160100       COUNT IN MQ-TNT-MSG-LEN                                            
160200*      WITH ENCODING 1208                                                 
160300*      WITH XML-DECLARATION                                               
160400       NAME OF logtext           IS "text"                                
160500       SUPPRESS WHEN SPACES                                               
160600       ON EXCEPTION                                                       
160700         DISPLAY IDPGM ' ERROR IN XML GEN :' XML-CODE                     
160800     END-XML                                                              
160900     COMPUTE MQ-TNT-MSG-LEN = FUNCTION BYTE-LENGTH (                      
161000                              FUNCTION TRIM (MQ-TNT-MSG))                 
161100                                                                          
161200     PERFORM S41-MQ-OPEN-TNT                                              
161300     PERFORM S41-MQ-PUT-TNT                                               
161400     PERFORM S41-MQ-CLOSEQ-TNT                                            
161500     .                                                                    
161600                                                                          
161700 S41-MQ-OPEN-TNT SECTION.                                                 
161800     MOVE 'VCC.TNT.LOGEVENT'     TO TTOD-OBJECTNAME                       
161900                                                                          
162000     MOVE MQOO-FAIL-IF-QUIESCING TO TNT-OPENOPTIONS                       
162100     ADD MQOO-OUTPUT             TO TNT-OPENOPTIONS                       
162200                                                                          
162300     CALL 'MQOPEN'            USING MQ-HCONN                              
162400                                    TTOD                                  
162500                                    TNT-OPENOPTIONS                       
162600                                    TNT-HOBJ                              
162700                                    MQ-COMPCODE                           
162800                                    MQ-REASON                             
162900                                                                          
163000     IF MQ-COMPCODE NOT = MQCC-OK                                         
163100       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
163200       MOVE MQ-REASON            TO REASON-DISPLAY                        
163300       MOVE SPACE                TO ERROR-TEXT-TNT                        
163400       STRING 'RC FROM TNT MQOPEN ' RC-DISPLAY                            
163500              ' REASON: ' REASON-DISPLAY                                  
163600              ' ' TTOD-OBJECTNAME                                         
163700          DELIMITED BY SIZE                                               
163800                               INTO ERROR-TEXT-TNT                        
163900       DISPLAY IDPGM ' ' ERROR-TEXT                                       
164000     END-IF                                                               
164100     .                                                                    
164200                                                                          
164300 S41-MQ-PUT-TNT SECTION.                                                  
164400*    -- PUT THE TNT MESSAGE                                               
164500                                                                          
164600     COMPUTE TTPMO-OPTIONS        = MQPMO-FAIL-IF-QUIESCING               
164700                                                                          
164800     MOVE MQMI-NONE              TO TTMD-MSGID                            
164900     MOVE MQCI-NONE              TO TTMD-CORRELID                         
165000     MOVE MQFMT-STRING           TO TTMD-FORMAT                           
165100     MOVE SWEDISH-EBCDIC         TO TTMD-CODEDCHARSETID                   
165200     MOVE MQENC-NATIVE           TO TTMD-ENCODING                         
165300     MOVE MQCCSI-Q-MGR           TO TTMD-CODEDCHARSETID                   
165400                                                                          
165500     MOVE MQPMO-VERSION-3        TO TTPMO-VERSION                         
165600     MOVE MQACTP-NEW             TO TTPMO-ACTION                          
165700                                                                          
165800     MOVE MQ-TNT-MSG-LEN         TO MQ-MSGLENGTH                          
165900                                                                          
166000     CALL 'MQPUT'             USING MQ-HCONN                              
166100                                    TNT-HOBJ                              
166200                                    TTMD                                  
166300                                    TTPMO                                 
166400                                    MQ-MSGLENGTH                          
166500                                    MQ-TNT-MSG                            
166600                                    MQ-COMPCODE                           
166700                                    MQ-REASON                             
166800                                                                          
166900     IF MQ-COMPCODE NOT = MQCC-OK                                         
167000       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
167100       MOVE MQ-REASON            TO REASON-DISPLAY                        
167200       MOVE SPACE                TO ERROR-TEXT-TNT                        
167300       STRING 'RC FROM TNT MQPUT '   RC-DISPLAY                           
167400              ' REASON: ' REASON-DISPLAY                                  
167500          DELIMITED BY SIZE    INTO ERROR-TEXT-TNT                        
167600       DISPLAY IDPGM ' ' ERROR-TEXT                                       
167700     END-IF                                                               
167800     .                                                                    
167900                                                                          
168000 S41-MQ-CLOSEQ-TNT  SECTION.                                              
168100                                                                          
168200     CALL 'MQCLOSE'           USING MQ-HCONN                              
168300                                    TNT-HOBJ                              
168400                                    MQCO-NONE                             
168500                                    MQ-COMPCODE                           
168600                                    MQ-REASON                             
168700                                                                          
168800     IF MQ-COMPCODE NOT = MQCC-OK                                         
168900       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
169000       MOVE MQ-REASON            TO REASON-DISPLAY                        
169100       MOVE SPACE                TO ERROR-TEXT-TNT                        
169200       STRING 'RC FROM TNT MQCLOSE ' RC-DISPLAY                           
169300              ' REASON: ' REASON-DISPLAY                                  
169400          DELIMITED BY SIZE    INTO ERROR-TEXT-TNT                        
169500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
169600     END-IF                                                               
169700     .                                                                    
169800                                                                          
169900                                                                          
170000     EJECT                                                                
170100 IMS-GET-FIRST-SEGMENT SECTION.                                           
170200                                                                          
170300     STRING 'GET-FIRST-SEGMENT ' GU DELIMITED BY SIZE                     
170400            INTO ERROR-TEXT2                                              
170500     MOVE 128         TO  AIB-LEN                                         
170600     MOVE SPACE       TO  AIB-SUB-FUNCTION                                
170700     MOVE 'IOPCB   '  TO  AIB-PCB-NAME                                    
170800     MOVE LENGTH OF IMSMSG-IO-AREA                                        
170900                      TO AIB-IOAREA-LENGTH                                
171000                                                                          
171100     CALL AIBTDLI USING GU AIB-AREA IMSMSG-IO-AREA                        
171200                                                                          
171300     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
171400     MOVE ALT-STATUS-CODE   TO STATUS-WS                                  
171500     MOVE '  QCCF'          TO VALID-STATUS-CODES                         
171600     PERFORM IMS-STATUS-CHECK                                             
171700                                                                          
171800     IF SEGMENT-EXISTS                                                    
171900       COMPUTE WSEGLEN = IMSMSG-KVLL - 4                                  
172000       MOVE 1 TO WSEGPOS                                                  
172100       MOVE IMSMSG-IO-DATA(1:IMSMSG-KVLL - 4) TO WSEGDATA                 
172200     ELSE                                                                 
172300       MOVE ZERO  TO WSEGLEN                                              
172400       MOVE ZERO  TO WSEGPOS                                              
172500       MOVE SPACE TO WSEGDATA                                             
172600     END-IF                                                               
172700     MOVE SPACE TO ERROR-TEXT2                                            
172800     .                                                                    
172900                                                                          
173000     EJECT                                                                
173100 IMS-GET-NEXT-SEGMENT SECTION.                                            
173200                                                                          
173300     STRING 'GET-NEXT-SEGMENT ' GN DELIMITED BY SIZE                      
173400            INTO ERROR-TEXT2                                              
173500     MOVE 128         TO  AIB-LEN                                         
173600     MOVE SPACE       TO  AIB-SUB-FUNCTION                                
173700     MOVE 'IOPCB   '  TO  AIB-PCB-NAME                                    
173800     MOVE LENGTH OF IMSMSG-IO-AREA                                        
173900                      TO AIB-IOAREA-LENGTH                                
174000                                                                          
174100     CALL AIBTDLI USING GN AIB-AREA IMSMSG-IO-AREA                        
174200                                                                          
174300     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
174400     MOVE ALT-STATUS-CODE   TO STATUS-WS                                  
174500     MOVE '  QDCF'          TO VALID-STATUS-CODES                         
174600     PERFORM IMS-STATUS-CHECK                                             
174700                                                                          
174800     IF SEGMENT-EXISTS                                                    
174900       COMPUTE WSEGLEN = IMSMSG-KVLL - 4                                  
175000       MOVE 1 TO WSEGPOS                                                  
175100       MOVE IMSMSG-IO-DATA(1:IMSMSG-KVLL - 4) TO WSEGDATA                 
175200     ELSE                                                                 
175300       MOVE ZERO  TO WSEGLEN                                              
175400       MOVE ZERO  TO WSEGPOS                                              
175500       MOVE SPACE TO WSEGDATA                                             
175600     END-IF                                                               
175700     MOVE SPACE TO ERROR-TEXT2                                            
175800     .                                                                    
175900     EJECT                                                                
176000 IMS-STATUS-CHECK   SECTION.                                              
176100                                                                          
176200     SET STATUS-IX TO 1                                                   
176300     SEARCH VALID-STATUS                                                  
176400       AT END                                                             
176500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
176600         DELIMITED BY SIZE INTO ERROR-TEXT                                
176700         DISPLAY IDPGM ' ' ERROR-TEXT                                     
176800         CALL FELLOG                                                      
176900       WHEN VALID-STATUS (STATUS-IX) = STATUS-WS                          
177000         CONTINUE                                                         
177100     END-SEARCH                                                           
177200     .                                                                    
177300                                                                          
177400 DB2-DCL-OPN-TZ1DIDA-CRS SECTION.                                         
177500                                                                          
177600     MOVE 000100 TO GOOD-SQLCODECODES                                     
177700                                                                          
177800     EXEC SQL                                                             
177900       DECLARE TZ1DIDA-CRS CURSOR FOR                                     
178000                                                                          
178100       SELECT DATA                                                        
178200                                                                          
178300       FROM    TZ1DIDA                                                    
178400                                                                          
178500       WHERE  ADDISPABS      = :DIDA-ADDISPABS                            
178600        AND   TIDATETIME_DB2 = :DIDA-TIDATETIME-DB2                       
178700                                                                          
178800       ORDER BY IDLOPNR                                                   
178900     END-EXEC                                                             
179000                                                                          
179100     MOVE 000100180  TO GOOD-SQLCODECODES                                 
179200                                                                          
179300     EXEC SQL                                                             
179400       OPEN TZ1DIDA-CRS                                                   
179500     END-EXEC                                                             
179600                                                                          
179700     MOVE SQLCODE TO SQLCODE-WS                                           
179800     PERFORM DB2-STATUS-CHECK                                             
179900     .                                                                    
180000                                                                          
180100 DB2-FETCH-TZ1DIDA-CRS SECTION.                                           
180200                                                                          
180300     MOVE 000100  TO GOOD-SQLCODECODES                                    
180400                                                                          
180500     EXEC SQL                                                             
180600                                                                          
180700       FETCH TZ1DIDA-CRS                                                  
180800                                                                          
180900       INTO :DIDA-DATA                                                    
181000                                                                          
181100     END-EXEC                                                             
181200                                                                          
181300     MOVE SQLCODE TO SQLCODE-WS                                           
181400     PERFORM DB2-STATUS-CHECK                                             
181500     .                                                                    
181600                                                                          
181700 DB2-CLOSE-TZ1DIDA-CRS SECTION.                                           
181800                                                                          
181900     EXEC SQL                                                             
182000        CLOSE TZ1DIDA-CRS                                                 
182100     END-EXEC                                                             
182200     .                                                                    
182300                                                                          
182400 DB2-DELETE-TZ1DIDA SECTION.                                              
182500                                                                          
182600     MOVE 000100904              TO GOOD-SQLCODECODES                     
182700     EXEC SQL                                                             
182800       DELETE FROM TZ1DIDA                                                
182900                                                                          
183000       WHERE ADDISPABS      = :DIDA-ADDISPABS                             
183100       AND   TIDATETIME_DB2 = :DIDA-TIDATETIME-DB2                        
183200     END-EXEC                                                             
183300                                                                          
183400     MOVE SQLCODE                TO SQLCODE-WS                            
183500     PERFORM DB2-STATUS-CHECK                                             
183600     .                                                                    
183700                                                                          
183800 DB2-UPDATE-TZ1DIDA SECTION.                                              
183900                                                                          
184000     MOVE 000100904              TO GOOD-SQLCODECODES                     
184100     EXEC SQL                                                             
184200       UPDATE TZ1DIDA                                                     
184300         SET KDRC_HTTP      = :DIDA-KDRC-HTTP,                            
184400             KDRC_PULS      = :DIDA-KDRC-PULS,                            
184500             KDKOMSTA       = :DIDA-KDKOMSTA,                             
184600             MESSAGE        = :DIDA-MESSAGE                               
184700                                                                          
184800       WHERE ADDISPABS      = :DIDA-ADDISPABS                             
184900       AND   TIDATETIME_DB2 = :DIDA-TIDATETIME-DB2                        
185000     END-EXEC                                                             
185100                                                                          
185200     MOVE SQLCODE                TO SQLCODE-WS                            
185300     PERFORM DB2-STATUS-CHECK                                             
185400     .                                                                    
185500                                                                          
185600 DB2-STATUS-CHECK  SECTION.                                               
185700                                                                          
185800     SET SQLCODE-IX TO 1                                                  
185900     SEARCH GOOD-SQLCODE                                                  
186000       AT END                                                             
186100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
186200          DELIMITED BY SIZE INTO ERROR-TEXT                               
186300          DISPLAY IDPGM ' ' ERROR-TEXT                                    
186400          CALL ABEND USING RKOD-ABEND-DB2                                 
186500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
186600     END-SEARCH                                                           
186700     .                                                                    
186800                                                                          
