000100**ROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ01SEND.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   01/11/20                                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*      GENERAL SUBPROGRAM FOR SENDING ASYNCHRONOUS MESSAGES.              
001000*      IT IS PART OF THE NEW DISPATCHER FRAMEWORK USED IN                 
001100*      THE CAR PARTS SYSTEMS. CURRENTLY IT HANDLES COMMUNICATION          
001200*      VIA VCOM OR IMS PROGRAM-TO-PROGRAM SWICHING.                       
001300*      MQ IS NOW ALSO SUPPORTED.                                          
001400*                                                                         
001500*      CALL SYNTAX:                                                       
001600*      (OPEN)                                                             
001700*        CALL WZ01SEND USING SEND-CONTROL-AREA                            
001800*                            SEND-OPEN-AREA                               
001900*                                                                         
002000*      (SEND)                                                             
002100*        CALL WZ01SEND USING SEND-CONTROL-AREA                            
002200*                            SEND-KVDLEN                                  
002300*                            MY-DATA-FIELD                                
002400*                                                                         
002500*      (CLOSE)                                                            
002600*        CALL WZ01SEND USING SEND-CONTROL-AREA                            
002700*                                                                         
002800*      THE PARAMETERS USED IN THE CALLS ARE ALL DEFINED IN                
002900*      COPYTEXT WZ01SEND AND EXPLAINED IN MORE DETAIL IN THIS             
003000*      COPYTEXT.                                                          
003100*                                                                         
003200                                                                          
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'WZ01SEND'.            
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100 77  CR                          PIC X       VALUE X'0D'.                 
004200 77  LF-EBCDIC                   PIC X       VALUE X'25'.                 
004300 77  LF-ASCII                    PIC X       VALUE X'0A'.                 
004400 77  SWEDISH-EBCDIC              PIC S9(9)   VALUE 278  BINARY.           
004500                                                                          
004600 77  ANTAL-SEND                  PIC S9(4)   BINARY VALUE ZERO.           
004700                                                                          
004800 77  CONTINUE-PREFIX             PIC X(8)    VALUE '*CONTINU'.            
004900 77  RETADDRESS-PREFIX           PIC X(8)    VALUE '*RETADDR'.            
005000 77  RESTARTKEY-PREFIX           PIC X(8)    VALUE '*DISPATC'.            
005100                                                                          
005200 77  OK-SWITCH                   PIC X       VALUE 'J'.                   
005300 88  ALL-OK                                  VALUE 'J'.                   
005400 88  SOME-ERROR                              VALUE 'N'.                   
005500                                                                          
005600 01  WS-TIMESTAMP-DB2            PIC X(26).                               
005700 01  WS-IDUSER                   PIC X(8).                                
005800 01  WS-IDCALLER                 PIC X(8).                                
005900                                                                          
006000 01  W-VIMSID                    PIC X(8)    VALUE SPACE.                 
006100 01  FILLER REDEFINES W-VIMSID.                                           
006200     03  W-IMS-ENV               PIC X(3).                                
006300         88  ENV-DEVE                        VALUE 'IMP'.                 
006400         88  ENV-IGRT                        VALUE 'IMY'.                 
006500         88  ENV-XDEV                        VALUE 'IMD'.                 
006600         88  ENV-ACPT                        VALUE 'IMB'.                 
006700         88  ENV-PROD                        VALUE 'IMG'.                 
006800     03  FILLER                  PIC X(5).                                
006900                                                                          
007000 01  RC-DISPLAY                  PIC Z(8)9.                               
007100 01  REASON-DISPLAY              PIC Z(8)9.                               
007200                                                                          
007300 01  DUMMY                       PIC X.                                   
007400 01  WS-TZ1DIDA-DUMMY            PIC X(50).                               
007500                                                                          
007600 01  FILLER                      PIC X(16)   VALUE 'ERROR-TEXT:'.         
007700 01  ERROR-TEXT                  PIC X(100).                              
007800 01  ERROR-TEXT-TNT              PIC X(100).                              
007900                                                                          
008000*    -- FIELDS USED WHEN CUTTING UP THE DATA INTO                         
008100*    -- PIECES OF TRANMSITTABLE SIZE (10K FOR VCOM, 8K FOR IMS)           
008200 01  WLEN                        PIC S9(9)   BINARY.                      
008300 01  WSTART                      PIC S9(9)   BINARY.                      
008400                                                                          
008500 01  WSEGMPREFIX                 PIC X(8).                                
008600 01  WDATA                       PIC X(1000000).                          
008700                                                                          
008800*    -- FLAG INDICATING THAT RETURN ADDRESS HAS BEEN SPECIFIED            
008900*    -- AND THIS INFO SHOULD BE SENT TO THE RECEIVER.                     
009000 01  RETURN-SWITCH               PIC X       VALUE 'N'.                   
009100     88 INCLUDE-RETURN-ADDRESS               VALUE 'J'.                   
009200     88 NO-RETURN-ADDRESS                    VALUE 'N'.                   
009300                                                                          
009400*    -- USED WHEN COMPUTING DYNAMIC SENDER TAG VALUE                      
009500 01  WORK-SENDERTAG              PIC X(20).                               
009600                                                                          
009700*    -- LENGTH USED WHEN BUFFERING DATA IN IMS-TRANSMISSIONS              
009800 01  WSEGLEN-X.                                                           
009900     03 WSEGLEN                  PIC S9(4)   BINARY.                      
010000 01  WSEGSTART                   PIC S9(9)   BINARY.                      
010100                                                                          
010200 01  DYNAMIC-SUBPROGRAMS.                                                 
010300     03 ABEND                    PIC X(8)    VALUE 'ABEND   '.            
010400     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
010500     03 WZ01ATAB                 PIC X(8)    VALUE 'WZ01ATAB'.            
010600     03 WZ11OUTP                 PIC X(8)    VALUE 'WZ11OUTP'.            
010700     03 WZ11OUTM                 PIC X(8)    VALUE 'WZ11OUTM'.            
010800     03 DSCONS                   PIC X(8)    VALUE 'DSCONS  '.            
010900     03 DSSEND                   PIC X(8)    VALUE 'DSSEND  '.            
011000     03 DSRLSE                   PIC X(8)    VALUE 'DSRLSE  '.            
011100     03 AIBTDLI                  PIC X(8)    VALUE 'AIBTDLI '.            
011200     03 VIMSID                   PIC X(8)    VALUE 'VIMSID  '.            
011300*    03  --- MQ CALLS ARE STATIC ----                                     
011400                                                                          
011500                                                                          
011600*    -- PARAMETERS TO ABEND                                               
011700                                                                          
011800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   BINARY VALUE +16.            
011900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   BINARY VALUE +1000.          
012000 77  RKOD-ABEND-DB2              PIC S9(4)   BINARY VALUE +998.           
012100                                                                          
012200*    -- PARAMETERS TO WZ01ATAB                                            
012300                                                                          
012400*01  -COPY WZ01ATAB                                                       
012500                                                                          
012600 01  VCOM-AREA-START             PIC X(16)   VALUE                        
012700                                 'VCOM-AREA-START '.                      
012800*01  -COPY W0028 -PRE VCOM-                                               
012900                                                                          
013000*    -- 9990 = CA 9999 - 8                                                
013100 01  MAX-VCOM-DATA-LENGTH        PIC S9(9)   BINARY VALUE +9990.          
013200                                                                          
013300 01  MQ-AREA-START               PIC X(16)   VALUE                        
013400                                 'MQ-AREA-START   '.                      
013500                                                                          
013600 01  MQ-QMGR                     PIC X(48) VALUE SPACE.                   
013700 01  MQ-QNAME                    PIC X(48) VALUE SPACE.                   
013800 01  MQ-COMPCODE                 PIC S9(9) BINARY.                        
013900 01  MQ-REASON                   PIC S9(9) BINARY.                        
014000 01  MQ-HCONN                    PIC S9(9) BINARY VALUE ZERO.             
014100 01  MQ-OPENOPTIONS              PIC S9(9) BINARY VALUE ZERO.             
014200 01  MQ-HOBJ                     PIC S9(9) BINARY VALUE ZERO.             
014300 01  MQ-MSGLENGTH                PIC S9(9) BINARY.                        
014400 01  MQ-DATALENGTH               PIC S9(9) BINARY.                        
014500                                                                          
014600 01  MQ-INTEGRATION-ID           PIC X(100)  VALUE SPACE.                 
014700 01  MQ-CONTRACT-ID              PIC X(100)  VALUE SPACE.                 
014800 01  MQ-TOPIC-STRING             PIC X(100)  VALUE SPACE.                 
014900 01  MQ-TYPE                     PIC X(100)  VALUE SPACE.                 
015000 01  MQ-PROPHANDLE               PIC S9(18) BINARY.                       
015100 01  MQ-PROPNAME-TO-SET          PIC X(25).                               
015200 01  MQ-PROPTYPE                 PIC S9(9)  BINARY.                       
015300 01  MQ-PROPVALUELENGTH          PIC S9(9)  BINARY.                       
015400 01  MQ-PROPVALUE                PIC X(100).                              
015500 01  MQ-PROPDATALENGTH           PIC S9(9)  BINARY.                       
015600                                                                          
015700 01  MQ-MESSAGE-DESCRIPTOR.                                               
015800*    -COPY CMQMDV                                                         
015900                                                                          
016000 01  MQ-PUT-MESSAGE-OPTIONS.                                              
016100*    -COPY CMQPMOV                                                        
016200                                                                          
016300 01  MQ-OBJECT-DESCRIPTOR.                                                
016400*    -COPY CMQODV                                                         
016500                                                                          
016600 01  MQ-CONSTANTS.                                                        
016700*    -COPY CMQV                                                           
016800                                                                          
016900 01  MQ-CREATE-HANDLE-OPTIONS.                                            
017000*    -COPY CMQCMHOV                                                       
017100                                                                          
017200 01  MQ-SET-PROP-OPTIONS.                                                 
017300*    -COPY CMQSMPOV                                                       
017400                                                                          
017500 01  MQ-PROPNAME.                                                         
017600*    -COPY CMQCHRVV                                                       
017700                                                                          
017800 01  MQ-PROPDESC.                                                         
017900*    -COPY CMQPDV                                                         
018000                                                                          
018100 77  MQ-TNT-MSG-LEN              PIC S9(9) BINARY VALUE ZERO.             
018200                                                                          
018300 01  MQ-TNT-MSG.                                                          
018400     03  FILLER                  PIC X(38) VALUE                          
018500                '<?xml version="1.0" encoding="UTF-8"?>'.                 
018600     03  MQ-TNT-MSG-DATA         PIC X(10000) VALUE SPACES.               
018700                                                                          
018800 01  WS-CURRENT-DATE             PIC X(21).                               
018900                                                                          
019000 01  log.                                                                 
019100     03  messageId               PIC X(48).                               
019200     03  timestamp               PIC X(24).                               
019300     03  messageSize             PIC X(09).                               
019400     03  integrationId           PIC X(10).                               
019500     03  contractId              PIC X(10).                               
019600     03  transactionId           PIC X(48).                               
019700*    03  logicalIds.                                                      
019800*        05  logicalId                       OCCURS 5.                    
019900*            07  logicalIdType   PIC X(50).                               
020000*            07  logicalIdValue  PIC X(50).                               
020100*    03  physicalId              PIC X(100).                              
020200*    03  hostName                PIC X(30).                               
020300*    03  hostIp                  PIC X(30).                               
020400     03  applicationId           PIC X(06).                               
020500*    03  objectId                PIC X(60).                               
020600     03  level                   PIC X(10).                               
020700     03  logtext                 PIC X(3000).                             
020800                                                                          
020900 01  MQ-COPYBOOKS-TNT.                                                    
021000     COPY CMQODV   REPLACING LEADING == MQOD == BY == TTOD ==.            
021100     COPY CMQMD2V  REPLACING LEADING == MQMD == BY == TTMD ==.            
021200     COPY CMQPMOV  REPLACING LEADING == MQPMO == BY == TTPMO ==.          
021300 01  TNT-FIELDS.                                                          
021400     03  TNT-OPENOPTIONS         PIC S9(9) BINARY VALUE ZERO.             
021500     03  TNT-HOBJ                PIC S9(9) BINARY VALUE ZERO.             
021600     03  TNT-INTEGRATION-ID      PIC X(10)   VALUE SPACE.                 
021700     03  TNT-CONTRACT-ID         PIC X(10)   VALUE SPACE.                 
021800                                                                          
021900 01  IDCOM-AREA-START             PIC X(16)   VALUE                       
022000                                 'IDCOM-AREA-START'.                      
022100                                                                          
022200*    -- NUMBER OF USED ENTRIES IN IDCOM-TABLE AND MAX VALUE               
022300 01  IDCOM-IX-MAX-USED           PIC S9(9)   BINARY VALUE +0.             
022400 01  IDCOM-TABLE-LENGTH          PIC S9(9)   BINARY VALUE +5.             
022500*    -- WORK INDEX                                                        
022600 01  IDCOM-IX                    PIC S9(9)   BINARY.                      
022700                                                                          
022800*    -- AREA FOR SAVING THE DATA FOR EACH IDCOM VALUE                     
022900 01  IDCOM-TABLE.                                                         
023000   03  IDCOM-ENTRY OCCURS 5.                                              
023100*    05  -COPY WZ01SEN2  -PRE IDCOM-                                      
023200     05  IDCOM-KDCOMTYPE         PIC X(10).                               
023300     05  IDCOM-OPEN              PIC X(1).                                
023400*    -- DATA FOR VCOM-TRANSMISSIONS:                                      
023500     05  IDCOM-VCOM-DISTID       PIC X(34).                               
023600*    -- DATA FOR IMS-TRANSMISSIONS:                                       
023700     05  IDCOM-KDTRANS           PIC X(8).                                
023800     05  IDCOM-PCBNAME           PIC X(8).                                
023900     05  IDCOM-IDPROCESS         PIC X(10).                               
024000     05  IDCOM-KDSOPFUNK         PIC X(1).                                
024100     05  IDCOM-FLBLOCK           PIC X(1).                                
024200*    -- BUFFER FOR BLOCKING DATA IN IMS & MQ-TRANSMISSIONS                
024300     05  IDCOM-CURRENT-LEN       PIC S9(9)  BINARY.                       
024400     05  IDCOM-CURRENT-DATA      PIC X(1000000).                          
024500     05  IDCOM-FLRESTART         PIC X.                                   
024600*    -- RESTART-TABLE KEYS:                                               
024700     05  IDCOM-RESTART-KEYS.                                              
024800       07 IDCOM-ADDISPABS        PIC X(50).                               
024900       07 IDCOM-TIDATETIME-DB2   PIC X(26).                               
025000       07 IDCOM-IDLOPNR          PIC S9(4) COMP.                          
025100     05  IDCOM-SEND-RESTART-SEGM PIC X.                                   
025200     05  IDCOM-MQ-HCONN          PIC S9(9) BINARY.                        
025300     05  IDCOM-MQ-HOBJ           PIC S9(9) BINARY.                        
025400     05  IDCOM-MQ-INTEGRATION-ID PIC X(100).                              
025500     05  IDCOM-MQ-CONTRACT-ID    PIC X(100).                              
025600     05  IDCOM-MQ-TOPIC-STRING   PIC X(100).                              
025700     05  IDCOM-MQ-TYPE           PIC X(100).                              
025800                                                                          
025900 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
026000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
026100                                                                          
026200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
026300 01  DB2-WS.                                                              
026400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
026500         88  CURSOR-OK                       VALUE 000.                   
026600         88  LINES-FOUND                     VALUE 000.                   
026700         88  LINES-MISSING                   VALUE 100.                   
026800         88  RESOURCE-WRONG                  VALUE 904.                   
026900     03  GOOD-SQLCODECODES.                                               
027000         05  GOOD-SQLCODE OCCURS 5                                        
027100             INDEXED BY SQLCODE-IX PIC 9(3).                              
027200                                                                          
027300 01  FILLER                      PIC X(16)  VALUE 'TZ1DIDA-AREA'.         
027400                                                                          
027500* -COPY TZ1DIDA -PRE DIDA-                                                
027600                                                                          
027700       EXEC SQL INCLUDE TZ1DIDA END-EXEC.                                 
027800                                                                          
027900 01  WS-CLOB     USAGE SQL TYPE IS CLOB(5242880).                         
028000*    -- IMS FUNKTIONSKODER                                                
028100*    -COPY W0003                                                          
028200                                                                          
028300 01  STATUS-WS                   PIC XX.                                  
028400 01  VALID-STATUS-CODES.                                                  
028500   03  VALID-STATUS OCCURS 5 INDEXED BY STATUS-IX                         
028600                                 PIC XX.                                  
028700                                                                          
028800 01  AIB-AREA-START              PIC X(16)   VALUE                        
028900                                 'AIB-AREA-START  '.                      
029000                                                                          
029100*    -COPY W0031                                                          
029200                                                                          
029300 01  MSG-AREA-START              PIC X(16)   VALUE                        
029400                                 'MSG-AREA-START  '.                      
029500 01  IMSMSG-IO-AREA.                                                      
029600   03  IMSMSG-KVLL               PIC S9(4)   BINARY.                      
029700   03  IMSMSG-KDZZ               PIC XX.                                  
029800   03  IMSMSG-IO-DATA            PIC X(32000).                            
029900                                                                          
030000*    -- 16370 < 16 KB - 8                                                 
030100*    -- DO NOT INCREASE THIS - RECEVIE CAN NOT DISTINGUISH                
030200*    -- BETWEEN UNBLOCKED/BLOCKED FORMAT IF > 16KB                        
030300 01  IMSMSG-MAX-DATA-LENGTH      PIC S9(9)   BINARY VALUE +16370.         
030400                                                                          
030500 01  MSGSOP-START                PIC X(16)   VALUE                        
030600                                 'MSGSOP-START    '.                      
030700*01  -COPY WMSGSOP                                                        
030800                                                                          
030900 01  WZ11OUTP-START              PIC X(16)   VALUE                        
031000                                 'WZ11OUTP-START  '.                      
031100*01  -COPY WZ11OUTP                                                       
031200                                                                          
031300 01  WZ11OUTM-START              PIC X(16)   VALUE                        
031400                                 'WZ11OUTM-START  '.                      
031500*01  -COPY WZ11OUTM                                                       
031600                                                                          
031700 LINKAGE SECTION.                                                         
031800                                                                          
031900*01 -COPY WZ01SEN1                                                        
032000                                                                          
032100*01 -COPY WZ01SEN2                                                        
032200                                                                          
032300 01  SEND-KVDLEN                PIC S9(9) BINARY.                         
032400                                                                          
032500 01  SEND-DATA                  PIC X(1500000).                           
032600                                                                          
032700*    -COPY W0009  -PRE ALT-                                               
032800                                                                          
032900 PROCEDURE DIVISION USING                                                 
033000                      SEND-CONTROL-AREA                                   
033100                      SEND-KVDLEN                                         
033200*                      ... OR SEND-KVDLEN                                 
033300                      SEND-DATA                                           
033400                     .                                                    
033500* -- NOTE: ARGUMENTS ABOVE DIFFER DEPENDING ON CALL TYPE                  
033600* --       SEE A-INIT                                                     
033700 MAIN SECTION.                                                            
033800                                                                          
033900     PERFORM A-INIT                                                       
034000                                                                          
034100     IF ALL-OK                                                            
034200       EVALUATE IDCOM-KDCOMTYPE(IDCOM-IX) ALSO SEND-KDFUNC                
034300         WHEN  'VCOM'  ALSO  'OPEN'                                       
034400           PERFORM B1-VCOM-CONNECT                                        
034500                                                                          
034600         WHEN  'VCOM'  ALSO  'PUT'                                        
034700           PERFORM B2-VCOM-SEND                                           
034800         WHEN  'VCOM'  ALSO  'CLOSE'                                      
034900           PERFORM B3-VCOM-RELEASE                                        
035000                                                                          
035100         WHEN  'IMSP2P'  ALSO  'OPEN'                                     
035200           PERFORM C1-IMSP2P-INIT                                         
035300         WHEN  'IMSP2P'  ALSO  'PUT'                                      
035400           PERFORM C2-IMSP2P-INSERT-DATA                                  
035500         WHEN  'IMSP2P'  ALSO  'CLOSE'                                    
035600           PERFORM C3-IMSP2P-CLOSE                                        
035700                                                                          
035800*        Communication Type API, in asynchronous mode,                    
035900*        will use IMSP2P as method to send the info to                    
036000*        a background IMS transaction.                                    
036100         WHEN  'API'     ALSO  'OPEN'                                     
036200           PERFORM C1-IMSP2P-INIT                                         
036300         WHEN  'API'     ALSO  'PUT'                                      
036400           PERFORM C2-IMSP2P-INSERT-DATA                                  
036500         WHEN  'API'     ALSO  'CLOSE'                                    
036600           PERFORM C3-IMSP2P-CLOSE                                        
036700                                                                          
036800         WHEN  'PRINT'  ALSO  'OPEN'                                      
036900           PERFORM D1-PRINT-OPEN                                          
037000         WHEN  'PRINT'  ALSO  'PUT'                                       
037100           PERFORM D2-PRINT-PUT-DATA                                      
037200         WHEN  'PRINT'  ALSO  'CLOSE'                                     
037300           PERFORM D3-PRINT-CLOSE                                         
037400                                                                          
037500         WHEN  'MAIL'  ALSO  'OPEN'                                       
037600           PERFORM E1-MAIL-OPEN                                           
037700         WHEN  'MAIL'  ALSO  'PUT'                                        
037800           PERFORM E2-MAIL-PUT-DATA                                       
037900         WHEN  'MAIL'  ALSO  'CLOSE'                                      
038000           PERFORM E3-MAIL-CLOSE                                          
038100                                                                          
038200         WHEN  'MQ'    ALSO  'OPEN'                                       
038300           PERFORM F1-MQ-OPEN                                             
038400         WHEN  'MQ'    ALSO  'PUT'                                        
038500           PERFORM F2-MQ-PUT-DATA                                         
038600         WHEN  'MQ'    ALSO  'CLOSE'                                      
038700           PERFORM F3-MQ-CLOSE                                            
038800         WHEN  'MQ'    ALSO  'CLOSEQ'                                     
038900           PERFORM F4-MQ-CLOSEQ                                           
039000         WHEN  'MQ'    ALSO  'DISCONNECT'                                 
039100           PERFORM F5-MQ-DISCONNECT                                       
039200                                                                          
039300         WHEN OTHER                                                       
039400           MOVE 'INVALID COMBINATION IN EVALUATE'                         
039500                TO ERROR-TEXT                                             
039600           IF SEND-KDFUNC NOT = 'OPEN' AND 'PUT' AND 'CLOSE'              
039700             STRING 'INVALID FUNCTION CODE ' SEND-KDFUNC                  
039800             DELIMITED BY SIZE INTO ERROR-TEXT                            
039900           END-IF                                                         
040000           IF IDCOM-KDCOMTYPE(IDCOM-IX) NOT =                             
040100           'VCOM' AND 'IMSP2P' AND 'PRINT' AND 'MAIL' AND 'MQ'            
040200             AND 'API'                                                    
040300             STRING 'INVALID COMTYPE IN ATAB '                            
040400                     IDCOM-KDCOMTYPE(IDCOM-IX)                            
040500             DELIMITED BY SIZE INTO ERROR-TEXT                            
040600           END-IF                                                         
040700           CALL ABEND  USING RKOD-ABEND-NO-DUMP                           
040800                                                                          
040900       END-EVALUATE                                                       
041000     END-IF                                                               
041100                                                                          
041200     IF SEND-KDRC = 0                                                     
041300       MOVE ZERO TO RETURN-CODE                                           
041400     ELSE                                                                 
041500       MOVE 12 TO RETURN-CODE                                             
041600     END-IF                                                               
041700                                                                          
041800     GOBACK                                                               
041900     .                                                                    
042000                                                                          
042100  A-INIT SECTION.                                                         
042200                                                                          
042300     SET ALL-OK TO TRUE                                                   
042400                                                                          
042500* -- ARGUMENTS DIFFERS DEPENDING ON CALL TYPE                             
042600* --   OPEN:  1) CONTROL-AREA   2) OPEN-AREA                              
042700* --   SEND:  1) CONTROL-AREA   2) KVDLEN     3) DATA                     
042800* --   CLOSE: 1) CONTROL-AREA                                             
042900* -- FIX SECOND ARGUMENT ON OPEN CALL:                                    
043000                                                                          
043100     IF SEND-KDFUNC = 'OPEN'                                              
043200       SET ADDRESS OF SEND-OPEN-AREA TO ADDRESS OF SEND-KVDLEN            
043300       IF SEND-ADDISPABS-RETURN NOT = SPACE AND LOW-VALUE                 
043400*        -- INFO ABOUT RETURN ADDRESS SHOULD BE SENT                      
043500*        -- TOGETHER WITH FIRST RECORD.                                   
043600         SET INCLUDE-RETURN-ADDRESS TO TRUE                               
043700       END-IF                                                             
043800       PERFORM AA-SEARCH-ATAB                                             
043900       CALL VIMSID USING W-VIMSID                                         
044000     END-IF                                                               
044100                                                                          
044200     IF ALL-OK                                                            
044300       PERFORM AB-LOOK-UP-IN-IDCOM-TAB                                    
044400     END-IF                                                               
044500     .                                                                    
044600                                                                          
044700 AA-SEARCH-ATAB SECTION.                                                  
044800                                                                          
044900     MOVE FUNCTION UPPER-CASE(SEND-ADDISPABS)                             
045000                                 TO ATAB-ADDISPABS                        
045100     CALL WZ01ATAB USING ATAB-WZ01ATAB                                    
045200                                                                          
045300     IF RETURN-CODE > ZERO                                                
045400*      -- ADDRESS NOT FOUND IN ATAB                                       
045500       MOVE 10 TO SEND-KDRC                                               
045600       SET SOME-ERROR TO TRUE                                             
045700     END-IF                                                               
045800     .                                                                    
045900                                                                          
046000 AB-LOOK-UP-IN-IDCOM-TAB  SECTION.                                        
046100                                                                          
046200     IF SEND-KDFUNC = 'OPEN'                                              
046300*      -- SEARCH FOR A FREE ENTRY IN IDCOM-TABLE                          
046400       MOVE 1 TO IDCOM-IX                                                 
046500       PERFORM UNTIL IDCOM-IX > IDCOM-TABLE-LENGTH                        
046600               OR IDCOM-OPEN(IDCOM-IX) NOT = YES                          
046700          ADD 1 TO IDCOM-IX                                               
046800       END-PERFORM                                                        
046900       IF IDCOM-IX > IDCOM-IX-MAX-USED                                    
047000         MOVE IDCOM-IX TO IDCOM-IX-MAX-USED                               
047100       END-IF                                                             
047200       IF IDCOM-IX-MAX-USED > IDCOM-TABLE-LENGTH                          
047300         MOVE  'IDCOM TABLE OVERFLOW'                                     
047400           TO ERROR-TEXT                                                  
047500         DISPLAY IDPGM ' ' ERROR-TEXT                                     
047600         CALL ABEND  USING RKOD-ABEND-NO-DUMP                             
047700       ELSE                                                               
047800         MOVE IDCOM-IX-MAX-USED  TO IDCOM-IX                              
047900         MOVE SEND-OPEN-AREA     TO IDCOM-SEND-OPEN-AREA(IDCOM-IX)        
048000         MOVE ATAB-KDCOMTYPE     TO IDCOM-KDCOMTYPE(IDCOM-IX)             
048100         MOVE YES                TO IDCOM-OPEN(IDCOM-IX)                  
048200         IF SEND-FLRESTART = YES                                          
048300           MOVE SEND-FLRESTART   TO IDCOM-FLRESTART (IDCOM-IX)            
048400           MOVE YES              TO IDCOM-SEND-RESTART-SEGM               
048500                                                    (IDCOM-IX)            
048600           MOVE SEND-ADDISPABS-RESTART                                    
048700                                 TO IDCOM-ADDISPABS (IDCOM-IX)            
048800           MOVE SEND-TIDATETIME-DB2                                       
048900                                 TO IDCOM-TIDATETIME-DB2                  
049000                                                    (IDCOM-IX)            
049100           MOVE 1                TO IDCOM-IDLOPNR   (IDCOM-IX)            
049200         ELSE                                                             
049300           IF ATAB-KVDAGAR-RESEND > SPACE                                 
049400             PERFORM AB1-CREATE-RESEND-KEY                                
049500             MOVE YES            TO IDCOM-SEND-RESTART-SEGM               
049600                                                    (IDCOM-IX)            
049700             MOVE WS-TIMESTAMP-DB2                                        
049800                                 TO IDCOM-TIDATETIME-DB2                  
049900                                                    (IDCOM-IX)            
050000             MOVE DIDA-IDLOPNR   TO IDCOM-IDLOPNR (IDCOM-IX)              
050100           ELSE                                                           
050200             MOVE NOO   TO IDCOM-SEND-RESTART-SEGM(IDCOM-IX)              
050300             MOVE SPACES         TO IDCOM-TIDATETIME-DB2(IDCOM-IX)        
050400             MOVE ZERO           TO IDCOM-IDLOPNR (IDCOM-IX)              
050500           END-IF                                                         
050600         END-IF                                                           
050700*        -- USE INDEX AS ID FOR THIS CALL                                 
050800         MOVE IDCOM-IX           TO SEND-IDCOM                            
050900       END-IF                                                             
051000     ELSE                                                                 
051100*      -- TAKE IDCOM INDEX FROM ARGUMENT                                  
051200       MOVE SEND-IDCOM TO IDCOM-IX                                        
051300       IF IDCOM-IX > IDCOM-IX-MAX-USED OR < 1                             
051400       OR IDCOM-OPEN(IDCOM-IX) = NOO                                      
051500         MOVE 11 TO SEND-KDRC                                             
051600         SET SOME-ERROR TO TRUE                                           
051700       END-IF                                                             
051800     END-IF                                                               
051900     .                                                                    
052000                                                                          
052100 AB1-CREATE-RESEND-KEY  SECTION.                                          
052200                                                                          
052300     INITIALIZE GOOD-SQLCODECODES                                         
052400     INITIALIZE DIDA-TZ1DIDA                                              
052500                                                                          
052600*      -- INITIATE KEYS FOR TZ1DIDA-TABEL FIRST RECORD                    
052700     PERFORM DB2-SELECT-SYSDUMMY1                                         
052800     MOVE IDCOM-SEND-ADDISPABS (IDCOM-IX)                                 
052900                                 TO DIDA-ADDISPABS                        
053000                                    IDCOM-ADDISPABS (IDCOM-IX)            
053100     MOVE 1                      TO DIDA-IDLOPNR                          
053200                                                                          
053300     PERFORM DB2-SELECT-TZ1DIDA-TAB                                       
053400     PERFORM UNTIL LINES-MISSING                                          
053500       PERFORM DB2-SELECT-SYSDUMMY1                                       
053600       PERFORM DB2-SELECT-TZ1DIDA-TAB                                     
053700     END-PERFORM                                                          
053800     .                                                                    
053900                                                                          
054000 B1-VCOM-CONNECT SECTION.                                                 
054100                                                                          
054200     MOVE ZERO TO TALLY                                                   
054300     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
054400             FOR CHARACTERS BEFORE INITIAL 'PAR:'                         
054500     MOVE SPACE TO VCOM-PARTNER                                           
054600     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
054700       UNSTRING ATAB-ADDISPINT(TALLY + 5:)  DELIMITED BY ';'              
054800       INTO VCOM-PARTNER                                                  
054900     END-IF                                                               
055000                                                                          
055100*    -- SENDER TAG ALSO USED IN MAIL SENDING (FOR TEST PURPOSE)           
055200     PERFORM S01-EXTRACT-SENDER-TAG                                       
055300                                                                          
055400     MOVE ZERO TO TALLY                                                   
055500     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
055600             FOR CHARACTERS BEFORE INITIAL 'INIT:'                        
055700     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
055800*      -- USE SPECIFIED INITIATOR                                         
055900       MOVE SPACE TO VCOM-INITIATOR                                       
056000       UNSTRING ATAB-ADDISPINT(TALLY + 6:)  DELIMITED BY ';'              
056100       INTO VCOM-INITIATOR                                                
056200     ELSE                                                                 
056300*      --NO INITIATOR SPECIFIED, USE INIT41 (SW EBCDIC) AS DEFAULT        
056400       MOVE 'INIT41  '   TO VCOM-INITIATOR                                
056500     END-IF                                                               
056600                                                                          
056700     MOVE SPACE          TO VCOM-RECEIPT-PARTNER                          
056800     MOVE ZERO           TO VCOM-RECEIPT-RC                               
056900     MOVE ZERO           TO VCOM-RECEIPT-LENGTH                           
057000     MOVE SPACE          TO VCOM-RECEIPT-DATA                             
057100     MOVE 'N'            TO VCOM-PRIO                                     
057200                                                                          
057300     CALL DSCONS USING VCOM-RC                                            
057400                       VCOM-DISTID                                        
057500                       VCOM-SECUR                                         
057600                       VCOM-TIMEOUT                                       
057700                       VCOM-SENDERTAG                                     
057800                       VCOM-PARTNER                                       
057900                       VCOM-RECEIPT                                       
058000                       VCOM-PRIO                                          
058100                       VCOM-INITIATOR                                     
058200                                                                          
058300     IF VCOM-RC NOT = ZERO                                                
058400       MOVE VCOM-RC TO RC-DISPLAY                                         
058500       STRING 'RC FROM DSCONS ' RC-DISPLAY                                
058600              ' ' VCOM-DISTID                                             
058700              ' ' VCOM-PARTNER                                            
058800              ' ' VCOM-SENDERTAG                                          
058900          DELIMITED BY SIZE                                               
059000          INTO ERROR-TEXT                                                 
059100       DISPLAY IDPGM ' ' ERROR-TEXT                                       
059200       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
059300     END-IF                                                               
059400                                                                          
059500     MOVE VCOM-DISTID TO IDCOM-VCOM-DISTID(IDCOM-IX)                      
059600     .                                                                    
059700                                                                          
059800 B2-VCOM-SEND    SECTION.                                                 
059900                                                                          
060000*    -- IF THE DATA IS LONGER THAN CA 10000 BYTES, IT IS SPLIT            
060100*    -- INTO 10K SEGMENTS. THE FIRST IS SENT AS-IS, BUT THE               
060200*    -- FOLLOWING SEGMENTS ARE PREFIXED BY A "CONTINUATION"               
060300*    -- PREFIX - '*CONTINU' SO THE RECEIVER CAN IDENTIFY THEM.            
060400*    -- IF THE SENDER SPECIFIED A RETURN ADRESS IN THE OPEN CALL,         
060500*    -- AN EXTRA SEGMENT CONTAINING THIS INFO IS INSERTED                 
060600*    -- AFTER THE LAST SEGMENT OF THE FIRST DATA-RECORD.                  
060700                                                                          
060800     MOVE IDCOM-VCOM-DISTID(IDCOM-IX) TO VCOM-DISTID                      
060900                                                                          
061000     MOVE 1 TO WSTART                                                     
061100     MOVE MAX-VCOM-DATA-LENGTH TO WLEN                                    
061200     MOVE SPACE         TO WSEGMPREFIX                                    
061300     PERFORM UNTIL WSTART > SEND-KVDLEN                                   
061400                                                                          
061500*      -- DON'T MOVE MORE THAN WHAT REMAINS OF THE DATA                   
061600       IF WSTART + WLEN > SEND-KVDLEN + 1                                 
061700         COMPUTE WLEN = SEND-KVDLEN - WSTART + 1                          
061800       END-IF                                                             
061900                                                                          
062000       MOVE SEND-DATA(WSTART:WLEN) TO WDATA                               
062100       PERFORM B2A-SEND-SEGMENT                                           
062200                                                                          
062300       MOVE CONTINUE-PREFIX TO WSEGMPREFIX                                
062400       ADD WLEN TO  WSTART                                                
062500     END-PERFORM                                                          
062600                                                                          
062700     IF IDCOM-SEND-ADDISPABS-RETURN(IDCOM-IX) > SPACE                     
062800       MOVE RETADDRESS-PREFIX TO WSEGMPREFIX                              
062900       MOVE IDCOM-SEND-ADDISPABS-RETURN(IDCOM-IX)                         
063000                              TO WDATA                                    
063100       MOVE 1 TO WSTART                                                   
063200       MOVE LENGTH OF SEND-ADDISPABS-RETURN TO WLEN                       
063300       PERFORM B2A-SEND-SEGMENT                                           
063400       MOVE SPACE TO IDCOM-SEND-ADDISPABS-RETURN(IDCOM-IX)                
063500     END-IF                                                               
063600                                                                          
063700*    -- ALSO SAVE SENT DATA IN THE DISPATCHER RESTART TABLE               
063800*    -- IF THIS HAS BEEN SPECIFIED.                                       
063900     IF IDCOM-TIDATETIME-DB2(IDCOM-IX) > SPACES                           
064000       IF IDCOM-FLRESTART (IDCOM-IX) = YES                                
064100         CONTINUE                                                         
064200       ELSE                                                               
064300         MOVE IDCOM-TIDATETIME-DB2(IDCOM-IX)                              
064400                                 TO DIDA-TIDATETIME-DB2                   
064500         MOVE IDCOM-IDLOPNR (IDCOM-IX)                                    
064600                                 TO DIDA-IDLOPNR                          
064700         MOVE IDCOM-SEND-ADDISPABS(IDCOM-IX)                              
064800                                 TO DIDA-ADDISPABS                        
064900         MOVE SEND-KVDLEN        TO WS-CLOB-LENGTH                        
065000         MOVE SEND-DATA(1:SEND-KVDLEN)                                    
065100                                 TO WS-CLOB-DATA (1:SEND-KVDLEN)          
065200         PERFORM DB2-INSERT-TZ1DIDA-TAB                                   
065300         ADD +1                  TO IDCOM-IDLOPNR (IDCOM-IX)              
065400       END-IF                                                             
065500     END-IF                                                               
065600     .                                                                    
065700                                                                          
065800 B2A-SEND-SEGMENT  SECTION.                                               
065900                                                                          
066000     MOVE SPACE TO VCOM-DATA                                              
066100     STRING                                                               
066200       WSEGMPREFIX  DELIMITED BY SPACE                                    
066300       WDATA        DELIMITED BY SIZE                                     
066400      INTO VCOM-DATA                                                      
066500                                                                          
066600     IF WSEGMPREFIX = SPACE                                               
066700       MOVE WLEN TO VCOM-ACTLENGTH                                        
066800     ELSE                                                                 
066900       COMPUTE VCOM-ACTLENGTH = WLEN + 8                                  
067000     END-IF                                                               
067100                                                                          
067200     CALL DSSEND USING VCOM-RC                                            
067300                       VCOM-DISTID                                        
067400                       VCOM-ACTLENGTH                                     
067500                       VCOM-DATA                                          
067600                                                                          
067700     IF VCOM-RC NOT = ZERO                                                
067800       MOVE VCOM-RC TO RC-DISPLAY                                         
067900       STRING 'RC FROM DSSEND ' RC-DISPLAY                                
068000              ' ' VCOM-DISTID                                             
068100              ' ' VCOM-PARTNER                                            
068200              ' ' VCOM-SENDERTAG                                          
068300          DELIMITED BY SIZE                                               
068400          INTO ERROR-TEXT                                                 
068500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
068600       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
068700     END-IF                                                               
068800                                                                          
068900     .                                                                    
069000                                                                          
069100 B3-VCOM-RELEASE SECTION.                                                 
069200                                                                          
069300     MOVE IDCOM-VCOM-DISTID(IDCOM-IX) TO VCOM-DISTID                      
069400     MOVE ZERO                        TO VCOM-RC                          
069500     MOVE +1                          TO VCOM-RVALUE                      
069600                                                                          
069700     CALL DSRLSE USING VCOM-RC                                            
069800                       VCOM-DISTID                                        
069900                       VCOM-RVALUE                                        
070000                                                                          
070100     IF VCOM-RC NOT = ZERO                                                
070200       MOVE VCOM-RC TO RC-DISPLAY                                         
070300       STRING 'RC FROM DSRLSE ' RC-DISPLAY                                
070400              ' ' VCOM-DISTID                                             
070500              ' ' VCOM-PARTNER                                            
070600              ' ' VCOM-SENDERTAG                                          
070700          DELIMITED BY SIZE                                               
070800          INTO ERROR-TEXT                                                 
070900       DISPLAY IDPGM ' ' ERROR-TEXT                                       
071000       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
071100     END-IF                                                               
071200                                                                          
071300     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
071400     .                                                                    
071500                                                                          
071600     EJECT                                                                
071700 C1-IMSP2P-INIT  SECTION.                                                 
071800                                                                          
071900*    -- SAVE TRANSACTION CODE FOR FUTURE INSERTS                          
072000     MOVE ZERO TO TALLY                                                   
072100     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
072200             FOR CHARACTERS BEFORE INITIAL 'TRAN:'                        
072300     MOVE SPACE TO IDCOM-KDTRANS(IDCOM-IX)                                
072400     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
072500       UNSTRING ATAB-ADDISPINT(TALLY + 6:)  DELIMITED BY ';'              
072600       INTO IDCOM-KDTRANS(IDCOM-IX)                                       
072700     END-IF                                                               
072800                                                                          
072900*    -- SAVE SOP PROCESS TO BE ORDERED IF IT HAS BEEN SPECIFIED           
073000     MOVE SPACE          TO IDCOM-IDPROCESS(IDCOM-IX)                     
073100     MOVE ZERO TO TALLY                                                   
073200     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
073300             FOR CHARACTERS BEFORE INITIAL 'SOP:'                         
073400     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
073500       UNSTRING ATAB-ADDISPINT(TALLY + 5:)  DELIMITED BY ';'              
073600       INTO IDCOM-IDPROCESS(IDCOM-IX)                                     
073700     END-IF                                                               
073800                                                                          
073900*    -- SAVE SOP ACTION IF IT HAS BEEN SPECIFIED                          
074000     MOVE SPACE          TO IDCOM-KDSOPFUNK(IDCOM-IX)                     
074100     MOVE ZERO TO TALLY                                                   
074200     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
074300             FOR CHARACTERS BEFORE INITIAL 'FUNC:'                        
074400     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
074500       UNSTRING ATAB-ADDISPINT(TALLY + 6:)  DELIMITED BY ';'              
074600       INTO IDCOM-KDSOPFUNK(IDCOM-IX)                                     
074700     ELSE                                                                 
074800       MOVE 'O' TO IDCOM-KDSOPFUNK(IDCOM-IX)                              
074900     END-IF                                                               
075000                                                                          
075100*    -- SAVE FLAG ABOUT BLOCKING IMS DATA                                 
075200     MOVE YES            TO IDCOM-FLBLOCK(IDCOM-IX)                       
075300     MOVE ZERO TO TALLY                                                   
075400     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
075500             FOR CHARACTERS BEFORE INITIAL 'BLOCK:NO'                     
075600     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
075700       MOVE NOO TO IDCOM-FLBLOCK(IDCOM-IX)                                
075800     END-IF                                                               
075900                                                                          
076000*    -- EXTRACT LAST PART OF ABSTRACT ADDRESS TO USE AS PCB NAME          
076100     MOVE SPACE TO IDCOM-PCBNAME(IDCOM-IX)                                
076200     UNSTRING ATAB-ADDISPABS DELIMITED BY '.'                             
076300       INTO DUMMY DUMMY IDCOM-PCBNAME(IDCOM-IX)                           
076400                                                                          
076500                                                                          
076600*    -- NO DATA IN BUFFER YET                                             
076700     MOVE ZERO TO IDCOM-CURRENT-LEN(IDCOM-IX)                             
076800     .                                                                    
076900                                                                          
077000     EJECT                                                                
077100 C2-IMSP2P-INSERT-DATA SECTION.                                           
077200                                                                          
077300*    -- If RESTART segment is being used, send only that in               
077400*    -- the IMS message. The actual data will be saved in                 
077500*    -- dispatcher table and WZ01RECV will fetch from it later.           
077600     IF IDCOM-TIDATETIME-DB2(IDCOM-IX) > SPACES                           
077700       CONTINUE                                                           
077800     ELSE                                                                 
077900       PERFORM C2A-SEND-DATA-TO-IMS                                       
078000     END-IF                                                               
078100                                                                          
078200*    -- ALSO SEND INFO ABOUT KEY TO RESTART TABLE IF POSSIBILITY          
078300*    -- TO RESEND HAS BEEN SPECIFIED, SO WZ01RECV CAN USE IT.             
078400*    -- THIS INFO IS ONLY SENT ON FIRST PUT.                              
078500     IF IDCOM-SEND-RESTART-SEGM(IDCOM-IX) = YES                           
078600       MOVE RESTARTKEY-PREFIX             TO WSEGMPREFIX                  
078700       MOVE IDCOM-RESTART-KEYS (IDCOM-IX) TO WDATA                        
078800       MOVE LENGTH OF IDCOM-RESTART-KEYS  TO WLEN                         
078900       PERFORM C2AA-BUFFER-SEGMENT-DATA                                   
079000       MOVE NOO  TO IDCOM-SEND-RESTART-SEGM(IDCOM-IX)                     
079100     END-IF                                                               
079200                                                                          
079300*    -- ALSO SEND INFO ABOUT RETURN ADDRESS IF SUCH HAS BEEN              
079400*    -- SPECIFIED.  THIS INFO IS ONLY SENT ON FIRST PUT.                  
079500     IF IDCOM-SEND-ADDISPABS-RETURN(IDCOM-IX) > SPACE                     
079600       MOVE RETADDRESS-PREFIX               TO WSEGMPREFIX                
079700       MOVE IDCOM-SEND-ADDISPABS-RETURN(IDCOM-IX)                         
079800                                            TO WDATA                      
079900       MOVE LENGTH OF SEND-ADDISPABS-RETURN TO WLEN                       
080000       PERFORM C2AA-BUFFER-SEGMENT-DATA                                   
080100       MOVE SPACE TO IDCOM-SEND-ADDISPABS-RETURN(IDCOM-IX)                
080200     END-IF                                                               
080300                                                                          
080400*    -- ALSO SAVE SENT DATA IN THE DISPATCHER RESTART TABLE               
080500*    -- IF THIS HAS BEEN SPECIFIED.                                       
080600     IF IDCOM-TIDATETIME-DB2(IDCOM-IX) > SPACES                           
080700       IF IDCOM-FLRESTART (IDCOM-IX) = YES                                
080800         CONTINUE                                                         
080900       ELSE                                                               
081000         MOVE IDCOM-TIDATETIME-DB2(IDCOM-IX)                              
081100                                 TO DIDA-TIDATETIME-DB2                   
081200         MOVE IDCOM-IDLOPNR (IDCOM-IX)                                    
081300                                 TO DIDA-IDLOPNR                          
081400         MOVE IDCOM-SEND-ADDISPABS(IDCOM-IX)                              
081500                                 TO DIDA-ADDISPABS                        
081600         MOVE SEND-KVDLEN        TO WS-CLOB-LENGTH                        
081700         MOVE SEND-DATA(1:SEND-KVDLEN)                                    
081800                                 TO WS-CLOB-DATA (1:SEND-KVDLEN)          
081900         PERFORM DB2-INSERT-TZ1DIDA-TAB                                   
082000         ADD +1                  TO IDCOM-IDLOPNR (IDCOM-IX)              
082100       END-IF                                                             
082200     END-IF                                                               
082300     .                                                                    
082400                                                                          
082500 C2A-SEND-DATA-TO-IMS SECTION.                                            
082600                                                                          
082700*    -- IF THE DATA IS LONGER THAN CA 8000 BYTES, IT IS SPLIT             
082800*    -- INTO 8K SEGMENTS. THE FIRST IS SENT PREFIXED BY THE               
082900*    -- TRANSACTION CODE,  BUT THE FOLLOWING SEGMENTS ARE INSTEAD         
083000*    -- PREFIXED BY A "CONTINUATION PREFIX" - '*CONTINU',                 
083100*    -- SO THE RECEIVER CAN IDENTIFY THEM                                 
083200*    -- IF THE SENDER SPECIFIED A RETURN ADRESS IN THE OPEN CALL,         
083300*    -- AN EXTRA SEGMENT CONTAINING THIS INFO IS INSERTED                 
083400*    -- AFTER THE LAST SEGMENT OF THE FIRST DATA-RECORD.                  
083500                                                                          
083600                                                                          
083700     MOVE 1 TO WSTART                                                     
083800     MOVE IMSMSG-MAX-DATA-LENGTH  TO WLEN                                 
083900                                                                          
084000     MOVE IDCOM-KDTRANS(IDCOM-IX) TO WSEGMPREFIX                          
084100     PERFORM UNTIL WSTART > SEND-KVDLEN                                   
084200                                                                          
084300*      -- DON'T MOVE MORE THAN WHAT REMAINS OF THE DATA                   
084400       IF WSTART + WLEN > SEND-KVDLEN + 1                                 
084500         COMPUTE WLEN = SEND-KVDLEN - WSTART + 1                          
084600       END-IF                                                             
084700                                                                          
084800       MOVE SEND-DATA(WSTART:WLEN) TO WDATA                               
084900       PERFORM C2AA-BUFFER-SEGMENT-DATA                                   
085000                                                                          
085100       ADD WLEN TO  WSTART                                                
085200       MOVE CONTINUE-PREFIX TO WSEGMPREFIX                                
085300     END-PERFORM                                                          
085400     .                                                                    
085500                                                                          
085600 C2AA-BUFFER-SEGMENT-DATA SECTION.                                        
085700                                                                          
085800     IF IDCOM-CURRENT-LEN(IDCOM-IX) > 0 AND                               
085900        ((IDCOM-CURRENT-LEN(IDCOM-IX) + WLEN + 2 >                        
086000                                 IMSMSG-MAX-DATA-LENGTH) OR               
086100         IDCOM-FLBLOCK(IDCOM-IX) = NOO )                                  
086200*      -- THE BUFFER IS FULL, SEND IT AND START A NEW BUFFER              
086300       PERFORM C2AAA-ISRT-SAVE-SEGMENT                                    
086400       MOVE ZERO  TO IDCOM-CURRENT-LEN(IDCOM-IX)                          
086500     END-IF                                                               
086600                                                                          
086700*    -- ADD DATA TO THE BUFFER                                            
086800     MOVE WLEN TO WSEGLEN                                                 
086900     IF IDCOM-FLBLOCK (IDCOM-IX) = YES                                    
087000       MOVE IDCOM-CURRENT-LEN(IDCOM-IX) TO WSEGSTART                      
087100       ADD 1 TO WSEGSTART                                                 
087200       STRING                                                             
087300         WSEGMPREFIX                                                      
087400         WSEGLEN-X                                                        
087500         WDATA (1:WLEN)                                                   
087600           DELIMITED BY SIZE                                              
087700         INTO IDCOM-CURRENT-DATA(IDCOM-IX)                                
087800         WITH POINTER WSEGSTART                                           
087900     ELSE                                                                 
088000       MOVE 1 TO WSEGSTART                                                
088100       STRING                                                             
088200         WSEGMPREFIX                                                      
088300         WDATA (1:WLEN)                                                   
088400           DELIMITED BY SIZE                                              
088500         INTO IDCOM-CURRENT-DATA(IDCOM-IX)                                
088600         WITH POINTER WSEGSTART                                           
088700     END-IF                                                               
088800     COMPUTE IDCOM-CURRENT-LEN(IDCOM-IX) = WSEGSTART - 1                  
088900     .                                                                    
089000     EJECT                                                                
089100                                                                          
089200 C2AAA-ISRT-SAVE-SEGMENT SECTION.                                         
089300                                                                          
089400     MOVE IDCOM-CURRENT-DATA(IDCOM-IX)                                    
089500          (1 : IDCOM-CURRENT-LEN (IDCOM-IX))                              
089600                                     TO IMSMSG-IO-DATA                    
089700                                                                          
089800*    -- LENGTH OF WHOLE MESSAGE (RDW+DATA)                                
089900     ADD IDCOM-CURRENT-LEN(IDCOM-IX) 4  GIVING IMSMSG-KVLL                
090000     MOVE LOW-VALUE                     TO     IMSMSG-KDZZ                
090100     PERFORM IMS-INSERT-MESSAGE                                           
090200                                                                          
090300     .                                                                    
090400                                                                          
090500 C3-IMSP2P-CLOSE          SECTION.                                        
090600                                                                          
090700*    -- SEND THE DATA STORED IN THE BUFFER                                
090800     IF IDCOM-CURRENT-LEN(IDCOM-IX) > 0                                   
090900       PERFORM C2AAA-ISRT-SAVE-SEGMENT                                    
091000     END-IF                                                               
091100                                                                          
091200*    -- CLOSE THE MESSAGE                                                 
091300     PERFORM IMS-PURGE                                                    
091400                                                                          
091500*    -- IF THE RECEIVER IS A BMP, IT MUST BE TRIGGERED VIA SOP.           
091600*    -- THIS IS DONE BY SENDING AN ORDER-MESSAGE TO W0T606U               
091700     IF IDCOM-IDPROCESS(IDCOM-IX) NOT = SPACE                             
091800                                                                          
091900       MOVE IDCOM-IDPROCESS(IDCOM-IX) TO MSGSOP-IDPROCESS                 
092000       MOVE IDCOM-KDSOPFUNK(IDCOM-IX) TO MSGSOP-KDSOPFUNK                 
092100                                                                          
092200       PERFORM IMS-INSERT-PURGE-SOP-ORDER                                 
092300     END-IF                                                               
092400                                                                          
092500     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
092600     .                                                                    
092700     EJECT                                                                
092800 D1-PRINT-OPEN SECTION.                                                   
092900                                                                          
093000*    -- INITIALIZE ADDL INFO FIELDS. THESE ARE NOT USED                   
093100*    -- AS RESTART IS NOT AVAILABLE HERE.                                 
093200     MOVE SPACES                    TO OUTP-IDOUTTYPE                     
093300     MOVE SPACES                    TO OUTP-IDOUTREC                      
093400     MOVE SPACES                    TO OUTP-IDLIST                        
093500     MOVE ZEROES                    TO OUTP-TIREGDAT                      
093600     MOVE ZEROES                    TO OUTP-TIKLOCK                       
093700                                                                          
093800*    -- USE SAME IDCALL NUMBER FOR OUTP:                                  
093900     MOVE IDCOM-IX  TO OUTP-IDCALL                                        
094000                                                                          
094100     MOVE 'OPEN'    TO OUTP-KDFUNC                                        
094200                                                                          
094300*    -- EXTRACT LAST PART OF ABSTRACT ADDRESS TO USE AS PCB NAME          
094400     MOVE SPACE TO IDCOM-PCBNAME(IDCOM-IX)                                
094500     UNSTRING ATAB-ADDISPABS DELIMITED BY '.'                             
094600       INTO DUMMY DUMMY IDCOM-PCBNAME(IDCOM-IX)                           
094700     MOVE IDCOM-PCBNAME(IDCOM-IX) TO OUTP-IDPCB                           
094800                                                                          
094900*    -- EXTRACT PRINTER NAME                                              
095000     MOVE ZERO TO TALLY                                                   
095100     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
095200             FOR CHARACTERS BEFORE INITIAL 'PNAME:'                       
095300     MOVE SPACE TO OUTP-IDOUTDEST                                         
095400     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
095500       UNSTRING ATAB-ADDISPINT(TALLY + 7:)  DELIMITED BY ';'              
095600       INTO OUTP-IDOUTDEST                                                
095700     END-IF                                                               
095800                                                                          
095900*    -- EXTRACT NUMBER OF COPIES                                          
096000     MOVE ZERO TO TALLY                                                   
096100     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
096200             FOR CHARACTERS BEFORE INITIAL 'COPIES:'                      
096300     MOVE SPACE TO OUTP-KVCOPIES                                          
096400     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
096500       UNSTRING ATAB-ADDISPINT(TALLY + 8:)  DELIMITED BY ';'              
096600       INTO OUTP-KVCOPIES                                                 
096700     END-IF                                                               
096800                                                                          
096900*    -- EXTRACT PAGEDEF/FORMDEF                                           
097000     MOVE ZERO TO TALLY                                                   
097100     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
097200             FOR CHARACTERS BEFORE INITIAL 'PFDEF:'                       
097300     MOVE SPACE TO OUTP-IDPFDEF                                           
097400     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
097500       UNSTRING ATAB-ADDISPINT(TALLY + 7:)  DELIMITED BY ';'              
097600       INTO OUTP-IDPFDEF                                                  
097700     END-IF                                                               
097800                                                                          
097900*    -- EXTRACT FORMS NAME                                                
098000     MOVE ZERO TO TALLY                                                   
098100     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
098200             FOR CHARACTERS BEFORE INITIAL 'FORMS:'                       
098300     MOVE SPACE TO OUTP-IDFORMSNM                                         
098400     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
098500       UNSTRING ATAB-ADDISPINT(TALLY + 7:)  DELIMITED BY ';'              
098600       INTO OUTP-IDFORMSNM                                                
098700     END-IF                                                               
098800                                                                          
098900*    -- EXTRACT CARRIAGE CONTROL FLAG                                     
099000     MOVE ZERO TO TALLY                                                   
099100     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
099200             FOR CHARACTERS BEFORE INITIAL 'CC:'                          
099300     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
099400       MOVE ATAB-ADDISPINT(TALLY + 4: 1)  TO OUTP-FLCARRCNTL              
099500     ELSE                                                                 
099600       MOVE NOO                           TO OUTP-FLCARRCNTL              
099700     END-IF                                                               
099800                                                                          
099900     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
100000     IF OUTP-KDRC > ZERO                                                  
100100       MOVE OUTP-KDRC TO RC-DISPLAY                                       
100200       STRING 'RC FROM PRINT OPEN ' RC-DISPLAY                            
100300              ' ' OUTP-TEOUTDATA(1:50)                                    
100400          DELIMITED BY SIZE                                               
100500          INTO ERROR-TEXT                                                 
100600       DISPLAY IDPGM ' ' ERROR-TEXT                                       
100700       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 D2-PRINT-PUT-DATA SECTION.                                               
101200                                                                          
101300     MOVE IDCOM-IX                   TO OUTP-IDCALL                       
101400     MOVE 'PUT'                      TO OUTP-KDFUNC                       
101500     MOVE SEND-KVDLEN                TO OUTP-TEOUTDATA-L                  
101600     MOVE SEND-DATA                  TO OUTP-TEOUTDATA                    
101700     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
101800     IF OUTP-KDRC > ZERO                                                  
101900       MOVE OUTP-KDRC TO RC-DISPLAY                                       
102000       STRING 'RC FROM PRINT PUT ' RC-DISPLAY                             
102100              ' ' OUTP-TEOUTDATA(1:50)                                    
102200          DELIMITED BY SIZE                                               
102300          INTO ERROR-TEXT                                                 
102400       DISPLAY IDPGM ' ' ERROR-TEXT                                       
102500       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
102600     END-IF                                                               
102700     .                                                                    
102800     EJECT                                                                
102900 D3-PRINT-CLOSE SECTION.                                                  
103000                                                                          
103100     MOVE IDCOM-IX                   TO OUTP-IDCALL                       
103200     MOVE 'CLOSE'                    TO OUTP-KDFUNC                       
103300                                                                          
103400     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
103500     IF OUTP-KDRC > ZERO                                                  
103600       MOVE OUTP-KDRC TO RC-DISPLAY                                       
103700       STRING 'RC FROM PRINT CLOSE ' RC-DISPLAY                           
103800              ' ' OUTP-TEOUTDATA(1:50)                                    
103900          DELIMITED BY SIZE                                               
104000          INTO ERROR-TEXT                                                 
104100       DISPLAY IDPGM ' ' ERROR-TEXT                                       
104200       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
104300     END-IF                                                               
104400                                                                          
104500     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
104600     .                                                                    
104700                                                                          
104800     EJECT                                                                
104900 E1-MAIL-OPEN SECTION.                                                    
105000                                                                          
105100*    -- INITIALIZE ADDL INFO FIELDS. THESE ARE NOT USED                   
105200*    -- AS RESTART IS NOT AVAILABLE HERE.                                 
105300     MOVE SPACES                    TO OUTM-IDOUTTYPE                     
105400     MOVE SPACES                    TO OUTM-IDOUTREC                      
105500     MOVE SPACES                    TO OUTM-IDLIST                        
105600     MOVE ZEROES                    TO OUTM-TIREGDAT                      
105700     MOVE ZEROES                    TO OUTM-TIKLOCK                       
105800                                                                          
105900*    -- USE SAME IDCALL NUMBER FOR OUTM:                                  
106000     MOVE IDCOM-IX  TO OUTM-IDCALL                                        
106100                                                                          
106200     MOVE 'OPEN'    TO OUTM-KDFUNC                                        
106300                                                                          
106400*    -- EXTRACT RECEIVER MAIL ID                                          
106500     MOVE ZERO TO TALLY                                                   
106600     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
106700             FOR CHARACTERS BEFORE INITIAL 'TO:'                          
106800     MOVE SPACE TO OUTM-IDOUTDEST                                         
106900     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
107000       UNSTRING ATAB-ADDISPINT(TALLY + 4:)  DELIMITED BY ';'              
107100       INTO OUTM-IDOUTDEST                                                
107200     END-IF                                                               
107300                                                                          
107400*    -- EXTRACT PAGEDEF/FORMDEF                                           
107500     MOVE ZERO TO TALLY                                                   
107600     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
107700             FOR CHARACTERS BEFORE INITIAL 'PFDEF:'                       
107800     MOVE SPACE TO OUTM-IDPFDEF                                           
107900     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
108000       UNSTRING ATAB-ADDISPINT(TALLY + 7:)  DELIMITED BY ';'              
108100       INTO OUTM-IDPFDEF                                                  
108200     END-IF                                                               
108300                                                                          
108400*    -- EXTRACT CARRIAGE CONTROL FLAG                                     
108500     MOVE ZERO TO TALLY                                                   
108600     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
108700             FOR CHARACTERS BEFORE INITIAL 'CC:'                          
108800     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
108900       MOVE ATAB-ADDISPINT(TALLY + 4: 1)  TO OUTM-FLCARRCNTL              
109000     ELSE                                                                 
109100       MOVE NOO                           TO OUTM-FLCARRCNTL              
109200     END-IF                                                               
109300                                                                          
109400     MOVE SPACE TO OUTM-TEFAX (1)                                         
109500                   OUTM-TEFAX (2)                                         
109600                   OUTM-TEFAX (3)                                         
109700                   OUTM-TEFAX (4)                                         
109800                   OUTM-TEFAX (5)                                         
109900                                                                          
110000*    -- VCOM SENDER-TAG MAY ALSO BE USED IN MAIL SENDING                  
110100*    -- TO VERYFY IN TESTS THAT SENDER-TAG IS OK.                         
110200*    -- IT WILL BE SENT IN ADDITON TO REAL MESSAGE DATA                   
110300     PERFORM S01-EXTRACT-SENDER-TAG                                       
110400     IF VCOM-SENDERTAG NOT = SPACE                                        
110500       STRING 'Sender tag: ' DELIMITED BY SIZE                            
110600            VCOM-SENDERTAG   DELIMITED BY SIZE                            
110700       INTO OUTM-TEFAX (1)                                                
110800     END-IF                                                               
110900                                                                          
111000*    -- USE DEFAULT SENDER                                                
111100     MOVE SPACE TO OUTM-IDMAIL-SENDER                                     
111200                                                                          
111300     MOVE SPACE TO OUTM-IDMAILTTL                                         
111400     STRING 'From ' ATAB-ADDISPABS                                        
111500       DELIMITED BY SIZE                                                  
111600       INTO  OUTM-IDMAILTTL                                               
111700                                                                          
111800     CALL WZ11OUTM USING OUTM-WZ11OUT                                     
111900     IF OUTM-KDRC > ZERO                                                  
112000       MOVE OUTM-KDRC TO RC-DISPLAY                                       
112100       STRING 'RC FROM MAIL OPEN ' RC-DISPLAY                             
112200              ' ' OUTM-TEOUTDATA(1:50)                                    
112300          DELIMITED BY SIZE                                               
112400          INTO ERROR-TEXT                                                 
112500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
112600       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
112700     END-IF                                                               
112800     .                                                                    
112900     EJECT                                                                
113000 E2-MAIL-PUT-DATA SECTION.                                                
113100                                                                          
113200     MOVE IDCOM-IX                   TO OUTM-IDCALL                       
113300     MOVE 'PUT'                      TO OUTM-KDFUNC                       
113400     MOVE SEND-KVDLEN                TO OUTM-TEOUTDATA-L                  
113500     MOVE SEND-DATA                  TO OUTM-TEOUTDATA                    
113600     CALL WZ11OUTM USING OUTM-WZ11OUT                                     
113700     IF OUTM-KDRC > ZERO                                                  
113800       MOVE OUTM-KDRC TO RC-DISPLAY                                       
113900       STRING 'RC FROM MAIL PUT ' RC-DISPLAY                              
114000              ' ' OUTM-TEOUTDATA(1:50)                                    
114100          DELIMITED BY SIZE                                               
114200          INTO ERROR-TEXT                                                 
114300       DISPLAY IDPGM ' ' ERROR-TEXT                                       
114400       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
114500     END-IF                                                               
114600                                                                          
114700     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
114800     .                                                                    
114900                                                                          
115000     EJECT                                                                
115100 E3-MAIL-CLOSE SECTION.                                                   
115200                                                                          
115300     MOVE IDCOM-IX                   TO OUTM-IDCALL                       
115400     MOVE 'CLOSE'                    TO OUTM-KDFUNC                       
115500                                                                          
115600     CALL WZ11OUTM USING OUTM-WZ11OUT                                     
115700     IF OUTM-KDRC > ZERO                                                  
115800       MOVE OUTM-KDRC TO RC-DISPLAY                                       
115900       STRING 'RC FROM MAIL CLOSE ' RC-DISPLAY                            
116000              ' ' OUTM-TEOUTDATA(1:50)                                    
116100          DELIMITED BY SIZE                                               
116200          INTO ERROR-TEXT                                                 
116300       DISPLAY IDPGM ' ' ERROR-TEXT                                       
116400       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
116500     END-IF                                                               
116600                                                                          
116700     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
116800     .                                                                    
116900                                                                          
117000     EJECT                                                                
117100 F1-MQ-OPEN SECTION.                                                      
117200                                                                          
117300*    --- FETCH INFO ABOUT QMGR, QUEUE NAME AND META DATA                  
117400     PERFORM S02-EXTRACT-MQ-PARAMETERS                                    
117500                                                                          
117600                                                                          
117700     IF ENV-DEVE                                                          
117800       CONTINUE                                                           
117900     ELSE                                                                 
118000       CALL 'MQCONN' USING MQ-QMGR                                        
118100                           MQ-HCONN                                       
118200                           MQ-COMPCODE                                    
118300                           MQ-REASON                                      
118400     END-IF                                                               
118500                                                                          
118600     IF MQ-COMPCODE NOT = MQCC-OK                                         
118700     AND MQ-REASON NOT = MQRC-ALREADY-CONNECTED                           
118800       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
118900       MOVE MQ-REASON  TO REASON-DISPLAY                                  
119000       STRING 'RC FROM MQCONN ' RC-DISPLAY                                
119100              ' REASON: ' REASON-DISPLAY                                  
119200              ' ' MQ-QMGR                                                 
119300          DELIMITED BY SIZE                                               
119400          INTO ERROR-TEXT                                                 
119500       DISPLAY IDPGM ' ' ERROR-TEXT                                       
119600       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
119700     END-IF                                                               
119800                                                                          
119900     MOVE MQ-QNAME TO MQOD-OBJECTNAME                                     
120000                                                                          
120100     COMPUTE MQ-OPENOPTIONS = MQOO-FAIL-IF-QUIESCING +                    
120200                              MQOO-OUTPUT                                 
120300                                                                          
120400     IF ENV-DEVE                                                          
120500       CONTINUE                                                           
120600     ELSE                                                                 
120700       CALL 'MQOPEN' USING MQ-HCONN                                       
120800                           MQOD                                           
120900                           MQ-OPENOPTIONS                                 
121000                           MQ-HOBJ                                        
121100                           MQ-COMPCODE                                    
121200                           MQ-REASON                                      
121300     END-IF                                                               
121400                                                                          
121500     IF MQ-COMPCODE NOT = MQCC-OK                                         
121600       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
121700       MOVE MQ-REASON  TO REASON-DISPLAY                                  
121800       STRING 'RC FROM MQOPEN ' RC-DISPLAY                                
121900              ' REASON: ' REASON-DISPLAY                                  
122000              ' ' MQ-QNAME                                                
122100          DELIMITED BY SIZE                                               
122200          INTO ERROR-TEXT                                                 
122300       DISPLAY IDPGM ' ' ERROR-TEXT                                       
122400       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
122500     END-IF                                                               
122600                                                                          
122700     MOVE MQ-HCONN           TO IDCOM-MQ-HCONN(IDCOM-IX)                  
122800     MOVE MQ-HOBJ            TO IDCOM-MQ-HOBJ(IDCOM-IX)                   
122900     MOVE MQ-INTEGRATION-ID  TO IDCOM-MQ-INTEGRATION-ID(IDCOM-IX)         
123000     MOVE MQ-CONTRACT-ID     TO IDCOM-MQ-CONTRACT-ID(IDCOM-IX)            
123100     MOVE MQ-TOPIC-STRING    TO IDCOM-MQ-TOPIC-STRING (IDCOM-IX)          
123200     MOVE MQ-TYPE            TO IDCOM-MQ-TYPE (IDCOM-IX)                  
123300                                                                          
123400*    -- INITIATE A NEW FRESH MESSAGE                                      
123500     MOVE ZERO               TO IDCOM-CURRENT-LEN(IDCOM-IX)               
123600     MOVE SPACE              TO IDCOM-CURRENT-DATA(IDCOM-IX)              
123700     .                                                                    
123800                                                                          
123900     EJECT                                                                
124000 F2-MQ-PUT-DATA  SECTION.                                                 
124100                                                                          
124200*    -- CONCATENATE NEW MESSAGE LINE TO CURRENT MESSAGE                   
124300                                                                          
124400     MOVE IDCOM-CURRENT-LEN(IDCOM-IX)  TO WLEN                            
124500     MOVE IDCOM-CURRENT-DATA(IDCOM-IX) TO WDATA                           
124600     ADD 1 TO WLEN GIVING WSTART                                          
124700                                                                          
124800     IF WSTART > 1 AND < LENGTH OF WDATA - 1                              
124900*      -- INSERT CR/LF SEPARATOR AFTER PREVIOUS MSG LINE                  
125000       STRING CR LF-EBCDIC DELIMITED BY SIZE                              
125100         INTO WDATA WITH POINTER WSTART                                   
125200     END-IF                                                               
125300                                                                          
125400     IF WSTART + SEND-KVDLEN - 1 > LENGTH OF WDATA                        
125500*      WDATA OVERFLOW - MESSAGE TOO LONG                                  
125600       MOVE LENGTH OF WDATA TO  RC-DISPLAY                                
125700       STRING 'WZ01SEND - TOTAL MQ MESSAGE LENGTH TOO LONG. '             
125800              ' MAX ALLLOWED IS ' RC-DISPLAY                              
125900          DELIMITED BY SIZE                                               
126000          INTO ERROR-TEXT                                                 
126100       DISPLAY IDPGM ' ' ERROR-TEXT                                       
126200       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
126300     END-IF                                                               
126400                                                                          
126500     STRING SEND-DATA(1:SEND-KVDLEN) DELIMITED BY SIZE                    
126600       INTO WDATA WITH POINTER WSTART                                     
126700     SUBTRACT 1 FROM WSTART GIVING WLEN                                   
126800                                                                          
126900     MOVE WDATA TO IDCOM-CURRENT-DATA(IDCOM-IX)                           
127000     MOVE WLEN  TO IDCOM-CURRENT-LEN(IDCOM-IX)                            
127100     .                                                                    
127200                                                                          
127300     EJECT                                                                
127400 F3-MQ-CLOSE SECTION.                                                     
127500                                                                          
127600*    -- SEND THE ACCUMULATED MESSAGE DATA                                 
127700*    -- AND CLOSE THE QUEUE                                               
127800*    -- AND BREAK CONNECTION WITH THE QUEUE MANAGER                       
127900                                                                          
128000     IF ENV-DEVE                                                          
128100       CONTINUE                                                           
128200     ELSE                                                                 
128300       PERFORM S05-MQ-CLOSEQ                                              
128400       PERFORM T-TRACK-AND-TRACE                                          
128500       PERFORM S06-MQ-DISCONNECT                                          
128600     END-IF                                                               
128700                                                                          
128800     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
128900     .                                                                    
129000                                                                          
129100     EJECT                                                                
129200 F4-MQ-CLOSEQ SECTION.                                                    
129300                                                                          
129400     IF ENV-DEVE                                                          
129500       CONTINUE                                                           
129600     ELSE                                                                 
129700*      -- SEND THE ACCUMULATED MESSAGE DATA AND CLOSE THE MQ QUEUE        
129800       PERFORM S05-MQ-CLOSEQ                                              
129900     END-IF                                                               
130000     .                                                                    
130100                                                                          
130200     EJECT                                                                
130300 F5-MQ-DISCONNECT SECTION.                                                
130400                                                                          
130500     IF ENV-DEVE                                                          
130600       CONTINUE                                                           
130700     ELSE                                                                 
130800*      -- BREAK CONNECTION WITH THE QUEUE MANAGER                         
130900       PERFORM S06-MQ-DISCONNECT                                          
131000     END-IF                                                               
131100                                                                          
131200     MOVE NOO TO IDCOM-OPEN(IDCOM-IX)                                     
131300     .                                                                    
131400                                                                          
131500     EJECT                                                                
131600 S01-EXTRACT-SENDER-TAG SECTION.                                          
131700                                                                          
131800     MOVE SPACE TO VCOM-SENDERTAG                                         
131900     MOVE ZERO TO TALLY                                                   
132000     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
132100             FOR CHARACTERS BEFORE INITIAL 'STAG:'                        
132200     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
132300*      -- SENDER TAG SPECIFIED                                            
132400       UNSTRING ATAB-ADDISPINT(TALLY + 6:)  DELIMITED BY ';'              
132500       INTO VCOM-SENDERTAG                                                
132600*      -- BLANKS ARE REPRESENTED AS UNDERSCORE, CONVERT TO SPACE          
132700       INSPECT VCOM-SENDERTAG CONVERTING '_' TO SPACE                     
132800                                                                          
132900       MOVE ZERO TO TALLY                                                 
133000       INSPECT VCOM-SENDERTAG TALLYING TALLY                              
133100               FOR CHARACTERS BEFORE INITIAL '?'                          
133200       IF TALLY < LENGTH OF VCOM-SENDERTAG                                
133300*        -- SENDER TAG CONTAINS ? - REPLACE IT WITH DYNAMIC               
133400*        -- VALUE FROM "EXTRA" ADDRESS FIELD                              
133500         MOVE VCOM-SENDERTAG TO WORK-SENDERTAG                            
133600         IF SEND-ADDISPXTRA = LOW-VALUE                                   
133700           MOVE SPACE TO SEND-ADDISPXTRA                                  
133800         END-IF                                                           
133900         IF TALLY > ZERO                                                  
134000           STRING WORK-SENDERTAG(1:TALLY) DELIMITED BY SIZE               
134100                  SEND-ADDISPXTRA DELIMITED BY SPACE                      
134200                  WORK-SENDERTAG(TALLY + 2:) DELIMITED BY SIZE            
134300           INTO VCOM-SENDERTAG                                            
134400         ELSE                                                             
134500           STRING SEND-ADDISPXTRA DELIMITED BY SPACE                      
134600                  WORK-SENDERTAG(TALLY + 2:) DELIMITED BY SIZE            
134700           INTO VCOM-SENDERTAG                                            
134800         END-IF                                                           
134900       END-IF                                                             
135000     END-IF                                                               
135100     .                                                                    
135200                                                                          
135300     EJECT                                                                
135400 S02-EXTRACT-MQ-PARAMETERS SECTION.                                       
135500                                                                          
135600*    -- QUEUE MANAGER NAME                                                
135700     MOVE ZERO TO TALLY                                                   
135800     MOVE SPACE TO MQ-QMGR                                                
135900     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
136000             FOR CHARACTERS BEFORE INITIAL 'QM:'                          
136100     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
136200       UNSTRING ATAB-ADDISPINT(TALLY + 4:) DELIMITED BY ';'               
136300       INTO MQ-QMGR                                                       
136400     END-IF                                                               
136500                                                                          
136600     IF MQ-QMGR = SPACES                                                  
136700*    -- IF QMGR NAME IS NOT GIVEN IN ATAB,                                
136800*    -- DETERMINE FROM THE IMS SYSTEM                                     
136900       IF ENV-PROD                                                        
137000         MOVE 'MC0P'   TO MQ-QMGR                                         
137100       ELSE                                                               
137200         IF ENV-ACPT                                                      
137300           MOVE 'MC0Q' TO MQ-QMGR                                         
137400         ELSE                                                             
137500           MOVE 'MC0T' TO MQ-QMGR                                         
137600         END-IF                                                           
137700       END-IF                                                             
137800     END-IF                                                               
137900                                                                          
138000*    -- QUEUE NAME                                                        
138100     MOVE ZERO TO TALLY                                                   
138200     MOVE SPACE TO MQ-QNAME                                               
138300     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
138400             FOR CHARACTERS BEFORE INITIAL 'RQ:'                          
138500     UNSTRING ATAB-ADDISPINT(TALLY + 4:)                                  
138600     DELIMITED BY ';'                                                     
138700     INTO MQ-QNAME                                                        
138800                                                                          
138900*    -- META DATA: INTEGRATION ID                                         
139000     MOVE ZERO TO TALLY                                                   
139100     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
139200             FOR CHARACTERS BEFORE INITIAL 'I:'                           
139300     MOVE SPACE TO MQ-INTEGRATION-ID                                      
139400     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
139500       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
139600       INTO MQ-INTEGRATION-ID                                             
139700     END-IF                                                               
139800     MOVE MQ-INTEGRATION-ID TO TNT-INTEGRATION-ID                         
139900                                                                          
140000*    -- META DATA: CONTRACT ID                                            
140100     MOVE ZERO TO TALLY                                                   
140200     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
140300             FOR CHARACTERS BEFORE INITIAL 'C:'                           
140400     MOVE SPACE TO MQ-CONTRACT-ID                                         
140500     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
140600       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
140700       INTO MQ-CONTRACT-ID                                                
140800     END-IF                                                               
140900     MOVE MQ-CONTRACT-ID TO TNT-CONTRACT-ID                               
141000                                                                          
141100*    -- META DATA: TOPIC STRING                                           
141200     MOVE ZERO                   TO TALLY                                 
141300     MOVE SPACE                  TO MQ-TOPIC-STRING                       
141400     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
141500             FOR CHARACTERS BEFORE INITIAL 'T:'                           
141600     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
141700       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
141800                               INTO MQ-TOPIC-STRING                       
141900     END-IF                                                               
142000                                                                          
142100*    -- META DATA: TYPE                                                   
142200     MOVE ZERO                   TO TALLY                                 
142300     MOVE SPACE                  TO MQ-TYPE                               
142400     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
142500             FOR CHARACTERS BEFORE INITIAL 'TY:'                          
142600     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
142700       UNSTRING ATAB-ADDISPMETA(TALLY + 4:)  DELIMITED BY ';'             
142800                               INTO MQ-TYPE                               
142900     END-IF                                                               
143000                                                                          
143100     .                                                                    
143200                                                                          
143300     EJECT                                                                
143400 S05-MQ-CLOSEQ SECTION.                                                   
143500                                                                          
143600*    -- SEND THE ACCUMULATED MESSAGE DATA AND                             
143700*    -- CLOSE THE MQ QUEUE                                                
143800                                                                          
143900*    -- FIRST, CREATE A HANDLE FOR HEADER PROPERTIES                      
144000     MOVE IDCOM-MQ-HCONN (IDCOM-IX)   TO MQ-HCONN                         
144100     MOVE MQCMHO-VALIDATE             TO MQCMHO-OPTIONS                   
144200     CALL 'MQCRTMH' USING MQ-HCONN                                        
144300                       MQ-CREATE-HANDLE-OPTIONS                           
144400                       MQ-PROPHANDLE                                      
144500                       MQ-COMPCODE                                        
144600                       MQ-REASON                                          
144700                                                                          
144800     IF MQ-COMPCODE NOT = MQCC-OK                                         
144900       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
145000       MOVE MQ-REASON  TO REASON-DISPLAY                                  
145100       STRING 'RC FROM MQCRTMH' RC-DISPLAY                                
145200              ' REASON: ' REASON-DISPLAY                                  
145300              ' ' MQ-QNAME                                                
145400          DELIMITED BY SIZE                                               
145500          INTO ERROR-TEXT                                                 
145600       MOVE SPACE TO ERROR-TEXT                                           
145700       DISPLAY IDPGM ' ' ERROR-TEXT                                       
145800       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
145900     END-IF                                                               
146000                                                                          
146100*    -- THEN SET THE MANDATORY PROPERTIES IF THEY HAVE VALUES             
146200     MOVE IDCOM-MQ-INTEGRATION-ID(IDCOM-IX) TO MQ-INTEGRATION-ID          
146300     MOVE IDCOM-MQ-CONTRACT-ID(IDCOM-IX)    TO MQ-CONTRACT-ID             
146400     MOVE IDCOM-MQ-TOPIC-STRING (IDCOM-IX)  TO MQ-TOPIC-STRING            
146500     MOVE IDCOM-MQ-TYPE (IDCOM-IX)          TO MQ-TYPE                    
146600                                                                          
146700     IF MQ-INTEGRATION-ID NOT = SPACE                                     
146800       SET MQCHARV-VSPTR     TO ADDRESS OF MQ-PROPNAME-TO-SET             
146900       MOVE 'IntegrationId'  TO MQ-PROPNAME-TO-SET                        
147000       MOVE 13               TO MQCHARV-VSLENGTH                          
147100                                                                          
147200       MOVE SWEDISH-EBCDIC   TO MQCHARV-VSCCSID                           
147300       MOVE MQTYPE-STRING    TO MQ-PROPTYPE                               
147400                                                                          
147500       MOVE MQ-INTEGRATION-ID TO MQ-PROPVALUE                             
147600                                                                          
147700       MOVE ZERO             TO MQ-PROPVALUELENGTH                        
147800       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
147900               FOR CHARACTERS BEFORE INITIAL SPACE                        
148000                                                                          
148100       CALL 'MQSETMP' USING MQ-HCONN                                      
148200                            MQ-PROPHANDLE                                 
148300                            MQ-SET-PROP-OPTIONS                           
148400                            MQ-PROPNAME                                   
148500                            MQ-PROPDESC                                   
148600                            MQ-PROPTYPE                                   
148700                            MQ-PROPVALUELENGTH                            
148800                            MQ-PROPVALUE                                  
148900                            MQ-COMPCODE                                   
149000                            MQ-REASON                                     
149100       IF MQ-COMPCODE NOT = MQCC-OK                                       
149200         MOVE MQ-COMPCODE TO RC-DISPLAY                                   
149300         MOVE MQ-REASON TO REASON-DISPLAY                                 
149400         STRING 'RC FROM MQSETMP (1)' RC-DISPLAY                          
149500                ' REASON: ' REASON-DISPLAY                                
149600            DELIMITED BY SIZE                                             
149700            INTO ERROR-TEXT                                               
149800         DISPLAY IDPGM ' ' ERROR-TEXT                                     
149900         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
150000       END-IF                                                             
150100     END-IF                                                               
150200                                                                          
150300     IF MQ-CONTRACT-ID NOT = SPACE                                        
150400       SET MQCHARV-VSPTR     TO ADDRESS OF MQ-PROPNAME-TO-SET             
150500       MOVE 'ContractId'     TO MQ-PROPNAME-TO-SET                        
150600       MOVE 10               TO MQCHARV-VSLENGTH                          
150700                                                                          
150800       MOVE SWEDISH-EBCDIC   TO MQCHARV-VSCCSID                           
150900       MOVE MQTYPE-STRING    TO MQ-PROPTYPE                               
151000                                                                          
151100       MOVE MQ-CONTRACT-ID   TO MQ-PROPVALUE                              
151200                                                                          
151300       MOVE ZERO             TO MQ-PROPVALUELENGTH                        
151400       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
151500               FOR CHARACTERS BEFORE INITIAL SPACE                        
151600                                                                          
151700       CALL 'MQSETMP' USING MQ-HCONN                                      
151800                            MQ-PROPHANDLE                                 
151900                            MQ-SET-PROP-OPTIONS                           
152000                            MQ-PROPNAME                                   
152100                            MQ-PROPDESC                                   
152200                            MQ-PROPTYPE                                   
152300                            MQ-PROPVALUELENGTH                            
152400                            MQ-PROPVALUE                                  
152500                            MQ-COMPCODE                                   
152600                            MQ-REASON                                     
152700       IF MQ-COMPCODE NOT = MQCC-OK                                       
152800         MOVE MQ-COMPCODE TO RC-DISPLAY                                   
152900         MOVE MQ-REASON TO REASON-DISPLAY                                 
153000         STRING 'RC FROM MQSETMP (2)' RC-DISPLAY                          
153100                ' REASON: ' REASON-DISPLAY                                
153200            DELIMITED BY SIZE                                             
153300            INTO ERROR-TEXT                                               
153400         DISPLAY IDPGM ' ' ERROR-TEXT                                     
153500         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
153600       END-IF                                                             
153700     END-IF                                                               
153800                                                                          
153900     IF MQ-TOPIC-STRING NOT = SPACE                                       
154000       SET MQCHARV-VSPTR     TO ADDRESS OF MQ-PROPNAME-TO-SET             
154100       MOVE 'TopicStr'       TO MQ-PROPNAME-TO-SET                        
154200       MOVE 8                TO MQCHARV-VSLENGTH                          
154300                                                                          
154400       MOVE SWEDISH-EBCDIC   TO MQCHARV-VSCCSID                           
154500       MOVE MQTYPE-STRING    TO MQ-PROPTYPE                               
154600                                                                          
154700       MOVE MQ-TOPIC-STRING  TO MQ-PROPVALUE                              
154800                                                                          
154900       MOVE ZERO             TO MQ-PROPVALUELENGTH                        
155000       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
155100               FOR CHARACTERS BEFORE INITIAL SPACE                        
155200                                                                          
155300       CALL 'MQSETMP' USING MQ-HCONN                                      
155400                            MQ-PROPHANDLE                                 
155500                            MQ-SET-PROP-OPTIONS                           
155600                            MQ-PROPNAME                                   
155700                            MQ-PROPDESC                                   
155800                            MQ-PROPTYPE                                   
155900                            MQ-PROPVALUELENGTH                            
156000                            MQ-PROPVALUE                                  
156100                            MQ-COMPCODE                                   
156200                            MQ-REASON                                     
156300       IF MQ-COMPCODE NOT = MQCC-OK                                       
156400         MOVE MQ-COMPCODE TO RC-DISPLAY                                   
156500         MOVE MQ-REASON TO REASON-DISPLAY                                 
156600         STRING 'RC FROM MQSETMP (3)' RC-DISPLAY                          
156700                ' REASON: ' REASON-DISPLAY                                
156800            DELIMITED BY SIZE                                             
156900            INTO ERROR-TEXT                                               
157000         DISPLAY IDPGM ' ' ERROR-TEXT                                     
157100         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
157200       END-IF                                                             
157300     END-IF                                                               
157400                                                                          
157500     IF MQ-TYPE NOT = SPACE                                               
157600       SET MQCHARV-VSPTR     TO ADDRESS OF MQ-PROPNAME-TO-SET             
157700       MOVE 'Type'           TO MQ-PROPNAME-TO-SET                        
157800       MOVE 4                TO MQCHARV-VSLENGTH                          
157900                                                                          
158000       MOVE SWEDISH-EBCDIC   TO MQCHARV-VSCCSID                           
158100       MOVE MQTYPE-STRING    TO MQ-PROPTYPE                               
158200                                                                          
158300       MOVE MQ-TYPE          TO MQ-PROPVALUE                              
158400                                                                          
158500       MOVE ZERO             TO MQ-PROPVALUELENGTH                        
158600       INSPECT MQ-PROPVALUE TALLYING MQ-PROPVALUELENGTH                   
158700               FOR CHARACTERS BEFORE INITIAL SPACE                        
158800                                                                          
158900       CALL 'MQSETMP' USING MQ-HCONN                                      
159000                            MQ-PROPHANDLE                                 
159100                            MQ-SET-PROP-OPTIONS                           
159200                            MQ-PROPNAME                                   
159300                            MQ-PROPDESC                                   
159400                            MQ-PROPTYPE                                   
159500                            MQ-PROPVALUELENGTH                            
159600                            MQ-PROPVALUE                                  
159700                            MQ-COMPCODE                                   
159800                            MQ-REASON                                     
159900       IF MQ-COMPCODE NOT = MQCC-OK                                       
160000         MOVE MQ-COMPCODE TO RC-DISPLAY                                   
160100         MOVE MQ-REASON TO REASON-DISPLAY                                 
160200         STRING 'RC FROM MQSETMP (4)' RC-DISPLAY                          
160300                ' REASON: ' REASON-DISPLAY                                
160400            DELIMITED BY SIZE                                             
160500            INTO ERROR-TEXT                                               
160600         DISPLAY IDPGM ' ' ERROR-TEXT                                     
160700         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
160800       END-IF                                                             
160900     END-IF                                                               
161000                                                                          
161100                                                                          
161200*    -- PUT THE MESSAGE                                                   
161300     MOVE IDCOM-MQ-HOBJ  (IDCOM-IX)    TO MQ-HOBJ                         
161400     MOVE IDCOM-CURRENT-LEN(IDCOM-IX)  TO WLEN                            
161500     MOVE IDCOM-CURRENT-DATA(IDCOM-IX) TO WDATA                           
161600                                                                          
161700     COMPUTE MQPMO-OPTIONS =                                              
161800                     MQPMO-FAIL-IF-QUIESCING                              
161900                                                                          
162000     MOVE MQMI-NONE          TO MQMD-MSGID                                
162100     MOVE MQMI-NONE          TO MQMD-CORRELID                             
162200*    AN EXCEPTION BELOW FOR PBV INTEGRATIONS UNTIL A TEST IS MADE         
162300*    THAT THIS CHANGE DOESNT EFFECT THE FUNCTIONALITY.                    
162400     IF MQ-INTEGRATION-ID(1:7) = 'INT1586'                                
162500       CONTINUE                                                           
162600     ELSE                                                                 
162700       MOVE MQFMT-STRING     TO MQMD-FORMAT                               
162800       MOVE SWEDISH-EBCDIC   TO MQMD-CODEDCHARSETID                       
162900     END-IF                                                               
163000     MOVE MQPMO-VERSION-3    TO MQPMO-VERSION                             
163100     MOVE MQ-PROPHANDLE      TO MQPMO-ORIGINALMSGHANDLE                   
163200     MOVE MQACTP-NEW         TO MQPMO-ACTION                              
163300                                                                          
163400     CALL 'MQPUT' USING  MQ-HCONN                                         
163500                         MQ-HOBJ                                          
163600                         MQMD                                             
163700                         MQ-PUT-MESSAGE-OPTIONS                           
163800                         WLEN                                             
163900                         WDATA                                            
164000                         MQ-COMPCODE                                      
164100                         MQ-REASON                                        
164200                                                                          
164300     IF MQ-COMPCODE NOT = MQCC-OK                                         
164400       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
164500       MOVE MQ-REASON  TO REASON-DISPLAY                                  
164600       STRING 'RC FROM MQPUT '   RC-DISPLAY                               
164700              ' REASON: ' REASON-DISPLAY                                  
164800          DELIMITED BY SIZE                                               
164900          INTO ERROR-TEXT                                                 
165000       DISPLAY IDPGM ' ' ERROR-TEXT                                       
165100       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
165200     END-IF                                                               
165300                                                                          
165400*    -- FINALLY, CLOSE THE QUEUE                                          
165500     CALL 'MQCLOSE' USING MQ-HCONN                                        
165600                         MQ-HOBJ                                          
165700                         MQCO-NONE                                        
165800                         MQ-COMPCODE                                      
165900                         MQ-REASON                                        
166000                                                                          
166100     IF MQ-COMPCODE NOT = MQCC-OK                                         
166200       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
166300       MOVE MQ-REASON  TO REASON-DISPLAY                                  
166400       STRING 'RC FROM MQCLOSE ' RC-DISPLAY                               
166500              ' REASON: ' REASON-DISPLAY                                  
166600          DELIMITED BY SIZE                                               
166700          INTO ERROR-TEXT                                                 
166800       DISPLAY IDPGM ' ' ERROR-TEXT                                       
166900       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
167000     END-IF                                                               
167100     .                                                                    
167200                                                                          
167300     EJECT                                                                
167400 T-TRACK-AND-TRACE SECTION.                                               
167500     MOVE FUNCTION HEX-OF (MQMD-MSGID)                                    
167600                                 TO messageId                             
167700     MOVE WLEN                   TO messageSize                           
167800     MOVE WDATA                  TO logtext                               
167900     INSPECT logtext REPLACING ALL X'00' BY '_'                           
168000                                                                          
168100     MOVE FUNCTION                                                        
168200       FORMATTED-CURRENT-DATE('YYYY-MM-DDThh:mm:ss.sssZ')                 
168300                                 TO timestamp                             
168400                                                                          
168500     MOVE TNT-INTEGRATION-ID     TO integrationId                         
168600     MOVE TNT-CONTRACT-ID        TO contractId                            
168700     MOVE SPACES                 TO transactionId                         
168800                                                                          
168900     MOVE '000573'               TO applicationId                         
169000                                                                          
169100     MOVE 'INFO'                 TO level                                 
169200                                                                          
169300     XML GENERATE MQ-TNT-MSG-DATA                                         
169400       FROM log                                                           
169500       COUNT IN MQ-TNT-MSG-LEN                                            
169600*      WITH ENCODING 1208                                                 
169700*      WITH XML-DECLARATION                                               
169800       NAME OF logtext           IS "text"                                
169900       SUPPRESS WHEN SPACES                                               
170000       ON EXCEPTION                                                       
170100         DISPLAY IDPGM ' ERROR IN XML GEN :' XML-CODE                     
170200     END-XML                                                              
170300     COMPUTE MQ-TNT-MSG-LEN = FUNCTION BYTE-LENGTH (                      
170400                              FUNCTION TRIM (MQ-TNT-MSG))                 
170500                                                                          
170600     PERFORM S41-MQ-OPEN-TNT                                              
170700     PERFORM S41-MQ-PUT-TNT                                               
170800     PERFORM S41-MQ-CLOSEQ-TNT                                            
170900     .                                                                    
171000                                                                          
171100 S06-MQ-DISCONNECT SECTION.                                               
171200                                                                          
171300*    -- BREAK CONTACT WITH MQ QUEUE MANAGER                               
171400                                                                          
171500     MOVE IDCOM-MQ-HCONN (IDCOM-IX)    TO MQ-HCONN                        
171600     CALL 'MQDISC' USING MQ-HCONN                                         
171700                         MQ-COMPCODE                                      
171800                         MQ-REASON                                        
171900                                                                          
172000     IF MQ-COMPCODE NOT = MQCC-OK                                         
172100       MOVE MQ-COMPCODE TO RC-DISPLAY                                     
172200       MOVE MQ-REASON  TO REASON-DISPLAY                                  
172300       STRING 'RC FROM MQDISC ' RC-DISPLAY                                
172400              ' REASON: ' REASON-DISPLAY                                  
172500          DELIMITED BY SIZE                                               
172600          INTO ERROR-TEXT                                                 
172700       DISPLAY IDPGM ' ' ERROR-TEXT                                       
172800       CALL ABEND  USING RKOD-ABEND-NO-DUMP                               
172900     END-IF                                                               
173000     .                                                                    
173100                                                                          
173200 S41-MQ-OPEN-TNT SECTION.                                                 
173300     MOVE 'VCC.TNT.LOGEVENT'     TO TTOD-OBJECTNAME                       
173400                                                                          
173500     MOVE MQOO-FAIL-IF-QUIESCING TO TNT-OPENOPTIONS                       
173600     ADD MQOO-OUTPUT             TO TNT-OPENOPTIONS                       
173700                                                                          
173800     CALL 'MQOPEN'            USING MQ-HCONN                              
173900                                    TTOD                                  
174000                                    TNT-OPENOPTIONS                       
174100                                    TNT-HOBJ                              
174200                                    MQ-COMPCODE                           
174300                                    MQ-REASON                             
174400                                                                          
174500     IF MQ-COMPCODE NOT = MQCC-OK                                         
174600       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
174700       MOVE MQ-REASON            TO REASON-DISPLAY                        
174800       MOVE SPACE                TO ERROR-TEXT-TNT                        
174900       STRING 'RC FROM TNT MQOPEN ' RC-DISPLAY                            
175000              ' REASON: ' REASON-DISPLAY                                  
175100              ' ' TTOD-OBJECTNAME                                         
175200          DELIMITED BY SIZE                                               
175300                               INTO ERROR-TEXT-TNT                        
175400       DISPLAY IDPGM ' ' ERROR-TEXT                                       
175500     END-IF                                                               
175600     .                                                                    
175700                                                                          
175800 S41-MQ-PUT-TNT SECTION.                                                  
175900*    -- PUT THE TNT MESSAGE                                               
176000                                                                          
176100     COMPUTE TTPMO-OPTIONS        = MQPMO-FAIL-IF-QUIESCING               
176200                                                                          
176300     MOVE MQMI-NONE              TO TTMD-MSGID                            
176400     MOVE MQCI-NONE              TO TTMD-CORRELID                         
176500     MOVE MQFMT-STRING           TO TTMD-FORMAT                           
176600     MOVE SWEDISH-EBCDIC         TO TTMD-CODEDCHARSETID                   
176700     MOVE MQENC-NATIVE           TO TTMD-ENCODING                         
176800     MOVE MQCCSI-Q-MGR           TO TTMD-CODEDCHARSETID                   
176900                                                                          
177000     MOVE MQPMO-VERSION-3        TO TTPMO-VERSION                         
177100     MOVE MQACTP-NEW             TO TTPMO-ACTION                          
177200                                                                          
177300     MOVE MQ-TNT-MSG-LEN         TO MQ-MSGLENGTH                          
177400                                                                          
177500     CALL 'MQPUT'             USING MQ-HCONN                              
177600                                    TNT-HOBJ                              
177700                                    TTMD                                  
177800                                    TTPMO                                 
177900                                    MQ-MSGLENGTH                          
178000                                    MQ-TNT-MSG                            
178100                                    MQ-COMPCODE                           
178200                                    MQ-REASON                             
178300                                                                          
178400     IF MQ-COMPCODE NOT = MQCC-OK                                         
178500       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
178600       MOVE MQ-REASON            TO REASON-DISPLAY                        
178700       MOVE SPACE                TO ERROR-TEXT-TNT                        
178800       STRING 'RC FROM TNT MQPUT '   RC-DISPLAY                           
178900              ' REASON: ' REASON-DISPLAY                                  
179000          DELIMITED BY SIZE    INTO ERROR-TEXT-TNT                        
179100       DISPLAY IDPGM ' ' ERROR-TEXT                                       
179200     END-IF                                                               
179300     .                                                                    
179400                                                                          
179500 S41-MQ-CLOSEQ-TNT  SECTION.                                              
179600                                                                          
179700     CALL 'MQCLOSE'           USING MQ-HCONN                              
179800                                    TNT-HOBJ                              
179900                                    MQCO-NONE                             
180000                                    MQ-COMPCODE                           
180100                                    MQ-REASON                             
180200                                                                          
180300     IF MQ-COMPCODE NOT = MQCC-OK                                         
180400       MOVE MQ-COMPCODE          TO RC-DISPLAY                            
180500       MOVE MQ-REASON            TO REASON-DISPLAY                        
180600       MOVE SPACE                TO ERROR-TEXT-TNT                        
180700       STRING 'RC FROM TNT MQCLOSE ' RC-DISPLAY                           
180800              ' REASON: ' REASON-DISPLAY                                  
180900          DELIMITED BY SIZE    INTO ERROR-TEXT-TNT                        
181000       DISPLAY IDPGM ' ' ERROR-TEXT                                       
181100     END-IF                                                               
181200     .                                                                    
181300                                                                          
181400 IMS-INSERT-MESSAGE SECTION.                                              
181500                                                                          
181600     MOVE 128         TO AIB-LEN                                          
181700     MOVE SPACE       TO AIB-SUB-FUNCTION                                 
181800     MOVE IDCOM-PCBNAME(IDCOM-IX)                                         
181900                      TO AIB-PCB-NAME                                     
182000     MOVE IMSMSG-KVLL TO AIB-IOAREA-USED                                  
182100                                                                          
182200     MOVE SPACE       TO VALID-STATUS-CODES                               
182300                                                                          
182400     CALL AIBTDLI USING ISRT AIB-AREA                                     
182500                        IMSMSG-IO-AREA                                    
182600                                                                          
182700     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
182800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
182900     PERFORM IMS-STATUS-CHECK                                             
183000     .                                                                    
183100                                                                          
183200     EJECT                                                                
183300 IMS-INSERT-PURGE-SOP-ORDER SECTION.                                      
183400                                                                          
183500     MOVE 128         TO AIB-LEN                                          
183600     MOVE SPACE       TO AIB-SUB-FUNCTION                                 
183700     MOVE 'SOP'       TO AIB-PCB-NAME                                     
183800     MOVE MSGSOP-LL   TO AIB-IOAREA-USED                                  
183900                                                                          
184000     MOVE SPACE       TO VALID-STATUS-CODES                               
184100                                                                          
184200     CALL AIBTDLI USING PURG AIB-AREA MSGSOP-WMSGSOP                      
184300                                                                          
184400     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
184500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
184600     PERFORM IMS-STATUS-CHECK                                             
184700     .                                                                    
184800                                                                          
184900     EJECT                                                                
185000 IMS-PURGE    SECTION.                                                    
185100                                                                          
185200     MOVE 128         TO AIB-LEN                                          
185300     MOVE SPACE       TO AIB-SUB-FUNCTION                                 
185400     MOVE IDCOM-PCBNAME(IDCOM-IX)                                         
185500                      TO AIB-PCB-NAME                                     
185600     MOVE ZERO        TO AIB-IOAREA-USED                                  
185700                                                                          
185800     MOVE SPACE       TO VALID-STATUS-CODES                               
185900                                                                          
186000     CALL AIBTDLI USING PURG AIB-AREA                                     
186100                                                                          
186200     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
186300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
186400     PERFORM IMS-STATUS-CHECK                                             
186500     .                                                                    
186600                                                                          
186700 IMS-STATUS-CHECK   SECTION.                                              
186800                                                                          
186900     IF STATUS-WS = LOW-VALUE                                             
187000       STRING ' CAN NOT FIND PCB WITH NAME: '                             
187100              IDCOM-PCBNAME(IDCOM-IX) ' IN THE PSB.'                      
187200         DELIMITED BY SIZE INTO ERROR-TEXT                                
187300         CALL FELLOG                                                      
187400     ELSE                                                                 
187500       SET STATUS-IX TO 1                                                 
187600       SEARCH VALID-STATUS                                                
187700         AT END                                                           
187800           STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS             
187900           DELIMITED BY SIZE INTO ERROR-TEXT                              
188000           DISPLAY IDPGM ' ' ERROR-TEXT                                   
188100           CALL FELLOG                                                    
188200         WHEN VALID-STATUS (STATUS-IX) = STATUS-WS                        
188300           CONTINUE                                                       
188400       END-SEARCH                                                         
188500     END-IF                                                               
188600     .                                                                    
188700                                                                          
188800 DB2-SELECT-SYSDUMMY1 SECTION.                                            
188900                                                                          
189000     MOVE 000                    TO GOOD-SQLCODECODES                     
189100     EXEC SQL                                                             
189200       SELECT CURRENT TIMESTAMP,                                          
189300              CURRENT SQLID,                                              
189400              LEFT(GETVARIABLE('SYSIBM.PLAN_NAME'),8)                     
189500         INTO :WS-TIMESTAMP-DB2                                           
189600             ,:WS-IDUSER                                                  
189700             ,:WS-IDCALLER                                                
189800         FROM SYSIBM.SYSDUMMY1                                            
189900     END-EXEC                                                             
190000                                                                          
190100     MOVE SQLCODE                TO SQLCODE-WS                            
190200     PERFORM DB2-STATUS-CHECK                                             
190300     .                                                                    
190400                                                                          
190500 DB2-SELECT-TZ1DIDA-TAB SECTION.                                          
190600     SKIP2                                                                
190700     MOVE 000100 TO GOOD-SQLCODECODES                                     
190800     EXEC SQL                                                             
190900       SELECT ADDISPABS                                                   
191000                                                                          
191100       INTO :WS-TZ1DIDA-DUMMY                                             
191200                                                                          
191300       FROM TZ1DIDA                                                       
191400                                                                          
191500       WHERE ADDISPABS      = :DIDA-ADDISPABS                             
191600       AND   TIDATETIME_DB2 = :WS-TIMESTAMP-DB2                           
191700       AND   IDLOPNR        = :DIDA-IDLOPNR                               
191800     END-EXEC                                                             
191900                                                                          
192000     MOVE SQLCODE TO SQLCODE-WS                                           
192100     PERFORM DB2-STATUS-CHECK                                             
192200     .                                                                    
192300                                                                          
192400 DB2-INSERT-TZ1DIDA-TAB  SECTION.                                         
192500     SKIP2                                                                
192600     MOVE 000                    TO GOOD-SQLCODECODES                     
192700     IF DIDA-IDLOPNR = 1                                                  
192800       MOVE 'S'                  TO DIDA-KDKOMSTA                         
192900     ELSE                                                                 
193000       MOVE SPACES               TO DIDA-KDKOMSTA                         
193100     END-IF                                                               
193200                                                                          
193300     EXEC SQL                                                             
193400       INSERT INTO TZ1DIDA                                                
193500         (ADDISPABS, TIDATETIME_DB2, IDLOPNR,                             
193600          IDCALLER, IDUSER, IDCPYTXT, KDRC_HTTP,                          
193700          KDRC_PULS, KDKOMSTA, MESSAGE, DATA)                             
193800                                                                          
193900       VALUES                                                             
194000         (:DIDA-ADDISPABS, :DIDA-TIDATETIME-DB2, :DIDA-IDLOPNR,           
194100          :WS-IDCALLER, :WS-IDUSER, '', '',                               
194200          '', :DIDA-KDKOMSTA, '', :WS-CLOB)                               
194300     END-EXEC                                                             
194400                                                                          
194500     MOVE SQLCODE                TO SQLCODE-WS                            
194600     PERFORM DB2-STATUS-CHECK                                             
194700     .                                                                    
194800                                                                          
194900 DB2-STATUS-CHECK  SECTION.                                               
195000                                                                          
195100     SET SQLCODE-IX TO 1                                                  
195200     SEARCH GOOD-SQLCODE                                                  
195300       AT END                                                             
195400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
195500          DELIMITED BY SIZE INTO ERROR-TEXT                               
195600          CALL ABEND USING RKOD-ABEND-DB2                                 
195700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
195800     END-SEARCH                                                           
195900     .                                                                    
