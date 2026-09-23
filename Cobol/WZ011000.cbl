000100*COMPOPT VPOSIX=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ011000.                                                
000400 AUTHOR.         RAHUL REDDY.                                             
000500 DATE-WRITTEN.   20/11/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       CARPARTS.PULS.WEBSERVICE.MOCK                            
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        THIS IS A TEST PROGRAM FOR ZOS CONNECT.                          
001200*        CAN ALSO BE USED FOR MOCK API (WHEN BACKEND IS NOT READY)        
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: WZ0110T                                             
001600*        REQUEST:     WZ0110I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    WZ0110O1                                            
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800                                                                          
002900****************************************************************          
003000 77  IDPGM                       PIC X(08)   VALUE 'WZ011000'.            
003100                                                                          
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003400 77  KDRC-DISPLAY                PIC Z(5).                                
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 01  WS-SEND-OPEN                PIC X       VALUE SPACES.                
004000     88 WS-SEND-OPEN-YES             VALUE 'J'.                           
004100                                                                          
004200 01  WS-ENV                      PIC X(4)    VALUE SPACES.                
004300     88 WS-ENV-DEVE                  VALUE 'DEVE'.                        
004400     88 WS-ENV-IGRT                  VALUE 'IGRT'.                        
004500     88 WS-ENV-XDEV                  VALUE 'XDEV'.                        
004600     88 WS-ENV-ACPT                  VALUE 'ACPT'.                        
004700     88 WS-ENV-PROD                  VALUE 'PROD'.                        
004800                                                                          
004900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005300     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
005400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005700     03  W403TMS1                PIC X(8)    VALUE 'W403TMS1'.            
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900                                                                          
006000*    --- PARAMETERS TO ABEND                                              
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006200 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006500                                                                          
006600*01  -COPY WZ01SEND                                                       
006700                                                                          
006800*TMS PACKNING INFO                                                        
006900 01  FILLER                      PIC X(8)   VALUE  'W403TMS1'.            
007000*    -COPY W403TMS1                                                       
007100                                                                          
007200*    --- PARAMETERS TO VIMSID                                             
007300 01 WS-VIMSID                PIC X(8)  VALUE SPACE.                       
007400 01 FILLER REDEFINES WS-VIMSID.                                           
007500    03 IMS-REGION            PIC X(3).                                    
007600       88 TEST-REGION                  VALUE 'IMD' 'IMP' 'IMY'            
007700                                             'IMB'.                       
007800       88 ENV-DEVE                     VALUE 'IMP'.                       
007900       88 ENV-IGRT                     VALUE 'IMY'.                       
008000       88 ENV-XDEV                     VALUE 'IMD'.                       
008100       88 ENV-ACPT                     VALUE 'IMB'.                       
008200       88 ENV-PROD                     VALUE 'IMG' 'IMR'.                 
008300    03 FILLER                PIC X(5).                                    
008400                                                                          
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008700*01  -COPY WZ01SUB                                                        
008800                                                                          
008900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009000 01  REQU-AREA.                                                           
009100     03 INPUT-AREA.                                                       
009200         05 REQU-ADDISPABS       PIC X(50).                               
009300         05 REQU-TIDATETIME-DB2  PIC X(26).                               
009400     03 FILLER REDEFINES INPUT-AREA.                                      
009500         05 REQU-IDENTITY        PIC X(9).                                
009600         05 REQU-IDDC            PIC X(2).                                
009700         05 REQU-IDDISTR         PIC 9(4).                                
009800         05 REQU-IDKUNDNR        PIC 9(6).                                
009900         05 REQU-IDORDNR7        PIC 9(7).                                
010000         05 REQU-IDKOLLI         PIC 9(5).                                
010100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010200 01  RESP-AREA.                                                           
010300     03 WS-STATUS PIC X(10).                                              
010400                                                                          
010500                                                                          
010600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010700*                                                                         
010800 01 FILLER                   PIC X(16) VALUE 'IMS-WS'.                    
010900                                                                          
011000 01 KEYS-FOR-DLI.                                                         
011100    03 W-WDGXKEY-0103-X.                                                  
011200       05  W-IDHTYP-0103     PIC X(4)  VALUE '0103'.                      
011300       05  FILLER            PIC X(26) VALUE LOW-VALUES.                  
011400    03 W-KY0104-X.                                                        
011500       05  W-ADDISPABS       PIC X(50) VALUE SPACES.                      
011600                                                                          
011700*    --- STATUS CODES FROM IMS                                            
011800 01 STATUS-WS                PIC XX.                                      
011900    88 SEGMENT-FOUND                   VALUE '  '.                        
012000    88 SEGMENT-FOUND-EXISTS            VALUE 'II'.                        
012100    88 SEGMENT-MISSING                 VALUE 'GE'.                        
012200                                                                          
012300 01 GOOD-STATUSCODES.                                                     
012400    03 GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX                          
012500                             PIC XX.                                      
012600                                                                          
012700 01 SSA1                     PIC X(64).                                   
012800 01 SSA2                     PIC X(128).                                  
012900                                                                          
013000*    --- IMS FUNCTION CODES                                               
013100*01  -COPY W0003.                                                         
013200                                                                          
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01 FILLER                   PIC X(16) VALUE 'DLI-IO-WDR501'.             
013500 01 DLI-IO-WDR501.                                                        
013600*   03  -COPY WDGX01                                                      
013700                                                                          
013800 01 FILLER                   PIC X(16) VALUE 'DLI-IO-WDGX0104'.           
013900 01 DLI-IO-WDGX0104.                                                      
014000*   03  -COPY WDGX0104                                                    
014100                                                                          
014200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
014300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
014400                                                                          
014500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
014600 01  DB2-WS.                                                              
014700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
014800         88  LINES-FOUND                     VALUE 000.                   
014900         88  LINES-MISSING                   VALUE 100.                   
015000         88  RESOURCE-WRONG                  VALUE 904.                   
015100     03  GOOD-SQLCODECODES.                                               
015200         05  GOOD-SQLCODE OCCURS 5                                        
015300             INDEXED BY SQLCODE-IX PIC 9(3).                              
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)   VALUE 'TZ1DIDA-AREA'.        
015600*01  -COPY TZ1DIDA -PRE DIDA-                                             
015700                                                                          
015800     EXEC SQL INCLUDE TZ1DIDA END-EXEC.                                   
015900                                                                          
016000 LINKAGE SECTION.                                                         
016100*01   -COPY W0009    -PRE MSG-                                            
016200 01  TMS-CRE-PCB                 PIC X.                                   
016300 01  TMS-DEL-PCB                 PIC X.                                   
016400                                                                          
016500*01   -COPY W0009    -PRE ATAB-                                           
016600                                                                          
016700 01  TMS-1165-PCB                PIC X.                                   
016800 01  TMS-4141-PCB                PIC X.                                   
016900 01  TMS-WDB2-PCB                PIC X.                                   
017000 01  TMS-WDB6-PCB                PIC X.                                   
017100 01  TMS-WDD3-PCB                PIC X.                                   
017200 01  TMS-WDB1-PCB                PIC X.                                   
017300 01  TMS-WDE4A-PCB               PIC X.                                   
017400 01  TMS-WDE4F-PCB               PIC X.                                   
017500 01  TMS-WDQ2-PCB                PIC X.                                   
017600 01  TMS-WDQ3-PCB                PIC X.                                   
017700 01  TMS-WDK6-PCB                PIC X.                                   
017800 01  TMS-WDE6-PCB                PIC X.                                   
017900 01  TMS-WDK5-PCB                PIC X.                                   
018000 01  TMS-WDQ2C-PCB               PIC X.                                   
018100                                                                          
018200 PROCEDURE DIVISION  USING MSG-PCB                                        
018300                           TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB               
018400     TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB TMS-WDB6-PCB                  
018500     TMS-WDD3-PCB TMS-WDB1-PCB TMS-WDE4A-PCB TMS-WDE4F-PCB                
018600     TMS-WDQ2-PCB TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                  
018700     TMS-WDK5-PCB TMS-WDQ2C-PCB.                                          
018800 MAIN SECTION.                                                            
018900                                                                          
019000     MOVE SPACES                 TO REQU-AREA                             
019100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
019200     IF SUB-KDRC = 0                                                      
019300       IF REQU-IDENTITY = 'TMSCREATE'                                     
019400         INITIALIZE TMS-W403TMS1                                          
019500         MOVE REQU-IDDC          TO TMS-IDDC                              
019600         MOVE REQU-IDDISTR       TO TMS-IDDISTR                           
019700         MOVE REQU-IDKUNDNR      TO TMS-IDKUNDNR                          
019800         MOVE REQU-IDORDNR7      TO TMS-IDORDNR7                          
019900         MOVE REQU-IDKOLLI       TO TMS-IDKOLLI(1)                        
020000         CALL W403TMS1 USING TMS-W403TMS1                                 
020100                 TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                         
020200                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
020300                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
020400                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
020500                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
020600                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
020700         MOVE 'TMSCR OK' TO WS-STATUS                                     
020800       ELSE                                                               
020900         IF REQU-ADDISPABS = SPACE OR REQU-TIDATETIME-DB2 = SPACE         
021000           MOVE 'INV KEY' TO WS-STATUS                                    
021100         ELSE                                                             
021200           PERFORM DB2-DCL-OPN-TZ1DIDA-CRS                                
021300           IF LINES-FOUND                                                 
021400             PERFORM DB2-FETCH-TZ1DIDA-CRS                                
021500             IF LINES-FOUND                                               
021600               PERFORM S11-SEND-OPEN                                      
021700               SET WS-SEND-OPEN-YES TO TRUE                               
021800             ELSE                                                         
021900               MOVE 'MISS NG' TO WS-STATUS                                
022000             END-IF                                                       
022100             PERFORM UNTIL LINES-MISSING                                  
022200               PERFORM S12-SEND-PUT                                       
022300               PERFORM DB2-FETCH-TZ1DIDA-CRS                              
022400             END-PERFORM                                                  
022500             IF WS-SEND-OPEN-YES                                          
022600               PERFORM S13-SEND-CLOSE                                     
022700               MOVE 'OK' TO WS-STATUS                                     
022800             END-IF                                                       
022900             PERFORM DB2-CLOSE-TZ1DIDA-CRS                                
023000           ELSE                                                           
023100             MOVE 'MISSING' TO WS-STATUS                                  
023200           END-IF                                                         
023300         END-IF                                                           
023400       END-IF                                                             
023500       PERFORM S02-RETURN-RESPONSE                                        
023600     END-IF                                                               
023700                                                                          
023800     MOVE ZERO TO RETURN-CODE                                             
023900     GOBACK                                                               
024000     .                                                                    
024100*    --- DISPATCHER SECTIONS                                              
024200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
024300                                                                          
024400     MOVE 'GETARG'               TO SUB-KDFUNC                            
024500     MOVE 'CARPARTS.PULS.WEBSERVICE.MOCK'    TO SUB-ADDISPABS             
024600     MOVE LENGTH OF REQU-AREA       TO SUB-KVDLEN                         
024700                                                                          
024800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
024900                                                                          
025000     IF SUB-KDRC > 0                                                      
025100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
025200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
025300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
025400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025500     END-IF                                                               
025600     .                                                                    
025700                                                                          
025800 S02-RETURN-RESPONSE SECTION.                                             
025900                                                                          
026000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
026100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
026200                                                                          
026300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
026400                                                                          
026500     IF SUB-KDRC > 0                                                      
026600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
026700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
026800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
026900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
027000     END-IF                                                               
027100     .                                                                    
027200                                                                          
027300 S11-SEND-OPEN       SECTION.                                             
027400                                                                          
027500     MOVE REQU-ADDISPABS          TO SEND-ADDISPABS                       
027600                                     SEND-ADDISPABS-RESTART               
027700     MOVE REQU-TIDATETIME-DB2     TO SEND-TIDATETIME-DB2                  
027800     MOVE YES                     TO SEND-FLRESTART                       
027900     MOVE 'OPEN'                  TO SEND-KDFUNC                          
028000     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
028100                                     SEND-OPEN-AREA                       
028200     IF SEND-KDRC > ZERO                                                  
028300       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
028400       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
028500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
028600       DISPLAY ERROR-TEXT                                                 
028700       CALL FELLOG                                                        
028800     END-IF                                                               
028900                                                                          
029000     .                                                                    
029100     SKIP2                                                                
029200 S12-SEND-PUT       SECTION.                                              
029300                                                                          
029400     MOVE 'PUT'                   TO SEND-KDFUNC                          
029500     MOVE DIDA-DATA-LENGTH        TO SEND-KVDLEN                          
029600     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
029700                                     SEND-KVDLEN                          
029800                                   DIDA-DATA-DATA (1:SEND-KVDLEN)         
029900     IF SEND-KDRC > ZERO                                                  
030000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
030100       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
030200       DELIMITED BY SIZE       INTO ERROR-TEXT                            
030300       DISPLAY ' ERRORTEXT   '   ERROR-TEXT                               
030400       CALL FELLOG                                                        
030500     END-IF                                                               
030600     .                                                                    
030700     SKIP2                                                                
030800                                                                          
030900 S13-SEND-CLOSE              SECTION.                                     
031000                                                                          
031100     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
031200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031300                                                                          
031400     IF SEND-KDRC > 0                                                     
031500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
031600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
031700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031800       DISPLAY ' ERRORTEXT '   ERROR-TEXT                                 
031900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032000     END-IF                                                               
032100     .                                                                    
032200 DB2-DCL-OPN-TZ1DIDA-CRS SECTION.                                         
032300                                                                          
032400                                                                          
032500     MOVE 000100 TO GOOD-SQLCODECODES                                     
032600                                                                          
032700     EXEC SQL                                                             
032800       DECLARE TZ1DIDA-CRS CURSOR FOR                                     
032900                                                                          
033000       SELECT DATA                                                        
033100                                                                          
033200       FROM    TZ1DIDA                                                    
033300                                                                          
033400       WHERE  ADDISPABS      = :REQU-ADDISPABS                            
033500        AND   TIDATETIME_DB2 = :REQU-TIDATETIME-DB2                       
033600                                                                          
033700       ORDER BY IDLOPNR                                                   
033800     END-EXEC                                                             
033900                                                                          
034000     MOVE 000100180  TO GOOD-SQLCODECODES                                 
034100                                                                          
034200     EXEC SQL                                                             
034300       OPEN TZ1DIDA-CRS                                                   
034400     END-EXEC                                                             
034500                                                                          
034600     MOVE SQLCODE TO SQLCODE-WS                                           
034700     PERFORM DB2-STATUS-CHECK                                             
034800     .                                                                    
034900                                                                          
035000     SKIP3                                                                
035100 DB2-FETCH-TZ1DIDA-CRS SECTION.                                           
035200                                                                          
035300     MOVE 000100  TO GOOD-SQLCODECODES                                    
035400                                                                          
035500     EXEC SQL                                                             
035600                                                                          
035700       FETCH TZ1DIDA-CRS                                                  
035800                                                                          
035900       INTO :DIDA-DATA                                                    
036000                                                                          
036100     END-EXEC                                                             
036200                                                                          
036300     MOVE SQLCODE TO SQLCODE-WS                                           
036400     PERFORM DB2-STATUS-CHECK                                             
036500     .                                                                    
036600                                                                          
036700 DB2-CLOSE-TZ1DIDA-CRS SECTION.                                           
036800                                                                          
036900     EXEC SQL                                                             
037000        CLOSE TZ1DIDA-CRS                                                 
037100     END-EXEC                                                             
037200     .                                                                    
037300                                                                          
037400 DB2-STATUS-CHECK  SECTION.                                               
037500                                                                          
037600     SET SQLCODE-IX TO 1                                                  
037700     SEARCH GOOD-SQLCODE                                                  
037800       AT END                                                             
037900          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
038000          DELIMITED BY SIZE INTO ERROR-TEXT                               
038100          CALL ABEND USING RKOD-ABEND-DB2                                 
038200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
038300     END-SEARCH                                                           
038400     .                                                                    
