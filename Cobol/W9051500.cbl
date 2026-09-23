000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9051500.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   20230724.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:    CARPARTS.PULS.APICONCERNUPDATE,                             
000800*             CAMPAIGN/CONCERN API                                        
000900*             COMMUNICATION PROGRAM TO UPDATE PROCURER AND LOGG           
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W9A515                                              
001500*        REQUEST:     W90515I1                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        RESPONSE:    W90515O1                                            
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500                                                                          
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -COPY WY2000W1                                                       
003100 77  IDPGM                       PIC X(08)   VALUE 'W9051500'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
003700 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
003800                                                                          
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100 77  IX                          PIC S9(3)   VALUE +0   COMP SYNC.        
004200 77  IX-MAX                      PIC S9(3)   VALUE +6   COMP SYNC.        
004300 77  MSG-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004400                                                                          
004500 77  WS-IDUSER                   PIC X(8)    VALUE SPACES.                
004600 77  W-IDARTNR                   PIC S9(9)           COMP-3.              
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
004900     88  KEYS-OK                             VALUE 'Y'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005300      88  INDATA-OK                          VALUE 'J'.                   
005400      88  INDATA-FEL                         VALUE 'N'.                   
005500     EJECT                                                                
005600                                                                          
005700 01  WS-API-DATE.                                                         
005800     03  FILLER                  PIC X(2)    VALUE '20'.                  
005900     03  WS-API-YEAR             PIC 9(2).                                
006000     03  FILLER                  PIC X(1)    VALUE '-'.                   
006100     03  WS-API-MONTH            PIC 9(2).                                
006200     03  FILLER                  PIC X(1)    VALUE '-'.                   
006300     03  WS-API-DAY              PIC 9(2).                                
006400     EJECT                                                                
006500                                                                          
006600                                                                          
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007300     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
007400     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
007500     03  W200CDCU                PIC X(8)    VALUE 'W200CDCU'.            
007600     03  W200LOGU                PIC X(8)    VALUE 'W200LOGU'.            
007700                                                                          
007800*    --- PARAMETERS TO ABEND                                              
007900                                                                          
008000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008300                                                                          
008400 01  MESSAGE-CODES.                                                       
008500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008600     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
008700                                                                          
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009000*01  -COPY WZ01SUB                                                        
009100                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
009300*01  -COPY WMSGCONV                                                       
009400                                                                          
009500 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
009600*01  -COPY WZ01AUTH                                                       
009700                                                                          
009800***  BELOW COPYBOOKS INCLUDES INPUT AND OUTPUT HEADERS                    
009900***  WZ01REQ2 IN INPUT AND WZ01RES2 IN OUTPUT                             
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010200 01  REQU-AREA.                                                           
010300*    03  -COPY W90515I1                                                   
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010600     SKIP3                                                                
010700 01  RESP-AREA.                                                           
010800*    03  -COPY W90515O1                                                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'CDCU-AREA'.           
011100     SKIP3                                                                
011200 01  CDCU-AREA.                                                           
011300*    03  -COPY W200CDCU                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'LOGU-AREA'.           
011600     SKIP3                                                                
011700 01  LOGU-AREA.                                                           
011800*    03  -COPY W200LOGU                                                   
011900     EJECT                                                                
012000***                                                                       
012100***  VALID IDDC CODES                                                     
012200***                                                                       
012300*01  -COPY WWDC99                                                         
012400     EJECT                                                                
012500                                                                          
012600******************************************************************        
012700                                                                          
012800 LINKAGE SECTION.                                                         
012900*                                                                         
013000*01    -COPY W0009     -PRE MSG-                                          
013100 01  ATAB-PCB                    PIC X.                                   
013200                                                                          
013300 01  CDCU-WDK6-PCB               PIC X.                                   
013400 01  LOGU-WDP5-PCB               PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION USING  MSG-PCB ATAB-PCB                               
013700                                   CDCU-WDK6-PCB                          
013800                                   LOGU-WDP5-PCB.                         
013900 MAIN SECTION.                                                            
014000     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB                               
014100                                   CDCU-WDK6-PCB                          
014200                                   LOGU-WDP5-PCB.                         
014300                                                                          
014400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014500     IF SUB-KDRC = 0                                                      
014600       PERFORM A-INIT                                                     
014700       IF KEYS-OK                                                         
014800         PERFORM B-CHECK-KEYS                                             
014900         IF KEYS-OK                                                       
015000            PERFORM H-UPDATE                                              
015100         END-IF                                                           
015200       END-IF                                                             
015300                                                                          
015400       IF SUB-KDTRANS(1:6) = 'W9A515'                                     
015500         PERFORM S11-MSG-CONV                                             
015600       END-IF                                                             
015700       PERFORM S02-RETURN-RESPONSE                                        
015800     END-IF                                                               
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500     MOVE 'A-INIT' TO CURR-SECTION                                        
016600                                                                          
016700     MOVE YES                    TO KEYS-SW                               
016800                                                                          
016900     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
017000                                    RESP-IDMSG-INFO                       
017100                                    RESP-IDELMT-ERROR                     
017200                                                                          
017300     IF SUB-KDTRANS(1:6) = 'W9A515'                                       
017400       MOVE 001                  TO AUTH-KDCALL                           
017500       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
017600                                    REQU-WZ01REQ2                         
017700       IF AUTH-KDRC > 0                                                   
017800         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
017900         MOVE NOO                TO KEYS-SW                               
018000       ELSE                                                               
018100         MOVE REQU-IDUSER        TO WS-IDUSER                             
018200       END-IF                                                             
018300     END-IF                                                               
018400     .                                                                    
018500     EJECT                                                                
018600                                                                          
018700 B-CHECK-KEYS SECTION.                                                    
018800     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
018900                                                                          
019000     IF REQU-UPDATE                                                       
019100        CONTINUE                                                          
019200     ELSE                                                                 
019300        MOVE '023'              TO RESP-IDMSG-ERROR                       
019400*       WRONG ACTION KEY ***                                              
019500        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
019600        MOVE NOO                TO KEYS-SW                                
019700     END-IF                                                               
019800                                                                          
019900     IF KEYS-OK                                                           
020000        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
020100        IF REQU-IDARTNR-KEY    NOT NUMERIC                                
020200           MOVE '024'           TO RESP-IDMSG-ERROR                       
020300*          NOT NUMERIC ***                                                
020400           MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                      
020500           MOVE NOO             TO KEYS-SW                                
020600        ELSE                                                              
020700           IF REQU-IDARTNR-KEY NOT > ZERO                                 
020800              MOVE '022'        TO RESP-IDMSG-ERROR                       
020900*             INVALID KEY ***                                             
021000              MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                      
021100              MOVE NOO          TO KEYS-SW                                
021200           ELSE                                                           
021300              MOVE REQU-IDARTNR-KEY                                       
021400                                TO W-IDARTNR                              
021500           END-IF                                                         
021600        END-IF                                                            
021700        IF REQU-FLANSK = YES OR NOO                                       
021800           CONTINUE                                                       
021900        ELSE                                                              
022000           MOVE '022'        TO RESP-IDMSG-ERROR                          
022100*          INVALID FLAG***                                                
022200           MOVE 'FLANSK'     TO RESP-IDELMT-ERROR                         
022300           MOVE NOO          TO KEYS-SW                                   
022400        END-IF                                                            
022500*                                                                         
022600        IF REQU-FLLOGG = YES OR NOO                                       
022700           CONTINUE                                                       
022800        ELSE                                                              
022900           MOVE '022'        TO RESP-IDMSG-ERROR                          
023000*          INVALID FLAG***                                                
023100           MOVE 'FLLOGG '    TO RESP-IDELMT-ERROR                         
023200           MOVE NOO          TO KEYS-SW                                   
023300        END-IF                                                            
023400     END-IF                                                               
023500                                                                          
023600     IF KEYS-OK                                                           
023700        IF REQU-FLLOGG = YES                                              
023800           IF REQU-IDSKYLT-KEY > SPACES AND                               
023900              REQU-GODK-IDSKYLT                                           
024000              CONTINUE                                                    
024100           ELSE                                                           
024200              MOVE '022'        TO RESP-IDMSG-ERROR                       
024300*             INVALID NATIONALITY LANG****                                
024400              MOVE 'IDSKYLT'    TO RESP-IDELMT-ERROR                      
024500              MOVE NOO          TO KEYS-SW                                
024600           END-IF                                                         
024700                                                                          
024800           IF REQU-IDDOKTYP-KEY > SPACES                                  
024900              CONTINUE                                                    
025000           ELSE                                                           
025100              MOVE '022'        TO RESP-IDMSG-ERROR                       
025200*             INVALID DOC TYPE - CAMPNOT FROM API***                      
025300              MOVE 'IDDOKTYP'   TO RESP-IDELMT-ERROR                      
025400              MOVE NOO          TO KEYS-SW                                
025500           END-IF                                                         
025600                                                                          
025700           IF REQU-IDDOK-KEY    > SPACES                                  
025800              CONTINUE                                                    
025900           ELSE                                                           
026000              MOVE '022'        TO RESP-IDMSG-ERROR                       
026100*             INVALID DOC ID ***                                          
026200              MOVE 'IDDOK   '   TO RESP-IDELMT-ERROR                      
026300              MOVE NOO          TO KEYS-SW                                
026400           END-IF                                                         
026500                                                                          
026600           IF REQU-TEINFO  > SPACES                                       
026700              CONTINUE                                                    
026800           ELSE                                                           
026900              MOVE '022'        TO RESP-IDMSG-ERROR                       
027000*             TEXT INFO MANDATORY***                                      
027100              MOVE 'TEINFO  '   TO RESP-IDELMT-ERROR                      
027200              MOVE NOO          TO KEYS-SW                                
027300           END-IF                                                         
027400        END-IF                                                            
027500     END-IF                                                               
027600     .                                                                    
027700     EJECT                                                                
027800                                                                          
027900 H-UPDATE SECTION.                                                        
028000     MOVE 'H-UPDATE'     TO CURR-SECTION                                  
028100                                                                          
028200     IF REQU-FLANSK = YES                                                 
028300        PERFORM HA-UPD-ANSK                                               
028400     END-IF                                                               
028500*                                                                         
028600     IF REQU-FLLOGG = YES                                                 
028700        PERFORM HB-UPD-LOGG                                               
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100                                                                          
029200 HA-UPD-ANSK SECTION.                                                     
029300     MOVE 'HA-UPD-ANSK'  TO CURR-SECTION                                  
029400                                                                          
029500     INITIALIZE CDCU-W200CDCU                                             
029600     MOVE W-IDARTNR              TO CDCU-IDARTNR                          
029700     CALL W200CDCU USING CDCU-W200CDCU                                    
029800                         CDCU-WDK6-PCB                                    
029900                                                                          
030000     IF CDCU-KDSVAR-OK                                                    
030100        CONTINUE                                                          
030200     ELSE                                                                 
030300*       ERROR FROM SUB-PGM*****                                           
030400        MOVE CDCU-IDMSG-ERROR     TO RESP-IDMSG-ERROR                     
030500        MOVE CDCU-IDELMT-ERROR    TO RESP-IDELMT-ERROR                    
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900                                                                          
031000 HB-UPD-LOGG SECTION.                                                     
031100     MOVE 'HB-UPD-LOGG'  TO CURR-SECTION                                  
031200                                                                          
031300     INITIALIZE LOGU-W200LOGU                                             
031400     MOVE REQU-IDSKYLT-KEY        TO LOGU-IDSKYLT                         
031500     MOVE REQU-IDDOKTYP-KEY       TO LOGU-IDDOKTYP                        
031600     MOVE REQU-IDDOK-KEY          TO LOGU-IDDOK                           
031700     MOVE WS-IDUSER               TO LOGU-IDUSER                          
031800     MOVE REQU-TEINFO             TO LOGU-TEINFO                          
031900     CALL W200LOGU USING LOGU-W200LOGU                                    
032000                         LOGU-WDP5-PCB                                    
032100                                                                          
032200     IF LOGU-KDSVAR-OK                                                    
032300        CONTINUE                                                          
032400     ELSE                                                                 
032500*       ERROR FROM SUB-PGM*****                                           
032600        MOVE LOGU-IDMSG-ERROR     TO RESP-IDMSG-ERROR                     
032700        MOVE LOGU-IDELMT-ERROR    TO RESP-IDELMT-ERROR                    
032800     END-IF                                                               
032900     .                                                                    
033000     EJECT                                                                
033100                                                                          
033200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
033300                                                                          
033400     MOVE 'GETARG'               TO SUB-KDFUNC                            
033500     MOVE 'CARPARTS.PULS.APICONCERNUPDATE' TO SUB-ADDISPABS               
033600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
033700                                                                          
033800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
033900                                                                          
034000     IF SUB-KDRC > 0                                                      
034100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
034200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
034300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
034400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034800                                                                          
034900 S02-RETURN-RESPONSE SECTION.                                             
035000                                                                          
035100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
035200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
035300                                                                          
035400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
035500                                                                          
035600     IF SUB-KDRC > 0                                                      
035700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
035800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
035900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
036000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400                                                                          
036500 S11-MSG-CONV SECTION.                                                    
036600     MOVE SPACES                  TO RESP-MESSAGES (1)                    
036700                                     RESP-MESSAGES (2)                    
036800     MOVE 1                       TO MSG-IX                               
036900*    REQUEST OK                                                           
037000     MOVE 200                     TO RESP-KDSTATUS-API                    
037100     IF RESP-IDMSG-INFO > SPACE                                           
037200       MOVE SPACES                TO MSG-CONV-AREA                        
037300       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
037400       CALL WMSGCONV           USING MSG-CONV-AREA                        
037500       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
037600       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
037700       ADD 1                      TO MSG-IX                               
037800     END-IF                                                               
037900     IF RESP-IDMSG-ERROR > SPACE                                          
038000*      BAD REQUEST                                                        
038100       MOVE 400                   TO RESP-KDSTATUS-API                    
038200       MOVE SPACES                TO MSG-CONV-AREA                        
038300       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
038400       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
038500       CALL WMSGCONV           USING MSG-CONV-AREA                        
038600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
038700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
038800     END-IF                                                               
038900     .                                                                    
039000     EJECT                                                                
