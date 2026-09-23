001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W5017100.                                                
001600 AUTHOR.         ARINDAM METIA.                                           
001700 DATE-WRITTEN.   24/08/22.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        COMMUNICATION FOR STANDARD PRICE API                             
002200*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W5A171                                              
002700*        MID:         W50171I1                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        MOD:         W50171O1                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W5017100'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004110 77  KDRC-DISPLAY                PIC Z(5).                                
004120 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
004130 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
004200                                                                          
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004410 77  MSG-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004500                                                                          
004700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005100                                                                          
005200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005300     88  KEYS-OK                             VALUE 'J'.                   
005400     88  KEYS-WRONG                          VALUE 'N'.                   
005500                                                                          
005510 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005520      88  INDATA-OK                          VALUE 'J'.                   
005530      88  INDATA-FEL                         VALUE 'N'.                   
005540                                                                          
006400     EJECT                                                                
006500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007110     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
007120     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
007120     03  W5017API                PIC X(8)    VALUE 'W5017API'.            
007200     EJECT                                                                
007210                                                                          
007211*    --- PARAMETERS TO ABEND                                              
007212                                                                          
007213 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007214 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007215 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007216                                                                          
007220 01  MESSAGE-CODES.                                                       
007230     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007240     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
007400*                                                                         
007500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
007600*01  -COPY WZ01SUB                                                        
007700                                                                          
007800 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
007900*01  -COPY WMSGCONV                                                       
008000                                                                          
008100 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
008200*01  -COPY WZ01AUTH                                                       
008300                                                                          
008400***  BELOW COPYBOOKS INCLUDES INPUT AND OUTPUT HEADERS                    
008500***  WZ01REQ2 IN INPUT AND WZ01RES2 IN OUTPUT                             
008710*                                                                         
008720 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008730 01  REQU-AREA.                                                           
008740*    03  -COPY W50171I1                                                   
008750     EJECT                                                                
008760 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
008770     SKIP3                                                                
008780 01  RESP-AREA.                                                           
008790*    03  -COPY W50171O1                                                   
008791     EJECT                                                                
008792 01  API-AREA.                                                            
008793*    03  -COPY W5017API                                                   
008794     EJECT                                                                
012900                                                                          
014000 LINKAGE SECTION.                                                         
014010                                                                          
014020*01    -COPY W0009     -PRE MSG-                                          
014030 01  ATAB-PCB                    PIC X.                                   
014031                                                                          
014030 01  API-WDK6-PCB                PIC X.                                   
014400                                                                          
014500     EJECT                                                                
014601 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB API-WDK6-PCB.                 
014602 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB API-WDK6-PCB.                 
014700                                                                          
014900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
015000     IF SUB-KDRC = 0                                                      
015100       PERFORM A-INIT                                                     
015110       IF KEYS-OK                                                         
015200         PERFORM B-CHECK-KEYS                                             
015300         IF KEYS-OK                                                       
015500           PERFORM F-GET-STD-PRICE                                        
015600         END-IF                                                           
015900       END-IF                                                             
016100                                                                          
016200       IF SUB-KDTRANS(1:6) = 'W5A171'                                     
016300         PERFORM S11-MSG-CONV                                             
016310       END-IF                                                             
016320       PERFORM S02-RETURN-RESPONSE                                        
016400     END-IF                                                               
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     MOVE 'A-INIT' TO CURR-SECTION                                        
017400                                                                          
017500     MOVE YES                    TO KEYS-SW                               
017800                                                                          
017900     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
018000                                    RESP-IDMSG-INFO                       
018100                                    RESP-IDELMT-ERROR                     
018400                                                                          
018500     IF SUB-KDTRANS(1:6) = 'W5A171'                                       
018600       MOVE 001                  TO AUTH-KDCALL                           
018700       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
018800                                    REQU-WZ01REQ2                         
018900       IF AUTH-KDRC > 0                                                   
019000         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
019100         MOVE NOO                TO KEYS-SW                               
019200       END-IF                                                             
019800     END-IF                                                               
019900     .                                                                    
020000                                                                          
020100     EJECT                                                                
020200 B-CHECK-KEYS SECTION.                                                    
020300                                                                          
020400     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
020600                                                                          
020700     IF REQU-QUERY                                                        
020800        CONTINUE                                                          
020900     ELSE                                                                 
021000        MOVE '023'              TO RESP-IDMSG-ERROR                       
021100*       WRONG ACTION KEY ***                                              
021200        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
021300        MOVE NOO                TO KEYS-SW                                
021400     END-IF                                                               
021500                                                                          
021600     IF KEYS-OK                                                           
021700        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
021800        IF REQU-IDARTNR-KEY    NOT NUMERIC                                
021900           MOVE '024'           TO RESP-IDMSG-ERROR                       
022000*          NOT NUMERIC ***                                                
022100           MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                      
022200           MOVE NOO             TO KEYS-SW                                
022300        ELSE                                                              
022400           IF REQU-IDARTNR-KEY NOT > ZERO                                 
022500              MOVE '022'        TO RESP-IDMSG-ERROR                       
022600*             INVALID KEY ***                                             
022700              MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                      
022800              MOVE NOO          TO KEYS-SW                                
022900           ELSE                                                           
023000              MOVE REQU-IDARTNR-KEY                                       
023100                                TO RESP-IDARTNR-UT                        
023300           END-IF                                                         
023400        END-IF                                                            
023500     END-IF                                                               
026000     .                                                                    
026100                                                                          
026300 F-GET-STD-PRICE SECTION.                                                 
026500     INITIALIZE API-W5017API                                              
026600     MOVE REQU-IDARTNR-KEY      TO API-IDARTNR-IN                         
026700     CALL W5017API USING API-W5017API                                     
026710                         API-WDK6-PCB                                     
026800     IF API-KDSVAR-OK                                                     
027000        PERFORM FA-POPULATE-OUTPUT                                        
027100     ELSE                                                                 
027200*      ERROR FROM SUB-PGM*****                                            
027300       MOVE API-IDMSG-ERROR    TO RESP-IDMSG-ERROR                        
027400       MOVE API-IDELMT-ERROR   TO RESP-IDELMT-ERROR                       
027500     END-IF                                                               
027600     .                                                                    
027700                                                                          
027800 FA-POPULATE-OUTPUT SECTION.                                              
027900                                                                          
028000     MOVE API-PRARTSTD-OUT     TO RESP-PRARTSTD-UT                        
028010     MOVE API-KDVALISO-OUT     TO RESP-KDVALISO-UT                        
028100     .                                                                    
028200                                                                          
028300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
028400                                                                          
028500     MOVE 'GETARG'               TO SUB-KDFUNC                            
028600     MOVE 'CARPARTS.PULS.APISTANDARDPRICE'  TO SUB-ADDISPABS              
028700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
028800                                                                          
028900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
029000                                                                          
029100     IF SUB-KDRC > 0                                                      
029200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
029300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
029400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
029500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 S02-RETURN-RESPONSE SECTION.                                             
030100                                                                          
030200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
030300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
030400                                                                          
030500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
030600                                                                          
030700     IF SUB-KDRC > 0                                                      
030800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
030900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
031000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500                                                                          
031600 S11-MSG-CONV SECTION.                                                    
031700     MOVE SPACES                  TO RESP-MESSAGES (1)                    
031800                                     RESP-MESSAGES (2)                    
031900     MOVE 1                       TO MSG-IX                               
032000*    REQUEST OK                                                           
032100     MOVE 200                     TO RESP-KDSTATUS-API                    
032200     IF RESP-IDMSG-INFO > SPACE                                           
032300       MOVE SPACES                TO MSG-CONV-AREA                        
032400       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
032500       CALL WMSGCONV           USING MSG-CONV-AREA                        
032600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
032700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
032800       ADD 1                      TO MSG-IX                               
032900     END-IF                                                               
033000     IF RESP-IDMSG-ERROR > SPACE                                          
033100*      BAD REQUEST                                                        
033200       MOVE 400                   TO RESP-KDSTATUS-API                    
033300       MOVE SPACES                TO MSG-CONV-AREA                        
033400       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
033500       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
033600       CALL WMSGCONV           USING MSG-CONV-AREA                        
033700       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
033800       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200                                                                          
