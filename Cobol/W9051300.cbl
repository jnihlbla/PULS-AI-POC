000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9051300.                                                
000300 AUTHOR.         SATHISH THIRUVENGADAM.                                   
000400 DATE-WRITTEN.   MARCH 2023.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:    CARPARTS.PULS.APIGENERALPARTINFO,                           
000800*             CAMPAIGN/CONCERN API                                        
000900*             COMMUNICATION PROGRAM TO GET GENERAL PART INFO.             
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSACTION: W9A513                                              
001300*        REQUEST:     W90513I1                                            
001400*                                                                         
001500*    OUTDATA.                                                             
001600*        RESPONSE:    W90513O1                                            
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300                                                                          
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800 77  IDPGM                       PIC X(08)   VALUE 'W9051300'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003200 77  KDRC-DISPLAY                PIC Z(5).                                
003300 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
003400 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003710 77  IX                          PIC S9(3)   VALUE +0   COMP SYNC.        
003800 77  IX-MAX                      PIC S9(3)   VALUE +100 COMP SYNC.        
003900 77  MSG-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004000                                                                          
004100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004200     88  KEYS-OK                             VALUE 'J'.                   
004300     88  KEYS-WRONG                          VALUE 'N'.                   
004400                                                                          
004500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004600      88  INDATA-OK                          VALUE 'J'.                   
004700      88  INDATA-FEL                         VALUE 'N'.                   
004800                                                                          
004900 77  ALLT-OK-SW                  PIC X       VALUE 'J'.                   
005000      88  ALLT-OK                            VALUE 'J'.                   
005100      88  ALLT-FEL                           VALUE 'N'.                   
005200                                                                          
005300     EJECT                                                                
005400                                                                          
005500 01  WS-IDARTNR                  PIC S9(9)   VALUE ZERO  COMP-3.          
005600                                                                          
005700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005800 01  GENERAL-SUBPROGRAMS.                                                 
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006300     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
006400     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
006500     03  W200PINF                PIC X(8)    VALUE 'W200PINF'.            
006600                                                                          
006700*    --- PARAMETERS TO ABEND                                              
006800                                                                          
006900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007200                                                                          
007300 01  MESSAGE-CODES.                                                       
007400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007500     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
007600                                                                          
007700*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
007900*01  -COPY WZ01SUB                                                        
008000                                                                          
008100 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
008200*01  -COPY WMSGCONV                                                       
008300                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
008500*01  -COPY WZ01AUTH                                                       
008600                                                                          
008700***  BELOW COPYBOOKS INCLUDES INPUT AND OUTPUT HEADERS                    
008800***  WZ01REQ2 IN INPUT AND WZ01RES2 IN OUTPUT                             
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009100 01  REQU-AREA.                                                           
009200*    03  -COPY W90513I1                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009500     SKIP3                                                                
009600 01  RESP-AREA.                                                           
009700*    03  -COPY W90513O1                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'PINF-AREA'.           
010000     SKIP3                                                                
010100 01  PNIF-AREA.                                                           
010200*    03  -COPY W200PINF                                                   
010300     EJECT                                                                
010400                                                                          
010500*                            IMS FUNKTIONSKODER                           
010600*01      -COPY W0003                                                      
010700                                                                          
010800                                                                          
010900 LINKAGE SECTION.                                                         
011000*                                                                         
011100*01    -COPY W0009     -PRE MSG-                                          
011200 01  ATAB-PCB                    PIC X.                                   
011300                                                                          
011400 01  PINF-WDD3-PCB               PIC X.                                   
011500 01  PINF-WDK6-PCB               PIC X.                                   
011600 01  PINF-WDJ1-PCB               PIC X.                                   
011700 01  PINF-WDP3-PCB               PIC X.                                   
011800     EJECT                                                                
011900 PROCEDURE DIVISION USING  MSG-PCB ATAB-PCB                               
012000                           PINF-WDD3-PCB PINF-WDK6-PCB                    
012100                           PINF-WDJ1-PCB PINF-WDP3-PCB.                   
012200 MAIN SECTION.                                                            
012300     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB                               
012400                           PINF-WDD3-PCB PINF-WDK6-PCB                    
012500                           PINF-WDJ1-PCB PINF-WDP3-PCB.                   
012600                                                                          
012700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012800     IF SUB-KDRC = 0                                                      
012900       PERFORM A-INIT                                                     
013000       IF KEYS-OK                                                         
013100         PERFORM B-CHECK-KEYS                                             
013200         IF KEYS-OK                                                       
013300            PERFORM F-GET-PART-INFO                                       
013400         END-IF                                                           
013500       END-IF                                                             
013600                                                                          
013700       IF SUB-KDTRANS(1:6) = 'W9A513'                                     
013800         PERFORM S11-MSG-CONV                                             
013900       END-IF                                                             
014000       PERFORM S02-RETURN-RESPONSE                                        
014100     END-IF                                                               
014200                                                                          
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800     MOVE 'A-INIT' TO CURR-SECTION                                        
014900                                                                          
015000     MOVE YES                    TO KEYS-SW                               
015100                                                                          
015200     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
015300                                    RESP-IDMSG-INFO                       
015400                                    RESP-IDELMT-ERROR                     
015500                                                                          
015600     IF SUB-KDTRANS(1:6) = 'W9A513'                                       
015700       MOVE 001                  TO AUTH-KDCALL                           
015800       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
015900                                    REQU-WZ01REQ2                         
016000       IF AUTH-KDRC > 0                                                   
016100         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
016200         MOVE NOO                TO KEYS-SW                               
016300       END-IF                                                             
016400                                                                          
016500     END-IF                                                               
016600     .                                                                    
016700                                                                          
016800 B-CHECK-KEYS SECTION.                                                    
016900     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
017000                                                                          
017100                                                                          
017200     IF REQU-QUERY                                                        
017300        CONTINUE                                                          
017400     ELSE                                                                 
017500        MOVE '023'              TO RESP-IDMSG-ERROR                       
017600*       WRONG ACTION KEY ***                                              
017700        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
017800        MOVE NOO                TO KEYS-SW                                
017900     END-IF                                                               
018000                                                                          
018100     IF KEYS-OK                                                           
018200        IF REQU-IDARTNR-KEY NOT > ZERO                                    
018300           MOVE '022'        TO RESP-IDMSG-ERROR                          
018400*          INVALID KEY ***                                                
018500           MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                         
018600           MOVE NOO          TO KEYS-SW                                   
018700        ELSE                                                              
018800           MOVE REQU-IDARTNR-KEY                                          
018900                             TO WS-IDARTNR                                
019000                                RESP-IDARTNR-UT                           
019100        END-IF                                                            
019200     END-IF                                                               
019300     .                                                                    
019400                                                                          
019500 F-GET-PART-INFO   SECTION.                                               
019600     MOVE 'F-GET ' TO CURR-SECTION                                        
019700                                                                          
019800     INITIALIZE PINF-W200PINF                                             
019900     MOVE WS-IDARTNR              TO PINF-IDARTNR-IN                      
020000     CALL W200PINF USING PINF-W200PINF                                    
020100                         PINF-WDD3-PCB                                    
020200                         PINF-WDK6-PCB                                    
020300                         PINF-WDJ1-PCB                                    
020400                         PINF-WDP3-PCB                                    
020500                                                                          
020600     IF PINF-KDSVAR-OK                                                    
020700        PERFORM FBA-POPULATE-OUTPUT                                       
020800     ELSE                                                                 
020900*      ERROR FROM SUB-PGM*****                                            
021000        MOVE PINF-IDMSG-ERROR     TO RESP-IDMSG-ERROR                     
021100        MOVE PINF-IDELMT-ERROR    TO RESP-IDELMT-ERROR                    
021200     END-IF                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 FBA-POPULATE-OUTPUT SECTION.                                             
021600     MOVE 'FBA-POPULATE  '  TO CURR-SECTION                               
021700                                                                          
021800     MOVE PINF-BEART-ENG    TO RESP-BEART-ENG                             
021900     MOVE PINF-KDERS        TO RESP-KDERS                                 
022000     MOVE PINF-KDAVT        TO RESP-KDAVT                                 
022100     MOVE PINF-IDANSK       TO RESP-IDANSK                                
022200     MOVE PINF-IDNAMN-ANSK  TO RESP-IDNAMN-ANSK                           
022300     MOVE PINF-KDFARLIG     TO RESP-KDFARLIG                              
022300     MOVE PINF-IDLEVNR-MFG  TO RESP-IDLEVNR-MFG                           
022300     MOVE PINF-IDLEVNR-SHIP TO RESP-IDLEVNR-SHIP                          
022400*                                                                         
022500     MOVE ZERO              TO RESP-KVRADER                               
022600     MOVE +1                TO IX                                         
022700     PERFORM UNTIL ( IX       > IX-MAX OR                                 
022800                     PINF-IDARTNR-SATS (IX) = ZERO )                      
023300       ADD +1               TO RESP-KVRADER                               
022900       MOVE PINF-IDARTNR-SATS (IX)                                        
023000                            TO RESP-IDARTNR-SATS (RESP-KVRADER)           
023100       MOVE PINF-REANTPSA(IX)                                             
023200                            TO RESP-REANTPSA (RESP-KVRADER)               
023310       ADD +1               TO IX                                         
023400     END-PERFORM                                                          
023500     .                                                                    
023600     EJECT                                                                
023700*    --- DISPATCHER SECTIONS                                              
023800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
023900     MOVE 'S01-FETCH     '  TO CURR-SECTION                               
024000                                                                          
024100     MOVE 'GETARG'               TO SUB-KDFUNC                            
024200     MOVE 'CARPARTS.PULS.APIGENERALPARTINFO' TO SUB-ADDISPABS             
024300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
024400                                                                          
024500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
024600                                                                          
024700     IF SUB-KDRC > 0                                                      
024800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
024900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
025000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
025100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025200     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500                                                                          
025600 S02-RETURN-RESPONSE SECTION.                                             
025700     MOVE 'S02-RETURN    '  TO CURR-SECTION                               
025800                                                                          
025900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
026000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
026100                                                                          
026200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
026300                                                                          
026400     IF SUB-KDRC > 0                                                      
026500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
026600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
026700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
026800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200                                                                          
027300 S11-MSG-CONV SECTION.                                                    
027400     MOVE 'S11-MSG-CONV  '  TO CURR-SECTION                               
027500                                                                          
027600     MOVE SPACES                  TO RESP-MESSAGES (1)                    
027700                                     RESP-MESSAGES (2)                    
027800     MOVE 1                       TO MSG-IX                               
027900*    REQUEST OK                                                           
028000     MOVE 200                     TO RESP-KDSTATUS-API                    
028100     IF RESP-IDMSG-INFO > SPACE                                           
028200       MOVE SPACES                TO MSG-CONV-AREA                        
028300       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
028400       CALL WMSGCONV           USING MSG-CONV-AREA                        
028500       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
028600       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
028700       ADD 1                      TO MSG-IX                               
028800     END-IF                                                               
028900     IF RESP-IDMSG-ERROR > SPACE                                          
029000*      BAD REQUEST                                                        
029100       MOVE 400                   TO RESP-KDSTATUS-API                    
029200       MOVE SPACES                TO MSG-CONV-AREA                        
029300       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
029400       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
029500       CALL WMSGCONV           USING MSG-CONV-AREA                        
029600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
029700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
029800     END-IF                                                               
029900     .                                                                    
