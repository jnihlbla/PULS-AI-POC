000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9051400.                                                
000300 AUTHOR.         P GALBAO.                                                
000400 DATE-WRITTEN.   MAR 2023.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:    CARPARTS.PULS.APICDCINFORMATION,                            
000800*             CAMPAIGN/CONCERN API                                        
000900*             COMMUNICATION PROGRAM TO GET CDC INFORMATION                
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W9A514                                              
001500*        REQUEST:     W90514I1                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        RESPONSE:    W90514O1                                            
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
003100 77  IDPGM                       PIC X(08)   VALUE 'W9051400'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
003700 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
003800                                                                          
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100 77  MSG-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004200                                                                          
004300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004400     88  KEYS-OK                             VALUE 'J'.                   
004500     88  KEYS-WRONG                          VALUE 'N'.                   
004600                                                                          
004700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004800      88  INDATA-OK                          VALUE 'J'.                   
004900      88  INDATA-FEL                         VALUE 'N'.                   
005000                                                                          
005100 77  ALLT-OK-SW                  PIC X       VALUE 'J'.                   
005200      88  ALLT-OK                            VALUE 'J'.                   
005300      88  ALLT-FEL                           VALUE 'N'.                   
005400                                                                          
005500     EJECT                                                                
005600                                                                          
005700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005800 01  GENERAL-SUBPROGRAMS.                                                 
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006300     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
006400     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
006500     03  W200CDCI                PIC X(8)    VALUE 'W200CDCI'.            
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
009200*    03  -COPY W90514I1                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009500     SKIP3                                                                
009600 01  RESP-AREA.                                                           
009700*    03  -COPY W90514O1                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'CDCI-AREA'.           
010000     SKIP3                                                                
010100 01  CDCI-AREA.                                                           
010200*    03  -COPY W200CDCI                                                   
010300     EJECT                                                                
010400                                                                          
010500******************************************************************        
010600*****                                                                     
010700*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800*****                                                                     
010900 01  IMS-KEYS.                                                            
011000   03    FILLER          PIC X(16)   VALUE 'IMS KEYS        '.            
011100                                                                          
011200 01    NYCKLAR-TILL-DLI.                                                  
011300   03    W-IDARTNR-X.                                                     
011400     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
011500                                                                          
011600*                                                                         
011700 01  IMS-WS.                                                              
011800   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
011900*****                    **** STATUS-KOD FRÅN IMS                         
012000   03    STATUS-WS       PIC XX.                                          
012100         88  SEGMENT-FOUND       VALUE '  '.                              
012200         88  SEGMENT-NOMORE      VALUE 'GE' 'GB'.                         
012300                                                                          
012400   03    GOOD-STATUSCODES.                                                
012500     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600                                                                          
012700 01      SSA1            PIC X(128) VALUE SPACE.                          
012800                                                                          
012900*                            IMS FUNKTIONSKODER                           
013000*01      -COPY W0003                                                      
013100                                                                          
013200 01    FILLER            PIC X(16)   VALUE 'IO-WDK601'.                   
013300 01    DLI-IO-AREA-WDK601.                                                
013400*  03  -COPY WDK601.                                                      
013500                                                                          
013600 LINKAGE SECTION.                                                         
013700*                                                                         
013800*01    -COPY W0009     -PRE MSG-                                          
013900 01  ATAB-PCB                    PIC X.                                   
014000                                                                          
014100*01    -COPY W0008     -PRE WDK6-                                         
014200     05  FILLER                  PIC X.                                   
014300 01  CDCI-WDK6-PCB               PIC X.                                   
014400 01  CDCI-WDD9-PCB               PIC X.                                   
014500     EJECT                                                                
014600 PROCEDURE DIVISION USING  MSG-PCB ATAB-PCB  WDK6-PCB                     
014700                                   CDCI-WDK6-PCB                          
014800                                   CDCI-WDD9-PCB.                         
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB  WDK6-PCB                     
015100                                   CDCI-WDK6-PCB                          
015200                                   CDCI-WDD9-PCB.                         
015300                                                                          
015400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
015500     IF SUB-KDRC = 0                                                      
015600       PERFORM A-INIT                                                     
015700       IF KEYS-OK                                                         
015800         PERFORM B-CHECK-KEYS                                             
015900         IF KEYS-OK                                                       
016000            PERFORM F-GET-PART-INFO                                       
016100         END-IF                                                           
016200       END-IF                                                             
016300                                                                          
016400       IF SUB-KDTRANS(1:6) = 'W9A514'                                     
016500         PERFORM S11-MSG-CONV                                             
016600       END-IF                                                             
016700       PERFORM S02-RETURN-RESPONSE                                        
016800     END-IF                                                               
016900                                                                          
017000     MOVE ZERO TO RETURN-CODE                                             
017100     GOBACK                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 A-INIT SECTION.                                                          
017500     MOVE 'A-INIT' TO CURR-SECTION                                        
017600                                                                          
017700     MOVE YES                    TO KEYS-SW                               
017800                                                                          
017900     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
018000                                    RESP-IDMSG-INFO                       
018100                                    RESP-IDELMT-ERROR                     
018200                                                                          
018300     IF SUB-KDTRANS(1:6) = 'W9A514'                                       
018400       MOVE 001                  TO AUTH-KDCALL                           
018500       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
018600                                    REQU-WZ01REQ2                         
018700       IF AUTH-KDRC > 0                                                   
018800         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
018900         MOVE NOO                TO KEYS-SW                               
019000       END-IF                                                             
019100                                                                          
019200     END-IF                                                               
019300     .                                                                    
019400                                                                          
019500 B-CHECK-KEYS SECTION.                                                    
019600     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
019700                                                                          
019800                                                                          
019900     IF REQU-QUERY                                                        
020000        CONTINUE                                                          
020100     ELSE                                                                 
020200        MOVE '023'              TO RESP-IDMSG-ERROR                       
020300*       WRONG ACTION KEY ***                                              
020400        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
020500        MOVE NOO                TO KEYS-SW                                
020600     END-IF                                                               
020700                                                                          
020800     IF KEYS-OK                                                           
020900        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
021000        IF REQU-IDARTNR-KEY    NOT NUMERIC                                
021100           MOVE '024'           TO RESP-IDMSG-ERROR                       
021200*          NOT NUMERIC ***                                                
021300           MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                      
021400           MOVE NOO             TO KEYS-SW                                
021500        ELSE                                                              
021600           IF REQU-IDARTNR-KEY NOT > ZERO                                 
021700              MOVE '022'        TO RESP-IDMSG-ERROR                       
021800*             INVALID KEY ***                                             
021900              MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                      
022000              MOVE NOO          TO KEYS-SW                                
022100           ELSE                                                           
022200              MOVE REQU-IDARTNR-KEY                                       
022300                                TO W-IDARTNR                              
022400                                   RESP-IDARTNR-UT                        
022500           END-IF                                                         
022600        END-IF                                                            
022700     END-IF                                                               
022800                                                                          
022900     IF KEYS-OK                                                           
023000        IF REQU-IDARTNR-KEY NOT NUMERIC                                   
023100           MOVE '024'        TO RESP-IDMSG-ERROR                          
023200*          NOT NUMERIC ***                                                
023300           MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                         
023400           MOVE NOO          TO KEYS-SW                                   
023500        ELSE                                                              
023600           MOVE REQU-IDARTNR-KEY                                          
023700                             TO W-IDARTNR                                 
023800                                RESP-IDARTNR-UT                           
023900        END-IF                                                            
024000     END-IF                                                               
024100                                                                          
024200     .                                                                    
024300                                                                          
024400 F-GET-PART-INFO SECTION.                                                 
024500                                                                          
024600     PERFORM FA-GET-ARTCLE-CDC                                            
024700     .                                                                    
024800     EJECT                                                                
024900 FA-GET-ARTCLE-CDC SECTION.                                               
025000                                                                          
025100     INITIALIZE CDCI-W200CDCI                                             
025200     PERFORM IMS-GU-WDK601                                                
025300     IF SEGMENT-FOUND                                                     
025400        MOVE W-IDARTNR               TO CDCI-IDARTNR-IN                   
025500        CALL W200CDCI USING CDCI-W200CDCI                                 
025600                            CDCI-WDK6-PCB                                 
025700                            CDCI-WDD9-PCB                                 
025800                                                                          
025900        IF CDCI-KDSVAR-OK                                                 
026000           PERFORM FBA-POPULATE-OUTPUT                                    
026100        ELSE                                                              
026200*         ERROR FROM SUB-PGM*****                                         
026300           MOVE CDCI-IDMSG-ERROR     TO RESP-IDMSG-ERROR                  
026400           MOVE CDCI-IDELMT-ERROR    TO RESP-IDELMT-ERROR                 
026500        END-IF                                                            
026600     ELSE                                                                 
026700        MOVE '025'                   TO RESP-IDMSG-ERROR                  
026800*       NOT FOUND ***                                                     
026900        MOVE 'IDARTNR'               TO RESP-IDELMT-ERROR                 
027000        MOVE NOO                     TO KEYS-SW                           
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 FBA-POPULATE-OUTPUT SECTION.                                             
027500                                                                          
027600     MOVE CDCI-BEFT                TO RESP-BEFT                           
027700     MOVE CDCI-IDARTNR-EMBQ0       TO RESP-IDARTNR-EMBQ0                  
027800     MOVE CDCI-IDARTNR-EMBQ1       TO RESP-IDARTNR-EMBQ1                  
027900     MOVE CDCI-IDARTNR-EMBQ2       TO RESP-IDARTNR-EMBQ2                  
028000     MOVE CDCI-IDARTNR-EMBQ3       TO RESP-IDARTNR-EMBQ3                  
028100     MOVE CDCI-IDPSN               TO RESP-IDPSN                          
028200     MOVE CDCI-KVQPACK-3           TO RESP-KVQPACK-3                      
028300     MOVE CDCI-KVAVROP             TO RESP-KVAVROP                        
028400     MOVE CDCI-TIAVROP-AVS         TO RESP-TIAVROP-AVS                    
028500     MOVE CDCI-VKART               TO RESP-VKART                          
028600     MOVE CDCI-VLARTNTO            TO RESP-VLARTNTO                       
028700     MOVE CDCI-ADLAGOMR            TO RESP-ADLAGOMR                       
028800     MOVE CDCI-ADGANG              TO RESP-ADGANG                         
028900     MOVE CDCI-ADPLATS             TO RESP-ADPLATS                        
029000     MOVE CDCI-KDARTURS            TO RESP-KDARTURS                       
029100     MOVE CDCI-IDLEVNR-MFG         TO RESP-IDLEVNR-MFG                    
029200     MOVE CDCI-IDLEVNR-SHIP        TO RESP-IDLEVNR-SHIP                   
029300     MOVE CDCI-KVVECKOR-LT         TO RESP-KVVECKOR-LT                    
029400     MOVE CDCI-KVDAGAR-TT          TO RESP-KVDAGAR-TT                     
029500     MOVE CDCI-KVDAGAR-INLEV       TO RESP-KVDAGAR-INLEV                  
029600     MOVE CDCI-TIETA               TO RESP-TIETA                          
029700     MOVE CDCI-KDFARLIG            TO RESP-KDFARLIG                       
029800     .                                                                    
029900     EJECT                                                                
030000*    --- DISPATCHER SECTIONS                                              
030100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
030200                                                                          
030300     MOVE 'GETARG'               TO SUB-KDFUNC                            
030400     MOVE 'CARPARTS.PULS.APICDCINFORMATION' TO SUB-ADDISPABS              
030500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
030600                                                                          
030700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
030800                                                                          
030900     IF SUB-KDRC > 0                                                      
031000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
031100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
031200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031400     END-IF                                                               
031500     .                                                                    
031600     EJECT                                                                
031700                                                                          
031800 S02-RETURN-RESPONSE SECTION.                                             
031900                                                                          
032000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
032100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
032200                                                                          
032300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
032400                                                                          
032500     IF SUB-KDRC > 0                                                      
032600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
032700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
032800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
032900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300                                                                          
033400 S11-MSG-CONV SECTION.                                                    
033500     MOVE SPACES                  TO RESP-MESSAGES (1)                    
033600                                     RESP-MESSAGES (2)                    
033700     MOVE 1                       TO MSG-IX                               
033800*    REQUEST OK                                                           
033900     MOVE 200                     TO RESP-KDSTATUS-API                    
034000     IF RESP-IDMSG-INFO > SPACE                                           
034100       MOVE SPACES                TO MSG-CONV-AREA                        
034200       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
034300       CALL WMSGCONV           USING MSG-CONV-AREA                        
034400       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
034500       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
034600       ADD 1                      TO MSG-IX                               
034700     END-IF                                                               
034800     IF RESP-IDMSG-ERROR > SPACE                                          
034900*      BAD REQUEST                                                        
035000       MOVE 400                   TO RESP-KDSTATUS-API                    
035100       MOVE SPACES                TO MSG-CONV-AREA                        
035200       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
035300       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
035400       CALL WMSGCONV           USING MSG-CONV-AREA                        
035500       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
035600       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
036100*    ---                                                                  
036200*    --- IMS SECTION.                                                     
036300*    ---                                                                  
036400 IMS-GU-WDK601 SECTION.                                                   
036500     MOVE 'IMS-GU-WDK601'         TO CURR-IMS-SECTION                     
036600                                                                          
036700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
036800              DELIMITED BY SIZE INTO SSA1                                 
036900     MOVE '  GE'                  TO GOOD-STATUSCODES                     
037000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
037100     MOVE WDK6-STATUS-CODE        TO STATUS-WS                            
037200     PERFORM IMS-STATUS-CHECK                                             
037300     .                                                                    
037400     EJECT                                                                
037500 IMS-STATUS-CHECK   SECTION.                                              
037600                                                                          
037700     SET STATUS-IX TO 1                                                   
037800     SEARCH GOOD-STATUS                                                   
037900       AT END                                                             
038000         CALL FELLOG                                                      
038100     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
038200       CONTINUE                                                           
038300     END-SEARCH                                                           
038400     .                                                                    
038500*    -COPY WY2000P1                                                       
