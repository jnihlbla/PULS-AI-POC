000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9051200.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   20230310.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:    CARPARTS.PULS.APICONCFCOIINFO,                              
000800*             CAMPAIGN/CONCERN API                                        
000900*             COMMUNICATION PROGRAM TO GET FC AND OI                      
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W9A512                                              
001500*        REQUEST:     W90512I1                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        RESPONSE:    W90512O1                                            
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
003100 77  IDPGM                       PIC X(08)   VALUE 'W9051200'.            
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
003700 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
003800                                                                          
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100 77  WS-KVRADER                  PIC S9(9)   VALUE +0.                    
004200 77  IX                          PIC S9(3)   VALUE +0   COMP SYNC.        
004300 77  IX-MAX                      PIC S9(3)   VALUE +6   COMP SYNC.        
004400 77  MSG-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004500                                                                          
004600 77  WS-CDC-SE                   PIC  X(2)   VALUE '11'.                  
004700 77  SW-URVAL-OK                 PIC  X(1)   VALUE SPACE.                 
004800                                                                          
004900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005000     88  KEYS-OK                             VALUE 'J'.                   
005100     88  KEYS-WRONG                          VALUE 'N'.                   
005200                                                                          
005300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005400      88  INDATA-OK                          VALUE 'J'.                   
005500      88  INDATA-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  ALLT-OK-SW                  PIC X       VALUE 'J'.                   
005800      88  ALLT-OK                            VALUE 'J'.                   
005900      88  ALLT-FEL                           VALUE 'N'.                   
006000                                                                          
006100     EJECT                                                                
006200 01  WS-IDDC-SPAR                PIC X(2)    VALUE SPACES.                
006300                                                                          
006400 01  WS-API-DATE.                                                         
006500     03  FILLER                  PIC X(2)    VALUE '20'.                  
006600     03  WS-API-YEAR             PIC 9(2).                                
006700     03  FILLER                  PIC X(1)    VALUE '-'.                   
006800     03  WS-API-MONTH            PIC 9(2).                                
006900     03  FILLER                  PIC X(1)    VALUE '-'.                   
007000     03  WS-API-DAY              PIC 9(2).                                
007100     EJECT                                                                
007200                                                                          
007300                                                                          
007400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007500 01  GENERAL-SUBPROGRAMS.                                                 
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008000     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
008100     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
008200     03  W200FCOI                PIC X(8)    VALUE 'W200FCOI'.            
008300                                                                          
008400*    --- PARAMETERS TO ABEND                                              
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900                                                                          
009000 01  MESSAGE-CODES.                                                       
009100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009200     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
009300                                                                          
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009600*01  -COPY WZ01SUB                                                        
009700                                                                          
009800 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
009900*01  -COPY WMSGCONV                                                       
010000                                                                          
010100 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
010200*01  -COPY WZ01AUTH                                                       
010300                                                                          
010400***  BELOW COPYBOOKS INCLUDES INPUT AND OUTPUT HEADERS                    
010500***  WZ01REQ2 IN INPUT AND WZ01RES2 IN OUTPUT                             
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010800 01  REQU-AREA.                                                           
010900*    03  -COPY W90512I1                                                   
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011200     SKIP3                                                                
011300 01  RESP-AREA.                                                           
011400*    03  -COPY W90512O1                                                   
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'FCOI-AREA'.           
011700     SKIP3                                                                
011800 01  FCOI-AREA.                                                           
011900*    03  -COPY W200FCOI                                                   
012000     EJECT                                                                
012100***                                                                       
012200***  VALID IDDC CODES                                                     
012300***                                                                       
012400*01  -COPY WWDC99                                                         
012500     EJECT                                                                
012600                                                                          
012700******************************************************************        
012800*****                                                                     
012900*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013000*****                                                                     
013100 01  IMS-KEYS.                                                            
013200   03    FILLER          PIC X(16)   VALUE 'IMS KEYS        '.            
013300                                                                          
013400 01    NYCKLAR-TILL-DLI.                                                  
013500   03    W-IDARTNR-X.                                                     
013600     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
013700                                                                          
013800   03    W-IDDC-X.                                                        
013900     05    W-IDDC                PIC  X(2)   VALUE SPACES.                
014000                                                                          
014100   03    W-IDDC-MIN-X.                                                    
014200     05    W-IDDC-MIN            PIC  X(2)   VALUE SPACES.                
014300                                                                          
014400   03    W-IDDC-MAX-X.                                                    
014500     05    W-IDDC-MAX            PIC  X(2)   VALUE SPACES.                
014600*                                                                         
014700                                                                          
014800 01  IMS-WS.                                                              
014900   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
015000*****                    **** STATUS-KOD FRÅN IMS                         
015100   03    STATUS-WS       PIC XX.                                          
015200         88  SEGMENT-FOUND       VALUE '  '.                              
015300         88  SEGMENT-NOMORE      VALUE 'GE' 'GB'.                         
015400                                                                          
015500   03    GOOD-STATUSCODES.                                                
015600     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015700                                                                          
015800 01      SSA1            PIC X(128) VALUE SPACE.                          
015900 01      SSA2            PIC X(128) VALUE SPACE.                          
016000                                                                          
016100*                            IMS FUNKTIONSKODER                           
016200*01      -COPY W0003                                                      
016300                                                                          
016400 01    FILLER            PIC X(16)   VALUE 'IO-WDK601'.                   
016500 01    DLI-IO-AREA-WDK601.                                                
016600*  03  -COPY WDK601.                                                      
016700                                                                          
016800 01    FILLER            PIC X(16)   VALUE 'IO-WDK611'.                   
016900 01    DLI-IO-AREA-WDK611.                                                
017000*  03  -COPY WDK611.                                                      
017100                                                                          
017200 01    FILLER            PIC X(16)   VALUE 'IO-WDK701'.                   
017300 01    DLI-IO-AREA-WDK701.                                                
017400*  03  -COPY WDK701.                                                      
017500                                                                          
017600 01    FILLER            PIC X(16)   VALUE 'IO-WDK711'.                   
017700 01    DLI-IO-AREA-WDK711.                                                
017800*  03  -COPY WDK711.                                                      
017900*                                                                         
018000                                                                          
018100 LINKAGE SECTION.                                                         
018200*                                                                         
018300*01    -COPY W0009     -PRE MSG-                                          
018400 01  ATAB-PCB                    PIC X.                                   
018500                                                                          
018600*01    -COPY W0008     -PRE WDK6-                                         
018700     05  FILLER                  PIC X.                                   
018800*01    -COPY W0008     -PRE WDK7-                                         
018900     05  FILLER                  PIC X.                                   
019000 01  FCOI-WDB6-PCB               PIC X.                                   
019100 01  FCOI-WDK7-PCB               PIC X.                                   
019200 01  FCOI-WDL7-PCB               PIC X.                                   
019300 01  FCOI-WDL8-PCB               PIC X.                                   
019400     EJECT                                                                
019500 PROCEDURE DIVISION USING  MSG-PCB ATAB-PCB  WDK6-PCB  WDK7-PCB           
019600                                   FCOI-WDB6-PCB                          
019700                                   FCOI-WDK7-PCB                          
019800                                   FCOI-WDL7-PCB                          
019900                                   FCOI-WDL8-PCB.                         
020000 MAIN SECTION.                                                            
020100     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB  WDK6-PCB  WDK7-PCB           
020200                                   FCOI-WDB6-PCB                          
020300                                   FCOI-WDK7-PCB                          
020400                                   FCOI-WDL7-PCB                          
020500                                   FCOI-WDL8-PCB.                         
020600                                                                          
020700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
020800     IF SUB-KDRC = 0                                                      
020900       PERFORM A-INIT                                                     
021000       IF KEYS-OK                                                         
021100         PERFORM B-CHECK-KEYS                                             
021200         IF KEYS-OK                                                       
021300            PERFORM F-GET-ORDER-INTAKE                                    
021400         END-IF                                                           
021500       END-IF                                                             
021600                                                                          
021700       IF SUB-KDTRANS(1:6) = 'W9A512'                                     
021800         PERFORM S11-MSG-CONV                                             
021900       END-IF                                                             
022000       PERFORM S02-RETURN-RESPONSE                                        
022100     END-IF                                                               
022200                                                                          
022300     MOVE ZERO TO RETURN-CODE                                             
022400     GOBACK                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 A-INIT SECTION.                                                          
022800     MOVE 'A-INIT' TO CURR-SECTION                                        
022900                                                                          
023000     MOVE YES                    TO KEYS-SW                               
023100                                                                          
023200     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
023300                                    RESP-IDMSG-INFO                       
023400                                    RESP-IDELMT-ERROR                     
023500                                    WS-IDDC                               
023600     MOVE ZERO                   TO RESP-KVRADER                          
023700                                                                          
023800     IF SUB-KDTRANS(1:6) = 'W9A512'                                       
023900       MOVE 001                  TO AUTH-KDCALL                           
024000       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
024100                                    REQU-WZ01REQ2                         
024200       IF AUTH-KDRC > 0                                                   
024300         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
024400         MOVE NOO                TO KEYS-SW                               
024500       END-IF                                                             
024600                                                                          
024700       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY-MIN)     TO                
024800                                 REQU-IDDC-KEY-MIN                        
024900       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY-MAX)     TO                
025000                                 REQU-IDDC-KEY-MAX                        
025100     END-IF                                                               
025200     .                                                                    
025300                                                                          
025400 B-CHECK-KEYS SECTION.                                                    
025500     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
025600                                                                          
025700                                                                          
025800     IF REQU-QUERY                                                        
025900        CONTINUE                                                          
026000     ELSE                                                                 
026100        MOVE '023'              TO RESP-IDMSG-ERROR                       
026200*       WRONG ACTION KEY ***                                              
026300        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
026400        MOVE NOO                TO KEYS-SW                                
026500     END-IF                                                               
026600                                                                          
026700     IF KEYS-OK                                                           
026800        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
026900        IF REQU-IDARTNR-KEY    NOT NUMERIC                                
027000           MOVE '024'           TO RESP-IDMSG-ERROR                       
027100*          NOT NUMERIC ***                                                
027200           MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                      
027300           MOVE NOO             TO KEYS-SW                                
027400        ELSE                                                              
027500           IF REQU-IDARTNR-KEY NOT > ZERO                                 
027600              MOVE '022'        TO RESP-IDMSG-ERROR                       
027700*             INVALID KEY ***                                             
027800              MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                      
027900              MOVE NOO          TO KEYS-SW                                
028000           ELSE                                                           
028100              MOVE REQU-IDARTNR-KEY                                       
028200                                TO W-IDARTNR                              
028300                                   RESP-IDARTNR-UT                        
028400           END-IF                                                         
028500        END-IF                                                            
028600     END-IF                                                               
028700                                                                          
028800     IF REQU-IDDC-KEY-MIN > SPACES                                        
028900        MOVE REQU-IDDC-KEY-MIN  TO W-IDDC-MIN                             
029000     ELSE                                                                 
029100        MOVE LOW-VALUES         TO W-IDDC-MIN                             
029200     END-IF                                                               
029300                                                                          
029400     IF REQU-IDDC-KEY-MAX > SPACES                                        
029500        MOVE REQU-IDDC-KEY-MAX  TO W-IDDC-MAX                             
029600     ELSE                                                                 
029700        MOVE HIGH-VALUES        TO W-IDDC-MAX                             
029800     END-IF                                                               
029900     IF W-IDDC-MIN > W-IDDC-MAX                                           
030000        MOVE W-IDDC-MAX         TO WS-IDDC-SPAR                           
030100        MOVE W-IDDC-MIN         TO W-IDDC-MAX                             
030200        MOVE WS-IDDC-SPAR       TO W-IDDC-MIN                             
030300     END-IF                                                               
030400*                                                                         
030500     IF W-IDDC-MIN > SPACES                                               
030600        IF W-IDDC-MIN = W-IDDC-MAX                                        
030700           MOVE W-IDDC-MIN      TO WS-IDDC                                
030800        END-IF                                                            
030900     END-IF                                                               
031000     .                                                                    
031100                                                                          
031200 F-GET-ORDER-INTAKE SECTION.                                              
031300                                                                          
031400     MOVE ZERO                       TO RESP-KVRADER                      
031500                                                                          
031600     IF CDC-SE                                                            
031700        PERFORM FA-GET-OI-CDC                                             
031800     ELSE                                                                 
031900        PERFORM FB-GET-FC-OI-XDC                                          
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 FA-GET-OI-CDC SECTION.                                                   
032400                                                                          
032500     PERFORM IMS-GU-WDK601                                                
032600     IF SEGMENT-FOUND                                                     
032700        INITIALIZE FCOI-W200FCOI                                          
032800        MOVE W-IDARTNR              TO FCOI-IDARTNR-IN                    
032900        MOVE WS-CDC-SE              TO FCOI-IDDC-IN                       
033000        CALL W200FCOI USING FCOI-W200FCOI                                 
033100                             FCOI-WDB6-PCB                                
033200                             FCOI-WDK7-PCB                                
033300                             FCOI-WDL7-PCB                                
033400                             FCOI-WDL8-PCB                                
033500                                                                          
033600        IF FCOI-KDSVAR-OK                                                 
033700           ADD 1                    TO RESP-KVRADER                       
033800           PERFORM FBA-POPULATE-OUTPUT                                    
033900        ELSE                                                              
034000*          ERROR FROM SUB-PGM*****                                        
034100           MOVE FCOI-IDMSG-ERROR     TO RESP-IDMSG-ERROR                  
034200           MOVE FCOI-IDELMT-ERROR    TO RESP-IDELMT-ERROR                 
034300         END-IF                                                           
034400     ELSE                                                                 
034500       MOVE '025'                    TO RESP-IDMSG-ERROR                  
034600*      NOT FOUND ***                                                      
034700       MOVE 'IDARTNR'                TO RESP-IDELMT-ERROR                 
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 FB-GET-FC-OI-XDC SECTION.                                                
035200                                                                          
035300     PERFORM IMS-GU-WDK701                                                
035400     IF SEGMENT-FOUND                                                     
035500       PERFORM IMS-GNP-WDK711                                             
035600       PERFORM UNTIL SEGMENT-NOMORE OR FCOI-KDSVAR-FEL                    
035700         INITIALIZE FCOI-W200FCOI                                         
035800         MOVE W-IDARTNR              TO FCOI-IDARTNR-IN                   
035900         MOVE SLAG-IDDC              TO FCOI-IDDC-IN                      
036000         CALL W200FCOI USING FCOI-W200FCOI                                
036100                             FCOI-WDB6-PCB                                
036200                             FCOI-WDK7-PCB                                
036300                             FCOI-WDL7-PCB                                
036400                             FCOI-WDL8-PCB                                
036500                                                                          
036600         IF FCOI-KDSVAR-OK                                                
036700            ADD 1                    TO RESP-KVRADER                      
036800            PERFORM FBA-POPULATE-OUTPUT                                   
036900         ELSE                                                             
037000*          ERROR FROM SUB-PGM*****                                        
037100           MOVE FCOI-IDMSG-ERROR     TO RESP-IDMSG-ERROR                  
037200           MOVE FCOI-IDELMT-ERROR    TO RESP-IDELMT-ERROR                 
037300         END-IF                                                           
037400         PERFORM IMS-GNP-WDK711                                           
037500       END-PERFORM                                                        
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 FBA-POPULATE-OUTPUT SECTION.                                             
038000                                                                          
038100     MOVE FCOI-IDDC         TO RESP-IDDC         (RESP-KVRADER)           
038200     MOVE FCOI-IDLANDX2     TO RESP-IDLANDX2     (RESP-KVRADER)           
038300     MOVE FCOI-KVPB-REF     TO RESP-KVPB-REF     (RESP-KVRADER)           
038400     MOVE FCOI-KVOI-RULL-12-CDC                                           
038500                      TO RESP-KVOI-RULL-12-CDC   (RESP-KVRADER)           
038600     MOVE FCOI-AARSFORB-CDC TO RESP-AARSFORB-CDC (RESP-KVRADER)           
038700     MOVE FCOI-KVOI-INNEV   TO RESP-KVOI-INNEV   (RESP-KVRADER)           
038800*                                                                         
038900     MOVE +1                TO IX                                         
039000     PERFORM UNTIL IX > IX-MAX                                            
039100       MOVE FCOI-TIPP (IX)  TO RESP-TIPP         (RESP-KVRADER,IX)        
039200       MOVE FCOI-TIVV-FOM-TOM (IX)                                        
039300                            TO RESP-TIVV-FOM-TOM (RESP-KVRADER,IX)        
039400       MOVE FCOI-KVOI (IX)  TO RESP-KVOI         (RESP-KVRADER,IX)        
039500       ADD +1               TO IX                                         
039600     END-PERFORM                                                          
039700     .                                                                    
039800*    --- DISPATCHER SECTIONS                                              
039900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
040000                                                                          
040100     MOVE 'GETARG'               TO SUB-KDFUNC                            
040200     MOVE 'CARPARTS.PULS.APICONCFCOIINFO'  TO SUB-ADDISPABS               
040300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
040400                                                                          
040500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
040600                                                                          
040700     IF SUB-KDRC > 0                                                      
040800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
040900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
041000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
041100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500                                                                          
041600 S02-RETURN-RESPONSE SECTION.                                             
041700                                                                          
041800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
041900     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
042000                                                                          
042100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
042200                                                                          
042300     IF SUB-KDRC > 0                                                      
042400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
042500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
042600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
042700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042800     END-IF                                                               
042900     .                                                                    
043000     EJECT                                                                
043100                                                                          
043200 S11-MSG-CONV SECTION.                                                    
043300     MOVE SPACES                  TO RESP-MESSAGES (1)                    
043400                                     RESP-MESSAGES (2)                    
043500     MOVE 1                       TO MSG-IX                               
043600*    REQUEST OK                                                           
043700     MOVE 200                     TO RESP-KDSTATUS-API                    
043800     IF RESP-IDMSG-INFO > SPACE                                           
043900       MOVE SPACES                TO MSG-CONV-AREA                        
044000       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
044100       CALL WMSGCONV           USING MSG-CONV-AREA                        
044200       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
044300       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
044400       ADD 1                      TO MSG-IX                               
044500     END-IF                                                               
044600     IF RESP-IDMSG-ERROR > SPACE                                          
044700*      BAD REQUEST                                                        
044800       MOVE 400                   TO RESP-KDSTATUS-API                    
044900       MOVE SPACES                TO MSG-CONV-AREA                        
045000       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
045100       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
045200       CALL WMSGCONV           USING MSG-CONV-AREA                        
045300       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
045400       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
045900*    ---                                                                  
046000*    --- IMS SECTION.                                                     
046100*    ---                                                                  
046200 IMS-GU-WDK601 SECTION.                                                   
046300     MOVE 'IMS-GU-WDK601'         TO CURR-IMS-SECTION                     
046400                                                                          
046500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
046600              DELIMITED BY SIZE INTO SSA1                                 
046700     MOVE '  GE'                  TO GOOD-STATUSCODES                     
046800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
046900     MOVE WDK6-STATUS-CODE        TO STATUS-WS                            
047000     PERFORM IMS-STATUS-CHECK                                             
047100     .                                                                    
047200     EJECT                                                                
047300 IMS-GU-WDK701 SECTION.                                                   
047400     MOVE 'IMS-GU-WDK701'         TO CURR-IMS-SECTION                     
047500                                                                          
047600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
047700              DELIMITED BY SIZE INTO SSA1                                 
047800     MOVE '  GE'                  TO GOOD-STATUSCODES                     
047900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
048000     MOVE WDK7-STATUS-CODE        TO STATUS-WS                            
048100     PERFORM IMS-STATUS-CHECK                                             
048200     .                                                                    
048300     EJECT                                                                
048400 IMS-GNP-WDK711  SECTION.                                                 
048500     MOVE 'IMS-GNP-WDK711'        TO CURR-IMS-SECTION                     
048600                                                                          
048700     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
048800                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
048900              DELIMITED BY SIZE INTO SSA1                                 
049000     MOVE '  GEGB'                TO GOOD-STATUSCODES                     
049100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1              
049200     MOVE WDK7-STATUS-CODE        TO STATUS-WS                            
049300     PERFORM IMS-STATUS-CHECK                                             
049400     .                                                                    
049500     EJECT                                                                
049600 IMS-STATUS-CHECK   SECTION.                                              
049700                                                                          
049800     SET STATUS-IX TO 1                                                   
049900     SEARCH GOOD-STATUS                                                   
050000       AT END                                                             
050100         CALL FELLOG                                                      
050200     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
050300       CONTINUE                                                           
050400     END-SEARCH                                                           
050500     .                                                                    
050600*    -COPY WY2000P1                                                       
