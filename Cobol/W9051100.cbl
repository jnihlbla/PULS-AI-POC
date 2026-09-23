000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9051100.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   20230207.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:    CARPARTS.PULS.APICONCSTOCKINFO,                             
000800*             CAMPAIGN/CONCERN API                                        
000900*             COMMUNICATION PROGRAM TO GET STOCK BALANCE                  
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W9A511                                              
001500*        REQUEST:     W90511I1                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        RESPONSE:    W90511O1                                            
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
003100 77  IDPGM                       PIC X(08)   VALUE 'W9051100'.            
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
004200 77  INDX                        PIC S9(3)   VALUE +0   COMP SYNC.        
004300 77  MSG-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004400                                                                          
004500 77  WS-CDC-SE                   PIC  X(2)   VALUE '11'.                  
004600 77  SW-URVAL-OK                 PIC  X(1)   VALUE SPACE.                 
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005300      88  INDATA-OK                          VALUE 'J'.                   
005400      88  INDATA-FEL                         VALUE 'N'.                   
005500                                                                          
005600 77  ALLT-OK-SW                  PIC X       VALUE 'J'.                   
005700      88  ALLT-OK                            VALUE 'J'.                   
005800      88  ALLT-FEL                           VALUE 'N'.                   
005900                                                                          
006000 77  DC-QUERY-SW                 PIC X       VALUE 'N'.                   
006100      88  SINGLE-DC-QUERY                    VALUE 'J'.                   
006200                                                                          
006300 77  DC-RANGE-SW                 PIC X       VALUE 'N'.                   
006400      88  NO-CDC-RANGE                       VALUE 'J'.                   
006500                                                                          
006600     EJECT                                                                
006700 01  WS-IDDC-SPAR                PIC X(2)    VALUE SPACES.                
006800                                                                          
006900 01  WS-API-DATE.                                                         
007000     03  FILLER                  PIC X(2)    VALUE '20'.                  
007100     03  WS-API-YEAR             PIC 9(2).                                
007200     03  FILLER                  PIC X(1)    VALUE '-'.                   
007300     03  WS-API-MONTH            PIC 9(2).                                
007400     03  FILLER                  PIC X(1)    VALUE '-'.                   
007500     03  WS-API-DAY              PIC 9(2).                                
007600     EJECT                                                                
007700                                                                          
007800                                                                          
007900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008000 01  GENERAL-SUBPROGRAMS.                                                 
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008500     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
008600     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
008700     03  W200DCST                PIC X(8)    VALUE 'W200DCST'.            
008800                                                                          
008900*    --- PARAMETERS TO ABEND                                              
009000                                                                          
009100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009400                                                                          
009500 01  MESSAGE-CODES.                                                       
009600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009700     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
009800                                                                          
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010100*01  -COPY WZ01SUB                                                        
010200                                                                          
010300 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
010400*01  -COPY WMSGCONV                                                       
010500                                                                          
010600 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
010700*01  -COPY WZ01AUTH                                                       
010800                                                                          
010900***  BELOW COPYBOOKS INCLUDES INPUT AND OUTPUT HEADERS                    
011000***  WZ01REQ2 IN INPUT AND WZ01RES2 IN OUTPUT                             
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011300 01  REQU-AREA.                                                           
011400*    03  -COPY W90511I1                                                   
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011700     SKIP3                                                                
011800 01  RESP-AREA.                                                           
011900*    03  -COPY W90511O1                                                   
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'DCST-AREA'.           
012200     SKIP3                                                                
012300 01  DCST-AREA.                                                           
012400*    03  -COPY W200DCST                                                   
012500     EJECT                                                                
012600***                                                                       
012700***  VALID IDDC CODES                                                     
012800***                                                                       
012900*01  -COPY WWDC99                                                         
013000     EJECT                                                                
013100                                                                          
013200******************************************************************        
013300*****                                                                     
013400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500*****                                                                     
013600 01  IMS-KEYS.                                                            
013700   03    FILLER          PIC X(16)   VALUE 'IMS KEYS        '.            
013800                                                                          
013900 01    NYCKLAR-TILL-DLI.                                                  
014000   03    W-IDARTNR-X.                                                     
014100     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
014200                                                                          
014300   03    W-IDDC-X.                                                        
014400     05    W-IDDC                PIC  X(2)   VALUE SPACES.                
014500                                                                          
014600   03    W-IDDC-MIN-X.                                                    
014700     05    W-IDDC-MIN            PIC  X(2)   VALUE SPACES.                
014800                                                                          
014900   03    W-IDDC-MAX-X.                                                    
015000     05    W-IDDC-MAX            PIC  X(2)   VALUE SPACES.                
015100*                                                                         
015200                                                                          
015300 01  IMS-WS.                                                              
015400   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
015500*****                    **** STATUS-KOD FRÅN IMS                         
015600   03    STATUS-WS       PIC XX.                                          
015700         88  SEGMENT-FOUND       VALUE '  '.                              
015800         88  SEGMENT-NOMORE      VALUE 'GE' 'GB'.                         
015900                                                                          
016000   03    GOOD-STATUSCODES.                                                
016100     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200                                                                          
016300 01      SSA1            PIC X(128) VALUE SPACE.                          
016400 01      SSA2            PIC X(128) VALUE SPACE.                          
016500                                                                          
016600*                            IMS FUNKTIONSKODER                           
016700*01      -COPY W0003                                                      
016800                                                                          
016900 01    FILLER            PIC X(16)   VALUE 'IO-WDK601'.                   
017000 01    DLI-IO-AREA-WDK601.                                                
017100*  03  -COPY WDK601.                                                      
017200                                                                          
017300 01    FILLER            PIC X(16)   VALUE 'IO-WDK611'.                   
017400 01    DLI-IO-AREA-WDK611.                                                
017500*  03  -COPY WDK611.                                                      
017600                                                                          
017700 01    FILLER            PIC X(16)   VALUE 'IO-WDK701'.                   
017800 01    DLI-IO-AREA-WDK701.                                                
017900*  03  -COPY WDK701.                                                      
018000                                                                          
018100 01    FILLER            PIC X(16)   VALUE 'IO-WDK711'.                   
018200 01    DLI-IO-AREA-WDK711.                                                
018300*  03  -COPY WDK711.                                                      
018400*                                                                         
018500                                                                          
018600 LINKAGE SECTION.                                                         
018700*                                                                         
018800*01    -COPY W0009     -PRE MSG-                                          
018900 01  ATAB-PCB                    PIC X.                                   
019000                                                                          
019100*01    -COPY W0008     -PRE WDK6-                                         
019200     05  FILLER                  PIC X.                                   
019300*01    -COPY W0008     -PRE WDK7-                                         
019400     05  FILLER                  PIC X.                                   
019500 01  DCST-WDB6-PCB               PIC X.                                   
019600 01  DCST-WDK6-PCB               PIC X.                                   
019700 01  DCST-WDK7-PCB               PIC X.                                   
019800 01  DCST-WDK9-PCB               PIC X.                                   
019900 01  DCST-WDQ4C-PCB              PIC X.                                   
020000 01  DCST-WDM2A-PCB              PIC X.                                   
020100     EJECT                                                                
020200 PROCEDURE DIVISION USING  MSG-PCB ATAB-PCB  WDK6-PCB  WDK7-PCB           
020300                                   DCST-WDB6-PCB                          
020400                                   DCST-WDK6-PCB                          
020500                                   DCST-WDK7-PCB                          
020600                                   DCST-WDK9-PCB                          
020700                                   DCST-WDQ4C-PCB                         
020800                                   DCST-WDM2A-PCB.                        
020900 MAIN SECTION.                                                            
021000     ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB  WDK6-PCB  WDK7-PCB           
021100                                   DCST-WDB6-PCB                          
021200                                   DCST-WDK6-PCB                          
021300                                   DCST-WDK7-PCB                          
021400                                   DCST-WDK9-PCB                          
021500                                   DCST-WDQ4C-PCB                         
021600                                   DCST-WDM2A-PCB.                        
021700                                                                          
021800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
021900     IF SUB-KDRC = 0                                                      
022000       PERFORM A-INIT                                                     
022100       IF KEYS-OK                                                         
022200         PERFORM B-CHECK-KEYS                                             
022300         IF KEYS-OK                                                       
022400            PERFORM F-GET-PART-INFO                                       
022500         END-IF                                                           
022600       END-IF                                                             
022700                                                                          
022800       IF SUB-KDTRANS(1:6) = 'W9A511'                                     
022900         PERFORM S11-MSG-CONV                                             
023000       END-IF                                                             
023100       PERFORM S02-RETURN-RESPONSE                                        
023200     END-IF                                                               
023300                                                                          
023400     MOVE ZERO TO RETURN-CODE                                             
023500     GOBACK                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 A-INIT SECTION.                                                          
023900     MOVE 'A-INIT' TO CURR-SECTION                                        
024000                                                                          
024100     MOVE YES                    TO KEYS-SW                               
024200     MOVE NOO                    TO DC-QUERY-SW                           
024300                                    DC-RANGE-SW                           
024400                                                                          
024500     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
024600                                    RESP-IDMSG-INFO                       
024700                                    RESP-IDELMT-ERROR                     
024800                                    WS-IDDC                               
024900     MOVE ZERO                   TO RESP-KVRADER                          
025000                                                                          
025100     IF SUB-KDTRANS(1:6) = 'W9A511'                                       
025200       MOVE 001                  TO AUTH-KDCALL                           
025300       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
025400                                    REQU-WZ01REQ2                         
025500       IF AUTH-KDRC > 0                                                   
025600         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
025700         MOVE NOO                TO KEYS-SW                               
025800       END-IF                                                             
025900                                                                          
026000       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY-MIN)     TO                
026100                                 REQU-IDDC-KEY-MIN                        
026200       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY-MAX)     TO                
026300                                 REQU-IDDC-KEY-MAX                        
026400     END-IF                                                               
026500     .                                                                    
026600                                                                          
026700 B-CHECK-KEYS SECTION.                                                    
026800     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
026900                                                                          
027000                                                                          
027100     IF REQU-QUERY                                                        
027200        CONTINUE                                                          
027300     ELSE                                                                 
027400        MOVE '023'              TO RESP-IDMSG-ERROR                       
027500*       WRONG ACTION KEY ***                                              
027600        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
027700        MOVE NOO                TO KEYS-SW                                
027800     END-IF                                                               
027900                                                                          
028000     IF KEYS-OK                                                           
028100        INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO          
028200        IF REQU-IDARTNR-KEY    NOT NUMERIC                                
028300           MOVE '024'           TO RESP-IDMSG-ERROR                       
028400*          NOT NUMERIC ***                                                
028500           MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                      
028600           MOVE NOO             TO KEYS-SW                                
028700        ELSE                                                              
028800           IF REQU-IDARTNR-KEY NOT > ZERO                                 
028900              MOVE '022'        TO RESP-IDMSG-ERROR                       
029000*             INVALID KEY ***                                             
029100              MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                      
029200              MOVE NOO          TO KEYS-SW                                
029300           ELSE                                                           
029400              MOVE REQU-IDARTNR-KEY                                       
029500                                TO W-IDARTNR                              
029600                                   RESP-IDARTNR-UT                        
029700           END-IF                                                         
029800        END-IF                                                            
029900     END-IF                                                               
030000                                                                          
030100     IF REQU-IDDC-KEY-MIN > SPACES                                        
030200        MOVE REQU-IDDC-KEY-MIN   TO W-IDDC-MIN                            
030300     ELSE                                                                 
030400        MOVE LOW-VALUES          TO W-IDDC-MIN                            
030500     END-IF                                                               
030600     IF REQU-IDDC-KEY-MAX > SPACES                                        
030700        MOVE REQU-IDDC-KEY-MAX   TO W-IDDC-MAX                            
030800     ELSE                                                                 
030900        MOVE HIGH-VALUES         TO W-IDDC-MAX                            
031000     END-IF                                                               
031100*                                                                         
031200     IF W-IDDC-MIN > W-IDDC-MAX                                           
031300        MOVE W-IDDC-MAX         TO WS-IDDC-SPAR                           
031400        MOVE W-IDDC-MIN         TO W-IDDC-MAX                             
031500        MOVE WS-IDDC-SPAR       TO W-IDDC-MIN                             
031600     END-IF                                                               
031700*                                                                         
031800     IF W-IDDC-MIN > SPACES                                               
031900        IF W-IDDC-MIN = W-IDDC-MAX                                        
032000           SET SINGLE-DC-QUERY   TO TRUE                                  
032100           MOVE W-IDDC-MIN       TO WS-IDDC                               
032200        END-IF                                                            
032300     END-IF                                                               
032400     .                                                                    
032500                                                                          
032600 F-GET-PART-INFO SECTION.                                                 
032700     MOVE ZERO                       TO RESP-KVRADER                      
032800     IF SINGLE-DC-QUERY                                                   
032900        IF CDC-SE                                                         
033000           PERFORM FA-GET-ARTCLE-CDC                                      
033100        ELSE                                                              
033200           PERFORM FB-GET-ARTCLE-XDC                                      
033300        END-IF                                                            
033400     ELSE                                                                 
033500        IF (W-IDDC-MIN <= WS-CDC-SE                                       
033600        AND W-IDDC-MAX >= WS-CDC-SE )                                     
033700            PERFORM FA-GET-ARTCLE-CDC                                     
033800            PERFORM FB-GET-ARTCLE-XDC                                     
033900        ELSE                                                              
034000            SET NO-CDC-RANGE         TO TRUE                              
034100            PERFORM FB-GET-ARTCLE-XDC                                     
034200        END-IF                                                            
034300     END-IF                                                               
034400     .                                                                    
034500     EJECT                                                                
034600 FA-GET-ARTCLE-CDC SECTION.                                               
034700                                                                          
034800     INITIALIZE DCST-W200DCST                                             
034900     PERFORM IMS-GU-WDK601                                                
035000     IF SEGMENT-FOUND                                                     
035100        MOVE W-IDARTNR               TO DCST-IDARTNR-IN                   
035200        MOVE WS-CDC-SE               TO DCST-IDDC-IN                      
035300        CALL W200DCST USING DCST-W200DCST                                 
035400                            DCST-WDB6-PCB                                 
035500                            DCST-WDK6-PCB                                 
035600                            DCST-WDK7-PCB                                 
035700                            DCST-WDK9-PCB                                 
035800                            DCST-WDQ4C-PCB                                
035900                            DCST-WDM2A-PCB                                
036000                                                                          
036100        IF DCST-KDSVAR-OK                                                 
036200           ADD 1                     TO RESP-KVRADER                      
036300           PERFORM FBA-POPULATE-OUTPUT                                    
036400        ELSE                                                              
036500*         ERROR FROM SUB-PGM*****                                         
036600           MOVE DCST-IDMSG-ERROR     TO RESP-IDMSG-ERROR                  
036700           MOVE DCST-IDELMT-ERROR    TO RESP-IDELMT-ERROR                 
036800        END-IF                                                            
036900     ELSE                                                                 
037000        MOVE '025'                   TO RESP-IDMSG-ERROR                  
037100*       NOT FOUND ***                                                     
037200        MOVE 'IDARTNR'               TO RESP-IDELMT-ERROR                 
037300        MOVE NOO                     TO KEYS-SW                           
037400     END-IF                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 FB-GET-ARTCLE-XDC SECTION.                                               
037800                                                                          
037900     PERFORM IMS-GU-WDK701                                                
038000     IF SEGMENT-FOUND                                                     
038100       PERFORM IMS-GNP-WDK711                                             
038200       PERFORM UNTIL SEGMENT-NOMORE OR DCST-KDSVAR-FEL                    
038300         INITIALIZE DCST-W200DCST                                         
038400         MOVE W-IDARTNR              TO DCST-IDARTNR-IN                   
038500         MOVE SLAG-IDDC              TO DCST-IDDC-IN                      
038600         CALL W200DCST USING DCST-W200DCST                                
038700                             DCST-WDB6-PCB                                
038800                             DCST-WDK6-PCB                                
038900                             DCST-WDK7-PCB                                
039000                             DCST-WDK9-PCB                                
039100                             DCST-WDQ4C-PCB                               
039200                             DCST-WDM2A-PCB                               
039300                                                                          
039400         IF DCST-KDSVAR-OK                                                
039500            ADD 1                    TO RESP-KVRADER                      
039600            PERFORM FBA-POPULATE-OUTPUT                                   
039700         ELSE                                                             
039800*          ERROR FROM SUB-PGM*****                                        
039900           MOVE DCST-IDMSG-ERROR     TO RESP-IDMSG-ERROR                  
040000           MOVE DCST-IDELMT-ERROR    TO RESP-IDELMT-ERROR                 
040100         END-IF                                                           
040200         PERFORM IMS-GNP-WDK711                                           
040300       END-PERFORM                                                        
040400     ELSE                                                                 
040500       IF SINGLE-DC-QUERY OR NO-CDC-RANGE                                 
040600          MOVE '025'                 TO RESP-IDMSG-ERROR                  
040700*         NOT FOUND ***                                                   
040800          MOVE 'IDARTNR'             TO RESP-IDELMT-ERROR                 
040900          MOVE NOO                   TO KEYS-SW                           
041000       END-IF                                                             
041100     END-IF                                                               
041200     .                                                                    
041300     EJECT                                                                
041400 FBA-POPULATE-OUTPUT SECTION.                                             
041500                                                                          
041600     MOVE DCST-IDDC         TO RESP-IDDC       (RESP-KVRADER)             
041700     MOVE DCST-KVLS         TO RESP-KVLS       (RESP-KVRADER)             
041800     MOVE DCST-KVDISP       TO RESP-KVDISP     (RESP-KVRADER)             
041900     MOVE DCST-KVOKS-DAG    TO RESP-KVOKS-DAG  (RESP-KVRADER)             
042000     MOVE DCST-KVOKS-BULK   TO RESP-KVOKS-BULK (RESP-KVRADER)             
042100     MOVE DCST-KVROS-DAG    TO RESP-KVROS-DAG  (RESP-KVRADER)             
042200     MOVE DCST-KVROS-BULK   TO RESP-KVROS-BULK (RESP-KVRADER)             
042300     MOVE DCST-KVOKS-PREL   TO RESP-KVOKS-PREL (RESP-KVRADER)             
042400     MOVE DCST-KVEFRS       TO RESP-KVEFRS     (RESP-KVRADER)             
042500     MOVE DCST-KVAKS        TO RESP-KVAKS      (RESP-KVRADER)             
042600     MOVE DCST-KVRESS       TO RESP-KVRESS     (RESP-KVRADER)             
042700     MOVE DCST-KVVORKO      TO RESP-KVVORKO    (RESP-KVRADER)             
042800     MOVE DCST-IDLANDX2     TO RESP-IDLANDX2   (RESP-KVRADER)             
042900     MOVE DCST-FLKAMPART    TO RESP-FLKAMPART  (RESP-KVRADER)             
043000     .                                                                    
043100     EJECT                                                                
043200*    --- DISPATCHER SECTIONS                                              
043300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
043400                                                                          
043500     MOVE 'GETARG'               TO SUB-KDFUNC                            
043600     MOVE 'CARPARTS.PULS.APICONCSTOCKINFO'  TO SUB-ADDISPABS              
043700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
043800                                                                          
043900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
044000                                                                          
044100     IF SUB-KDRC > 0                                                      
044200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
044300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
044400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
044500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
044600     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000 S02-RETURN-RESPONSE SECTION.                                             
045100                                                                          
045200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
045300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
045400                                                                          
045500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
045600                                                                          
045700     IF SUB-KDRC > 0                                                      
045800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
045900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
046000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
046100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500                                                                          
046600 S11-MSG-CONV SECTION.                                                    
046700     MOVE SPACES                  TO RESP-MESSAGES (1)                    
046800                                     RESP-MESSAGES (2)                    
046900     MOVE 1                       TO MSG-IX                               
047000*    REQUEST OK                                                           
047100     MOVE 200                     TO RESP-KDSTATUS-API                    
047200     IF RESP-IDMSG-INFO > SPACE                                           
047300       MOVE SPACES                TO MSG-CONV-AREA                        
047400       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
047500       CALL WMSGCONV           USING MSG-CONV-AREA                        
047600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
047700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
047800       ADD 1                      TO MSG-IX                               
047900     END-IF                                                               
048000     IF RESP-IDMSG-ERROR > SPACE                                          
048100*      BAD REQUEST                                                        
048200       MOVE 400                   TO RESP-KDSTATUS-API                    
048300       MOVE SPACES                TO MSG-CONV-AREA                        
048400       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
048500       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
048600       CALL WMSGCONV           USING MSG-CONV-AREA                        
048700       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
048800       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200                                                                          
049300*    ---                                                                  
049400*    --- IMS SECTION.                                                     
049500*    ---                                                                  
049600 IMS-GU-WDK601 SECTION.                                                   
049700     MOVE 'IMS-GU-WDK601'         TO CURR-IMS-SECTION                     
049800                                                                          
049900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
050000              DELIMITED BY SIZE INTO SSA1                                 
050100     MOVE '  GE'                  TO GOOD-STATUSCODES                     
050200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
050300     MOVE WDK6-STATUS-CODE        TO STATUS-WS                            
050400     PERFORM IMS-STATUS-CHECK                                             
050500     .                                                                    
050600     EJECT                                                                
050700 IMS-GU-WDK701 SECTION.                                                   
050800     MOVE 'IMS-GU-WDK701'         TO CURR-IMS-SECTION                     
050900                                                                          
051000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
051100              DELIMITED BY SIZE INTO SSA1                                 
051200     MOVE '  GE'                  TO GOOD-STATUSCODES                     
051300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
051400     MOVE WDK7-STATUS-CODE        TO STATUS-WS                            
051500     PERFORM IMS-STATUS-CHECK                                             
051600     .                                                                    
051700     EJECT                                                                
051800 IMS-GNP-WDK711  SECTION.                                                 
051900     MOVE 'IMS-GNP-WDK711'        TO CURR-IMS-SECTION                     
052000                                                                          
052100     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
052200                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
052300              DELIMITED BY SIZE INTO SSA1                                 
052400     MOVE '  GEGB'                TO GOOD-STATUSCODES                     
052500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1              
052600     MOVE WDK7-STATUS-CODE        TO STATUS-WS                            
052700     PERFORM IMS-STATUS-CHECK                                             
052800     .                                                                    
052900     EJECT                                                                
053000 IMS-STATUS-CHECK   SECTION.                                              
053100                                                                          
053200     SET STATUS-IX TO 1                                                   
053300     SEARCH GOOD-STATUS                                                   
053400       AT END                                                             
053500         CALL FELLOG                                                      
053600     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
053700       CONTINUE                                                           
053800     END-SEARCH                                                           
053900     .                                                                    
054000*    -COPY WY2000P1                                                       
