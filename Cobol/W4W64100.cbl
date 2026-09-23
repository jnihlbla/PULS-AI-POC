000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4W64100.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   21/03/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       CHANGE CARRIER ID                                        
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        CHANGE OF CARRIER ID. IDLBBET                                    
001200*                                                                         
001300*        THE PROGRAM UPDATES   WDR5                                       
001400*        THE PROGRAM READS     WDR5                                       
001500*                              WDB6                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W4W641T                                             
001900*                     W4W641U                                             
002000*        REQUEST:     W40641I1                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        RESPONSE:    W40641O1                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700*   -COPY WY2000W1                                                        
003800*   -COPY WWDC99                                                          
003900 77  IDPGM                       PIC X(08)   VALUE 'W4W64100'.            
004000                                                                          
004100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600                                                                          
004700 77  WS-ADRESS-DP                PIC X(50)                                
004800         VALUE 'CARPARTS.DAP.DISTRDOCWEB'.                                
004900 77  WS-RESP-AREA                PIC S9(5) VALUE ZERO COMP-3.             
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  SW-REQU-RAD-IFYLLD          PIC X       VALUE 'N'.                   
005400 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005500 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
005510 77  SW-VISA-RAD                 PIC X       VALUE 'N'.                   
005600 77  WS-KVANT                    PIC S9(3)   VALUE ZERO COMP-3.           
005700 77  WS-ADRESS                   PIC X(50)                                
005800            VALUE 'CARPARTS.NDC.REFILLCARRIERIDHISTORY'.                  
005900 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
006000 77  INDX-DISPLAY                PIC 999     VALUE ZERO.                  
006100 77  MAX-INDX                    PIC S9(4)   VALUE 500  COMP SYNC.        
006200 77  WS-IDDC-SEND-KEY            PIC X(2)    VALUE SPACE.                 
006300 77  WS-IDDC-REC                 PIC X(2)    VALUE SPACE.                 
006400 77  WS-TIFAKT                   PIC X(6).                                
006500 77  TIFAKT-WS REDEFINES WS-TIFAKT PIC 9(6).                              
006600                                                                          
006700 77  W-IDSEKVNR-SAP              PIC S9(3) VALUE 0   COMP-3.              
006800 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
006900 77  W-ANTAL-LASN                PIC S9(7)   VALUE ZERO.                  
007000                                                                          
007100 01  W-TIBERANK-8                PIC 9(8)    VALUE ZERO.                  
007200                                                                          
007300 01  TMP1-YYMMDD                 PIC S9(7)   VALUE ZERO.                  
007400 01  TMP2-YYMMDD                 PIC S9(7)   VALUE ZERO.                  
007500                                                                          
007600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 01  FILLER REDEFINES DAGENS-DATUM.                                       
007800     03  DAGENS-AA               PIC 9(2).                                
007900     03  DAGENS-MM               PIC 9(2).                                
008000     03  DAGENS-DD               PIC 9(2).                                
008100                                                                          
008200 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008300                                                                          
008400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008500     88  INDATA-OK                           VALUE 'J'.                   
008600     88  INDATA-FEL                          VALUE 'N'.                   
008700                                                                          
008800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008900     88  NYCKLAR-OK                          VALUE 'J'.                   
009000     88  NYCKLAR-FEL                         VALUE 'N'.                   
009100                                                                          
009200 01  WLOGG-TID                   PIC S9(9)   VALUE ZERO.                  
009300 01  LOGG-DATUM                  PIC S9(8)   VALUE ZERO.                  
009400                                                                          
009500 01  LOCAL-DATE                  PIC 9(6)    VALUE ZERO.                  
009600 01  FILLER REDEFINES LOCAL-DATE.                                         
009700     03  LOCAL-AA                PIC 9(2).                                
009800     03  LOCAL-MM                PIC 9(2).                                
009900     03  LOCAL-DD                PIC 9(2).                                
010000                                                                          
010100 77  LOCAL-TIME                  PIC 9(8)    VALUE ZERO.                  
010200                                                                          
010300                                                                          
010400 01  W.                                                                   
010500     05  W-KVANT-UPD             PIC S9(3).                               
010600     05  W-KVAVIS                PIC S9(7)   VALUE ZERO COMP-3.           
010700     05  W-CMD                   PIC X(3).                                
010800                                                                          
010900                                                                          
011000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
011100     88  KEYS-OK                             VALUE 'J'.                   
011200     88  KEYS-WRONG                          VALUE 'N'.                   
011300     EJECT                                                                
011400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
011500 01  GENERAL-SUBPROGRAMS.                                                 
011600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB'.             
012000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
012100     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
012200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012300     SKIP3                                                                
012400*    --- PARAMETERS TO ABEND                                              
012500                                                                          
012600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012900     SKIP3                                                                
013000 01  MESSAGE-CODES.                                                       
013100     05  NO-DATA-ENTERED         PIC X(3)   VALUE '014'.                  
013200     05  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
013300     05  IS-INVALID              PIC X(3)   VALUE '023'.                  
013400     05  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
013500     05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
013600     05  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
013700     05  LINES-NOT-FOUND         PIC X(3)   VALUE '027'.                  
013800     05  PRINTING-REQUESTED      PIC X(3)   VALUE '015'.                  
013900     05  TRAILER-RECEIVED        PIC X(3)   VALUE '001'.                  
014000     EJECT                                                                
014100*01  -COPY WZ01SEND                                                       
014200     EJECT                                                                
014300*                                                                         
014400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014500     SKIP3                                                                
014600*01  -COPY WZ01SUB                                                        
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014900     SKIP3                                                                
015000 01  REQU-AREA.                                                           
015100*    03  -COPY WZ01REQU                                                   
015200*    03  -COPY W40641I1                                                   
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015500     SKIP3                                                                
015600 01  RESP-AREA.                                                           
015700*    03  -COPY WZ01RESP                                                   
015800*    03  -COPY W40641O1                                                   
015900     EJECT                                                                
016000*01  -COPY WL01TIDZ                                                       
016100     EJECT                                                                
016400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
016500*                                                                         
016600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016700     SKIP3                                                                
016800 01  KEYS-FOR-DLI.                                                        
016900     03  W-WDGXKEY-6301-X.                                                
017000         05  W-IDHTYP-6301       PIC X(4)      VALUE '6301'.              
017100         05  W-IDDC-6301         PIC X(2)      VALUE SPACE.               
017200         05  W-LOWVALUE-6301     PIC X(24)     VALUE LOW-VALUE.           
017300                                                                          
017400*    03  W-WDGXKEY-6302.                                                  
017500*        05  W-DABERANK-6302      PIC 9(8).                               
017600*        05  W-IDFAKT-6302        PIC S9(7)  COMP-3.                      
017700                                                                          
017800     03  W-KEY6302-X.                                                     
017900         05  W-DABERANK-6302      PIC 9(8).                               
018000         05  W-IDFAKT-6302        PIC S9(7)  COMP-3.                      
018100                                                                          
018200     03  W-IDDC                  PIC X(2).                                
018300                                                                          
018400     03  W-IDLBBET               PIC X(12)   VALUE SPACE.                 
018500                                                                          
018600     03  W-IDDCSEND              PIC X(2)    VALUE SPACE.                 
018700                                                                          
018800     03  W-IDDCLEV               PIC X(2)    VALUE SPACE.                 
018900                                                                          
019000     03  W-IDDC-B6-X.                                                     
019100         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
019200     EJECT                                                                
019300                                                                          
019400*    --- STATUS-KOD FRÅN IMS                                              
019500 01  STATUS-WS                   PIC XX.                                  
019600     88  SEGMENT-FOUND                       VALUE '  '.                  
019700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019900     SKIP2                                                                
020000 01  GOOD-STATUSCODES.                                                    
020100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020200     SKIP3                                                                
020300 01  SSA1                        PIC X(64).                               
020400 01  SSA2                        PIC X(64).                               
020500     EJECT                                                                
020600*    --- IMS FUNCTION CODES                                               
020700*01  -COPY W0003                                                          
020800     EJECT                                                                
020900                                                                          
021000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021100     SKIP3                                                                
021200*01  DLI-IO-AREA-WL6301.                                                  
021300*    03  IO-AREA-WL6301          PIC X(260)  VALUE SPACE.                 
021400*    SKIP3                                                                
021500 01   DLI-IO-AREA-6301.                                                   
021600*     03  -COPY WDGX6301                                                  
021700*                                                                         
021800 01   DLI-IO-AREA-6302.                                                   
021900*     03  -COPY WDGX6302                                                  
022000*                                                                         
022100 01   DLI-IO-AREA-B601.                                                   
022200*     03  -COPY WDB601                                                    
022300*                                                                         
022400 LINKAGE SECTION.                                                         
022500*01  -COPY W0009   -PRE MSG-                                              
022600                                                                          
022700*01  -COPY W0008  -PRE 6301-                                              
022800     05  FILLER                  PIC X.                                   
022900                                                                          
023000*01  -COPY W0008  -PRE WDB6-                                              
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300 PROCEDURE DIVISION  USING MSG-PCB 6301-PCB WDB6-PCB.                     
023400 MAIN SECTION.                                                            
023500     ENTRY 'DLITCBL' USING MSG-PCB 6301-PCB WDB6-PCB.                     
023600                                                                          
023700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
023800     IF SUB-KDRC = 0                                                      
023900       PERFORM A-INIT                                                     
024000       PERFORM B-CHECK-KEYS                                               
024100       IF NYCKLAR-OK                                                      
024200          IF REQU-KDPGMACT = 'E'                                          
024300             PERFORM G-CHECK-INPUT                                        
024400             IF INDATA-OK                                                 
024500                PERFORM H-UPDATE                                          
024600             END-IF                                                       
024700          END-IF                                                          
024800       END-IF                                                             
024900                                                                          
025000       IF NYCKLAR-OK                                                      
025100          PERFORM F-READ-SHOW-INFO                                        
025200       END-IF                                                             
025300       PERFORM S02-RETURN-RESPONSE                                        
025400     END-IF                                                               
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 A-INIT SECTION.                                                          
026000     MOVE ALL '+' TO RESP-AREA                                            
026100     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
026200                     RESP-IDMSG-INFO                                      
026300                     RESP-IDELMT-ERROR                                    
026400     MOVE ZERO    TO RESP-KVRADER                                         
026500     MOVE '001'   TO RESP-IDMSGVER                                        
026600     ACCEPT DAGENS-TID   FROM TIME                                        
026700     ACCEPT DAGENS-DATUM FROM DATE                                        
026800                                                                          
026900     .                                                                    
027000     EJECT                                                                
027100 B-CHECK-KEYS SECTION.                                                    
027200                                                                          
027300     MOVE JA             TO NYCKLAR-SW                                    
027400                                                                          
027500***  CONTROL OF REQU-KDPGMACT                                             
027600     IF REQU-KDPGMACT = 'S' OR 'E'                                        
027700        CONTINUE                                                          
027800     ELSE                                                                 
027900        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
028000        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
028100        MOVE NEJ TO NYCKLAR-SW                                            
028200     END-IF                                                               
028300                                                                          
028400***  CONTROL OF TIFAKT                                                    
028500                                                                          
028600     IF REQU-TIFAKT-KEY = ALL '+'                                         
028700        MOVE ZERO TO WS-TIFAKT                                            
028800     ELSE                                                                 
028900        MOVE REQU-TIFAKT-KEY TO WS-TIFAKT                                 
029000     END-IF                                                               
029100     INSPECT WS-TIFAKT REPLACING LEADING SPACE BY ZERO                    
029200     IF WS-TIFAKT NUMERIC                                                 
029300        CONTINUE                                                          
029400     ELSE                                                                 
029500        MOVE 'TIFAKT' TO RESP-IDELMT-ERROR                                
029600        MOVE NEJ      TO NYCKLAR-SW                                       
029700     END-IF                                                               
029800                                                                          
029900***  CONTROL OF IDDC-REC                                                  
030000                                                                          
030100     IF REQU-IDDC-REC-KEY = ALL '+'                                       
030200        MOVE SPACE              TO WS-IDDC-REC                            
030300     ELSE                                                                 
030400        MOVE REQU-IDDC-REC-KEY TO WS-IDDC-REC                             
030500     END-IF                                                               
030600*                                                                         
030700     IF WS-IDDC-REC      = SPACE                                          
030800        MOVE 'IDDC-REC ' TO RESP-IDELMT-ERROR                             
030900        MOVE NEJ            TO NYCKLAR-SW                                 
031000     ELSE                                                                 
031100        MOVE WS-IDDC-REC      TO W-IDDC-B6                                
031200        PERFORM IMS-GU-WDB601                                             
031300        IF SEGMENT-MISSING                                                
031400           MOVE 'IDDC-REC ' TO RESP-IDELMT-ERROR                          
031500           MOVE NEJ         TO NYCKLAR-SW                                 
031600        END-IF                                                            
031700     END-IF                                                               
031800                                                                          
031900***  CONTROL OF IDLBBET                                                   
032000     IF REQU-IDLBBET-KEY = ALL '+'                                        
032100        MOVE SPACE            TO W-IDLBBET                                
032200     ELSE                                                                 
032300        MOVE REQU-IDLBBET-KEY TO W-IDLBBET                                
032400     END-IF                                                               
032500                                                                          
032600***  CONTROL OF SENDING DC                                                
032700     MOVE REQU-IDDC-KEY   TO WS-IDDC-SEND-KEY                             
032800                             W-IDDCSEND                                   
032900                             W-IDDCLEV                                    
033000                             W-IDDC                                       
033100     IF W-IDDC = ALL '+' OR SPACE                                         
033200       MOVE 'IDDC'   TO RESP-IDELMT-ERROR                                 
033300       MOVE NEJ      TO NYCKLAR-SW                                        
033400     ELSE                                                                 
033500       MOVE W-IDDC   TO W-IDDC-B6                                         
033600       PERFORM IMS-GU-WDB601                                              
033700       IF SEGMENT-FOUND                                                   
033800         IF DCS-DDC                                                       
033900           MOVE 'IDDC'    TO RESP-IDELMT-ERROR                            
034000           MOVE NEJ       TO NYCKLAR-SW                                   
034100         END-IF                                                           
034200       END-IF                                                             
034300     END-IF                                                               
034400                                                                          
034500     MOVE WS-TIFAKT        TO RESP-TIFAKT-KEY                             
034600     MOVE WS-IDDC-REC      TO RESP-IDDC-REC-KEY                           
034700     MOVE W-IDLBBET       TO RESP-IDLBBET-KEY                             
034800     MOVE W-IDDC          TO RESP-IDDC-KEY                                
034900******** ADAPT DATE AND TIME FOR TIMEZONES                                
035000                                                                          
035100     MOVE '011'                TO MSGI-KDCALL                             
035200     MOVE DCS-IDTIDZON     TO MSGI-IDTIDZON                               
035200     MOVE DCS-IDDC         TO MSGI-IDDC                                   
035300     MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                           
035400     MOVE DAGENS-TID           TO MSGI-TILOKTID                           
035500                                  LOCAL-TIME                              
035600     CALL WL01TIDZ USING          MSGI-WL01TIDZ                           
035700     MOVE MSGI-TILOKDAT(1:6)   TO LOCAL-DATE                              
035800     MOVE MSGI-TILOKTID(1:4)   TO LOCAL-TIME(1:4)                         
035900********                                                                  
036000                                                                          
036100     IF RESP-TIFAKT-KEY = ZERO                                            
036200        INSPECT RESP-TIFAKT-KEY REPLACING LEADING ZERO BY SPACE           
036300     END-IF                                                               
036400                                                                          
036500     IF NYCKLAR-FEL                                                       
036600        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
036700           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
036800        ELSE                                                              
036900           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
037000        END-IF                                                            
037100     END-IF                                                               
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037500 F-READ-SHOW-INFO SECTION.                                                
037600                                                                          
037700                                                                          
037800     MOVE 500 TO MAX-INDX                                                 
037900                                                                          
038000     MOVE WS-IDDC-REC     TO W-IDDC-6301                                  
038100     PERFORM IMS-GU-WDGX6301                                              
038200                                                                          
038300     IF SEGMENT-MISSING                                                   
038400        MOVE LINES-NOT-FOUND  TO RESP-IDMSG-ERROR                         
038500        MOVE 'KEY' TO RESP-IDELMT-ERROR                                   
038600     ELSE                                                                 
038700                                                                          
038800****************** FÖRSTA SÖKTA                                           
038900        MOVE ZERO TO INDX                                                 
039000                     WS-KVANT                                             
039100        MOVE NEJ  TO SW-TRAEFF                                            
039110                     SW-VISA-RAD                                          
039200        PERFORM FB-READ-ROWDATA                                           
039300                                                                          
039400       IF SEGMENT-FOUND                                                   
039500****************** SÖKTA FAKTUROR                                         
039600         PERFORM UNTIL SEGMENT-MISSING                                    
039700                                                                          
039800           MOVE 6302-TIFAKT             TO TMP1-YYMMDD                    
039900           MOVE 6302-TIFAKT             TO TMP1-YYMMDD                    
040000           MOVE TIFAKT-WS               TO TMP2-YYMMDD                    
040100           IF TMP2-YYMMDD = ZERO                                          
040200             IF TMP1-YYMMDD >= TMP2-YYMMDD                                
040210               MOVE JA TO SW-VISA-RAD                                     
040240             END-IF                                                       
040300           ELSE                                                           
040400             IF TMP1-YYMMDD = TMP2-YYMMDD                                 
040410               MOVE JA TO SW-VISA-RAD                                     
040420             END-IF                                                       
040430           END-IF                                                         
040440           IF SW-VISA-RAD = JA                                            
040500             IF 6302-KDTRPSTA = SPACE                                     
040600               MOVE JA TO SW-TRAEFF                                       
040700               IF INDX < MAX-INDX                                         
040800                 ADD +1 TO INDX                                           
040900                 MOVE 6302-IDFAKT       TO RESP-IDFAKT(INDX)              
041000                 INSPECT RESP-IDFAKT(INDX) REPLACING LEADING              
041100                      SPACE BY ZERO                                       
041200                 MOVE 6302-IDSHIPM      TO RESP-IDSHIPM(INDX)             
041300                 MOVE 6302-TIFAKT       TO RESP-TIFAKT(INDX)              
041400                 MOVE 6302-DABERANK(3:6) TO RESP-TIBERANK(INDX)           
041500                 MOVE 6302-IDLBBET      TO RESP-IDLBBET(INDX)             
041600                 MOVE 6302-KVKOLLI-FAKT TO                                
041700                      RESP-KVKOLLI-FAKT(INDX)                             
041800                 MOVE 6302-KVRADER-FAKT TO                                
041900                      RESP-KVRADER-FAKT(INDX)                             
042000                                                                          
042100                 IF REQU-KDPGMACT = 'S' OR                                
042200                    ((REQU-KDPGMACT = 'E' OR 'X')                         
042300                      AND INDATA-OK)                                      
042400                    MOVE SPACE     TO RESP-IDMSG-ERROR-LINE(INDX)         
042500                 END-IF                                                   
042600                                                                          
042700               END-IF                                                     
042800               ADD +1 TO WS-KVANT                                         
042900             END-IF                                                       
043100           END-IF                                                         
043200                                                                          
043300           PERFORM FB-READ-ROWDATA                                        
043310           MOVE NEJ TO SW-VISA-RAD                                        
043400         END-PERFORM                                                      
043500         MOVE WS-KVANT TO RESP-KVRADER                                    
043600         IF WS-KVANT > 500                                                
043700            MOVE 500 TO RESP-KVRADER                                      
043800            MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                       
043900         END-IF                                                           
044000         IF SW-TRAEFF = NEJ                                               
044100            MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                      
044200         END-IF                                                           
044300       ELSE                                                               
044400          MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                        
044500       END-IF                                                             
044600     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900 FB-READ-ROWDATA SECTION.                                                 
045000                                                                          
045100     MOVE ZERO     TO W-DABERANK-6302                                     
045200                      W-IDFAKT-6302                                       
045300     IF W-IDLBBET = SPACE                                                 
045400*       PERFORM IMS-GNP-WDGX6302-A                                        
045500       IF WS-IDDC-REC = '41'                                              
045600         PERFORM IMS-GNP-WDGX6302-D                                       
045700       ELSE                                                               
045800         PERFORM IMS-GNP-WDGX6302-C                                       
045900       END-IF                                                             
046000     ELSE                                                                 
046100        PERFORM IMS-GNP-WDGX6302-B                                        
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 G-CHECK-INPUT SECTION.                                                   
046600                                                                          
046700     MOVE JA TO INDATA-SW                                                 
046800     IF REQU-KVRADER NUMERIC                                              
046900     AND REQU-KVRADER > ZERO                                              
047000        MOVE REQU-KVRADER TO MAX-INDX                                     
047100     END-IF                                                               
047200                                                                          
047300     MOVE +1 TO INDX                                                      
047400     MOVE NEJ TO SW-REQU-RAD-IFYLLD                                       
047500     PERFORM UNTIL INDX > MAX-INDX                                        
047600        IF REQU-IDLBBET (INDX) = ALL '+'                                  
047700           CONTINUE                                                       
047800        ELSE                                                              
047900           MOVE JA TO SW-REQU-RAD-IFYLLD                                  
048000           MOVE MAX-INDX TO INDX                                          
048100        END-IF                                                            
048200        ADD +1 TO INDX                                                    
048300     END-PERFORM                                                          
048400                                                                          
048500     IF SW-REQU-RAD-IFYLLD = NEJ                                          
048600        MOVE NO-DATA-ENTERED TO RESP-IDMSG-ERROR                          
048700        MOVE NEJ TO INDATA-SW                                             
048800     ELSE                                                                 
048900        MOVE SPACE TO W-CMD                                               
049000        MOVE +1 TO INDX                                                   
049100        PERFORM UNTIL INDX > MAX-INDX                                     
049200**            VALIDATION FOR IDLBBET                                      
049300           IF REQU-IDLBBET (INDX) NOT = ALL  '+'                          
049400             CONTINUE                                                     
049500           ELSE                                                           
049600             IF REQU-IDLBBET (INDX) = SPACE                               
049700               MOVE NEJ TO INDATA-SW                                      
049800               MOVE INDX TO INDX-DISPLAY                                  
049900               STRING 'IDLBBET *' INDX-DISPLAY                            
050000                 DELIMITED BY SIZE INTO RESP-IDELMT-ERROR                 
050100               MOVE 'IDL' TO       RESP-IDMSG-ERROR-LINE(INDX)            
050200             END-IF                                                       
050300           END-IF                                                         
050400           ADD +1 TO INDX                                                 
050500        END-PERFORM                                                       
050600        IF INDATA-FEL                                                     
050700           MOVE IS-INVALID TO RESP-IDMSG-ERROR                            
050800        END-IF                                                            
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200                                                                          
051300 H-UPDATE    SECTION.                                                     
051400                                                                          
051500     MOVE +1        TO INDX                                               
051600     MOVE ZERO      TO W-KVANT-UPD                                        
051700*    MOVE W-IDDC    TO W-IDDC-6301                                        
051800     MOVE WS-IDDC-REC TO W-IDDC-6301                                      
051900                                                                          
052000     PERFORM UNTIL INDX  > MAX-INDX                                       
052100       PERFORM HA-UPDATE-IDLBBET                                          
052200       ADD +1 TO INDX                                                     
052300                                                                          
052400     END-PERFORM                                                          
052500     .                                                                    
052600     EJECT                                                                
052700 HA-UPDATE-IDLBBET SECTION.                                               
052800                                                                          
052900     MOVE 20                   TO W-DABERANK-6302 (1:2)                   
053000*    MOVE REQU-TIBERANK (INDX) TO W-TIBERANK-8                            
053100*    MOVE W-TIBERANK-8 (1:6)   TO 6302-DABERANK (3:6)                     
053200     MOVE REQU-TIBERANK (INDX) TO W-DABERANK-6302 (3:6)                   
053300     MOVE REQU-IDFAKT   (INDX) TO W-IDFAKT-6302                           
053400     PERFORM IMS-GHU-WDGX6302                                             
053500     IF SEGMENT-FOUND                                                     
053600       IF 6302-KDTRPSTA = SPACE                                           
053700                                                                          
053800         MOVE REQU-IDLBBET (INDX) TO 6302-IDLBBET                         
053900*        MOVE REQU-CMD-IN(INDX) TO 6302-KDTRPSTA                          
054000          PERFORM IMS-REPL-WDGX6302                                       
054100       END-IF                                                             
054200     END-IF                                                               
054300     .                                                                    
054400     EJECT                                                                
054500                                                                          
054600*    --- DISPATCHER SECTIONS                                              
054700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
054800                                                                          
054900     MOVE 'GETARG'               TO SUB-KDFUNC                            
055000     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
055100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
055200                                                                          
055300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
055400                                                                          
055500     IF SUB-KDRC > 0                                                      
055600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
055700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
055800       DELIMITED BY SIZE INTO FELTEXT                                     
055900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056000     END-IF                                                               
056100     .                                                                    
056200     SKIP3                                                                
056300 S02-RETURN-RESPONSE SECTION.                                             
056400                                                                          
056500     COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA                           
056600       - LENGTH OF RESP-TABELLRAD * (500 - WS-KVANT)                      
056700                                                                          
056800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
056900     MOVE WS-RESP-AREA               TO SUB-KVDLEN                        
057000                                                                          
057100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
057200                                                                          
057300     IF SUB-KDRC > 0                                                      
057400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
057500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
057600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057800     END-IF                                                               
057900     .                                                                    
058000     EJECT                                                                
058100     EJECT                                                                
058200 IMS-GU-WDGX6301 SECTION.                                                 
058300     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6301-X ')'                    
058400          DELIMITED BY SIZE INTO SSA1                                     
058500     MOVE '  GE' TO GOOD-STATUSCODES                                      
058600     CALL CBLTDLI USING GU 6301-PCB DLI-IO-AREA-6301 SSA1                 
058700                                                                          
058800     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
058900     PERFORM IMS-STATUSKONTROLL                                           
059000     .                                                                    
059100     SKIP3                                                                
059200 IMS-GNP-WDGX6302-A SECTION.                                              
059300     STRING 'WDGX6302(KEY6302 =>' W-KEY6302-X ')'                         
059400          DELIMITED BY SIZE INTO SSA1                                     
059500     MOVE '  GE' TO GOOD-STATUSCODES                                      
059600     CALL CBLTDLI USING GNP 6301-PCB DLI-IO-AREA-6302 SSA1                
059700                                                                          
059800     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
059900     PERFORM IMS-STATUSKONTROLL                                           
060000     .                                                                    
060100     EJECT                                                                
060200 IMS-GNP-WDGX6302-B SECTION.                                              
060300     STRING 'WDGX6302(KEY6302 =>' W-KEY6302-X                             
060400                    '&IDLBBET  =' W-IDLBBET ')'                           
060500          DELIMITED BY SIZE INTO SSA1                                     
060600     MOVE '  GE' TO GOOD-STATUSCODES                                      
060700     CALL CBLTDLI USING GNP 6301-PCB DLI-IO-AREA-6302 SSA1                
060800                                                                          
060900     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
061000     PERFORM IMS-STATUSKONTROLL                                           
061100     .                                                                    
061200     SKIP3                                                                
061300 IMS-GNP-WDGX6302-C SECTION.                                              
061400     STRING 'WDGX6302(KEY6302 =>' W-KEY6302-X                             
061500                    '&IDDCSEND =' W-IDDCSEND ')'                          
061600          DELIMITED BY SIZE INTO SSA1                                     
061700     MOVE '  GE' TO GOOD-STATUSCODES                                      
061800     CALL CBLTDLI USING GNP 6301-PCB DLI-IO-AREA-6302 SSA1                
061900                                                                          
062000     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
062100     PERFORM IMS-STATUSKONTROLL                                           
062200     .                                                                    
062300     SKIP3                                                                
062400 IMS-GNP-WDGX6302-D SECTION.                                              
062500     STRING 'WDGX6302(KEY6302 =>' W-KEY6302-X                             
062600                    '&IDDCLEV  =' W-IDDCLEV ')'                           
062700          DELIMITED BY SIZE INTO SSA1                                     
062800     MOVE '  GE' TO GOOD-STATUSCODES                                      
062900     CALL CBLTDLI USING GNP 6301-PCB DLI-IO-AREA-6302 SSA1                
063000                                                                          
063100     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400     SKIP3                                                                
063500 IMS-REPL-WDGX6302 SECTION.                                               
063600     MOVE '  ' TO GOOD-STATUSCODES                                        
063700     CALL CBLTDLI USING REPL 6301-PCB DLI-IO-AREA-6302                    
063800     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100     EJECT                                                                
064200 IMS-GHU-WDGX6302 SECTION.                                                
064300                                                                          
064400     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6301-X ')'                    
064500          DELIMITED BY SIZE INTO SSA1                                     
064600     STRING 'WDGX6302(KEY6302  =' W-KEY6302-X ')'                         
064700          DELIMITED BY SIZE INTO SSA2                                     
064800     MOVE '  GE' TO GOOD-STATUSCODES                                      
064900     CALL CBLTDLI USING GHU 6301-PCB DLI-IO-AREA-6302 SSA1 SSA2           
065000     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
065100     PERFORM IMS-STATUSKONTROLL                                           
065200     .                                                                    
065300     EJECT                                                                
065400 IMS-GU-WDB601 SECTION.                                                   
065500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
065600          DELIMITED BY SIZE INTO SSA1                                     
065700     MOVE '  GE' TO GOOD-STATUSCODES                                      
065800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
065900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200     SKIP3                                                                
066300                                                                          
066400                                                                          
066500 IMS-STATUSKONTROLL SECTION.                                              
066600     SET STATUS-IX TO 1                                                   
066700     SEARCH GOOD-STATUS                                                   
066800       AT END                                                             
066900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
067000         DELIMITED BY SIZE INTO FELTEXT                                   
067100         CALL FELLOG                                                      
067200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
067300         CONTINUE                                                         
067400     END-SEARCH                                                           
067500     .                                                                    
067600     EJECT                                                                
