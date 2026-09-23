000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4069100.                                                
000300 AUTHOR.         SRINADH NADIMPALLI.                                      
000400 DATE-WRITTEN.   25/05/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       PULSRECEIVE.SHOWTRANSPORT                                
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SHOW SUMMARY OF ORDERS THAT ARE READY FOR TRANSPORT              
001100*                                                                         
001200*        THE PROGRAM READS     WDE6C                                      
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W4A691T                                             
001600*        REQUEST:     W40691I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    W40691O1                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W4069100'.            
003400                                                                          
003500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700 77  KDRC-DISPLAY                PIC Z(5).                                
003800                                                                          
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  WS-KVRADER                  PIC S9(9).                               
004300 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
004400 77  WS-KVKOLLI-TOT              PIC S9(3)   VALUE ZERO COMP-3.           
004500 77  W-TIRFS                     PIC 9(8)    VALUE ZERO.                  
004600 01  WS-DARFS.                                                            
004700     03  WS-DARFS-YY             PIC 9(02)   VALUE 20.                    
004800     03  WS-DARFS-YYMMDD         PIC 9(06).                               
004900 77  WS-IDTRPTNR                 PIC S9(3)   VALUE ZERO COMP-3.           
005000 77  MSG-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
005100                                                                          
005200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005300     88  KEYS-OK                             VALUE 'J'.                   
005400     88  KEYS-WRONG                          VALUE 'N'.                   
005500     EJECT                                                                
005600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005700 01  GENERAL-SUBPROGRAMS.                                                 
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006300     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
006400     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
006500     SKIP3                                                                
006600*    --- PARAMETERS TO ABEND                                              
006700                                                                          
006800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007100     SKIP3                                                                
007200 01  MESSAGE-CODES.                                                       
007300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007400     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
007500     03  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
007600     EJECT                                                                
007700*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
007900     SKIP3                                                                
008000*01  -COPY WZ01SUB                                                        
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
008300*01  -COPY WMSGCONV                                                       
008400                                                                          
008500 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
008600*01  -COPY WZ01AUTH                                                       
008700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008800     SKIP3                                                                
008900 01  REQU-AREA.                                                           
009000*    03  -COPY WZ01REQ2                                                   
009100*    03  -COPY W40691I1                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009400     SKIP3                                                                
009500 01  RESP-AREA.                                                           
009600*    03  -COPY WZ01RES2                                                   
009700*    03  -COPY W40691O1                                                   
009800     EJECT                                                                
009900*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300 01  KEYS-FOR-DLI.                                                        
010400     03  W-WDE6C1KY-MIN-X.                                                
010500         05 W-IDTRPTNR-MIN     PIC S9(3)   VALUE ZERO  COMP-3.            
010600         05 W-DARFS-MIN        PIC 9(12)   VALUE ZERO.                    
010700         05 W-ADCLGEO-MIN.                                                
010800           07 W-IDDC-MIN       PIC X(2)    VALUE SPACE.                   
010900           07 W-ADFLGEO-MIN    PIC X(3)    VALUE SPACE.                   
011000         05 W-ADFLOMR-MIN      PIC S9(3)   VALUE ZERO  COMP-3.            
011100         05 W-ADRUTNIV-MIN     PIC S9(3)   VALUE ZERO  COMP-3.            
011200         05 W-ADVMODUL-MIN     PIC S9(3)   VALUE ZERO  COMP-3.            
011300         05 W-IDDISTR-MIN      PIC S9(5)   VALUE ZERO  COMP-3.            
011400         05 W-IDKUNDNR-MIN     PIC S9(7)   VALUE ZERO  COMP-3.            
011500         05 W-IDPRODNR-KOLLI-MIN PIC S9(7) VALUE ZERO  COMP-3.            
011600         05 W-IDKOLLI-FLER-MIN PIC S9(5)   VALUE ZERO  COMP-3.            
011700         05 W-IDKOLLI-MIN      PIC S9(5)   VALUE ZERO  COMP-3.            
011800     03  W-WDE6C1KY-MAX-X.                                                
011900         05 W-IDTRPTNR-MAX     PIC S9(3)   VALUE ZERO  COMP-3.            
012000         05 W-DARFS-MAX        PIC 9(12)   VALUE ZERO.                    
012100         05 W-ADCLGEO-MAX.                                                
012200           07 W-IDDC-MAX       PIC X(2)    VALUE SPACE.                   
012300           07 W-ADFLGEO-MAX    PIC X(3)    VALUE SPACE.                   
012400         05 W-ADFLOMR-MAX      PIC S9(3)   VALUE ZERO  COMP-3.            
012500         05 W-ADRUTNIV-MAX     PIC S9(3)   VALUE ZERO  COMP-3.            
012600         05 W-ADVMODUL-MAX     PIC S9(3)   VALUE ZERO  COMP-3.            
012700         05 W-IDDISTR-MAX      PIC S9(5)   VALUE ZERO  COMP-3.            
012800         05 W-IDKUNDNR-MAX     PIC S9(7)   VALUE ZERO  COMP-3.            
012900         05 W-IDPRODNR-KOLLI-MAX PIC S9(7) VALUE ZERO  COMP-3.            
013000         05 W-IDKOLLI-FLER-MAX PIC S9(5)   VALUE ZERO  COMP-3.            
013100         05 W-IDKOLLI-MAX      PIC S9(5)   VALUE ZERO  COMP-3.            
013200     03  W-WDE6H1KY-MIN-X.                                                
013300         05 W-IDDC-CROSS-MIN      PIC X(2).                               
013400         05 W-KDKOLSTA-CROSS-MIN  PIC S9      VALUE 1  COMP-3.            
013500         05 W-TIRFS-MIN           PIC 9(06)   VALUE ZERO.                 
013600         05 FILLER                PIC X(14)   VALUE LOW-VALUE.            
013700                                                                          
013800     03  W-WDE6H1KY-MAX-X.                                                
013900         05 W-IDDC-CROSS-MAX      PIC X(2).                               
014000         05 W-KDKOLSTA-CROSS-MAX  PIC S9      VALUE 1  COMP-3.            
014100         05 W-TIRFS-MAX           PIC 9(06)   VALUE 999999.               
014200         05 FILLER                PIC X(14)   VALUE HIGH-VALUE.           
014300                                                                          
014400     03  W-TIRECXDAT-X.                                                   
014500         05 W-TIRECXDAT        PIC 9(6)    VALUE 0.                       
014600                                                                          
014700     03  W-KDKOLSTX-X.                                                    
014800         05 W-KDKOLSTA-CROSS   PIC S9      VALUE 1     COMP-3.            
014900                                                                          
015000     03  W-IDTRPTNC-X.                                                    
015100         05 W-IDTRPTNR-CROSS   PIC S9(3)   VALUE 0     COMP-3.            
015200     SKIP2                                                                
015300*    --- STATUS-KOD FRÅN IMS                                              
015400 01  STATUS-WS                   PIC XX.                                  
015500     88  SEGMENT-FOUND                       VALUE '  '.                  
015600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015800     88  END-OF-DB           VALUE 'GB'.                                  
015900     SKIP2                                                                
016000 01  GOOD-STATUSCODES.                                                    
016100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01  SSA1                        PIC X(256).                              
016400 01  SSA2                        PIC X(256).                              
016500     EJECT                                                                
016600*    --- IMS FUNCTION CODES                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900                                                                          
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-E6C1'.                        
017100 01  DLI-IO-E6C1.                                                         
017200*    03  -COPY WDE6C1                                                     
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-E6H1'.                        
017400 01  DLI-IO-E6H1.                                                         
017500*    03  -COPY WDE6H1                                                     
017600                                                                          
017700 LINKAGE SECTION.                                                         
017800*01  -COPY W0009   -PRE MSG-                                              
017900 01  ATAB-PCB                 PIC X.                                      
018000                                                                          
018100*01  -COPY W0008  -PRE WDE6C-                                             
018200     05  FILLER                  PIC X.                                   
018300*01  -COPY W0008  -PRE WDE6H-                                             
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600 PROCEDURE DIVISION  USING MSG-PCB ATAB-PCB WDE6C-PCB WDE6H-PCB.          
018700 MAIN SECTION.                                                            
018800      ENTRY 'DLITCBL' USING MSG-PCB ATAB-PCB WDE6C-PCB WDE6H-PCB.         
018900                                                                          
019000      PERFORM S01-FETCH-REQUEST-ARGUMENT                                  
019100      IF SUB-KDRC = 0                                                     
019200       PERFORM A-INIT                                                     
019300       PERFORM B-CHECK-KEYS                                               
019400       IF KEYS-OK                                                         
019500         PERFORM F-READ-SHOW-INFO                                         
019600       END-IF                                                             
019700       PERFORM S11-MSG-CONV                                               
019800       PERFORM S02-RETURN-RESPONSE                                        
019900      END-IF                                                              
020000                                                                          
020100      MOVE ZERO TO RETURN-CODE                                            
020200      GOBACK                                                              
020300      .                                                                   
020400      EJECT                                                               
020500 A-INIT SECTION.                                                          
020600      MOVE LOW-VALUES TO RESP-AREA                                        
020700      MOVE SPACE   TO RESP-IDMSG-ERROR                                    
020800                      RESP-IDMSG-INFO                                     
020900                      RESP-IDELMT-ERROR                                   
021000      MOVE '001'   TO RESP-IDRESVER                                       
021100      MOVE ZERO    TO RESP-KVRADER-MAX1                                   
021200      IF SUB-KDTRANS(1:6) = 'W4A691'                                      
021300       MOVE 001                  TO AUTH-KDCALL                           
021400       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
021500                                    REQU-WZ01REQ2                         
021600       IF AUTH-KDRC > 0                                                   
021700         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
021800         MOVE NOO                TO KEYS-SW                               
021900       END-IF                                                             
022000       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY) TO                        
022100                                 REQU-IDDC-KEY                            
022200      END-IF                                                              
022300      MOVE LOW-VALUE            TO W-WDE6C1KY-MIN-X                       
022400      MOVE HIGH-VALUE           TO W-WDE6C1KY-MAX-X                       
022500      .                                                                   
022600      EJECT                                                               
022700 B-CHECK-KEYS SECTION.                                                    
022800                                                                          
022900      MOVE YES TO KEYS-SW                                                 
023000                                                                          
023100                                                                          
023200      IF KEYS-WRONG                                                       
023300        MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                            
023400      END-IF                                                              
023500      .                                                                   
023600      EJECT                                                               
023700 F-READ-SHOW-INFO SECTION.                                                
023800      MOVE REQU-IDDC-KEY          TO W-IDDC-MIN                           
023900                                     W-IDDC-MAX                           
024000                                     W-IDDC-CROSS-MIN                     
024100                                     W-IDDC-CROSS-MAX                     
024200      MOVE ZERO TO WS-KVRADER                                             
024300      MOVE ZERO                   TO INDX                                 
024400      PERFORM FA-SHOW-BASIC-DATA                                          
024500      IF REQU-IDDC-KEY = '11'                                             
024600       MOVE 'SE' TO W-IDDC-MIN                                            
024700                    W-IDDC-MAX                                            
024800                    W-IDDC-CROSS-MIN                                      
024900                    W-IDDC-CROSS-MAX                                      
025000       PERFORM FA-SHOW-BASIC-DATA                                         
025100      END-IF                                                              
025200      MOVE WS-KVRADER TO RESP-KVRADER-MAX1                                
025300      IF WS-KVRADER > 500                                                 
025400         MOVE 500 TO RESP-KVRADER-MAX1                                    
025500         MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                          
025600      END-IF                                                              
025700      .                                                                   
025800      EJECT                                                               
025900 FA-SHOW-BASIC-DATA SECTION.                                              
026000      PERFORM IMS-GU-WDE6C                                                
026100      PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-MISSING                    
026200                                   OR END-OF-DB                           
026300        ADD +1                   TO WS-KVRADER                            
026400                                    INDX                                  
026500        MOVE ZERO                TO WS-KVKOLLI-TOT                        
026600        MOVE 99999999            TO W-TIRFS                               
026700        MOVE SEQC-IDTRPTNR       TO WS-IDTRPTNR                           
026800                                    RESP-IDTRPTNR (INDX)                  
026900        MOVE SEQC-IDDC TO RESP-IDDC (INDX)                                
027000        PERFORM UNTIL SEQC-IDTRPTNR NOT = WS-IDTRPTNR OR END-OF-DB        
027100                                     OR SEGMENT-MISSING                   
027200          ADD +1                 TO WS-KVKOLLI-TOT                        
027300          IF SEQC-DARFS(1:8) < W-TIRFS                                    
027400            MOVE SEQC-DARFS(3:6) TO RESP-TIRFSDAT (INDX)                  
027500            MOVE SEQC-DARFS(1:8) TO W-TIRFS                               
027600                                    WS-DARFS                              
027700          END-IF                                                          
027800          PERFORM IMS-GN-WDE6C                                            
027900        END-PERFORM                                                       
028000        MOVE WS-KVKOLLI-TOT      TO RESP-KVKOLLI-TRPT (INDX)              
028100      END-PERFORM                                                         
028200      PERFORM IMS-GU-WDE6H                                                
028300      PERFORM UNTIL SEGMENT-MISSING OR END-OF-DB                          
028400        PERFORM                                                           
028500        VARYING INDX FROM 1 BY 1                                          
028600          UNTIL INDX > MAX-INDX                                           
028700             OR RESP-IDDC (INDX) = LOW-VALUES                             
028800             OR (RESP-IDDC (INDX) = SEQH-IDDC-CROSS                       
028900                 AND RESP-IDTRPTNR (INDX) = SEQH-IDTRPTNR-CROSS)          
029000        END-PERFORM                                                       
029100                                                                          
029200        IF INDX > MAX-INDX                                                
029300          SET END-OF-DB          TO TRUE                                  
029400        ELSE                                                              
029500          IF RESP-IDDC (INDX) = LOW-VALUES                                
029600            ADD +1               TO WS-KVRADER                            
029700            MOVE SEQH-IDDC-CROSS TO RESP-IDDC         (INDX)              
029800            MOVE SEQH-IDTRPTNR-CROSS                                      
029900                                 TO RESP-IDTRPTNR     (INDX)              
030000            MOVE SEQH-TIRFSDAT   TO RESP-TIRFSDAT     (INDX)              
030100            MOVE 1               TO RESP-KVKOLLI-TRPT (INDX)              
030200          ELSE                                                            
030300            IF SEQH-TIRFSDAT < RESP-TIRFSDAT (INDX)                       
030400              MOVE SEQH-TIRFSDAT TO RESP-TIRFSDAT     (INDX)              
030500            END-IF                                                        
030600            ADD +1               TO RESP-KVKOLLI-TRPT (INDX)              
030700          END-IF                                                          
030800        END-IF                                                            
030900        PERFORM IMS-GN-WDE6H                                              
031000      END-PERFORM                                                         
031100      .                                                                   
031200      EJECT                                                               
031300*    --- DISPATCHER SECTIONS                                              
031400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
031500                                                                          
031600      MOVE 'GETARG'               TO SUB-KDFUNC                           
031700      MOVE 'CARPARTS.PULS.SHOWTRANSPORT'      TO SUB-ADDISPABS            
031800      MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                           
031900                                                                          
032000      CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA            
032100                                                                          
032200      IF SUB-KDRC > 0                                                     
032300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
032400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
032500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
032600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032700      END-IF                                                              
032800      .                                                                   
032900      SKIP3                                                               
033000 S02-RETURN-RESPONSE SECTION.                                             
033100                                                                          
033200      MOVE 'RETURN'                   TO SUB-KDFUNC                       
033300      MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                       
033400                                                                          
033500      CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA            
033600                                                                          
033700      IF SUB-KDRC > 0                                                     
033800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
033900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
034000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
034100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034200      END-IF                                                              
034300      .                                                                   
034400      EJECT                                                               
034500 S11-MSG-CONV SECTION.                                                    
034600      MOVE LOW-VALUES              TO RESP-MESSAGES (1)                   
034700                                      RESP-MESSAGES (2)                   
034800      MOVE 1                       TO MSG-IX                              
034900*    REQUEST OK                                                           
035000     MOVE 200                     TO RESP-KDSTATUS-API                    
035100     IF RESP-IDMSG-INFO > SPACE                                           
035200     MOVE SPACES                TO MSG-CONV-AREA                          
035300     MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                      
035400     CALL WMSGCONV           USING MSG-CONV-AREA                          
035500     MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                  
035600     MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                  
035700     ADD 1                      TO MSG-IX                                 
035800     END-IF                                                               
035900     IF RESP-IDMSG-ERROR > SPACE                                          
036000*      BAD REQUEST                                                        
036100       MOVE 400                   TO RESP-KDSTATUS-API                    
036200       MOVE SPACES                TO MSG-CONV-AREA                        
036300       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
036400       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
036500       CALL WMSGCONV           USING MSG-CONV-AREA                        
036600       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
036700       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
036800     END-IF                                                               
036900     .                                                                    
037000 IMS-GU-WDE6C                  SECTION.                                   
037100                                                                          
037200      STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-MIN-X                       
037300                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X                        
037400                    '&IDDC     =' W-IDDC-MIN ')'                          
037500            DELIMITED BY SIZE INTO SSA1                                   
037600      MOVE '  GE'                TO GOOD-STATUSCODES                      
037700      CALL CBLTDLI USING GU WDE6C-PCB DLI-IO-E6C1 SSA1                    
037800      MOVE WDE6C-STATUS-CODE     TO STATUS-WS                             
037900      PERFORM IMS-STATUS-CHECK                                            
038000      .                                                                   
038100 IMS-GN-WDE6C            SECTION.                                         
038200                                                                          
038300      STRING 'WDE6C1  (WDE6C1KY>=' W-WDE6C1KY-MIN-X                       
038400                    '&WDE6C1KY<=' W-WDE6C1KY-MAX-X                        
038500                    '&IDDC     =' W-IDDC-MIN ')'                          
038600             DELIMITED BY SIZE INTO SSA1                                  
038700      MOVE '  GEGB'              TO GOOD-STATUSCODES                      
038800      CALL CBLTDLI USING GN WDE6C-PCB DLI-IO-E6C1 SSA1                    
038900      MOVE WDE6C-STATUS-CODE     TO STATUS-WS                             
039000      PERFORM IMS-STATUS-CHECK                                            
039100      .                                                                   
039200 IMS-GU-WDE6H                  SECTION.                                   
039300                                                                          
039400     STRING 'WDE6H1  (WDE6H1KY>=' W-WDE6H1KY-MIN-X                        
039500                    '&WDE6H1KY<=' W-WDE6H1KY-MAX-X                        
039600                    '&TIRECDAT >' W-TIRECXDAT-X ')'                       
039700            DELIMITED BY SIZE INTO SSA1                                   
039800     MOVE '  GE'                TO GOOD-STATUSCODES                       
039900     CALL CBLTDLI USING GU WDE6H-PCB DLI-IO-E6H1 SSA1                     
040000     MOVE WDE6H-STATUS-CODE     TO STATUS-WS                              
040100                                                                          
040200     PERFORM IMS-STATUS-CHECK                                             
040300     .                                                                    
040400 IMS-GN-WDE6H                  SECTION.                                   
040500                                                                          
040600     STRING 'WDE6H1  (WDE6H1KY>=' W-WDE6H1KY-MIN-X                        
040700                    '&WDE6H1KY<=' W-WDE6H1KY-MAX-X                        
040800                    '&TIRECDAT >' W-TIRECXDAT-X ')'                       
040900            DELIMITED BY SIZE INTO SSA1                                   
041000     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
041100     CALL CBLTDLI USING GN WDE6H-PCB DLI-IO-E6H1 SSA1                     
041200     MOVE WDE6H-STATUS-CODE     TO STATUS-WS                              
041300                                                                          
041400     PERFORM IMS-STATUS-CHECK                                             
041500     .                                                                    
041600 IMS-STATUS-CHECK   SECTION.                                              
041700                                                                          
041800     SET STATUS-IX TO 1                                                   
041900     SEARCH GOOD-STATUS                                                   
042000       AT END                                                             
042100         CALL FELLOG                                                      
042200     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
042300       CONTINUE                                                           
042400     END-SEARCH                                                           
042500     .                                                                    
