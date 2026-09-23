000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF029400.                                                
000400 AUTHOR.         SARASWATHY S.                                            
000500 DATE-WRITTEN.   20/04/07.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    NAME:                                                                
000900*        'CARPARTS.BILLIT.APPROVEDYRCURRENCIESMAINTENANCE'                
001000*    FUNCTION:                                                            
001100*        CURRENCIES YEARLY SCREEN - MAINTENANCE                           
001200*        READS NEW DB2 TABLE FOR THE YEARLY CURRENCIES T01CUYE            
001300*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001400*                                                                         
001500*        THE PROGRAM READS     TABLE T01LSEL                              
001600*        THE PROGRAM READS     TABLE T01CUYE                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSACTION: WF0294T                                             
002000*        REQUEST:     WF0294I1                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        RESPONSE:    WF0294O1                                            
002400*                                                                         
002500*                                                                         
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'WF029400'.            
003500                                                                          
003600*    --- WORK FIELD FOR ERROR MESSAGE WHEN CALLING ABEND.                 
003700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003800 77  KDRC-DISPLAY                PIC Z(5).                                
003900                                                                          
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  KEYS-SW                     PIC X       VALUE SPACE.                 
004400     88  KEYS-OK                             VALUE 'Y'.                   
004500     88  KEYS-FEL                            VALUE 'N'.                   
004600     EJECT                                                                
004700                                                                          
004800*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
004900 01  GENERAL-SUBPROGRAMS.                                                 
005000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
005300                                                                          
005400*    --- PARAMETERS TO ABEND                                              
005500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
005900                                                                          
006000 01  MESSAGE-CODES.                                                       
006100     03  ERROR-CODES.                                                     
006200       05 NOTHING-HAS-BEEN-UPDATED   PIC X(3)    VALUE '004'.             
006300       05 NOTHING-HAS-BEEN-INSERTED  PIC X(3)    VALUE '005'.             
006400       05 NOTHING-HAS-BEEN-DELETED   PIC X(3)    VALUE '006'.             
006500       05 UPDATE-NOT-ALLOWED         PIC X(3)    VALUE '007'.             
006600       05 INSERT-NOT-ALLOWED         PIC X(3)    VALUE '008'.             
006700       05 DELETE-NOT-ALLOWED         PIC X(3)    VALUE '009'.             
006800       05 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.             
006900       05 IS-INVALID                 PIC X(3)    VALUE '023'.             
007000       05 MUST-BE-NUMERIC            PIC X(3)    VALUE '024'.             
007100       05 NOT-FOUND                  PIC X(3)    VALUE '025'.             
007200       05 MUST-BE-ENTERED            PIC X(3)    VALUE '026'.             
007300       05 LINE-NOT-FOUND             PIC X(3)    VALUE '027'.             
007400       05 ALREADY-EXISTS             PIC X(3)    VALUE '030'.             
007500       05 SYSTEM-ERROR               PIC X(3)    VALUE '099'.             
007600     03  INFO-KODER.                                                      
007700       05 UPDATE-DONE                PIC X(3)    VALUE '001'.             
007800       05 INSERT-DONE                PIC X(3)    VALUE '002'.             
007900       05 DELETE-DONE                PIC X(3)    VALUE '003'.             
008000     EJECT                                                                
008100*01  -COPY WDECAREA                                                       
008200 01  FILLER                      PIC X(16)   VALUE 'DECEDIT    '.         
008300     EJECT                                                                
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008600                                                                          
008700 01  -COPY WZ01SUB                                                        
008800     EJECT                                                                
008900                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009100                                                                          
009200 01  REQU-AREA.                                                           
009300*    03  -COPY WZ01REQU                                                   
009400*    03  -COPY WF0294I1                                                   
009500     EJECT                                                                
009600                                                                          
009700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009800                                                                          
009900 01  RESP-AREA.                                                           
010000*    03  -COPY WZ01RESP                                                   
010100*    03  -COPY WF0294O1                                                   
010200     EJECT                                                                
010300                                                                          
010400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
010500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010600                                                                          
010700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
010800 01  DB2-WS.                                                              
010900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011000         88  CURSOR-OK                       VALUE 000.                   
011100         88  LINES-FOUND                     VALUE 000.                   
011200         88  LINES-MISSING                   VALUE 100.                   
011300         88  RESOURCE-WRONG                  VALUE 904.                   
011400                                                                          
011500     03  GOOD-SQLCODECODES.                                               
011600         05  GOOD-SQLCODE OCCURS 5                                        
011700             INDEXED BY SQLCODE-IX PIC 9(3).                              
011800     EJECT                                                                
011900                                                                          
012000 01  WS-AREA.                                                             
012100     03 WS-PRKURS-RED           PIC Z(6)9.9(6).                           
012200     03 WS-PRKURS-NUM           PIC S9(6)V9(6) COMP-3.                    
012300     03 WS-PRKURS-DEC           PIC 9(6)V9(6)  VALUE ZERO.                
012400     03 WS-PRKURS-HELTAL        PIC 9(6).                                 
012500     03 WS-ACTIVE               PIC X(8)       VALUE '00000000'.          
012600     03 WS-IDMSG-INFO           PIC X(3)       VALUE SPACE.               
012700     03 WS-IDMSG-ERROR          PIC X(3)       VALUE SPACE.               
012800     03 WS-IDELMT-ERROR         PIC X(16)      VALUE SPACE.               
012900     03 WS-DATUM                PIC X(8)       VALUE SPACE.               
012910     03 WS-DATUM-N              PIC 9(4)       VALUE ZEROS.               
012911     03 WS-DATUM-M              PIC 9(2)       VALUE ZEROS.               
012920     03 WS-NEXT-YEAR            PIC 9(4)       VALUE ZEROS.               
013000     03 WS-DASTADAT-KEY         PIC X(8)       VALUE SPACE.               
013010     03 WS-DASTADAT-KEY-N       PIC 9(4)       VALUE ZEROS.               
013100     03 WS-REVALUTA-FROM        PIC S9(5)      COMP-3.                    
013200     03 WS-REVALUTA-TO          PIC S9(5)      COMP-3.                    
013210     03 WS-CNT1                 PIC 9(2)       VALUE ZEROS.               
013220     03 WS-CNT2                 PIC 9(2)       VALUE ZEROS.               
013300 01  WS-PRKURS-JUST2            PIC 9(6)V9(6)  VALUE ZERO.                
013400 01  WS-PRKURS-JUST             PIC 9(6)V9(6)  VALUE ZERO.                
013500 01  FILLER REDEFINES WS-PRKURS-JUST.                                     
013600     03  FILLER                 PIC 9(6)V9(4).                            
013700     03  WS-PRKURS-SIST         PIC 9(2).                                 
013800     EJECT                                                                
013900                                                                          
014000 01  FILLER                  PIC X(16)    VALUE 'T01LSEL-AREA'.           
014100*01  -COPY T01LSEL -PRE T01LSEL-                                          
014200     EJECT                                                                
014300                                                                          
014400 01  FILLER                  PIC X(16)    VALUE 'T01CUYE-AREA'.           
014500*01  -COPY T01CUYE -PRE T01CUYE-                                          
014600     EJECT                                                                
014700                                                                          
014800                                                                          
014900     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
015000     EJECT                                                                
015100                                                                          
015200     EXEC SQL INCLUDE T01CUYE END-EXEC.                                   
015300     EJECT                                                                
015400                                                                          
015500                                                                          
015600 LINKAGE SECTION.                                                         
015700 PROCEDURE DIVISION.                                                      
015800 MAIN SECTION.                                                            
015900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
016000     IF SUB-KDRC = ZERO                                                   
016100       PERFORM A-INIT                                                     
016200       PERFORM B-CHECK-KEYS                                               
016300       IF KEYS-OK                                                         
016400         PERFORM C-CHECK-INDATA                                           
016500       END-IF                                                             
016600       IF KEYS-OK                                                         
016700         PERFORM D-PERFORM-REQUEST                                        
016800         IF KEYS-OK                                                       
016900           PERFORM E-BUILD-DATA-RESPONSE                                  
017000         END-IF                                                           
017100       END-IF                                                             
017200       PERFORM F-BUILD-HEADER-RESPONSE                                    
017300       PERFORM S02-RETURN-RESPONSE                                        
017400     END-IF                                                               
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000                                                                          
018100 A-INIT SECTION.                                                          
018200     MOVE YES                         TO KEYS-SW                          
018300                                                                          
018400     MOVE ALL ' '                     TO RESP-AREA                        
018500     MOVE SPACE TO RESP-IDMSG-ERROR                                       
018600     MOVE SPACE TO RESP-IDMSG-INFO                                        
018700     MOVE SPACE TO RESP-IDELMT-ERROR                                      
018800                                                                          
018900     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
019000                                                                          
019100     INITIALIZE GOOD-SQLCODECODES                                         
019200     .                                                                    
019300     EJECT                                                                
019400                                                                          
019500 B-CHECK-KEYS SECTION.                                                    
019600     IF REQU-KDPGMACT = 'S'                                               
019700     AND REQU-IDMSGVER NUMERIC                                            
019800       CONTINUE                                                           
019900     ELSE                                                                 
020000       IF REQU-KDPGMACT = 'I'                                             
020100       AND REQU-IDMSGVER NUMERIC                                          
020200         CONTINUE                                                         
020300       ELSE                                                               
020400         IF REQU-KDPGMACT = 'U'                                           
020500         AND REQU-IDMSGVER NUMERIC                                        
020600           CONTINUE                                                       
020700         ELSE                                                             
020800           IF REQU-KDPGMACT = 'D'                                         
020900           AND REQU-IDMSGVER NUMERIC                                      
021000             CONTINUE                                                     
021100           ELSE                                                           
021200             MOVE NOO TO KEYS-SW                                          
021300             MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                         
021400           END-IF                                                         
021500         END-IF                                                           
021600       END-IF                                                             
021700     END-IF                                                               
021800                                                                          
021900     IF REQU-IDLEGSEL-KEY = SPACE OR = ALL '+'                            
022000       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
022100       MOVE NOO TO KEYS-SW                                                
022200     END-IF                                                               
022300                                                                          
022400     IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                            
022500       MOVE NOO TO KEYS-SW                                                
022600       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
022700     END-IF                                                               
022800                                                                          
022900     MOVE REQU-DASTADAT-KEY TO WS-DASTADAT-KEY                            
023000     IF WS-DASTADAT-KEY = SPACE OR = ALL '+'                              
023100       MOVE NOO TO KEYS-SW                                                
023200       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
023300       MOVE ZEROS         TO REQU-DASTADAT-KEY                            
023400     END-IF                                                               
023500                                                                          
023600     IF WS-DASTADAT-KEY(5:4) NOT = '0101'                                 
023700       MOVE NOO TO KEYS-SW                                                
023800       MOVE IS-INVALID   TO WS-IDMSG-ERROR                                
023900       MOVE 'DASTADAT'   TO WS-IDELMT-ERROR                               
024000     END-IF                                                               
024100                                                                          
024210     IF WS-DASTADAT-KEY(1:4) < WS-DATUM(1:4)                              
024300       MOVE NOO TO KEYS-SW                                                
024400       MOVE IS-INVALID   TO WS-IDMSG-ERROR                                
024500       MOVE 'DASTADAT'   TO WS-IDELMT-ERROR                               
024600     END-IF                                                               
024700                                                                          
024710     MOVE WS-DATUM(1:4) TO WS-DATUM-N                                     
024711     MOVE WS-DATUM(5:2) TO WS-DATUM-M                                     
024712     MOVE WS-DASTADAT-KEY(1:4) TO WS-DASTADAT-KEY-N                       
024713     COMPUTE WS-NEXT-YEAR = WS-DATUM-N + 1                                
024800     IF (REQU-KDPGMACT = 'U' OR 'D')                                      
024801     AND ((WS-DASTADAT-KEY(1:4) <= WS-DATUM(1:4))                         
024802     OR   (WS-DASTADAT-KEY-N NOT = WS-NEXT-YEAR)                          
024803     OR   (WS-DATUM-M < 08 OR WS-DATUM-M > 12))                           
024809       MOVE NOO TO KEYS-SW                                                
024810       MOVE IS-INVALID   TO WS-IDMSG-ERROR                                
024811       MOVE 'DASTADAT'   TO WS-IDELMT-ERROR                               
024812     END-IF                                                               
024813                                                                          
024814     IF REQU-KDPGMACT = 'I'                                               
024820      IF WS-DASTADAT-KEY(1:4) = WS-DATUM(1:4)                             
024821      OR WS-DASTADAT-KEY-N    = WS-NEXT-YEAR                              
024830        MOVE YES TO KEYS-SW                                               
024831      ELSE                                                                
024832        MOVE NOO TO KEYS-SW                                               
024833        MOVE IS-INVALID   TO WS-IDMSG-ERROR                               
024834        MOVE 'DASTADAT'   TO WS-IDELMT-ERROR                              
024840     END-IF                                                               
024850                                                                          
024900     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
025000       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
025100       MOVE NOO TO KEYS-SW                                                
025200     END-IF                                                               
025300                                                                          
025400     IF KEYS-FEL                                                          
025500       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
025600       IF REQU-KDPGMACT = 'S' OR = 'U' OR = 'I' OR = 'D'                  
025700         CONTINUE                                                         
025800       ELSE                                                               
025900         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
026000         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
026100       END-IF                                                             
026200       IF REQU-IDMSGVER NUMERIC                                           
026300         CONTINUE                                                         
026400       ELSE                                                               
026500         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
026600         MOVE 'IDMSGVER'   TO WS-IDELMT-ERROR                             
026700       END-IF                                                             
026800       IF REQU-IDUSER = SPACE OR = ALL '+'                                
026900         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
027000         MOVE 'IDUSER'     TO WS-IDELMT-ERROR                             
027100       ELSE                                                               
027200         CONTINUE                                                         
027300       END-IF                                                             
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027700                                                                          
027800 C-CHECK-INDATA SECTION.                                                  
027900     PERFORM DB2-SELECT-T01LSEL                                           
028000     IF LINES-FOUND                                                       
028100       CONTINUE                                                           
028200     ELSE                                                                 
028300       MOVE NOO            TO KEYS-SW                                     
028400       MOVE NOT-FOUND      TO WS-IDMSG-ERROR                              
028500       MOVE 'IDLEGSEL'     TO WS-IDELMT-ERROR                             
028600     END-IF                                                               
028700                                                                          
028800     IF KEYS-OK                                                           
028900       IF REQU-KDPGMACT = 'U' OR 'I'                                      
028910         INSPECT REQU-REVALUTA-FROM TALLYING WS-CNT1 FOR ALL '.'          
028920         IF WS-CNT1 > 0                                                   
028921           MOVE IS-INVALID            TO WS-IDMSG-ERROR                   
028922           MOVE 'REVALUTA-FROM'       TO WS-IDELMT-ERROR                  
028923           MOVE NOO                   TO KEYS-SW                          
028924           MOVE ZERO                  TO WS-CNT1                          
028930         END-IF                                                           
028940         INSPECT REQU-REVALUTA-TO   TALLYING WS-CNT2 FOR ALL '.'          
028950         IF WS-CNT2 > 0                                                   
028960           MOVE IS-INVALID            TO WS-IDMSG-ERROR                   
028970           MOVE 'REVALUTA-TO'         TO WS-IDELMT-ERROR                  
028980           MOVE NOO                   TO KEYS-SW                          
028981           MOVE ZERO                  TO WS-CNT2                          
028990         END-IF                                                           
028991         IF KEYS-OK                                                       
029000           IF REQU-REVALUTA-FROM = ALL '+' OR SPACES                      
029100             MOVE MUST-BE-ENTERED       TO WS-IDMSG-ERROR                 
029200             MOVE 'REVALUTA-FROM'       TO WS-IDELMT-ERROR                
029300             MOVE NOO                   TO KEYS-SW                        
029500           ELSE                                                           
029600             MOVE REQU-REVALUTA-FROM TO WS-REVALUTA-FROM                  
029700           END-IF                                                         
029800           IF REQU-REVALUTA-TO = ALL '+' OR SPACES                        
029900             MOVE MUST-BE-ENTERED       TO WS-IDMSG-ERROR                 
030000             MOVE 'REVALUTA-TO'         TO WS-IDELMT-ERROR                
030100             MOVE NOO                   TO KEYS-SW                        
030300           ELSE                                                           
030400             MOVE REQU-REVALUTA-TO   TO WS-REVALUTA-TO                    
030500           END-IF                                                         
030600           IF WS-REVALUTA-FROM NOT NUMERIC OR                             
030700              REQU-REVALUTA-FROM NOT NUMERIC                              
030800             MOVE MUST-BE-NUMERIC       TO WS-IDMSG-ERROR                 
030900             MOVE 'REVALUTA-FROM'       TO WS-IDELMT-ERROR                
031000             MOVE NOO            TO KEYS-SW                               
031100           END-IF                                                         
031200           IF WS-REVALUTA-TO   NOT NUMERIC OR                             
031300              REQU-REVALUTA-TO NOT NUMERIC                                
031400             MOVE MUST-BE-NUMERIC       TO WS-IDMSG-ERROR                 
031500             MOVE 'REVALUTA-TO'         TO WS-IDELMT-ERROR                
031600             MOVE NOO            TO KEYS-SW                               
031700           END-IF                                                         
031710         END-IF                                                           
031800                                                                          
031900         IF KEYS-OK                                                       
032000           IF REQU-PRKURS-NEW(1:1) = '.'                                  
032100             IF REQU-PRKURS-NEW(6:2) NUMERIC                              
032200               MOVE REQU-PRKURS-NEW(6:2) TO WS-PRKURS-SIST                
032300               MOVE SPACE TO REQU-PRKURS-NEW(6:2)                         
032400             END-IF                                                       
032500           END-IF                                                         
032600           IF REQU-PRKURS-NEW(2:1) = '.'                                  
032700             IF REQU-PRKURS-NEW(7:2) NUMERIC                              
032800               MOVE REQU-PRKURS-NEW(7:2) TO WS-PRKURS-SIST                
032900               MOVE SPACE TO REQU-PRKURS-NEW(7:2)                         
033000             END-IF                                                       
033100           END-IF                                                         
033200           IF REQU-PRKURS-NEW(3:1) = '.'                                  
033300             IF REQU-PRKURS-NEW(8:2) NUMERIC                              
033400               MOVE REQU-PRKURS-NEW(8:2) TO WS-PRKURS-SIST                
033500               MOVE SPACE TO REQU-PRKURS-NEW(8:2)                         
033600             END-IF                                                       
033700           END-IF                                                         
033800           IF REQU-PRKURS-NEW(4:1) = '.'                                  
033900             IF REQU-PRKURS-NEW(9:2) NUMERIC                              
034000               MOVE REQU-PRKURS-NEW(9:2) TO WS-PRKURS-SIST                
034100               MOVE SPACE TO REQU-PRKURS-NEW(9:2)                         
034200             END-IF                                                       
034300           END-IF                                                         
034400           IF REQU-PRKURS-NEW(5:1) = '.'                                  
034500             IF REQU-PRKURS-NEW(10:2) NUMERIC                             
034600               MOVE REQU-PRKURS-NEW(10:2) TO WS-PRKURS-SIST               
034700               MOVE SPACE TO REQU-PRKURS-NEW(10:2)                        
034800             END-IF                                                       
034900           END-IF                                                         
035000           IF REQU-PRKURS-NEW(6:1) = '.'                                  
035100             IF REQU-PRKURS-NEW(11:2) NUMERIC                             
035200               MOVE REQU-PRKURS-NEW(11:2) TO WS-PRKURS-SIST               
035300               MOVE SPACE TO REQU-PRKURS-NEW(11:2)                        
035400             END-IF                                                       
035500           END-IF                                                         
035600           IF REQU-PRKURS-NEW(7:1) = '.'                                  
035700             IF REQU-PRKURS-NEW(12:2) NUMERIC                             
035800               MOVE REQU-PRKURS-NEW(12:2) TO WS-PRKURS-SIST               
035900               MOVE SPACE TO REQU-PRKURS-NEW(12:2)                        
036000             END-IF                                                       
036100           END-IF                                                         
036200           MOVE REQU-PRKURS-NEW TO DEC-IDFRIDATA                          
036300           MOVE 6               TO DEC-KVHELTAL                           
036400           MOVE 4               TO DEC-KVDECIMAL                          
036500                                                                          
036600           CALL WDECEDIT USING DEC-WDECAREA                               
036700                                                                          
036800           IF DEC-KDSVAR-OK                                               
036900             MOVE DEC-IDEDITDATA   TO WS-PRKURS-DEC                       
037000             IF WS-PRKURS-DEC NUMERIC                                     
037100               MOVE WS-PRKURS-DEC         TO WS-PRKURS-HELTAL             
037200               IF WS-PRKURS-HELTAL > 1000000                              
037300                 MOVE MUST-BE-NUMERIC       TO WS-IDMSG-ERROR             
037400                 MOVE 'PRKURS'              TO WS-IDELMT-ERROR            
037500                 MOVE NOO            TO KEYS-SW                           
037600               ELSE                                                       
037700                 MOVE WS-PRKURS-DEC    TO WS-PRKURS-NUM                   
037800                 IF REQU-PRKURS-NEW(1:1) = '.'                            
037900                   IF WS-PRKURS-SIST > ZERO                               
038000                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
038100                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
038200                                              WS-PRKURS-JUST2             
038300                   END-IF                                                 
038400                 END-IF                                                   
038500                 IF REQU-PRKURS-NEW(2:1) = '.'                            
038600                   IF WS-PRKURS-SIST > ZERO                               
038700                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
038800                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
038900                                              WS-PRKURS-JUST2             
039000                   END-IF                                                 
039100                 END-IF                                                   
039200                 IF REQU-PRKURS-NEW(3:1) = '.'                            
039300                   IF WS-PRKURS-SIST > ZERO                               
039400                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
039500                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
039600                                              WS-PRKURS-JUST2             
039700                   END-IF                                                 
039800                 END-IF                                                   
039900                 IF REQU-PRKURS-NEW(4:1) = '.'                            
040000                   IF WS-PRKURS-SIST > ZERO                               
040100                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
040200                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
040300                                              WS-PRKURS-JUST2             
040400                   END-IF                                                 
040500                 END-IF                                                   
040600                 IF REQU-PRKURS-NEW(5:1) = '.'                            
040700                   IF WS-PRKURS-SIST > ZERO                               
040800                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
040900                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
041000                                              WS-PRKURS-JUST2             
041100                   END-IF                                                 
041200                 END-IF                                                   
041300                 IF REQU-PRKURS-NEW(6:1) = '.'                            
041400                   IF WS-PRKURS-SIST > ZERO                               
041500                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
041600                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
041700                                              WS-PRKURS-JUST2             
041800                   END-IF                                                 
041900                 END-IF                                                   
042000                 IF REQU-PRKURS-NEW(7:1) = '.'                            
042100                   IF WS-PRKURS-SIST > ZERO                               
042200                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
042300                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
042400                                              WS-PRKURS-JUST2             
042500                   END-IF                                                 
042600                 END-IF                                                   
042700               END-IF                                                     
042800             ELSE                                                         
042900               MOVE MUST-BE-NUMERIC       TO WS-IDMSG-ERROR               
043000               MOVE 'PRKURS'              TO WS-IDELMT-ERROR              
043100               MOVE NOO            TO KEYS-SW                             
043200             END-IF                                                       
043300           ELSE                                                           
043400             MOVE MUST-BE-NUMERIC       TO WS-IDMSG-ERROR                 
043500             MOVE 'PRKURS'              TO WS-IDELMT-ERROR                
043600             MOVE NOO            TO KEYS-SW                               
043700           END-IF                                                         
043800         END-IF                                                           
043900       END-IF                                                             
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300                                                                          
044400 D-PERFORM-REQUEST SECTION.                                               
044500     IF REQU-KDPGMACT = 'S'                                               
044600         PERFORM DB2-SELECT-T01CUYE                                       
044700         IF LINES-FOUND                                                   
044800           CONTINUE                                                       
044900         ELSE                                                             
045000           MOVE NOT-FOUND       TO WS-IDMSG-ERROR                         
045100           MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                        
045200           MOVE NOO TO KEYS-SW                                            
045300         END-IF                                                           
045400     END-IF                                                               
045500                                                                          
045600     IF REQU-KDPGMACT = 'U'                                               
045700         PERFORM DB2-SELECT-T01CUYE                                       
045800         IF LINES-FOUND AND                                               
045900           T01CUYE-DADELDAT = WS-ACTIVE                                   
046000           PERFORM DB2-UPDATE-T01CUYE                                     
046100           PERFORM DB2-SELECT-T01CUYE                                     
046200           MOVE UPDATE-DONE        TO WS-IDMSG-INFO                       
046300         ELSE                                                             
046400           IF LINES-FOUND AND                                             
046500              T01CUYE-DADELDAT NOT = WS-ACTIVE                            
046600             MOVE UPDATE-NOT-ALLOWED TO WS-IDMSG-ERROR                    
046700             MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                      
046800             MOVE NOO              TO KEYS-SW                             
046900           ELSE                                                           
047000             IF LINES-MISSING                                             
047100               MOVE NOT-FOUND      TO WS-IDMSG-ERROR                      
047200               MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                    
047300               MOVE NOO            TO KEYS-SW                             
047400             END-IF                                                       
047500           END-IF                                                         
047600         END-IF                                                           
047700     END-IF                                                               
047800                                                                          
047900     IF REQU-KDPGMACT = 'I'                                               
048000         PERFORM DB2-SELECT-T01CUYE                                       
048100         IF LINES-FOUND AND                                               
048200            T01CUYE-DADELDAT = WS-ACTIVE                                  
048300           MOVE ALREADY-EXISTS   TO WS-IDMSG-ERROR                        
048400           MOVE 'CURRENCY CODE'  TO WS-IDELMT-ERROR                       
048500           MOVE NOO              TO KEYS-SW                               
048600         ELSE                                                             
048700           IF  LINES-FOUND                                                
048800           AND T01CUYE-DADELDAT     NOT = WS-ACTIVE                       
048810           AND WS-DASTADAT-KEY(1:4) NOT = WS-DATUM(1:4)                   
048820           AND WS-DASTADAT-KEY-N        = WS-NEXT-YEAR                    
048900             PERFORM DB2-DELETE-T01CUYE                                   
049000             PERFORM DB2-INSERT-T01CUYE                                   
049100             PERFORM DB2-SELECT-T01CUYE                                   
049200             MOVE INSERT-DONE    TO WS-IDMSG-INFO                         
049300           ELSE                                                           
049400             IF LINES-MISSING                                             
049500                 PERFORM DB2-INSERT-T01CUYE                               
049600                 PERFORM DB2-SELECT-T01CUYE                               
049700                 MOVE INSERT-DONE    TO WS-IDMSG-INFO                     
049800             END-IF                                                       
049900           END-IF                                                         
050000         END-IF                                                           
050100     END-IF                                                               
050200                                                                          
050210     IF REQU-KDPGMACT = 'D'                                               
050220         PERFORM DB2-SELECT-T01CUYE                                       
050230         IF LINES-FOUND AND                                               
050240           T01CUYE-DADELDAT = WS-ACTIVE                                   
050250           PERFORM DB2-UPDATE-T01CUYE-DELETE                              
050260           PERFORM DB2-SELECT-T01CUYE                                     
050270           MOVE DELETE-DONE        TO WS-IDMSG-INFO                       
050280         ELSE                                                             
050290           IF LINES-FOUND AND                                             
050291             T01CUYE-DADELDAT NOT = WS-ACTIVE                             
050292             MOVE DELETE-NOT-ALLOWED TO WS-IDMSG-ERROR                    
050293             MOVE 'CURRENCY CODE'  TO WS-IDELMT-ERROR                     
050294             MOVE NOO              TO KEYS-SW                             
050295           ELSE                                                           
050296             IF LINES-MISSING                                             
050297               MOVE NOT-FOUND      TO WS-IDMSG-ERROR                      
050298               MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                    
050299               MOVE NOO            TO KEYS-SW                             
050300             END-IF                                                       
050301           END-IF                                                         
050310         END-IF                                                           
050400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700                                                                          
052800 E-BUILD-DATA-RESPONSE SECTION.                                           
052900       MOVE T01CUYE-REVALUTA-FROM   TO RESP-REVALUTA-FROM                 
053000       MOVE T01CUYE-REVALUTA-TO     TO RESP-REVALUTA-TO                   
053100       MOVE T01CUYE-PRKURS-NEW      TO RESP-PRKURS-NEW                    
053200       MOVE T01CUYE-DAREGDAT        TO RESP-DAREGDAT                      
053300       MOVE T01CUYE-DAUPPDAT        TO RESP-DAUPPDAT                      
053400       MOVE T01CUYE-DADELDAT        TO RESP-DADELDAT                      
053500       MOVE T01CUYE-IDUSER          TO RESP-IDUSER                        
053600     .                                                                    
053700     EJECT                                                                
053800                                                                          
053900 F-BUILD-HEADER-RESPONSE SECTION.                                         
054000     MOVE REQU-IDMSGVER             TO RESP-IDMSGVER                      
054100     MOVE WS-IDMSG-INFO             TO RESP-IDMSG-INFO                    
054200     MOVE WS-IDMSG-ERROR            TO RESP-IDMSG-ERROR                   
054300     MOVE WS-IDELMT-ERROR           TO RESP-IDELMT-ERROR                  
054400     MOVE REQU-IDLEGSEL-KEY         TO RESP-IDLEGSEL-KEY                  
054500     MOVE T01LSEL-BELEGRAD-1        TO RESP-BELEGRAD-1                    
054600     MOVE REQU-KDVALISO-KEY         TO RESP-KDVALISO-KEY                  
054700     MOVE REQU-DASTADAT-KEY(1:4)    TO RESP-DASTADAT-KEY(1:4)             
054800     IF REQU-DASTADAT-KEY(1:4) = SPACES OR ZEROS                          
054900       MOVE ZEROS                   TO RESP-DASTADAT-KEY                  
055000     ELSE                                                                 
055100       MOVE 0101                    TO RESP-DASTADAT-KEY(5:4)             
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500                                                                          
055600*    --- DISPATCHER-SECTIONS                                              
055700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
055800     MOVE 'GETARG'                   TO SUB-KDFUNC                        
055900     MOVE 'CARPARTS.BILLIT.APPROVEDYRCURRENCIESMAINTENANCE'               
056000       TO SUB-ADDISPABS                                                   
056100     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
056200                                                                          
056300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
056400                                                                          
056500     IF SUB-KDRC > 0                                                      
056600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
056700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
056800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
056900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057000     END-IF                                                               
057100     .                                                                    
057200                                                                          
057300 S02-RETURN-RESPONSE SECTION.                                             
057400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
057500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
057600                                                                          
057700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
057800                                                                          
057900     IF SUB-KDRC > 0                                                      
058000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
058100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
058200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
058300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700                                                                          
058800 DB2-SELECT-T01LSEL SECTION.                                              
058900     MOVE 000100  TO GOOD-SQLCODECODES                                    
059000                                                                          
059100     EXEC SQL                                                             
059200         SELECT  BELEGRAD_1                                               
059300                                                                          
059400         INTO    :T01LSEL-BELEGRAD-1                                      
059500                                                                          
059600         FROM    T01LSEL                                                  
059700                                                                          
059800         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
059900         AND     KDSTATUS = 001                                           
060000     END-EXEC                                                             
060100     MOVE SQLCODE TO SQLCODE-WS                                           
060200     PERFORM DB2-STATUS-CHECK                                             
060300     .                                                                    
060400     EJECT                                                                
060500                                                                          
060600 DB2-SELECT-T01CUYE SECTION.                                              
060700     MOVE 000100  TO GOOD-SQLCODECODES                                    
060800                                                                          
060900     EXEC SQL                                                             
061000         SELECT  IDLEGSEL                                                 
061100                ,KDVALISO                                                 
061200                ,DASTADAT                                                 
061300                ,PRKURS_NEW                                               
061400                ,REVALUTA_FROM                                            
061500                ,REVALUTA_TO                                              
061600                ,DAREGDAT                                                 
061700                ,DAUPPDAT                                                 
061800                ,DADELDAT                                                 
061900                ,IDUSER                                                   
062000                                                                          
062100         INTO   :T01CUYE-IDLEGSEL                                         
062200               ,:T01CUYE-KDVALISO                                         
062300               ,:T01CUYE-DASTADAT                                         
062400               ,:T01CUYE-PRKURS-NEW                                       
062500               ,:T01CUYE-REVALUTA-FROM                                    
062600               ,:T01CUYE-REVALUTA-TO                                      
062700               ,:T01CUYE-DAREGDAT                                         
062800               ,:T01CUYE-DAUPPDAT                                         
062900               ,:T01CUYE-DADELDAT                                         
063000               ,:T01CUYE-IDUSER                                           
063100                                                                          
063200         FROM    T01CUYE                                                  
063300                                                                          
063400         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
063500         AND     KDVALISO = :REQU-KDVALISO-KEY                            
063600         AND     DASTADAT = :WS-DASTADAT-KEY                              
063700     END-EXEC                                                             
063800                                                                          
063900     MOVE SQLCODE TO SQLCODE-WS                                           
064000     PERFORM DB2-STATUS-CHECK                                             
064100     .                                                                    
064200     EJECT                                                                
064300                                                                          
064400                                                                          
064500 DB2-UPDATE-T01CUYE SECTION.                                              
064600     MOVE 000    TO GOOD-SQLCODECODES                                     
064700     EXEC SQL                                                             
064800         UPDATE T01CUYE                                                   
064900                                                                          
065000         SET    PRKURS_NEW    = :WS-PRKURS-NUM                            
065100               ,REVALUTA_FROM = :WS-REVALUTA-FROM                         
065200               ,REVALUTA_TO   = :WS-REVALUTA-TO                           
065300               ,DAUPPDAT      = :WS-DATUM                                 
065400               ,IDUSER        = :REQU-IDUSER                              
065500                                                                          
065600         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
065700         AND    KDVALISO = :REQU-KDVALISO-KEY                             
065800         AND    DASTADAT = :WS-DASTADAT-KEY                               
065900     END-EXEC                                                             
066000                                                                          
066100     MOVE SQLCODE TO SQLCODE-WS                                           
066200     PERFORM DB2-STATUS-CHECK                                             
066300     .                                                                    
066400     EJECT                                                                
066500                                                                          
066600 DB2-UPDATE-T01CUYE-DELETE SECTION.                                       
066700     MOVE 000    TO GOOD-SQLCODECODES                                     
066800     EXEC SQL                                                             
066900         UPDATE T01CUYE                                                   
067000                                                                          
067100         SET    DADELDAT = :WS-DATUM                                      
067200               ,IDUSER   = :REQU-IDUSER                                   
067300                                                                          
067400         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
067500         AND    KDVALISO = :REQU-KDVALISO-KEY                             
067600         AND    DASTADAT = :WS-DASTADAT-KEY                               
067700     END-EXEC                                                             
067800                                                                          
067900     MOVE SQLCODE TO SQLCODE-WS                                           
068000     PERFORM DB2-STATUS-CHECK                                             
068100     .                                                                    
068200     EJECT                                                                
068300                                                                          
068400 DB2-INSERT-T01CUYE SECTION.                                              
068500     MOVE 000   TO GOOD-SQLCODECODES                                      
068600                                                                          
068700     EXEC SQL                                                             
068800         INSERT                                                           
068900                                                                          
069000         INTO     T01CUYE                                                 
069100                                                                          
069200                 (IDLEGSEL                                                
069300                 ,KDVALISO                                                
069400                 ,DASTADAT                                                
069500                 ,PRKURS_NEW                                              
069600                 ,REVALUTA_FROM                                           
069700                 ,REVALUTA_TO                                             
069800                 ,DAREGDAT                                                
069900                 ,DAUPPDAT                                                
070000                 ,DADELDAT                                                
070100                 ,IDUSER)                                                 
070200                                                                          
070300         VALUES (:REQU-IDLEGSEL-KEY                                       
070400                ,:REQU-KDVALISO-KEY                                       
070500                ,:WS-DASTADAT-KEY                                         
070600                ,:WS-PRKURS-NUM                                           
070700                ,:WS-REVALUTA-FROM                                        
070800                ,:WS-REVALUTA-TO                                          
070900                ,:WS-DATUM                                                
071000                ,'00000000'                                               
071100                ,'00000000'                                               
071200                ,:REQU-IDUSER)                                            
071300     END-EXEC                                                             
071400                                                                          
071500     MOVE SQLCODE TO SQLCODE-WS                                           
071600     PERFORM DB2-STATUS-CHECK                                             
071700     .                                                                    
071800     EJECT                                                                
071900                                                                          
072000 DB2-DELETE-T01CUYE SECTION.                                              
072100     MOVE 000    TO GOOD-SQLCODECODES                                     
072200                                                                          
072300     EXEC SQL                                                             
072400         DELETE                                                           
072500                                                                          
072600         FROM   T01CUYE                                                   
072700                                                                          
072800         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
072900         AND    KDVALISO = :REQU-KDVALISO-KEY                             
073000         AND    DASTADAT = :WS-DASTADAT-KEY                               
073100     END-EXEC                                                             
073200                                                                          
073300     MOVE SQLCODE TO SQLCODE-WS                                           
073400     PERFORM DB2-STATUS-CHECK                                             
073500     .                                                                    
073600     EJECT                                                                
073700                                                                          
073800 DB2-STATUS-CHECK     SECTION.                                            
073900     SET SQLCODE-IX TO 1                                                  
074000     SEARCH GOOD-SQLCODE                                                  
074100       AT END                                                             
074200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
074300          DELIMITED BY SIZE INTO ERROR-TEXT                               
074400          CALL ABEND USING RKOD-ABEND-DB2                                 
074500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
074600     END-SEARCH                                                           
074700     .                                                                    
