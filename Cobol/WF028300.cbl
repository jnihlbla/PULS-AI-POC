000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF028300.                                                
000400 AUTHOR.         ANDERS HENRIKSSON                                        
000500 DATE-WRITTEN.   MARCH   2006.                                            
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME                                                                 
000900*        CARPARTS.BILLIT.APPROVEDCURRENCIESMAINTENANCE                    
001000*    FUNCTION:                                                            
001100*        MAINTENANCE YEARLY CURRENCY.                                     
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS     TABLE T01LSEL                              
001500*        THE PROGRAM UPDATES   TABLE T01CURY                              
001600*        THE PROGRAM READS     TABLE T01CURR                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: WF0283U                                             
002000*        REQUEST:     WF0283I1                                            
002100*                                                                         
002200*    OUTDATA.                                                             
002300*        RESPONSE:    WF0283O1                                            
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600 INPUT-OUTPUT SECTION.                                                    
002700 FILE-CONTROL.                                                            
002800 DATA DIVISION.                                                           
002900 FILE SECTION.                                                            
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'WF028300'.            
003300                                                                          
003400*    --- WORK FIELD FOR ERROR MESSAGE WHEN CALLING ABEND.                 
003500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700                                                                          
003800 77  YES                         PIC X       VALUE 'Y'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
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
009400*    03  -COPY WF0283I1                                                   
009500     EJECT                                                                
009600                                                                          
009700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009800                                                                          
009900 01  RESP-AREA.                                                           
010000*    03  -COPY WZ01RESP                                                   
010100*    03  -COPY WF0283O1                                                   
010200     EJECT                                                                
010700                                                                          
010800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
010900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011000                                                                          
011100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
011200 01  DB2-WS.                                                              
011300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011400         88  CURSOR-OK                       VALUE 000.                   
011500         88  LINES-FOUND                     VALUE 000.                   
011600         88  LINES-MISSING                   VALUE 100.                   
011700         88  RESOURCE-WRONG                  VALUE 904.                   
011800                                                                          
011900     03  GOOD-SQLCODECODES.                                               
012000         05  GOOD-SQLCODE OCCURS 5                                        
012100             INDEXED BY SQLCODE-IX PIC 9(3).                              
012200     EJECT                                                                
012300                                                                          
012400 01  WS-AREA.                                                             
012500     03 WS-PRKURS-RED           PIC Z(6)9.9(5).                           
012600     03 WS-PRKURS-NUM           PIC S9(6)V9(5) COMP-3.                    
012700     03 WS-PRKURS-DEC           PIC 9(6)V9(5)  VALUE ZERO.                
012800     03 WS-PRKURS-HELTAL        PIC 9(6).                                 
012900     03 WS-ACTIVE               PIC X(8)       VALUE '00000000'.          
013000     03 WS-IDMSG-INFO           PIC X(3)       VALUE SPACE.               
013100     03 WS-IDMSG-ERROR          PIC X(3)       VALUE SPACE.               
013200     03 WS-IDELMT-ERROR         PIC X(16)      VALUE SPACE.               
013600     03 WS-DASTADAT-KEY         PIC X(8)       VALUE SPACE.               
013700     03 WS-REVALUTA             PIC S9(3)      COMP-3.                    
013710     03 WS-DATUM                PIC X(8)       VALUE SPACE.               
013800 01  WS-PRKURS-JUST2            PIC 9(6)V9(5)  VALUE ZERO.                
013900 01  WS-PRKURS-JUST             PIC 9(6)V9(5)  VALUE ZERO.                
014000 01  FILLER REDEFINES WS-PRKURS-JUST.                                     
014100     03  FILLER                 PIC 9(6)V9(4).                            
014200     03  WS-PRKURS-SIST         PIC 9(1).                                 
014300     EJECT                                                                
014400                                                                          
014500 01  FILLER                  PIC X(16)    VALUE 'T01LSEL-AREA'.           
014600*01  -COPY T01LSEL -PRE T01LSEL-                                          
014700     EJECT                                                                
014800                                                                          
014900 01  FILLER                  PIC X(16)    VALUE 'T01CURY-AREA'.           
015000*01  -COPY T01CURY -PRE T01CURY-                                          
015100     EJECT                                                                
015200                                                                          
015300 01  FILLER                  PIC X(16)    VALUE 'T01CURR-AREA'.           
015400*01  -COPY T01CURR -PRE T01CURR-                                          
015500     EJECT                                                                
015600                                                                          
015700     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
015800     EJECT                                                                
015900                                                                          
016000     EXEC SQL INCLUDE T01CURY END-EXEC.                                   
016100     EJECT                                                                
016200                                                                          
016300     EXEC SQL INCLUDE T01CURR END-EXEC.                                   
016400     EJECT                                                                
016500                                                                          
016600 LINKAGE SECTION.                                                         
016700 PROCEDURE DIVISION.                                                      
016800 MAIN SECTION.                                                            
016900     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017000     IF SUB-KDRC = ZERO                                                   
017100       PERFORM A-INIT                                                     
017200       PERFORM B-CHECK-KEYS                                               
017300       IF KEYS-OK                                                         
017400         PERFORM C-CHECK-INDATA                                           
017500       END-IF                                                             
017600       IF KEYS-OK                                                         
017700         PERFORM D-PERFORM-REQUEST                                        
017800         IF KEYS-OK                                                       
017900           PERFORM E-BUILD-DATA-RESPONSE                                  
018000         END-IF                                                           
018100       END-IF                                                             
018200       PERFORM F-BUILD-HEADER-RESPONSE                                    
018300       PERFORM S02-RETURN-RESPONSE                                        
018400     END-IF                                                               
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
018900     EJECT                                                                
019000                                                                          
019100 A-INIT SECTION.                                                          
019200     MOVE YES                         TO KEYS-SW                          
019300                                                                          
019400     MOVE ALL ' '                     TO RESP-AREA                        
019500     MOVE SPACE TO RESP-IDMSG-ERROR                                       
019600     MOVE SPACE TO RESP-IDMSG-INFO                                        
019700     MOVE SPACE TO RESP-IDELMT-ERROR                                      
019800                                                                          
019900     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
020000                                                                          
020100     INITIALIZE GOOD-SQLCODECODES                                         
020200     .                                                                    
020300     EJECT                                                                
020400                                                                          
020500 B-CHECK-KEYS SECTION.                                                    
020600     IF REQU-KDPGMACT = 'S'                                               
020700     AND REQU-IDMSGVER NUMERIC                                            
020800       CONTINUE                                                           
020900     ELSE                                                                 
021000       IF REQU-KDPGMACT = 'I'                                             
021100       AND REQU-IDMSGVER NUMERIC                                          
021200         CONTINUE                                                         
021300       ELSE                                                               
021400         IF REQU-KDPGMACT = 'U'                                           
021500         AND REQU-IDMSGVER NUMERIC                                        
021600           CONTINUE                                                       
021700         ELSE                                                             
021800           IF REQU-KDPGMACT = 'D'                                         
021900           AND REQU-IDMSGVER NUMERIC                                      
022000             CONTINUE                                                     
022100           ELSE                                                           
022200             MOVE NOO TO KEYS-SW                                          
022300             MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                         
022400           END-IF                                                         
022500         END-IF                                                           
022600       END-IF                                                             
022700     END-IF                                                               
022800                                                                          
022900     IF REQU-IDLEGSEL-KEY = SPACE OR = ALL '+'                            
023000       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
023100       MOVE NOO TO KEYS-SW                                                
023200     END-IF                                                               
023300                                                                          
023400     IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                            
023500       MOVE NOO TO KEYS-SW                                                
023600       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
023700     END-IF                                                               
023800                                                                          
023900     MOVE REQU-DASTADAT-KEY TO WS-DASTADAT-KEY                            
024000     IF WS-DASTADAT-KEY = SPACE OR = ALL '+'                              
024100       MOVE NOO TO KEYS-SW                                                
024200       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
024300     END-IF                                                               
024400                                                                          
024500     IF WS-DASTADAT-KEY(5:4) NOT = '0101'                                 
024600       MOVE NOO TO KEYS-SW                                                
024700       MOVE IS-INVALID   TO WS-IDMSG-ERROR                                
024800       MOVE 'DASTADAT'   TO WS-IDELMT-ERROR                               
024900     END-IF                                                               
025000                                                                          
025100     IF WS-DASTADAT-KEY(1:4) < WS-DATUM(1:4)                              
025200       MOVE NOO TO KEYS-SW                                                
025300       MOVE IS-INVALID   TO WS-IDMSG-ERROR                                
025400       MOVE 'DASTADAT'   TO WS-IDELMT-ERROR                               
025500     END-IF                                                               
025600     MOVE REQU-DASTADAT-KEY      TO WS-DASTADAT-KEY                       
025700                                                                          
025800     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
025900       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
026000       MOVE NOO TO KEYS-SW                                                
026100     END-IF                                                               
026200                                                                          
026300     IF KEYS-FEL                                                          
026400       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
026500       IF REQU-KDPGMACT = 'S' OR = 'U' OR = 'I' OR = 'D'                  
026600         CONTINUE                                                         
026700       ELSE                                                               
026800         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
026900         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
027000       END-IF                                                             
027100       IF REQU-IDMSGVER NUMERIC                                           
027200         CONTINUE                                                         
027300       ELSE                                                               
027400         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
027500         MOVE 'IDMSGVER'   TO WS-IDELMT-ERROR                             
027600       END-IF                                                             
027700       IF REQU-IDUSER = SPACE OR = ALL '+'                                
027800         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
027900         MOVE 'IDUSER'     TO WS-IDELMT-ERROR                             
028000       ELSE                                                               
028100         CONTINUE                                                         
028200       END-IF                                                             
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600                                                                          
028700 C-CHECK-INDATA SECTION.                                                  
028800     PERFORM DB2-SELECT-T01LSEL                                           
028900     IF LINES-FOUND                                                       
029000       CONTINUE                                                           
029100     ELSE                                                                 
029200       MOVE NOO            TO KEYS-SW                                     
029300       MOVE NOT-FOUND      TO WS-IDMSG-ERROR                              
029400       MOVE 'IDLEGSEL'     TO WS-IDELMT-ERROR                             
029500     END-IF                                                               
029600                                                                          
029700     IF KEYS-OK                                                           
029800       IF REQU-KDPGMACT = 'U' OR 'I'                                      
029900         IF REQU-REVALUTA = ALL '+' OR SPACES                             
030000           MOVE ZEROS              TO WS-REVALUTA                         
030100         ELSE                                                             
030200           MOVE REQU-REVALUTA      TO WS-REVALUTA                         
030300         END-IF                                                           
030400         IF WS-REVALUTA NOT NUMERIC                                       
030500           MOVE MUST-BE-NUMERIC    TO WS-IDMSG-ERROR                      
030600           MOVE 'REVALUTA'         TO WS-IDELMT-ERROR                     
030700           MOVE NOO                TO KEYS-SW                             
030800         END-IF                                                           
030900         IF WS-REVALUTA = ZEROS                                           
031000           MOVE MUST-BE-ENTERED    TO WS-IDMSG-ERROR                      
031100           MOVE 'REVALUTA'         TO WS-IDELMT-ERROR                     
031200           MOVE NOO                TO KEYS-SW                             
031300         END-IF                                                           
031400                                                                          
031500         IF KEYS-OK                                                       
031600           IF REQU-PRKURS(1:1) = '.'                                      
031700             IF REQU-PRKURS(6:1) NUMERIC                                  
031800               MOVE REQU-PRKURS(6:1) TO WS-PRKURS-SIST                    
031900               MOVE SPACE TO REQU-PRKURS(6:1)                             
032000             END-IF                                                       
032100           END-IF                                                         
032200           IF REQU-PRKURS(2:1) = '.'                                      
032300             IF REQU-PRKURS(7:1) NUMERIC                                  
032400               MOVE REQU-PRKURS(7:1) TO WS-PRKURS-SIST                    
032500               MOVE SPACE TO REQU-PRKURS(7:1)                             
032600             END-IF                                                       
032700           END-IF                                                         
032800           IF REQU-PRKURS(3:1) = '.'                                      
032900             IF REQU-PRKURS(8:1) NUMERIC                                  
033000               MOVE REQU-PRKURS(8:1) TO WS-PRKURS-SIST                    
033100               MOVE SPACE TO REQU-PRKURS(8:1)                             
033200             END-IF                                                       
033300           END-IF                                                         
033400           IF REQU-PRKURS(4:1) = '.'                                      
033500             IF REQU-PRKURS(9:1) NUMERIC                                  
033600               MOVE REQU-PRKURS(9:1) TO WS-PRKURS-SIST                    
033700               MOVE SPACE TO REQU-PRKURS(9:1)                             
033800             END-IF                                                       
033900           END-IF                                                         
034000           IF REQU-PRKURS(5:1) = '.'                                      
034100             IF REQU-PRKURS(10:1) NUMERIC                                 
034200               MOVE REQU-PRKURS(10:1) TO WS-PRKURS-SIST                   
034300               MOVE SPACE TO REQU-PRKURS(10:1)                            
034400             END-IF                                                       
034500           END-IF                                                         
034600           IF REQU-PRKURS(6:1) = '.'                                      
034700             IF REQU-PRKURS(11:1) NUMERIC                                 
034800               MOVE REQU-PRKURS(11:1) TO WS-PRKURS-SIST                   
034900               MOVE SPACE TO REQU-PRKURS(11:1)                            
035000             END-IF                                                       
035100           END-IF                                                         
035200           IF REQU-PRKURS(7:1) = '.'                                      
035300             IF REQU-PRKURS(12:1) NUMERIC                                 
035400               MOVE REQU-PRKURS(12:1) TO WS-PRKURS-SIST                   
035500               MOVE SPACE TO REQU-PRKURS(12:1)                            
035600             END-IF                                                       
035700           END-IF                                                         
035800           MOVE REQU-PRKURS    TO DEC-IDFRIDATA                           
035900           MOVE 6              TO DEC-KVHELTAL                            
036000           MOVE 4              TO DEC-KVDECIMAL                           
036100                                                                          
036200           CALL WDECEDIT USING DEC-WDECAREA                               
036300                                                                          
036400           IF DEC-KDSVAR-OK                                               
036500             MOVE DEC-IDEDITDATA   TO WS-PRKURS-DEC                       
036600             IF WS-PRKURS-DEC NUMERIC                                     
036700               MOVE WS-PRKURS-DEC         TO WS-PRKURS-HELTAL             
036800               IF WS-PRKURS-HELTAL > 100000                               
036900                 MOVE MUST-BE-NUMERIC       TO WS-IDMSG-ERROR             
037000                 MOVE 'PRKURS'              TO WS-IDELMT-ERROR            
037100                 MOVE NOO            TO KEYS-SW                           
037200               ELSE                                                       
037300                 MOVE WS-PRKURS-DEC    TO WS-PRKURS-NUM                   
037400                 IF REQU-PRKURS(1:1) = '.'                                
037500                   IF WS-PRKURS-SIST > ZERO                               
037600                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
037700                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
037800                                              WS-PRKURS-JUST2             
037900                   END-IF                                                 
038000                 END-IF                                                   
038100                 IF REQU-PRKURS(2:1) = '.'                                
038200                   IF WS-PRKURS-SIST > ZERO                               
038300                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
038400                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
038500                                              WS-PRKURS-JUST2             
038600                   END-IF                                                 
038700                 END-IF                                                   
038800                 IF REQU-PRKURS(3:1) = '.'                                
038900                   IF WS-PRKURS-SIST > ZERO                               
039000                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
039100                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
039200                                              WS-PRKURS-JUST2             
039300                   END-IF                                                 
039400                 END-IF                                                   
039500                 IF REQU-PRKURS(4:1) = '.'                                
039600                   IF WS-PRKURS-SIST > ZERO                               
039700                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
039800                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
039900                                              WS-PRKURS-JUST2             
040000                   END-IF                                                 
040100                 END-IF                                                   
040200                 IF REQU-PRKURS(5:1) = '.'                                
040300                   IF WS-PRKURS-SIST > ZERO                               
040400                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
040500                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
040600                                              WS-PRKURS-JUST2             
040700                   END-IF                                                 
040800                 END-IF                                                   
040900                 IF REQU-PRKURS(6:1) = '.'                                
041000                   IF WS-PRKURS-SIST > ZERO                               
041100                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
041200                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
041300                                              WS-PRKURS-JUST2             
041400                   END-IF                                                 
041500                 END-IF                                                   
041600                 IF REQU-PRKURS(7:1) = '.'                                
041700                   IF WS-PRKURS-SIST > ZERO                               
041800                     MOVE WS-PRKURS-JUST   TO WS-PRKURS-JUST2             
041900                     COMPUTE WS-PRKURS-NUM =  WS-PRKURS-NUM +             
042000                                              WS-PRKURS-JUST2             
042100                   END-IF                                                 
042200                 END-IF                                                   
042300               END-IF                                                     
042400             ELSE                                                         
042500               MOVE MUST-BE-NUMERIC       TO WS-IDMSG-ERROR               
042600               MOVE 'PRKURS'              TO WS-IDELMT-ERROR              
042700               MOVE NOO            TO KEYS-SW                             
042800             END-IF                                                       
042900           ELSE                                                           
043000             MOVE MUST-BE-NUMERIC       TO WS-IDMSG-ERROR                 
043100             MOVE 'PRKURS'              TO WS-IDELMT-ERROR                
043200             MOVE NOO            TO KEYS-SW                               
043300           END-IF                                                         
043400         END-IF                                                           
043500       END-IF                                                             
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043900                                                                          
044000 D-PERFORM-REQUEST SECTION.                                               
044100     IF REQU-KDPGMACT = 'S'                                               
044200       IF T01LSEL-FLCUSUPD = 'J'                                          
044300         PERFORM DB2-SELECT-T01CURR-FLCUSUPD                              
044400         IF LINES-FOUND                                                   
044500           CONTINUE                                                       
044600         ELSE                                                             
044700           MOVE NOT-FOUND       TO WS-IDMSG-ERROR                         
044800           MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                        
044900           MOVE NOO TO KEYS-SW                                            
045000         END-IF                                                           
045100       ELSE                                                               
045200         PERFORM DB2-SELECT-T01CURY                                       
045300         IF LINES-FOUND                                                   
045400           CONTINUE                                                       
045500         ELSE                                                             
045600           MOVE NOT-FOUND       TO WS-IDMSG-ERROR                         
045700           MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                        
045800           MOVE NOO TO KEYS-SW                                            
045900         END-IF                                                           
046000       END-IF                                                             
046100     END-IF                                                               
046200                                                                          
046300     IF REQU-KDPGMACT = 'U'                                               
046400       IF T01LSEL-FLCUSUPD = 'J'                                          
046500         PERFORM DB2-SELECT-T01CURR-FLCUSUPD                              
046600         IF LINES-FOUND                                                   
046700           PERFORM DB2-UPDATE-T01CURR                                     
046800           PERFORM DB2-SELECT-T01CURR-FLCUSUPD                            
046900           MOVE UPDATE-DONE          TO WS-IDMSG-INFO                     
047000         ELSE                                                             
047100           IF LINES-MISSING                                               
047200             MOVE NOT-FOUND        TO WS-IDMSG-ERROR                      
047300             MOVE 'CURRENCY CODE'  TO WS-IDELMT-ERROR                     
047400             MOVE NOO              TO KEYS-SW                             
047500           END-IF                                                         
047600         END-IF                                                           
047700       END-IF                                                             
047800       IF T01LSEL-FLCUSUPD = 'N'                                          
047900         PERFORM DB2-SELECT-T01CURY                                       
048000         IF LINES-FOUND AND                                               
048100           T01CURY-DADELDAT = WS-ACTIVE                                   
048200           PERFORM DB2-UPDATE-T01CURY                                     
048300           PERFORM DB2-SELECT-T01CURY                                     
048400           MOVE UPDATE-DONE        TO WS-IDMSG-INFO                       
048500         ELSE                                                             
048600           IF LINES-FOUND AND                                             
048700              T01CURY-DADELDAT NOT = WS-ACTIVE                            
048800             MOVE UPDATE-NOT-ALLOWED TO WS-IDMSG-ERROR                    
048900             MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                      
049000             MOVE NOO              TO KEYS-SW                             
049100           ELSE                                                           
049200             IF LINES-MISSING                                             
049300               MOVE NOT-FOUND      TO WS-IDMSG-ERROR                      
049400               MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                    
049500               MOVE NOO            TO KEYS-SW                             
049600             END-IF                                                       
049700           END-IF                                                         
049800         END-IF                                                           
049900       END-IF                                                             
050000     END-IF                                                               
050100                                                                          
050200     IF REQU-KDPGMACT = 'I'                                               
050300       IF T01LSEL-FLCUSUPD = 'J'                                          
050400         PERFORM DB2-SELECT-T01CURR-FLCUSUPD                              
050500         IF LINES-FOUND                                                   
050600           MOVE ALREADY-EXISTS   TO WS-IDMSG-ERROR                        
050700           MOVE 'CURRENCY CODE'  TO WS-IDELMT-ERROR                       
050800           MOVE NOO              TO KEYS-SW                               
050900         ELSE                                                             
051000           IF LINES-MISSING                                               
051100             PERFORM DB2-INSERT-T01CURR                                   
051200             PERFORM DB2-SELECT-T01CURR-FLCUSUPD                          
052920             MOVE INSERT-DONE  TO WS-IDMSG-INFO                           
053200           END-IF                                                         
053300         END-IF                                                           
053400       END-IF                                                             
053500       IF T01LSEL-FLCUSUPD = 'N'                                          
053600         PERFORM DB2-SELECT-T01CURY                                       
053700         IF LINES-FOUND AND                                               
053800            T01CURY-DADELDAT = WS-ACTIVE                                  
053900           MOVE ALREADY-EXISTS   TO WS-IDMSG-ERROR                        
054000           MOVE 'CURRENCY CODE'  TO WS-IDELMT-ERROR                       
054100           MOVE NOO              TO KEYS-SW                               
054200         ELSE                                                             
054300           IF LINES-FOUND AND                                             
054400              T01CURY-DADELDAT NOT = WS-ACTIVE                            
054500             PERFORM DB2-DELETE-T01CURY                                   
054600             PERFORM DB2-INSERT-T01CURY                                   
054700             PERFORM DB2-SELECT-T01CURY                                   
054800             MOVE INSERT-DONE    TO WS-IDMSG-INFO                         
054900           ELSE                                                           
055000             IF LINES-MISSING                                             
055100               PERFORM DB2-SELECT-T01CURR                                 
055200               IF LINES-FOUND                                             
055300                 PERFORM DB2-INSERT-T01CURY                               
055400                 PERFORM DB2-SELECT-T01CURY                               
055500                 MOVE INSERT-DONE  TO WS-IDMSG-INFO                       
055600               ELSE                                                       
055700                 MOVE IS-INVALID       TO WS-IDMSG-ERROR                  
055800                 MOVE 'CURRENCY CODE'  TO WS-IDELMT-ERROR                 
055900                 MOVE NOO              TO KEYS-SW                         
056000               END-IF                                                     
056100             END-IF                                                       
056200           END-IF                                                         
056300         END-IF                                                           
056400       END-IF                                                             
056500     END-IF                                                               
056600                                                                          
056700     IF REQU-KDPGMACT = 'D'                                               
056800       IF T01LSEL-FLCUSUPD = 'J'                                          
056900         PERFORM DB2-SELECT-T01CURR-FLCUSUPD                              
057000         IF LINES-FOUND                                                   
057100           PERFORM DB2-DELETE-T01CURR                                     
057200           MOVE DELETE-DONE        TO WS-IDMSG-INFO                       
057300         ELSE                                                             
057400           IF LINES-MISSING                                               
057500             MOVE NOT-FOUND      TO WS-IDMSG-ERROR                        
057600             MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                      
057700             MOVE NOO            TO KEYS-SW                               
057800           END-IF                                                         
057900         END-IF                                                           
058000       END-IF                                                             
058100       IF T01LSEL-FLCUSUPD = 'N'                                          
058200         PERFORM DB2-SELECT-T01CURY                                       
058300         IF LINES-FOUND AND                                               
058400           T01CURY-DADELDAT = WS-ACTIVE                                   
058500           PERFORM DB2-UPDATE-T01CURY-DELETE                              
058600           PERFORM DB2-SELECT-T01CURY                                     
058700           MOVE DELETE-DONE        TO WS-IDMSG-INFO                       
058800         ELSE                                                             
058900           IF LINES-FOUND AND                                             
059000             T01CURY-DADELDAT NOT = WS-ACTIVE                             
059100             MOVE DELETE-NOT-ALLOWED TO WS-IDMSG-ERROR                    
059200             MOVE 'CURRENCY CODE'  TO WS-IDELMT-ERROR                     
059300             MOVE NOO              TO KEYS-SW                             
059400           ELSE                                                           
059500             IF LINES-MISSING                                             
059600               MOVE NOT-FOUND      TO WS-IDMSG-ERROR                      
059700               MOVE 'CURRENCY CODE' TO WS-IDELMT-ERROR                    
059800               MOVE NOO            TO KEYS-SW                             
059900             END-IF                                                       
060000           END-IF                                                         
060100         END-IF                                                           
060200       END-IF                                                             
060300     END-IF                                                               
060400     .                                                                    
060500     EJECT                                                                
060600                                                                          
060700 E-BUILD-DATA-RESPONSE SECTION.                                           
060800     IF T01LSEL-FLCUSUPD = 'N'                                            
060900       MOVE T01CURY-REVALUTA        TO RESP-REVALUTA                      
061000       MOVE T01CURY-PRKURS          TO RESP-PRKURS                        
061100       MOVE T01CURY-DAREGDAT        TO RESP-DAREGDAT                      
061200       MOVE T01CURY-DAUPPDAT        TO RESP-DAUPPDAT                      
061300       MOVE T01CURY-DADELDAT        TO RESP-DADELDAT                      
061400       MOVE T01CURY-IDUSER          TO RESP-IDUSER                        
061500     ELSE                                                                 
061600       MOVE T01CURR-REVALUTA        TO RESP-REVALUTA                      
061720       MOVE T01CURR-PRKURS          TO RESP-PRKURS                        
061800       MOVE T01CURR-DAREGDAT        TO RESP-DAREGDAT                      
061900       MOVE '00000000'              TO RESP-DAUPPDAT                      
062000       MOVE '00000000'              TO RESP-DADELDAT                      
062010       MOVE T01CURR-IDUSER          TO RESP-IDUSER                        
062200     END-IF                                                               
062300     .                                                                    
062400     EJECT                                                                
062500                                                                          
062600 F-BUILD-HEADER-RESPONSE SECTION.                                         
062700     MOVE REQU-IDMSGVER             TO RESP-IDMSGVER                      
062800     MOVE WS-IDMSG-INFO             TO RESP-IDMSG-INFO                    
062900     MOVE WS-IDMSG-ERROR            TO RESP-IDMSG-ERROR                   
063000     MOVE WS-IDELMT-ERROR           TO RESP-IDELMT-ERROR                  
063100     MOVE REQU-IDLEGSEL-KEY         TO RESP-IDLEGSEL-KEY                  
063200     MOVE T01LSEL-BELEGRAD-1        TO RESP-BELEGRAD-1                    
063300     MOVE REQU-KDVALISO-KEY         TO RESP-KDVALISO-KEY                  
063400     MOVE REQU-DASTADAT-KEY(1:4)    TO RESP-DASTADAT-KEY(1:4)             
063500     MOVE 0101                      TO RESP-DASTADAT-KEY(5:4)             
063600     .                                                                    
063700     EJECT                                                                
063800                                                                          
063900*    --- DISPATCHER-SECTIONS                                              
064000 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
064100     MOVE 'GETARG'                   TO SUB-KDFUNC                        
064200     MOVE 'CARPARTS.BILLIT.APPROVEDCURRENCIESMAINTENANCE'                 
064300       TO SUB-ADDISPABS                                                   
064400     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
064500                                                                          
064600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
064700                                                                          
064800     IF SUB-KDRC > 0                                                      
064900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
065000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
065100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
065200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
065300     END-IF                                                               
065400     .                                                                    
065500                                                                          
065600 S02-RETURN-RESPONSE SECTION.                                             
065700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
065800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
065900                                                                          
066000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
066100                                                                          
066200     IF SUB-KDRC > 0                                                      
066300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
066400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
066500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
066600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000                                                                          
067100 DB2-SELECT-T01LSEL SECTION.                                              
067200     MOVE 000100  TO GOOD-SQLCODECODES                                    
067300                                                                          
067400     EXEC SQL                                                             
067500         SELECT  BELEGRAD_1                                               
067600                ,FLCUSUPD                                                 
067700                                                                          
067800         INTO    :T01LSEL-BELEGRAD-1                                      
067900                ,:T01LSEL-FLCUSUPD                                        
068000                                                                          
068100         FROM    T01LSEL                                                  
068200                                                                          
068300         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
068400         AND     KDSTATUS = 001                                           
068500     END-EXEC                                                             
068600     MOVE SQLCODE TO SQLCODE-WS                                           
068700     PERFORM DB2-STATUS-CHECK                                             
068800     .                                                                    
068900     EJECT                                                                
069000                                                                          
069100 DB2-SELECT-T01CURY SECTION.                                              
069200     MOVE 000100  TO GOOD-SQLCODECODES                                    
069300                                                                          
069400     EXEC SQL                                                             
069500         SELECT  IDLEGSEL                                                 
069600                ,KDVALISO                                                 
069700                ,DASTADAT                                                 
069800                ,PRKURS                                                   
069900                ,REVALUTA                                                 
070000                ,DAREGDAT                                                 
070100                ,DAUPPDAT                                                 
070200                ,DADELDAT                                                 
070300                ,IDUSER                                                   
070400                                                                          
070500         INTO   :T01CURY-IDLEGSEL                                         
070600               ,:T01CURY-KDVALISO                                         
070700               ,:T01CURY-DASTADAT                                         
070800               ,:T01CURY-PRKURS                                           
070900               ,:T01CURY-REVALUTA                                         
071000               ,:T01CURY-DAREGDAT                                         
071100               ,:T01CURY-DAUPPDAT                                         
071200               ,:T01CURY-DADELDAT                                         
071300               ,:T01CURY-IDUSER                                           
071400                                                                          
071500         FROM    T01CURY                                                  
071600                                                                          
071700         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
071800         AND     KDVALISO = :REQU-KDVALISO-KEY                            
071900         AND     DASTADAT = :WS-DASTADAT-KEY                              
072000     END-EXEC                                                             
072100                                                                          
072200     MOVE SQLCODE TO SQLCODE-WS                                           
072300     PERFORM DB2-STATUS-CHECK                                             
072400     .                                                                    
072500     EJECT                                                                
072600                                                                          
072700 DB2-SELECT-T01CURR-FLCUSUPD SECTION.                                     
072800     MOVE 000100  TO GOOD-SQLCODECODES                                    
072900                                                                          
073000     EXEC SQL                                                             
073100         SELECT  IDLEGSEL                                                 
073200                ,KDVALISO                                                 
073300                ,DASTADAT                                                 
073400                ,PRKURS                                                   
073500                ,REVALUTA                                                 
073600                ,DAREGDAT                                                 
073700                ,REVALUTA-FROM                                            
073800                ,REVALUTA-TO                                              
073900                ,PRKURS-NEW                                               
074000                ,IDUSER                                                   
074100                                                                          
074200         INTO   :T01CURR-IDLEGSEL                                         
074300               ,:T01CURR-KDVALISO                                         
074400               ,:T01CURR-DASTADAT                                         
074500               ,:T01CURR-PRKURS                                           
074600               ,:T01CURR-REVALUTA                                         
074700               ,:T01CURR-DAREGDAT                                         
074800               ,:T01CURR-REVALUTA-FROM                                    
074900               ,:T01CURR-REVALUTA-TO                                      
075000               ,:T01CURR-PRKURS-NEW                                       
075100               ,:T01CURR-IDUSER                                           
075200                                                                          
075300         FROM    T01CURR                                                  
075400                                                                          
075500         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
075600         AND     KDVALISO = :REQU-KDVALISO-KEY                            
075700         AND     DASTADAT = :WS-DASTADAT-KEY                              
075800     END-EXEC                                                             
075900                                                                          
076000     MOVE SQLCODE TO SQLCODE-WS                                           
076100     PERFORM DB2-STATUS-CHECK                                             
076200     .                                                                    
076300     EJECT                                                                
076400                                                                          
076500 DB2-SELECT-T01CURR SECTION.                                              
076600     MOVE 000100  TO GOOD-SQLCODECODES                                    
076700                                                                          
076800     EXEC SQL                                                             
076900         SELECT  DISTINCT(KDVALISO)                                       
077000                                                                          
077100         INTO   :T01CURR-KDVALISO                                         
077200                                                                          
077300         FROM    T01CURR                                                  
077400                                                                          
077500         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
077600         AND     KDVALISO = :REQU-KDVALISO-KEY                            
077700     END-EXEC                                                             
077800                                                                          
077900     MOVE SQLCODE TO SQLCODE-WS                                           
078000     PERFORM DB2-STATUS-CHECK                                             
078100     .                                                                    
078200     EJECT                                                                
078300                                                                          
078400 DB2-UPDATE-T01CURY SECTION.                                              
078500     MOVE 000    TO GOOD-SQLCODECODES                                     
078600     EXEC SQL                                                             
078700         UPDATE T01CURY                                                   
078800                                                                          
078900         SET    PRKURS   = :WS-PRKURS-NUM                                 
079000               ,REVALUTA = :WS-REVALUTA                                   
079100               ,DAUPPDAT = :WS-DATUM                                      
079200               ,IDUSER   = :REQU-IDUSER                                   
079300                                                                          
079400         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
079500         AND    KDVALISO = :REQU-KDVALISO-KEY                             
079600         AND    DASTADAT = :WS-DASTADAT-KEY                               
079700     END-EXEC                                                             
079800                                                                          
079900     MOVE SQLCODE TO SQLCODE-WS                                           
080000     PERFORM DB2-STATUS-CHECK                                             
080100     .                                                                    
080200     EJECT                                                                
080300                                                                          
080400 DB2-UPDATE-T01CURR SECTION.                                              
080500     MOVE 000    TO GOOD-SQLCODECODES                                     
080600     EXEC SQL                                                             
080700         UPDATE T01CURR                                                   
080800                                                                          
080900         SET    PRKURS   = :WS-PRKURS-NUM                                 
081000               ,REVALUTA = :WS-REVALUTA                                   
081100               ,IDUSER   = :REQU-IDUSER                                   
081200                                                                          
081300         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
081400         AND    KDVALISO = :REQU-KDVALISO-KEY                             
081500         AND    DASTADAT = :WS-DASTADAT-KEY                               
081600     END-EXEC                                                             
081700                                                                          
081800     MOVE SQLCODE TO SQLCODE-WS                                           
081900     PERFORM DB2-STATUS-CHECK                                             
082000     .                                                                    
082100     EJECT                                                                
082200                                                                          
082300 DB2-UPDATE-T01CURY-DELETE SECTION.                                       
082400     MOVE 000    TO GOOD-SQLCODECODES                                     
082500     EXEC SQL                                                             
082600         UPDATE T01CURY                                                   
082700                                                                          
082800         SET    DADELDAT = :WS-DATUM                                      
082900               ,IDUSER   = :REQU-IDUSER                                   
083000                                                                          
083100         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
083200         AND    KDVALISO = :REQU-KDVALISO-KEY                             
083300         AND    DASTADAT = :WS-DASTADAT-KEY                               
083400     END-EXEC                                                             
083500                                                                          
083600     MOVE SQLCODE TO SQLCODE-WS                                           
083700     PERFORM DB2-STATUS-CHECK                                             
083800     .                                                                    
083900     EJECT                                                                
084000                                                                          
084100 DB2-DELETE-T01CURR SECTION.                                              
084200     MOVE 000    TO GOOD-SQLCODECODES                                     
084300     EXEC SQL                                                             
084400         DELETE FROM T01CURR                                              
084500                                                                          
084600         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
084700         AND    KDVALISO = :REQU-KDVALISO-KEY                             
084800         AND    DASTADAT = :WS-DASTADAT-KEY                               
084900     END-EXEC                                                             
085000                                                                          
085100     MOVE SQLCODE TO SQLCODE-WS                                           
085200     PERFORM DB2-STATUS-CHECK                                             
085300     .                                                                    
085400     EJECT                                                                
085500                                                                          
085600 DB2-INSERT-T01CURY SECTION.                                              
085700     MOVE 000   TO GOOD-SQLCODECODES                                      
085800                                                                          
085900     EXEC SQL                                                             
086000         INSERT                                                           
086100                                                                          
086200         INTO     T01CURY                                                 
086300                                                                          
086400                 (IDLEGSEL                                                
086500                 ,KDVALISO                                                
086600                 ,DASTADAT                                                
086700                 ,PRKURS                                                  
086800                 ,REVALUTA                                                
086900                 ,DAREGDAT                                                
087000                 ,DAUPPDAT                                                
087100                 ,DADELDAT                                                
087200                 ,IDUSER)                                                 
087300                                                                          
087400         VALUES (:REQU-IDLEGSEL-KEY                                       
087500                ,:REQU-KDVALISO-KEY                                       
087600                ,:WS-DASTADAT-KEY                                         
087700                ,:WS-PRKURS-NUM                                           
087800                ,:WS-REVALUTA                                             
087900                ,:WS-DATUM                                                
088000                ,'00000000'                                               
088100                ,'00000000'                                               
088200                ,:REQU-IDUSER)                                            
088300     END-EXEC                                                             
088400                                                                          
088500     MOVE SQLCODE TO SQLCODE-WS                                           
088600     PERFORM DB2-STATUS-CHECK                                             
088700     .                                                                    
088800     EJECT                                                                
088900                                                                          
089000 DB2-INSERT-T01CURR SECTION.                                              
089100     MOVE 000   TO GOOD-SQLCODECODES                                      
089200                                                                          
089300     EXEC SQL                                                             
089400         INSERT                                                           
089500                                                                          
089600         INTO     T01CURR                                                 
089700                                                                          
089800                 (IDLEGSEL                                                
089900                 ,KDVALISO                                                
090000                 ,DASTADAT                                                
090100                 ,PRKURS                                                  
090200                 ,REVALUTA                                                
090300                 ,DAREGDAT                                                
090400                 ,REVALUTA-FROM                                           
090500                 ,REVALUTA-TO                                             
090600                 ,PRKURS-NEW                                              
090700                 ,IDUSER)                                                 
090800                                                                          
090900         VALUES (:REQU-IDLEGSEL-KEY                                       
091000                ,:REQU-KDVALISO-KEY                                       
091100                ,:WS-DASTADAT-KEY                                         
091200                ,:WS-PRKURS-NUM                                           
091300                ,:WS-REVALUTA                                             
091400                ,:WS-DATUM                                                
091500                ,:WS-REVALUTA                                             
091600                ,:WS-REVALUTA                                             
091700                ,:WS-PRKURS-NUM                                           
091800                ,:REQU-IDUSER)                                            
091900     END-EXEC                                                             
092000                                                                          
092100     MOVE SQLCODE TO SQLCODE-WS                                           
092200     PERFORM DB2-STATUS-CHECK                                             
092300     .                                                                    
092400     EJECT                                                                
092500                                                                          
092600 DB2-DELETE-T01CURY SECTION.                                              
092700     MOVE 000    TO GOOD-SQLCODECODES                                     
092800                                                                          
092900     EXEC SQL                                                             
093000         DELETE                                                           
093100                                                                          
093200         FROM   T01CURY                                                   
093300                                                                          
093400         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
093500         AND    KDVALISO = :REQU-KDVALISO-KEY                             
093600         AND    DASTADAT = :WS-DASTADAT-KEY                               
093700     END-EXEC                                                             
093800                                                                          
093900     MOVE SQLCODE TO SQLCODE-WS                                           
094000     PERFORM DB2-STATUS-CHECK                                             
094100     .                                                                    
094200     EJECT                                                                
094300                                                                          
094400 DB2-STATUS-CHECK     SECTION.                                            
094500     SET SQLCODE-IX TO 1                                                  
094600     SEARCH GOOD-SQLCODE                                                  
094700       AT END                                                             
094800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
094900          DELIMITED BY SIZE INTO ERROR-TEXT                               
095000          CALL ABEND USING RKOD-ABEND-DB2                                 
095100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
095200     END-SEARCH                                                           
095300     .                                                                    
