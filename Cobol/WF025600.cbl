000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF025600.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   02/03/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME                                                                 
000900*        CARPARTS.BILLIT.PERIODCALENDARMAINTENANCE                        
001000*    FUNCTION:                                                            
001100*        MAINTENANCE PERIOD CALENDAR.                                     
001200*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001300*                                                                         
001400*        THE PROGRAM READS     TABLE T01LSEL                              
001500*        THE PROGRAM UPDATES   TABLE T01PECA                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: WF0256T                                             
001900*        REQUEST:     WF0256I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    WF0256O1                                            
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500 INPUT-OUTPUT SECTION.                                                    
002600 FILE-CONTROL.                                                            
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'WF025600'.            
003200                                                                          
003300*    --- WORK FIELD FOR ERROR MESSAGE WHEN CALLING ABEND.                 
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600                                                                          
003700 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NOO                         PIC X       VALUE 'N'.                   
003900                                                                          
004000 77  KEYS-SW                     PIC X       VALUE SPACE.                 
004100     88  KEYS-OK                             VALUE 'Y'.                   
004200     88  KEYS-FEL                            VALUE 'N'.                   
004300     EJECT                                                                
004400                                                                          
004500*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
004900     SKIP3                                                                
005000                                                                          
005100*    --- PARAMETERS TO ABEND                                              
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
005600                                                                          
005700     SKIP3                                                                
005800                                                                          
005900 01  MESSAGE-CODES.                                                       
006000     03  ERROR-CODES.                                                     
006100       05 NOTHING-HAS-BEEN-UPDATED   PIC X(3)    VALUE '004'.             
006200       05 NOTHING-HAS-BEEN-INSERTED  PIC X(3)    VALUE '005'.             
006300       05 NOTHING-HAS-BEEN-DELETED   PIC X(3)    VALUE '006'.             
006400       05 UPDATE-NOT-ALLOWED         PIC X(3)    VALUE '007'.             
006500       05 INSERT-NOT-ALLOWED         PIC X(3)    VALUE '008'.             
006600       05 DELETE-NOT-ALLOWED         PIC X(3)    VALUE '009'.             
006700       05 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.             
006800       05 IS-INVALID                 PIC X(3)    VALUE '023'.             
006900       05 MUST-BE-NUMERIC            PIC X(3)    VALUE '024'.             
007000       05 NOT-FOUND                  PIC X(3)    VALUE '025'.             
007100       05 MUST-BE-ENTERED            PIC X(3)    VALUE '026'.             
007200       05 LINE-NOT-FOUND             PIC X(3)    VALUE '027'.             
007300       05 ALREADY-EXIST              PIC X(3)    VALUE '030'.             
007400       05 SYSTEM-ERROR               PIC X(3)    VALUE '099'.             
007500     03  INFO-KODER.                                                      
007600       05 UPDATE-DONE                PIC X(3)    VALUE '001'.             
007700       05 INSERT-DONE                PIC X(3)    VALUE '002'.             
007800       05 DELETE-DONE                PIC X(3)    VALUE '003'.             
007900     EJECT                                                                
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008200     SKIP3                                                                
008300 01  -COPY WZ01SUB                                                        
008400     EJECT                                                                
008500                                                                          
008600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008700     SKIP3                                                                
008800 01  REQU-AREA.                                                           
008900*    03  -COPY WZ01REQU                                                   
009000*    03  -COPY WF0256I1                                                   
009100     EJECT                                                                
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009400     SKIP3                                                                
009500 01  RESP-AREA.                                                           
009600*    03  -COPY WZ01RESP                                                   
009700*    03  -COPY WF0256O1                                                   
009800     EJECT                                                                
009900                                                                          
010000 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
010100     SKIP3                                                                
010200*    -COPY WZ20DATE                                                       
010300     EJECT                                                                
010400                                                                          
010500 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
010600     SKIP3                                                                
010700*    -COPY WZ20DAYS                                                       
010800     EJECT                                                                
010900                                                                          
011000 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
011100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011200                                                                          
011300 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
011400 01  DB2-WS.                                                              
011500     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011600         88  CURSOR-OK                       VALUE 000.                   
011700         88  LINES-FOUND                     VALUE 000.                   
011800         88  LINES-MISSING                   VALUE 100.                   
011900         88  RESOURCE-WRONG                  VALUE 904.                   
012000                                                                          
012100     03  GOOD-SQLCODECODES.                                               
012200         05  GOOD-SQLCODE OCCURS 5                                        
012300             INDEXED BY SQLCODE-IX PIC 9(3).                              
012400     EJECT                                                                
012500                                                                          
012600 01  WS-AREA.                                                             
012700     03 WS-IDLEGSEL             PIC X(4)     VALUE SPACE.                 
012800     03 WS-IDUSER               PIC X(8)     VALUE SPACE.                 
012900     03 WS-DASTADAT             PIC X(8)     VALUE SPACE.                 
013000     03 WS-DATUMMIN             PIC X(8)     VALUE SPACE.                 
013100     03 WS-DATUMMAX             PIC X(8)     VALUE SPACE.                 
013200     03 WS-DASTADAT-2           PIC 9(8)     VALUE ZERO.                  
013300     03 WS-TIRP                 PIC 9(2)     VALUE ZERO.                  
013400     03 WS-FLPERIOD             PIC X(1)     VALUE SPACE.                 
013500     03 WS-DAREGDAT             PIC X(8)     VALUE SPACE.                 
013600     03 WS-DAREGDAT-2           PIC 9(8)     VALUE ZERO.                  
013700     03 WS-DAFINDOC             PIC X(8)     VALUE SPACE.                 
013800     03 WS-DAFINDOC-2           PIC 9(8)     VALUE ZERO.                  
013900     03 WS-DAUPPDAT             PIC X(8)     VALUE SPACE.                 
014000     03 WS-DAUPPDAT-2           PIC 9(8)     VALUE ZERO.                  
014100     03 WS-DADELDAT             PIC X(8)     VALUE SPACE.                 
014200     03 WS-DADELDAT-2           PIC 9(8)     VALUE ZERO.                  
014300     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
014400     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
014500     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
014600     03 WS-DATUM                PIC X(8)     VALUE SPACE.                 
014700     03 WS-DATUM-2              PIC 9(8)     VALUE ZERO.                  
014800                                                                          
014900     03 LSEL-BELEGRAD-1         PIC X(35)    VALUE SPACE.                 
015000                                                                          
015100     03 PECA-IDLEGSEL           PIC X(4)     VALUE SPACE.                 
015200     03 PECA-DASTADAT           PIC X(8)     VALUE SPACE.                 
015300     03 PECA-DAFINDOC           PIC X(8)     VALUE SPACE.                 
015400     03 PECA-TIRP               PIC S9(2)    COMP-3 VALUE ZERO.           
015500     03 PECA-DAREGDAT           PIC X(8)     VALUE SPACE.                 
015600     03 PECA-DAUPPDAT           PIC X(8)     VALUE SPACE.                 
015700     03 PECA-FLPERIOD           PIC X(1)     VALUE SPACE.                 
015800     03 PECA-IDUSER             PIC X(8)     VALUE SPACE.                 
015900                                                                          
016000 01  FILLER                  PIC X(16)    VALUE 'T01LSEL-AREA'.           
016100*01  -COPY T01LSEL -PRE T01LSEL-                                          
016200     EJECT                                                                
016300                                                                          
016400 01  FILLER                  PIC X(16)    VALUE 'T01PECA-AREA'.           
016500*01  -COPY T01PECA -PRE T01PECA-                                          
016600     EJECT                                                                
016700                                                                          
016800     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
016900     EJECT                                                                
017000     EXEC SQL INCLUDE T01PECA END-EXEC.                                   
017100     EJECT                                                                
017200                                                                          
017300 LINKAGE SECTION.                                                         
017400 PROCEDURE DIVISION.                                                      
017500 MAIN SECTION.                                                            
017600                                                                          
017700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
017800     IF SUB-KDRC = ZERO                                                   
017900       PERFORM A-INIT                                                     
018000       PERFORM B-CHECK-KEYS                                               
018100       IF KEYS-OK                                                         
018200         PERFORM C-CHECK-INDATA                                           
018300       END-IF                                                             
018400       IF KEYS-OK                                                         
018500         PERFORM D-PERFORM-REQUEST                                        
018600         IF KEYS-OK                                                       
018700           PERFORM S03-READ-SHOW-INFO                                     
018800         END-IF                                                           
018900       END-IF                                                             
019000       IF KEYS-FEL                                                        
019100         PERFORM S04-READ-SHOW-INFO                                       
019200       END-IF                                                             
019300       PERFORM E-CREATE-RESPONSE-OK                                       
019400       PERFORM S02-RETURN-RESPONSE                                        
019500     END-IF                                                               
019600                                                                          
019700     MOVE ZERO TO RETURN-CODE                                             
019800     GOBACK                                                               
019900     .                                                                    
020000     EJECT                                                                
020100                                                                          
020200 A-INIT SECTION.                                                          
020300     MOVE REQU-DASTADAT-KEY           TO WS-DASTADAT                      
020400                                                                          
020500     MOVE YES                         TO KEYS-SW                          
020600                                                                          
020700     MOVE ALL ' '                     TO RESP-AREA                        
020800     MOVE SPACE TO RESP-IDMSG-ERROR                                       
020900     MOVE SPACE TO RESP-IDMSG-INFO                                        
021000     MOVE SPACE TO RESP-IDELMT-ERROR                                      
021100                                                                          
021200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM                         
021300                                                                          
021400     INITIALIZE GOOD-SQLCODECODES                                         
021500     .                                                                    
021600     EJECT                                                                
021700                                                                          
021800 B-CHECK-KEYS SECTION.                                                    
021900     IF REQU-KDPGMACT = 'S'                                               
022000     AND REQU-IDMSGVER NUMERIC                                            
022100       CONTINUE                                                           
022200     ELSE                                                                 
022300       IF REQU-KDPGMACT = 'I'                                             
022400       AND REQU-IDMSGVER NUMERIC                                          
022500         CONTINUE                                                         
022600       ELSE                                                               
022700         IF REQU-KDPGMACT = 'U'                                           
022800         AND REQU-IDMSGVER NUMERIC                                        
022900           CONTINUE                                                       
023000         ELSE                                                             
023100           IF REQU-KDPGMACT = 'D'                                         
023200           AND REQU-IDMSGVER NUMERIC                                      
023300             CONTINUE                                                     
023400           ELSE                                                           
023500             MOVE NOO TO KEYS-SW                                          
023600             MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                         
023700           END-IF                                                         
023800         END-IF                                                           
023900       END-IF                                                             
024000     END-IF                                                               
024100                                                                          
024200     IF REQU-DASTADAT-KEY = SPACE OR = ALL '+'                            
024300       MOVE NOO TO KEYS-SW                                                
024400       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
024500     END-IF                                                               
024600                                                                          
024700     IF REQU-IDLEGSEL-KEY = SPACE OR = ALL '+'                            
024800       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
024900       MOVE NOO TO KEYS-SW                                                
025000     END-IF                                                               
025100                                                                          
025200     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
025300       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
025400       MOVE NOO TO KEYS-SW                                                
025500     END-IF                                                               
025600                                                                          
025700     IF KEYS-FEL                                                          
025800       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
025900       IF REQU-KDPGMACT = 'S' OR = 'U' OR = 'I' OR = 'D'                  
026000         CONTINUE                                                         
026100       ELSE                                                               
026200         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
026300         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
026400       END-IF                                                             
026500       IF REQU-IDMSGVER NUMERIC                                           
026600         CONTINUE                                                         
026700       ELSE                                                               
026800         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
026900         MOVE 'IDMSGVER'   TO WS-IDELMT-ERROR                             
027000       END-IF                                                             
027100       IF REQU-IDUSER = SPACE OR = ALL '+'                                
027200         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
027300         MOVE 'IDUSER'     TO WS-IDELMT-ERROR                             
027400       ELSE                                                               
027500         CONTINUE                                                         
027600       END-IF                                                             
027700     END-IF                                                               
027800     .                                                                    
027900     EJECT                                                                
028000                                                                          
028100 C-CHECK-INDATA SECTION.                                                  
028200     IF REQU-DASTADAT-KEY NUMERIC                                         
028300       MOVE REQU-DASTADAT-KEY    TO DATE-TIDATE                           
028400       MOVE 'YYYYMMDD'    TO DATE-KDDATFMT                                
028500       CALL WZ20DATE USING DATE-WZ20DATE                                  
028600       IF DATE-KDRC > ZERO                                                
028700         MOVE NOO TO KEYS-SW                                              
028800         MOVE IS-INVALID         TO WS-IDMSG-ERROR                        
028900         MOVE 'DASTADAT'         TO WS-IDELMT-ERROR                       
029000       ELSE                                                               
029100         CONTINUE                                                         
029200       END-IF                                                             
029300     ELSE                                                                 
029400       MOVE NOO TO KEYS-SW                                                
029500       MOVE MUST-BE-NUMERIC    TO WS-IDMSG-ERROR                          
029600       MOVE 'DASTADAT'         TO WS-IDELMT-ERROR                         
029700     END-IF                                                               
029800     PERFORM DB2-SELECT-T01LSEL                                           
029900                                                                          
030000     IF LINES-FOUND                                                       
030100       CONTINUE                                                           
030200     ELSE                                                                 
030300       MOVE NOO TO KEYS-SW                                                
030400       MOVE NOT-FOUND      TO WS-IDMSG-ERROR                              
030500       MOVE 'IDLEGSEL'     TO WS-IDELMT-ERROR                             
030600     END-IF                                                               
030700                                                                          
030800     IF KEYS-OK                                                           
030900       IF REQU-KDPGMACT = 'I'                                             
031000         IF REQU-DAFINDOC NUMERIC                                         
031100           MOVE REQU-DAFINDOC  TO DATE-TIDATE                             
031200           MOVE 'YYYYMMDD'  TO DATE-KDDATFMT                              
031300           CALL WZ20DATE USING DATE-WZ20DATE                              
031400           IF DATE-KDRC > ZERO                                            
031500             MOVE NOO TO KEYS-SW                                          
031600             MOVE IS-INVALID       TO WS-IDMSG-ERROR                      
031700             MOVE 'DAFINDOC'       TO WS-IDELMT-ERROR                     
031800           ELSE                                                           
031900             IF REQU-DAFINDOC > REQU-DASTADAT-KEY                         
032000                 MOVE NOO TO KEYS-SW                                      
032100                 MOVE IS-INVALID TO WS-IDMSG-ERROR                        
032200                 MOVE 'DAFINDOC' TO WS-IDELMT-ERROR                       
032300             END-IF                                                       
032400           END-IF                                                         
032500         ELSE                                                             
032600           MOVE NOO TO KEYS-SW                                            
032700           MOVE MUST-BE-NUMERIC  TO WS-IDMSG-ERROR                        
032800           MOVE 'DAFINDOC'       TO WS-IDELMT-ERROR                       
032900         END-IF                                                           
033000         IF KEYS-OK                                                       
033100           PERFORM DB2-OPEN-T01PECA-DATUM1                                
033200           PERFORM DB2-FETCH-T01PECA-DATUM1                               
033300           IF LINES-FOUND                                                 
033400             MOVE WS-DATUMMIN       TO DAYS-TIDATE1                       
033500             MOVE 'YYYYMMDD'        TO DAYS-KDDATFMT1                     
033600             MOVE REQU-DASTADAT-KEY TO DAYS-TIDATE2                       
033700             MOVE 'YYYYMMDD'        TO DAYS-KDDATFMT2                     
033800             MOVE ' '               TO DAYS-IDCALEND                      
033900             CALL WZ20DAYS USING DAYS-WZ20DAYS                            
034000             IF DAYS-KDRC = ZERO                                          
034100               IF DAYS-KVDAYS < 7                                         
034200                 MOVE NOO TO KEYS-SW                                      
034300                 MOVE IS-INVALID      TO WS-IDMSG-ERROR                   
034400                 MOVE 'DASTADAT' TO WS-IDELMT-ERROR                       
034500               END-IF                                                     
034600             ELSE                                                         
034700               MOVE NOO TO KEYS-SW                                        
034800               MOVE IS-INVALID      TO WS-IDMSG-ERROR                     
034900               MOVE 'DASTADAT' TO WS-IDELMT-ERROR                         
035000             END-IF                                                       
035100           END-IF                                                         
035200           PERFORM DB2-CLOSE-T01PECA-DATUM1                               
035300         END-IF                                                           
035400         IF KEYS-OK                                                       
035500           PERFORM DB2-OPEN-T01PECA-DATUM2                                
035600           PERFORM DB2-FETCH-T01PECA-DATUM2                               
035700           IF LINES-FOUND                                                 
035800             MOVE REQU-DASTADAT-KEY TO DAYS-TIDATE1                       
035900             MOVE 'YYYYMMDD'        TO DAYS-KDDATFMT1                     
036000             MOVE WS-DATUMMAX       TO DAYS-TIDATE2                       
036100             MOVE 'YYYYMMDD'        TO DAYS-KDDATFMT2                     
036200             MOVE ' '               TO DAYS-IDCALEND                      
036300             CALL WZ20DAYS USING DAYS-WZ20DAYS                            
036400             IF DAYS-KDRC = ZERO                                          
036500               IF DAYS-KVDAYS < 7                                         
036600                 MOVE NOO TO KEYS-SW                                      
036700                 MOVE IS-INVALID      TO WS-IDMSG-ERROR                   
036800                 MOVE 'DASTADAT' TO WS-IDELMT-ERROR                       
036900               END-IF                                                     
037000             ELSE                                                         
037100               MOVE NOO TO KEYS-SW                                        
037200               MOVE IS-INVALID      TO WS-IDMSG-ERROR                     
037300               MOVE 'DASTADAT' TO WS-IDELMT-ERROR                         
037400             END-IF                                                       
037500           END-IF                                                         
037600           PERFORM DB2-CLOSE-T01PECA-DATUM2                               
037700         END-IF                                                           
037800         IF KEYS-OK                                                       
037900           IF REQU-TIRP = SPACE OR = ALL '+'                              
038000             MOVE NOO TO KEYS-SW                                          
038100             MOVE MUST-BE-ENTERED TO WS-IDMSG-ERROR                       
038200             MOVE 'TIRP' TO WS-IDELMT-ERROR                               
038300           END-IF                                                         
038400           IF REQU-TIRP NOT NUMERIC                                       
038500             MOVE NOO TO KEYS-SW                                          
038600             MOVE MUST-BE-NUMERIC TO WS-IDMSG-ERROR                       
038700             MOVE 'TIRP' TO WS-IDELMT-ERROR                               
038800           END-IF                                                         
038900           IF REQU-TIRP < 1                                               
039000             MOVE NOO TO KEYS-SW                                          
039100             MOVE IS-INVALID TO WS-IDMSG-ERROR                            
039200             MOVE 'TIRP' TO WS-IDELMT-ERROR                               
039300           END-IF                                                         
039400           IF REQU-DAFINDOC <= WS-DATUM                                   
039500             PERFORM DB2-SELECT-T01PECA                                   
039600             IF LINES-FOUND                                               
039700                 MOVE NOO TO KEYS-SW                                      
039800                 MOVE IS-INVALID TO WS-IDMSG-ERROR                        
039900                 MOVE 'DAFINDOC' TO WS-IDELMT-ERROR                       
040000             END-IF                                                       
040100           END-IF                                                         
040200         END-IF                                                           
040300       END-IF                                                             
040400                                                                          
040500       IF REQU-KDPGMACT = 'U'                                             
040600         IF REQU-DAFINDOC NUMERIC                                         
040700           MOVE REQU-DAFINDOC  TO DATE-TIDATE                             
040800           MOVE 'YYYYMMDD'  TO DATE-KDDATFMT                              
040900           CALL WZ20DATE USING DATE-WZ20DATE                              
041000           IF DATE-KDRC > ZERO                                            
041100             MOVE IS-INVALID       TO WS-IDMSG-ERROR                      
041200             MOVE 'DAFINDOC'       TO WS-IDELMT-ERROR                     
041300           ELSE                                                           
041400             IF REQU-DAFINDOC > REQU-DASTADAT-KEY                         
041500               MOVE NOO TO KEYS-SW                                        
041600               MOVE IS-INVALID TO WS-IDMSG-ERROR                          
041700               MOVE 'DAFINDOC' TO WS-IDELMT-ERROR                         
041800             END-IF                                                       
041900           END-IF                                                         
042000         ELSE                                                             
042100           MOVE NOO TO KEYS-SW                                            
042200           MOVE MUST-BE-NUMERIC  TO WS-IDMSG-ERROR                        
042300           MOVE 'DAFINDOC'       TO WS-IDELMT-ERROR                       
042400         END-IF                                                           
042500         IF KEYS-OK                                                       
042600           IF REQU-TIRP = SPACE OR = ALL '+'                              
042700             MOVE NOO TO KEYS-SW                                          
042800             MOVE MUST-BE-ENTERED TO WS-IDMSG-ERROR                       
042900             MOVE 'TIRP' TO WS-IDELMT-ERROR                               
043000           END-IF                                                         
043100           IF REQU-TIRP NOT NUMERIC                                       
043200             MOVE NOO TO KEYS-SW                                          
043300             MOVE MUST-BE-NUMERIC TO WS-IDMSG-ERROR                       
043400             MOVE 'TIRP' TO WS-IDELMT-ERROR                               
043500           END-IF                                                         
043600           IF REQU-TIRP < 1                                               
043700             MOVE NOO TO KEYS-SW                                          
043800             MOVE IS-INVALID TO WS-IDMSG-ERROR                            
043900             MOVE 'TIRP' TO WS-IDELMT-ERROR                               
044000           END-IF                                                         
044100           IF REQU-DASTADAT-KEY <= WS-DATUM                               
044200             MOVE NOO TO KEYS-SW                                          
044300             MOVE IS-INVALID TO WS-IDMSG-ERROR                            
044400             MOVE 'DASTADAT' TO WS-IDELMT-ERROR                           
044500           END-IF                                                         
044600           IF REQU-DAFINDOC <= WS-DATUM                                   
044700             MOVE NOO TO KEYS-SW                                          
044800             MOVE IS-INVALID TO WS-IDMSG-ERROR                            
044900             MOVE 'DAFINDOC' TO WS-IDELMT-ERROR                           
045000           END-IF                                                         
045100         END-IF                                                           
045200       END-IF                                                             
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 D-PERFORM-REQUEST SECTION.                                               
045800     IF REQU-KDPGMACT = 'S'                                               
045900       PERFORM DB2-SELECT-T01PECA                                         
046000       IF LINES-FOUND                                                     
046100         MOVE PECA-TIRP     TO WS-TIRP                                    
046200         MOVE PECA-DAFINDOC TO WS-DAFINDOC                                
046300       ELSE                                                               
046400         MOVE NOT-FOUND      TO WS-IDMSG-ERROR                            
046500         MOVE 'DASTADAT'     TO WS-IDELMT-ERROR                           
046600         MOVE NOO TO KEYS-SW                                              
046700       END-IF                                                             
046800     END-IF                                                               
046900                                                                          
047000     IF REQU-KDPGMACT = 'U'                                               
047100       MOVE REQU-TIRP                   TO WS-TIRP                        
047200       MOVE WS-TIRP                     TO PECA-TIRP                      
047300       PERFORM DB2-OPEN-T01PECA-CRS                                       
047400       PERFORM DB2-FETCH-T01PECA-CRS                                      
047500       IF LINES-FOUND                                                     
047600         PERFORM DB2-UPDATE-T01PECA                                       
047700         PERFORM DB2-SELECT-T01PECA                                       
047800         IF LINES-FOUND                                                   
047900           MOVE PECA-DAFINDOC TO WS-DAFINDOC                              
048000           MOVE UPDATE-DONE TO WS-IDMSG-INFO                              
048100           MOVE PECA-TIRP TO WS-TIRP                                      
048200           MOVE WS-DATUM TO WS-DAUPPDAT                                   
048300         END-IF                                                           
048400       ELSE                                                               
048500         MOVE NOT-FOUND      TO WS-IDMSG-ERROR                            
048600         MOVE 'DASTADAT'     TO WS-IDELMT-ERROR                           
048700         MOVE NOO TO KEYS-SW                                              
048800       END-IF                                                             
048900       PERFORM DB2-CLOSE-T01PECA-CRS                                      
049000     END-IF                                                               
049100                                                                          
049200     IF REQU-KDPGMACT = 'D'                                               
049300       MOVE 'N' TO WS-FLPERIOD                                            
049400       PERFORM DB2-SELECT-T01PECA-DELETE                                  
049500       IF LINES-FOUND                                                     
049600         PERFORM DB2-DELETE-T01PECA                                       
049700         MOVE DELETE-DONE TO WS-IDMSG-INFO                                
049800         MOVE SPACE    TO WS-FLPERIOD                                     
049900         MOVE ZERO     TO WS-TIRP                                         
050000                          WS-DASTADAT                                     
050100                          WS-DAREGDAT                                     
050200                          WS-DAFINDOC                                     
050300                          WS-DAUPPDAT                                     
050400         MOVE WS-DATUM TO WS-DADELDAT                                     
050500         MOVE ZERO     TO WS-DATUM                                        
050600       ELSE                                                               
050700         MOVE NOO TO KEYS-SW                                              
050800         MOVE DELETE-NOT-ALLOWED TO WS-IDMSG-ERROR                        
050900       END-IF                                                             
051000     END-IF                                                               
051100                                                                          
051200     IF REQU-KDPGMACT = 'I'                                               
051300       MOVE REQU-TIRP                   TO WS-TIRP                        
051400       MOVE WS-TIRP                     TO PECA-TIRP                      
051500       IF REQU-DASTADAT-KEY <= WS-DATUM                                   
051600         MOVE 'J'   TO    WS-FLPERIOD                                     
051700       ELSE                                                               
051800         MOVE 'N'   TO    WS-FLPERIOD                                     
051900       END-IF                                                             
052000       PERFORM DB2-SELECT-T01PECA-INSERT                                  
052100       IF LINES-MISSING                                                   
052200         PERFORM DB2-INSERT-T01PECA                                       
052300         MOVE INSERT-DONE TO WS-IDMSG-INFO                                
052400         PERFORM DB2-SELECT-T01PECA                                       
052500         IF LINES-FOUND                                                   
052600           MOVE PECA-TIRP     TO WS-TIRP                                  
052700           MOVE PECA-DAFINDOC TO WS-DAFINDOC                              
052800           MOVE 'N' TO WS-FLPERIOD                                        
052900           MOVE ZERO TO WS-DAUPPDAT                                       
053000           MOVE REQU-IDUSER TO WS-IDUSER                                  
053100         END-IF                                                           
053200       ELSE                                                               
053300         MOVE ALREADY-EXIST  TO WS-IDMSG-ERROR                            
053400         MOVE 'DASTADAT'     TO WS-IDELMT-ERROR                           
053500         MOVE NOO TO KEYS-SW                                              
053600       END-IF                                                             
053700     END-IF                                                               
053800     .                                                                    
053900     EJECT                                                                
054000                                                                          
054100 E-CREATE-RESPONSE-OK SECTION.                                            
054200     MOVE REQU-IDMSGVER             TO RESP-IDMSGVER                      
054300     MOVE WS-IDMSG-INFO             TO RESP-IDMSG-INFO                    
054400     MOVE WS-IDMSG-ERROR            TO RESP-IDMSG-ERROR                   
054500     MOVE WS-IDELMT-ERROR           TO RESP-IDELMT-ERROR                  
054600     MOVE REQU-IDLEGSEL-KEY         TO RESP-IDLEGSEL-KEY                  
054700     MOVE REQU-DASTADAT-KEY         TO RESP-DASTADAT-KEY                  
054800     .                                                                    
054900     EJECT                                                                
055000                                                                          
055100*    --- DISPATCHER-SECTIONS                                              
055200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
055300     MOVE 'GETARG'                   TO SUB-KDFUNC                        
055400     MOVE 'CARPARTS.BILLIT.PERIODCALENDARMAINTENANCE'                     
055500       TO SUB-ADDISPABS                                                   
055600     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
055700                                                                          
055800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
055900                                                                          
056000     IF SUB-KDRC > 0                                                      
056100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
056200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
056300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
056400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056500     END-IF                                                               
056600     .                                                                    
056700     SKIP3                                                                
056800                                                                          
056900 S02-RETURN-RESPONSE SECTION.                                             
057000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
057100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
057200                                                                          
057300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
057400                                                                          
057500     IF SUB-KDRC > 0                                                      
057600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
057700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
057800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400 S03-READ-SHOW-INFO SECTION.                                              
058500     IF WS-FLPERIOD = 'J'                                                 
058600       MOVE 'Y' TO WS-FLPERIOD                                            
058700     END-IF                                                               
058800     MOVE WS-DAFINDOC               TO WS-DAFINDOC-2                      
058900     MOVE WS-DATUM                  TO WS-DATUM-2                         
059000     MOVE WS-DAUPPDAT               TO WS-DAUPPDAT-2                      
059100     MOVE WS-DADELDAT               TO WS-DADELDAT-2                      
059200     MOVE WS-DAFINDOC-2             TO RESP-DAFINDOC                      
059300     MOVE WS-TIRP                   TO RESP-TIRP                          
059400     MOVE LSEL-BELEGRAD-1           TO RESP-BELEGRAD-1                    
059500     MOVE WS-FLPERIOD               TO RESP-FLPERIOD                      
059600     MOVE WS-IDUSER                 TO RESP-IDUSER                        
059700     MOVE WS-DATUM-2                TO RESP-DAREGDAT                      
059800     MOVE WS-DAUPPDAT-2             TO RESP-DAUPPDAT                      
059900     MOVE WS-DADELDAT-2             TO RESP-DADELDAT                      
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300 S04-READ-SHOW-INFO SECTION.                                              
060400     IF WS-FLPERIOD = 'J'                                                 
060500       MOVE 'Y' TO WS-FLPERIOD                                            
060600     END-IF                                                               
060700     MOVE WS-DAFINDOC               TO WS-DAFINDOC-2                      
060800     MOVE WS-DATUM                  TO WS-DATUM-2                         
060900     MOVE WS-DAUPPDAT               TO WS-DAUPPDAT-2                      
061000     MOVE WS-DADELDAT               TO WS-DADELDAT-2                      
061100     MOVE WS-DAFINDOC-2             TO RESP-DAFINDOC                      
061200     MOVE WS-TIRP                   TO RESP-TIRP                          
061300     MOVE LSEL-BELEGRAD-1           TO RESP-BELEGRAD-1                    
061400     MOVE WS-FLPERIOD               TO RESP-FLPERIOD                      
061500     MOVE WS-IDUSER                 TO RESP-IDUSER                        
061600     MOVE WS-DATUM-2                TO RESP-DAREGDAT                      
061700     MOVE WS-DAUPPDAT-2             TO RESP-DAUPPDAT                      
061800     MOVE WS-DADELDAT-2             TO RESP-DADELDAT                      
061900     .                                                                    
062000     EJECT                                                                
062100                                                                          
062200 DB2-SELECT-T01LSEL SECTION.                                              
062300     MOVE 000100  TO GOOD-SQLCODECODES                                    
062400     EXEC SQL                                                             
062500         SELECT  BELEGRAD_1                                               
062600                                                                          
062700         INTO    :LSEL-BELEGRAD-1                                         
062800                                                                          
062900         FROM    T01LSEL                                                  
063000                                                                          
063100         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
063200         AND     KDSTATUS = 001                                           
063300     END-EXEC                                                             
063400     MOVE SQLCODE TO SQLCODE-WS                                           
063500     PERFORM DB2-STATUS-CHECK                                             
063600     .                                                                    
063700     EJECT                                                                
063800                                                                          
063900 DB2-SELECT-T01PECA SECTION.                                              
064000     MOVE 000100  TO GOOD-SQLCODECODES                                    
064100     EXEC SQL                                                             
064200         SELECT  DAREGDAT, DAUPPDAT, DAFINDOC,                            
064300                 FLPERIOD, IDUSER, TIRP                                   
064400                                                                          
064500         INTO    :WS-DAREGDAT,                                            
064600                 :WS-DAUPPDAT,                                            
064700                 :PECA-DAFINDOC,                                          
064800                 :WS-FLPERIOD,                                            
064900                 :WS-IDUSER,                                              
065000                 :PECA-TIRP                                               
065100                                                                          
065200         FROM    T01PECA                                                  
065300                                                                          
065400         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
065500         AND     DASTADAT = :REQU-DASTADAT-KEY                            
065600     END-EXEC                                                             
065700     MOVE SQLCODE TO SQLCODE-WS                                           
065800     PERFORM DB2-STATUS-CHECK                                             
065900     .                                                                    
066000     EJECT                                                                
066100                                                                          
066200 DB2-SELECT-T01PECA1 SECTION.                                             
066300     MOVE 000100  TO GOOD-SQLCODECODES                                    
066400     EXEC SQL                                                             
066500         SELECT  DAREGDAT, DAUPPDAT, DAFINDOC,                            
066600                 FLPERIOD, IDUSER, TIRP                                   
066700                                                                          
066800         INTO    :WS-DAREGDAT,                                            
066900                 :WS-DAUPPDAT,                                            
067000                 :PECA-DAFINDOC,                                          
067100                 :WS-FLPERIOD,                                            
067200                 :WS-IDUSER,                                              
067300                 :PECA-TIRP                                               
067400                                                                          
067500         FROM    T01PECA                                                  
067600                                                                          
067700         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
067800     END-EXEC                                                             
067900     MOVE SQLCODE TO SQLCODE-WS                                           
068000     PERFORM DB2-STATUS-CHECK                                             
068100     .                                                                    
068200     EJECT                                                                
068300 DB2-SELECT-T01PECA-DELETE SECTION.                                       
068400                                                                          
068500     MOVE 000100  TO GOOD-SQLCODECODES                                    
068600     EXEC SQL                                                             
068700         SELECT  DAREGDAT, DAUPPDAT, DAFINDOC,                            
068800                 FLPERIOD, IDUSER                                         
068900                                                                          
069000         INTO    :WS-DAREGDAT,                                            
069100                 :WS-DAUPPDAT,                                            
069200                 :WS-DAFINDOC,                                            
069300                 :WS-FLPERIOD,                                            
069400                 :WS-IDUSER                                               
069500                                                                          
069600         FROM    T01PECA                                                  
069700                                                                          
069800         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
069900         AND     DASTADAT = :REQU-DASTADAT-KEY                            
070000         AND     FLPERIOD = :WS-FLPERIOD                                  
070100     END-EXEC                                                             
070200     MOVE SQLCODE TO SQLCODE-WS                                           
070300     PERFORM DB2-STATUS-CHECK                                             
070400     .                                                                    
070500     EJECT                                                                
070600                                                                          
070700 DB2-SELECT-T01PECA-INSERT SECTION.                                       
070800     MOVE 000100  TO GOOD-SQLCODECODES                                    
070900     EXEC SQL                                                             
071000         SELECT  IDLEGSEL                                                 
071100                                                                          
071200         INTO    :WS-IDLEGSEL                                             
071300                                                                          
071400         FROM    T01PECA                                                  
071500                                                                          
071600         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
071700         AND     DASTADAT = :REQU-DASTADAT-KEY                            
071800     END-EXEC                                                             
071900     MOVE SQLCODE TO SQLCODE-WS                                           
072000     PERFORM DB2-STATUS-CHECK                                             
072100     .                                                                    
072200     EJECT                                                                
072300                                                                          
072400 DB2-OPEN-T01PECA-CRS  SECTION.                                           
072500     MOVE 000100  TO GOOD-SQLCODECODES                                    
072600     EXEC SQL                                                             
072700         DECLARE T01PECA-CRS CURSOR WITH HOLD FOR                         
072800                                                                          
072900           SELECT  DAREGDAT, DAFINDOC,                                    
073000                   DAUPPDAT, FLPERIOD, IDUSER                             
073100                                                                          
073200           FROM    T01PECA                                                
073300                                                                          
073400           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
073500           AND     DASTADAT = :REQU-DASTADAT-KEY                          
073600           AND     FLPERIOD = 'N'                                         
073700                                                                          
073800           FOR UPDATE OF                                                  
073900                         TIRP,                                            
074000                         DAREGDAT,                                        
074100                         DAFINDOC,                                        
074200                         DAUPPDAT,                                        
074300                         FLPERIOD,                                        
074400                         IDUSER                                           
074500     END-EXEC                                                             
074600                                                                          
074700     MOVE 000100  TO GOOD-SQLCODECODES                                    
074800     EXEC SQL OPEN T01PECA-CRS END-EXEC                                   
074900     .                                                                    
075000     SKIP3                                                                
075100                                                                          
075200 DB2-FETCH-T01PECA-CRS SECTION.                                           
075300     MOVE 000100  TO GOOD-SQLCODECODES                                    
075400     EXEC SQL                                                             
075500         FETCH T01PECA-CRS INTO                                           
075600                                :WS-DAREGDAT,                             
075700                                :WS-DAFINDOC,                             
075800                                :WS-DAUPPDAT,                             
075900                                :WS-FLPERIOD,                             
076000                                :WS-IDUSER                                
076100     END-EXEC                                                             
076200     MOVE SQLCODE TO SQLCODE-WS                                           
076300     PERFORM DB2-STATUS-CHECK                                             
076400     .                                                                    
076500     SKIP3                                                                
076600                                                                          
076700 DB2-CLOSE-T01PECA-CRS SECTION.                                           
076800     EXEC SQL CLOSE T01PECA-CRS END-EXEC                                  
076900     .                                                                    
077000     EJECT                                                                
077100                                                                          
077200 DB2-OPEN-T01PECA-DATUM1 SECTION.                                         
077300     MOVE 000100  TO GOOD-SQLCODECODES                                    
077400     EXEC SQL                                                             
077500         DECLARE T01PECA-DATUM1 CURSOR WITH HOLD FOR                      
077600                                                                          
077700           SELECT  DASTADAT                                               
077800                                                                          
077900           FROM    T01PECA                                                
078000                                                                          
078100           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
078200           AND     DASTADAT < :REQU-DASTADAT-KEY                          
078300                                                                          
078400           ORDER BY DASTADAT DESC                                         
078500                                                                          
078600     END-EXEC                                                             
078700                                                                          
078800     MOVE 000100  TO GOOD-SQLCODECODES                                    
078900     EXEC SQL OPEN T01PECA-DATUM1 END-EXEC                                
079000     .                                                                    
079100     SKIP3                                                                
079200                                                                          
079300 DB2-FETCH-T01PECA-DATUM1 SECTION.                                        
079400     MOVE 000100  TO GOOD-SQLCODECODES                                    
079500     EXEC SQL                                                             
079600         FETCH T01PECA-DATUM1 INTO                                        
079700                                :WS-DATUMMIN                              
079800     END-EXEC                                                             
079900     MOVE SQLCODE TO SQLCODE-WS                                           
080000     PERFORM DB2-STATUS-CHECK                                             
080100     .                                                                    
080200     SKIP3                                                                
080300                                                                          
080400 DB2-CLOSE-T01PECA-DATUM1 SECTION.                                        
080500     EXEC SQL CLOSE T01PECA-DATUM1 END-EXEC                               
080600     .                                                                    
080700     EJECT                                                                
080800                                                                          
080900 DB2-OPEN-T01PECA-DATUM2 SECTION.                                         
081000     MOVE 000100  TO GOOD-SQLCODECODES                                    
081100     EXEC SQL                                                             
081200         DECLARE T01PECA-DATUM2 CURSOR WITH HOLD FOR                      
081300                                                                          
081400           SELECT  DASTADAT                                               
081500                                                                          
081600           FROM    T01PECA                                                
081700                                                                          
081800           WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                          
081900           AND     DASTADAT > :REQU-DASTADAT-KEY                          
082000                                                                          
082100           ORDER BY DASTADAT                                              
082200                                                                          
082300     END-EXEC                                                             
082400                                                                          
082500     MOVE 000100  TO GOOD-SQLCODECODES                                    
082600     EXEC SQL OPEN T01PECA-DATUM2 END-EXEC                                
082700     .                                                                    
082800     SKIP3                                                                
082900                                                                          
083000 DB2-FETCH-T01PECA-DATUM2 SECTION.                                        
083100     MOVE 000100  TO GOOD-SQLCODECODES                                    
083200     EXEC SQL                                                             
083300         FETCH T01PECA-DATUM2 INTO                                        
083400                                :WS-DATUMMAX                              
083500     END-EXEC                                                             
083600     MOVE SQLCODE TO SQLCODE-WS                                           
083700     PERFORM DB2-STATUS-CHECK                                             
083800     .                                                                    
083900     SKIP3                                                                
084000                                                                          
084100 DB2-CLOSE-T01PECA-DATUM2 SECTION.                                        
084200     EXEC SQL CLOSE T01PECA-DATUM2 END-EXEC                               
084300     .                                                                    
084400     EJECT                                                                
084500                                                                          
084600 DB2-DELETE-T01PECA SECTION.                                              
084700     MOVE 000    TO GOOD-SQLCODECODES                                     
084800     EXEC SQL                                                             
084900         DELETE FROM T01PECA                                              
085000         WHERE  IDLEGSEL = :REQU-IDLEGSEL-KEY                             
085100         AND    DASTADAT = :REQU-DASTADAT-KEY                             
085200         AND    FLPERIOD = :WS-FLPERIOD                                   
085300     END-EXEC                                                             
085400     MOVE SQLCODE TO SQLCODE-WS                                           
085500     PERFORM DB2-STATUS-CHECK                                             
085600     .                                                                    
085700     EJECT                                                                
085800                                                                          
085900 DB2-UPDATE-T01PECA SECTION.                                              
086000     MOVE 000    TO GOOD-SQLCODECODES                                     
086100     EXEC SQL                                                             
086200         UPDATE T01PECA                                                   
086300         SET DAUPPDAT = :WS-DATUM,                                        
086400             DAFINDOC = :REQU-DAFINDOC,                                   
086500             IDUSER   = :REQU-IDUSER,                                     
086600             TIRP     = :PECA-TIRP                                        
086700         WHERE  CURRENT OF T01PECA-CRS                                    
086800     END-EXEC                                                             
086900     MOVE SQLCODE TO SQLCODE-WS                                           
087000     PERFORM DB2-STATUS-CHECK                                             
087100     .                                                                    
087200     EJECT                                                                
087300                                                                          
087400 DB2-INSERT-T01PECA SECTION.                                              
087500     MOVE 000   TO GOOD-SQLCODECODES                                      
087600     EXEC SQL                                                             
087700         INSERT INTO T01PECA                                              
087800         (IDLEGSEL, DASTADAT, TIRP, DAREGDAT, DAUPPDAT,                   
087900          DAFINDOC, FLPERIOD, IDUSER)                                     
088000         VALUES(:REQU-IDLEGSEL-KEY, :REQU-DASTADAT-KEY,                   
088100                :PECA-TIRP, :WS-DATUM, '00000000', :REQU-DAFINDOC,        
088200                :WS-FLPERIOD, :REQU-IDUSER)                               
088300     END-EXEC                                                             
088400     MOVE SQLCODE TO SQLCODE-WS                                           
088500     PERFORM DB2-STATUS-CHECK                                             
088600     .                                                                    
088700     EJECT                                                                
088800                                                                          
088900 DB2-STATUS-CHECK     SECTION.                                            
089000     SET SQLCODE-IX TO 1                                                  
089100     SEARCH GOOD-SQLCODE                                                  
089200       AT END                                                             
089300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
089400          DELIMITED BY SIZE INTO ERROR-TEXT                               
089500          CALL ABEND USING RKOD-ABEND-DB2                                 
089600       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
089700     END-SEARCH                                                           
089800     .                                                                    
