000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ042000.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/08/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.DAP.DISTRDOC                                            
001000*                                                                         
001100*                                                                         
001200*    FUNCTION:                                                            
001300*        - RECEIVE OUTPUT DATA VIA WZ01RECV.                              
001400*        - SEND ERROR RESPONSE VIA WZ01SEND OR SUBPROGRAM                 
001500*        - CALL SUBPROGRAMS FOR DISTRIBUTION DEPENDING ON VALUES          
001600*               IN TABLE TZ4DIRU FIELD KDOUTMETH.                         
001700*                                                                         
001800*        THE PROGRAM READS   TABLE TZ4DIRU                                
001900*        THE PROGRAM UPDATES TABLE TZ4REKY                                
002000*        THE PROGRAM UPDATES TABLE TZ4REDA                                
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSACTION:   WZ0420X                                           
002400*        REQUEST:       WZ04REQU, WZ04HDR                                 
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        RESPONSE:      WZ11OUTX                                          
002800*        ERROR-MESSAGE: WZ04RESP, WZ04HDR                                 
002900*                                                                         
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'WZ042000'.            
004300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004500 77  KDRC-DISPLAY                PIC Z(5).                                
004600 77  WS-OUT-ERROR-TEXT           PIC X(80)   VALUE SPACE.                 
004700                                                                          
004800 77  WS-KDTRANS                  PIC X(8)    VALUE SPACE.                 
004900                                                                          
005000 77  WS-MY-OWN-ADRESS            PIC X(50)   VALUE                        
005100                                          'CARPARTS.DAP.DISTRDOC'.        
005200 77  YES                         PIC X       VALUE 'J'.                   
005300 77  NOO                         PIC X       VALUE 'N'.                   
005400 77  WS-PRINT                    PIC X(4)    VALUE 'PRT '.                
005500 77  WS-EDI                      PIC X(4)    VALUE 'EDI '.                
005600 77  WS-FAX                      PIC X(4)    VALUE 'FAX '.                
005700 77  WS-VCOM                     PIC X(4)    VALUE 'VCOM'.                
005800 77  WS-MAIL                     PIC X(4)    VALUE 'MAIL'.                
005900 77  WS-ONDEMAND                 PIC X(4)    VALUE 'ONDE'.                
006000 77  WS-SAVE                     PIC X(4)    VALUE 'SAVE'.                
006100 77  WS-GET-IT                   PIC X(4)    VALUE 'GETI'.                
006200 77  WS-WEB                      PIC X(4)    VALUE 'WEB '.                
006300 77  WS-DEFAULT-RESTARTABLE-DAYS PIC S9(3)   VALUE 20 COMP-3.             
006400 77  WS-DP-ERROR                 PIC X(8)    VALUE 'DP-ERROR'.            
006500                                                                          
006600 77  HEADER-SW                   PIC X       VALUE 'J'.                   
006700     88  HEADER-OK                           VALUE 'J'.                   
006800     88  HEADER-WRONG                        VALUE 'N'.                   
006900                                                                          
007000 77  FIRST-LOOP-SW               PIC X       VALUE 'J'.                   
007100     88  FIRST-LOOP                          VALUE 'J'.                   
007200     88  OTHER-LOOP                          VALUE 'N'.                   
007300                                                                          
007400 77  WS-FLRULEMISS               PIC X       VALUE SPACE.                 
007500     88  RULE-MISSING                        VALUE 'J'.                   
007600     88  RULE-FOUND                          VALUE 'N'.                   
007700                                                                          
007800 77  WS-FLRULEMULT               PIC X       VALUE SPACE.                 
007900     88  MULTIPLE-RULES                      VALUE 'J'.                   
008000     88  SINGLE-RULE                         VALUE 'N'.                   
008100                                                                          
008200 77  WZ11OUTX-OPEN-SW            PIC X       VALUE 'J'.                   
008300     88  WZ11OUTX-OPEN-OK                    VALUE 'J'.                   
008400     88  WZ11OUTX-OPEN-FAILED                VALUE 'N'.                   
008500                                                                          
008600 77  WZ11OUTX-PUT-SW             PIC X       VALUE 'J'.                   
008700     88  WZ11OUTX-PUT-OK                     VALUE 'J'.                   
008800     88  WZ11OUTX-PUT-FAILED                 VALUE 'N'.                   
008900                                                                          
009000 01  WS-IDCALL                   PIC S9(9)   COMP VALUE +0.               
009100 01  WS-KDFUNC                   PIC X(10)   VALUE SPACE.                 
009200 01  WS-KDRC                     PIC S9(9)   COMP VALUE +0.               
009300 01  WS-TEOUTDATA-L              PIC S9(9)   COMP VALUE +0.               
009400 01  WS-TEOUTDATA                PIC X(3000) VALUE SPACE.                 
009500                                                                          
009600 01  WS-CURRENT-DATE-X6          PIC X(6)    VALUE SPACE.                 
009700 01  WS-CURRENT-DATE             PIC S9(7)   VALUE ZERO COMP-3.           
009800 01  WS-TIDATE2                  PIC X(6)    VALUE SPACE.                 
009900 01  WS-CURRENT-TIME             PIC S9(9)   VALUE ZERO COMP-3.           
010000 01  WS-CURRENT-TIME2            PIC S9(7)   VALUE ZERO COMP-3.           
010100 01  WS-TZ4REKY-DUMMY            PIC X(15)   VALUE SPACE.                 
010200 01  WS-TZ4REDA-DUMMY            PIC X(15)   VALUE SPACE.                 
010300 01  WS-KVDAGAR-RESEND           PIC S9(3)   VALUE ZERO COMP-3.           
010400 01  WS-TEFAX-1                  PIC X(50)   VALUE SPACE.                 
010500 01  WS-TEFAX-2                  PIC X(50)   VALUE SPACE.                 
010600 01  WS-TEFAX-3                  PIC X(50)   VALUE SPACE.                 
010700 01  WS-TEFAX-4                  PIC X(50)   VALUE SPACE.                 
010800 01  WS-TEFAX-5                  PIC X(50)   VALUE SPACE.                 
010900 01  WS-IDMAIL-SENDER            PIC X(60)   VALUE SPACE.                 
011000 01  WS-RECV-KVDLEN              PIC S9(9)   VALUE ZERO COMP.             
011100 01  WS-IDOUTREC-SAVE            PIC X(30)   VALUE SPACE.                 
011200 01  WS-IDOUTTYPE-SAVE           PIC X(15)   VALUE SPACE.                 
011300 01  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
011400                                                                          
011500 01  TAB-DATA.                                                            
011600     03 TAB-OUT-DATA OCCURS 15.                                           
011700        05 TAB-KDOUTMETH         PIC X(4)    VALUE SPACE.                 
011800        05 TAB-IDOUTDEST         PIC X(60)   VALUE SPACE.                 
011900        05 TAB-KVCOPIES          PIC X       VALUE SPACE.                 
012000        05 TAB-IDPFDEF           PIC X(8)    VALUE SPACE.                 
012100        05 TAB-IDFORMSNM         PIC X(8)    VALUE SPACE.                 
012200        05 TAB-FLCARRCNTL        PIC X       VALUE SPACE.                 
012300        05 TAB-FLACIF            PIC X       VALUE SPACE.                 
012400        05 TAB-TEVCOMST          PIC X(20)   VALUE SPACE.                 
012500        05 TAB-IDVCINIT          PIC X(8)    VALUE SPACE.                 
012600        05 TAB-KVPOST            PIC S9(7)   VALUE ZERO COMP-3.           
012700                                                                          
012800 01  W-SAVE-IDLOPNR              PIC S9(3)   COMP-3.                      
012900                                                                          
013000 01  IX                          PIC 99       VALUE ZERO.                 
013100 01  IX-SAVE-NBR-OF-RULES        PIC 99       VALUE ZERO.                 
013200 01  MAX-INDEX                   PIC 99       VALUE 15.                   
013300                                                                          
013400*    --- WORK FIELDS FOR VARIABLE SUBSTITUTION IN RULE DATA               
013500 01  TIX1                        PIC S9(4)   BINARY.                      
013600 01  TIX2                        PIC S9(4)   BINARY.                      
013700 01  TIX3                        PIC S9(4)   BINARY.                      
013800 01  TIXS                        PIC S9(4)   BINARY.                      
013900 01  TIXL                        PIC S9(4)   BINARY.                      
014000 01  TINTERVAL                   PIC X(10).                               
014100 01  TDATE                       PIC 9(6)    BINARY.                      
014200 01  TTIME                       PIC 9(8)    BINARY.                      
014300 01  FROM-TEXT                   PIC X(100).                              
014400 01  TO-TEXT                     PIC X(100).                              
014500 01  TEMP-TEXT                   PIC X(100).                              
014600 01  SYMBVAL-TEXT                PIC X(50).                               
014700 01  SUBST-DONE                  PIC X.                                   
014800                                                                          
014900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
015000 01  GENERAL-SUBPROGRAMS.                                                 
015100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015300     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
015400     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
015500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015600     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
015700     03  WZ11OUTP                PIC X(8)    VALUE 'WZ11OUTP'.            
015800     03  WZ11OUTF                PIC X(8)    VALUE 'WZ11OUTF'.            
015900     03  WZ11OUTM                PIC X(8)    VALUE 'WZ11OUTM'.            
016000     03  WZ11OUTV                PIC X(8)    VALUE 'WZ11OUTV'.            
016100     03  WZ11OUTO                PIC X(8)    VALUE 'WZ11OUTO'.            
016200     03  WZ11OUTA                PIC X(8)    VALUE 'WZ11OUTA'.            
016300     03  WZ11OUTW                PIC X(8)    VALUE 'WZ11OUTW'.            
016400     SKIP3                                                                
016500                                                                          
016600*    --- PARAMETERS TO ABEND                                              
016700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
017000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
017100                                                                          
017200*    --- PARAMETERS TO VIMSID                                             
017300 77  WS-VIMSID                   PIC X(8)    VALUE SPACE.                 
017400     SKIP3                                                                
017500 01  MESSAGE-CODES.                                                       
017600     03  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.                 
017700     03  ERR-MULTIPLE-RULES      PIC X(3)    VALUE '200'.                 
017800     03  ERR-DISTR-RULE-MISSING  PIC X(3)    VALUE '201'.                 
017900     03  ERR-PRINT-FAILED        PIC X(3)    VALUE '202'.                 
018000     03  ERR-FAX-FAILED          PIC X(3)    VALUE '203'.                 
018100     03  ERR-MAIL-FAILED         PIC X(3)    VALUE '204'.                 
018200     03  ERR-EDI-FAILED          PIC X(3)    VALUE '205'.                 
018300     03  ERR-VCOM-FAILED         PIC X(3)    VALUE '206'.                 
018400     03  ERR-ONDEMAND-FAILED     PIC X(3)    VALUE '207'.                 
018500     03  ERR-GET-IT-FAILED       PIC X(3)    VALUE '208'.                 
018600     03  ERR-WEB-FAILED          PIC X(3)    VALUE '209'.                 
018700     03  ERR-INVALID-HDR         PIC X(3)    VALUE '222'.                 
018800 01  WS-ERROR-TYPE               PIC X(3)    VALUE SPACE.                 
018900     EJECT                                                                
019000                                                                          
019100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
019200     SKIP3                                                                
019300*01  -COPY WZ01SEND                                                       
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
019600     SKIP3                                                                
019700*01  -COPY WZ01RECV                                                       
019800     EJECT                                                                
019900 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
020000     SKIP3                                                                
020100 01  RECV-AREA.                                                           
020200*    03  -COPY WZ01REQU                                                   
020300*    03  -COPY WZ04HDR                                                    
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)  VALUE 'IN-TEOUT-AREA'.        
020600     SKIP3                                                                
020700 01  IN-TEOUTDATA.                                                        
020800     03 IN-DATA                  PIC X(3000) VALUE SPACE.                 
020900     EJECT                                                                
021000 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTF-AREA'.        
021100     SKIP3                                                                
021200*01 -COPY WZ11OUTF                                                        
021300 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTM-AREA'.        
021400     SKIP3                                                                
021500*01 -COPY WZ11OUTM                                                        
021600 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTO-AREA'.        
021700     SKIP3                                                                
021800*01 -COPY WZ11OUTO                                                        
021900 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTP-AREA'.        
022000     SKIP3                                                                
022100*01 -COPY WZ11OUTP                                                        
022200 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTV-AREA'.        
022300     SKIP3                                                                
022400*01 -COPY WZ11OUTV                                                        
022500 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTA-AREA'.        
022600     SKIP3                                                                
022700*01 -COPY WZ11OUTA                                                        
022800 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTW-AREA'.        
022900     SKIP3                                                                
023000*01 -COPY WZ11OUTW                                                        
023100     EJECT                                                                
023200 01  FILLER                      PIC X(16)   VALUE 'ERROR-AREA'.          
023300     SKIP3                                                                
023400 01  ERROR-RESP-AREA.                                                     
023500*    03  -COPY WZ01RESP   -PRE ERR-                                       
023600*    03  -COPY WZ04HDR    -PRE ERR-                                       
023700     SKIP3                                                                
023800 01  ERR2-REQU-AREA.                                                      
023900*    03  -COPY WZ01REQU   -PRE ERR2-                                      
024000*    03  -COPY WZ04HDR    -PRE ERR2-                                      
024100     EJECT                                                                
024200 01  FILLER                      PIC X(16)  VALUE 'WZ20DAYS-AREA'.        
024300     SKIP3                                                                
024400*    -COPY WZ20DAYS                                                       
024500     EJECT                                                                
024600                                                                          
024700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
024800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
024900                                                                          
025000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
025100 01  DB2-WS.                                                              
025200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
025300         88  CURSOR-OK                       VALUE 000.                   
025400         88  LINES-FOUND                     VALUE 000.                   
025500         88  LINES-MISSING                   VALUE 100.                   
025600         88  RESOURCE-WRONG                  VALUE 904.                   
025700         88  MULTIPLE-LINES                  VALUE 811.                   
025800     03  GOOD-SQLCODECODES.                                               
025900         05  GOOD-SQLCODE OCCURS 5                                        
026000             INDEXED BY SQLCODE-IX PIC 9(3).                              
026100                                                                          
026200     EJECT                                                                
026300 01  FILLER                      PIC X(16)  VALUE 'TZ4DIRU-AREA'.         
026400                                                                          
026500*01  -COPY TZ4DIRU -PRE DIRU-                                             
026600     EJECT                                                                
026700 01  FILLER                      PIC X(16)  VALUE 'TZ4REKY-AREA'.         
026800                                                                          
026900*01  -COPY TZ4REKY -PRE REKY-                                             
027000     EJECT                                                                
027100 01  FILLER                      PIC X(16)  VALUE 'TZ4REDA-AREA'.         
027200                                                                          
027300*01  -COPY TZ4REDA -PRE REDA-                                             
027400     EJECT                                                                
027500                                                                          
027600     EJECT                                                                
027700     EXEC SQL INCLUDE TZ4DIRU END-EXEC.                                   
027800     EJECT                                                                
027900     EXEC SQL INCLUDE TZ4REKY END-EXEC.                                   
028000     EJECT                                                                
028100     EXEC SQL INCLUDE TZ4REDA END-EXEC.                                   
028200     EJECT                                                                
028300                                                                          
028400 LINKAGE SECTION.                                                         
028500     EJECT                                                                
028600 PROCEDURE DIVISION.                                                      
028700 MAIN SECTION.                                                            
028800                                                                          
028900     PERFORM S03-RECEIVE-OPEN                                             
029000     PERFORM S03-RECEIVE-MESSAGE-HEADER                                   
029100     IF RECV-KDRC = 0                                                     
029200       PERFORM A-INIT                                                     
029300       PERFORM B-CHECK-MESSAGE-HEADER                                     
029400       IF HEADER-OK                                                       
029500         PERFORM C-FETCH-DISTRIB-RULE                                     
029600*        - DATA IS SAVED, BUT NOT DISTRIBUTED, IF RULE IS MISSING         
029700         PERFORM F-RECEIVE-DISTRIBUTE-SAVE-DATA                           
029800         IF RULE-MISSING                                                  
029900           MOVE ERR-DISTR-RULE-MISSING TO ERR-RESP-IDMSG-ERROR            
030000           MOVE 'MISSING RULE'         TO ERR-RESP-IDELMT-ERROR           
030100           IF MULTIPLE-RULES                                              
030200             MOVE ERR-MULTIPLE-RULES   TO ERR-RESP-IDMSG-ERROR            
030300             MOVE 'MULTIPLE RULES'     TO ERR-RESP-IDELMT-ERROR           
030400           END-IF                                                         
030500           PERFORM G-HANDLE-ERROR                                         
030600         END-IF                                                           
030700         IF WZ11OUTX-OPEN-FAILED OR WZ11OUTX-PUT-FAILED                   
030800*          - TYPE OF ERROR IS SET IN S04-DISTRIBUTE                       
030900           PERFORM G-HANDLE-ERROR                                         
031000         END-IF                                                           
031100       ELSE                                                               
031200         MOVE ERR-INVALID-KEY TO ERR-RESP-IDMSG-ERROR                     
031300         MOVE 'INVALID HDR DATA' TO ERR-RESP-IDELMT-ERROR                 
031400         PERFORM G-HANDLE-ERROR                                           
031500       END-IF                                                             
031600     ELSE                                                                 
031700       MOVE ERR-INVALID-HDR TO ERR-RESP-IDMSG-ERROR                       
031800       MOVE 'MISSING WZ04HDR' TO ERR-RESP-IDELMT-ERROR                    
031900       PERFORM G-HANDLE-ERROR                                             
032000     END-IF                                                               
032100                                                                          
032200     PERFORM S03-RECEIVE-CLOSE                                            
032300                                                                          
032400     MOVE ZERO TO RETURN-CODE                                             
032500     GOBACK                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 A-INIT SECTION.                                                          
032900                                                                          
033000     INITIALIZE GOOD-SQLCODECODES                                         
033100     INITIALIZE REKY-TZ4REKY                                              
033200     INITIALIZE REDA-TZ4REDA                                              
033300     MOVE FUNCTION CURRENT-DATE (3:6) TO WS-CURRENT-DATE                  
033400                                         WS-CURRENT-DATE-X6               
033500     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-CURRENT-TIME                  
033600     MOVE FUNCTION CURRENT-DATE (9:6) TO WS-CURRENT-TIME2                 
033700     .                                                                    
033800     EJECT                                                                
033900 B-CHECK-MESSAGE-HEADER SECTION.                                          
034000                                                                          
034100     MOVE YES TO HEADER-SW                                                
034200                                                                          
034300*    -- CHECK HEADER VERSION                                              
034400     IF REQU-IDMSGVER NUMERIC                                             
034500     AND REQU-IDMSGVER = 001                                              
034600       CONTINUE                                                           
034700     ELSE                                                                 
034800       MOVE NOO TO HEADER-SW                                              
034900     END-IF                                                               
035000                                                                          
035100*    -- OUTPUT TYPE MUST BE SPECIFIED, OTHER FIELDS ARE OPTIONAL          
035200     IF HDR-IDOUTTYPE = SPACE                                             
035300       MOVE NOO TO HEADER-SW                                              
035400     END-IF                                                               
035500     .                                                                    
035600     EJECT                                                                
035700 C-FETCH-DISTRIB-RULE SECTION.                                            
035800                                                                          
035900*    -- USE HEADER DATA TO SEARCH FOR A DISTRIBUTION RULE                 
036000                                                                          
036100     PERFORM CA-CLEAR-RULE-TABLE                                          
036200     PERFORM DB2-SELECT-TZ4DIRU-TAB                                       
036300     IF LINES-FOUND                                                       
036400       MOVE NOO TO WS-FLRULEMISS                                          
036500       PERFORM CB-BUILD-RULE-TABLE                                        
036600       PERFORM CC-OPEN-DISTRIB-CHANNELS                                   
036700       PERFORM DB2-UPDATE-TZ4DIRU-TIANVDAT                                
036800     ELSE                                                                 
036900       MOVE YES TO WS-FLRULEMISS                                          
037000       MOVE 1 TO IX-SAVE-NBR-OF-RULES                                     
037100     END-IF                                                               
037200     .                                                                    
037300     SKIP3                                                                
037400 CA-CLEAR-RULE-TABLE   SECTION.                                           
037500                                                                          
037600     MOVE SPACE   TO WS-TEFAX-1                                           
037700                     WS-TEFAX-2                                           
037800                     WS-TEFAX-3                                           
037900                     WS-TEFAX-4                                           
038000                     WS-TEFAX-5                                           
038100                                                                          
038200     MOVE 1 TO IX                                                         
038300     PERFORM UNTIL IX > MAX-INDEX                                         
038400       MOVE SPACE TO TAB-KDOUTMETH(IX)                                    
038500                     TAB-IDOUTDEST(IX)                                    
038600                     TAB-KVCOPIES(IX)                                     
038700                     TAB-IDPFDEF(IX)                                      
038800                     TAB-IDFORMSNM(IX)                                    
038900                     TAB-FLCARRCNTL(IX)                                   
039000                     TAB-FLACIF(IX)                                       
039100                     TAB-TEVCOMST(IX)                                     
039200                     TAB-IDVCINIT(IX)                                     
039300       MOVE ZERO TO  TAB-KVPOST(IX)                                       
039400       ADD 1 TO IX                                                        
039500     END-PERFORM                                                          
039600     MOVE ZERO TO IX-SAVE-NBR-OF-RULES                                    
039700     MOVE ZERO TO W-SAVE-IDLOPNR                                          
039800     .                                                                    
039900     EJECT                                                                
040000 CB-BUILD-RULE-TABLE SECTION.                                             
040100                                                                          
040200*    -- COPY RULE DATA TO AN INTERNAL TABLE TO MAKE IT POSSIBLE           
040300*    -- TO LOOP OVER THE RULE LINES.                                      
040400*    -- (TEFAX AND IDMAIL-ENDER DATA ARE DIFFERENT -                      
040500*    --  THEY ARE USED WITH MANY LINES)                                   
040600                                                                          
040700     MOVE DIRU-TEFAX-1       TO WS-TEFAX-1                                
040800     MOVE DIRU-TEFAX-2       TO WS-TEFAX-2                                
040900     MOVE DIRU-TEFAX-3       TO WS-TEFAX-3                                
041000     MOVE DIRU-TEFAX-4       TO WS-TEFAX-4                                
041100     MOVE DIRU-TEFAX-5       TO WS-TEFAX-5                                
041200     MOVE DIRU-IDMAIL-SENDER TO WS-IDMAIL-SENDER                          
041300                                                                          
041400     MOVE DIRU-KDOUTMETH-1   TO TAB-KDOUTMETH(1)                          
041500     MOVE DIRU-KDOUTMETH-2   TO TAB-KDOUTMETH(2)                          
041600     MOVE DIRU-KDOUTMETH-3   TO TAB-KDOUTMETH(3)                          
041700     MOVE DIRU-KDOUTMETH-4   TO TAB-KDOUTMETH(4)                          
041800     MOVE DIRU-KDOUTMETH-5   TO TAB-KDOUTMETH(5)                          
041900     MOVE DIRU-KDOUTMETH-6   TO TAB-KDOUTMETH(6)                          
042000     MOVE DIRU-KDOUTMETH-7   TO TAB-KDOUTMETH(7)                          
042100     MOVE DIRU-KDOUTMETH-8   TO TAB-KDOUTMETH(8)                          
042200     MOVE DIRU-KDOUTMETH-9   TO TAB-KDOUTMETH(9)                          
042300     MOVE DIRU-KDOUTMETH-10  TO TAB-KDOUTMETH(10)                         
042400     MOVE DIRU-KDOUTMETH-11  TO TAB-KDOUTMETH(11)                         
042500     MOVE DIRU-KDOUTMETH-12  TO TAB-KDOUTMETH(12)                         
042600     MOVE DIRU-KDOUTMETH-13  TO TAB-KDOUTMETH(13)                         
042700     MOVE DIRU-KDOUTMETH-14  TO TAB-KDOUTMETH(14)                         
042800     MOVE DIRU-KDOUTMETH-15  TO TAB-KDOUTMETH(15)                         
042900     MOVE DIRU-IDOUTDEST-1   TO TAB-IDOUTDEST(1)                          
043000     MOVE DIRU-IDOUTDEST-2   TO TAB-IDOUTDEST(2)                          
043100     MOVE DIRU-IDOUTDEST-3   TO TAB-IDOUTDEST(3)                          
043200     MOVE DIRU-IDOUTDEST-4   TO TAB-IDOUTDEST(4)                          
043300     MOVE DIRU-IDOUTDEST-5   TO TAB-IDOUTDEST(5)                          
043400     MOVE DIRU-IDOUTDEST-6   TO TAB-IDOUTDEST(6)                          
043500     MOVE DIRU-IDOUTDEST-7   TO TAB-IDOUTDEST(7)                          
043600     MOVE DIRU-IDOUTDEST-8   TO TAB-IDOUTDEST(8)                          
043700     MOVE DIRU-IDOUTDEST-9   TO TAB-IDOUTDEST(9)                          
043800     MOVE DIRU-IDOUTDEST-10  TO TAB-IDOUTDEST(10)                         
043900     MOVE DIRU-IDOUTDEST-11  TO TAB-IDOUTDEST(11)                         
044000     MOVE DIRU-IDOUTDEST-12  TO TAB-IDOUTDEST(12)                         
044100     MOVE DIRU-IDOUTDEST-13  TO TAB-IDOUTDEST(13)                         
044200     MOVE DIRU-IDOUTDEST-14  TO TAB-IDOUTDEST(14)                         
044300     MOVE DIRU-IDOUTDEST-15  TO TAB-IDOUTDEST(15)                         
044400     MOVE DIRU-KVCOPIES-1    TO TAB-KVCOPIES(1)                           
044500     MOVE DIRU-KVCOPIES-2    TO TAB-KVCOPIES(2)                           
044600     MOVE DIRU-KVCOPIES-3    TO TAB-KVCOPIES(3)                           
044700     MOVE DIRU-KVCOPIES-4    TO TAB-KVCOPIES(4)                           
044800     MOVE DIRU-KVCOPIES-5    TO TAB-KVCOPIES(5)                           
044900     MOVE DIRU-KVCOPIES-6    TO TAB-KVCOPIES(6)                           
045000     MOVE DIRU-KVCOPIES-7    TO TAB-KVCOPIES(7)                           
045100     MOVE DIRU-KVCOPIES-8    TO TAB-KVCOPIES(8)                           
045200     MOVE DIRU-KVCOPIES-9    TO TAB-KVCOPIES(9)                           
045300     MOVE DIRU-KVCOPIES-10   TO TAB-KVCOPIES(10)                          
045400     MOVE DIRU-KVCOPIES-11   TO TAB-KVCOPIES(11)                          
045500     MOVE DIRU-KVCOPIES-12   TO TAB-KVCOPIES(12)                          
045600     MOVE DIRU-KVCOPIES-13   TO TAB-KVCOPIES(13)                          
045700     MOVE DIRU-KVCOPIES-14   TO TAB-KVCOPIES(14)                          
045800     MOVE DIRU-KVCOPIES-15   TO TAB-KVCOPIES(15)                          
045900     MOVE DIRU-IDPFDEF-1     TO TAB-IDPFDEF(1)                            
046000     MOVE DIRU-IDPFDEF-2     TO TAB-IDPFDEF(2)                            
046100     MOVE DIRU-IDPFDEF-3     TO TAB-IDPFDEF(3)                            
046200     MOVE DIRU-IDPFDEF-4     TO TAB-IDPFDEF(4)                            
046300     MOVE DIRU-IDPFDEF-5     TO TAB-IDPFDEF(5)                            
046400     MOVE DIRU-IDPFDEF-6     TO TAB-IDPFDEF(6)                            
046500     MOVE DIRU-IDPFDEF-7     TO TAB-IDPFDEF(7)                            
046600     MOVE DIRU-IDPFDEF-8     TO TAB-IDPFDEF(8)                            
046700     MOVE DIRU-IDPFDEF-9     TO TAB-IDPFDEF(9)                            
046800     MOVE DIRU-IDPFDEF-10    TO TAB-IDPFDEF(10)                           
046900     MOVE DIRU-IDPFDEF-11    TO TAB-IDPFDEF(11)                           
047000     MOVE DIRU-IDPFDEF-12    TO TAB-IDPFDEF(12)                           
047100     MOVE DIRU-IDPFDEF-13    TO TAB-IDPFDEF(13)                           
047200     MOVE DIRU-IDPFDEF-14    TO TAB-IDPFDEF(14)                           
047300     MOVE DIRU-IDPFDEF-15    TO TAB-IDPFDEF(15)                           
047400     MOVE DIRU-IDFORMSNM-1   TO TAB-IDFORMSNM(1)                          
047500     MOVE DIRU-IDFORMSNM-2   TO TAB-IDFORMSNM(2)                          
047600     MOVE DIRU-IDFORMSNM-3   TO TAB-IDFORMSNM(3)                          
047700     MOVE DIRU-IDFORMSNM-4   TO TAB-IDFORMSNM(4)                          
047800     MOVE DIRU-IDFORMSNM-5   TO TAB-IDFORMSNM(5)                          
047900     MOVE DIRU-IDFORMSNM-6   TO TAB-IDFORMSNM(6)                          
048000     MOVE DIRU-IDFORMSNM-7   TO TAB-IDFORMSNM(7)                          
048100     MOVE DIRU-IDFORMSNM-8   TO TAB-IDFORMSNM(8)                          
048200     MOVE DIRU-IDFORMSNM-9   TO TAB-IDFORMSNM(9)                          
048300     MOVE DIRU-IDFORMSNM-10  TO TAB-IDFORMSNM(10)                         
048400     MOVE DIRU-IDFORMSNM-11  TO TAB-IDFORMSNM(11)                         
048500     MOVE DIRU-IDFORMSNM-12  TO TAB-IDFORMSNM(12)                         
048600     MOVE DIRU-IDFORMSNM-13  TO TAB-IDFORMSNM(13)                         
048700     MOVE DIRU-IDFORMSNM-14  TO TAB-IDFORMSNM(14)                         
048800     MOVE DIRU-IDFORMSNM-15  TO TAB-IDFORMSNM(15)                         
048900     MOVE DIRU-FLCARRCNTL-1  TO TAB-FLCARRCNTL(1)                         
049000     MOVE DIRU-FLCARRCNTL-2  TO TAB-FLCARRCNTL(2)                         
049100     MOVE DIRU-FLCARRCNTL-3  TO TAB-FLCARRCNTL(3)                         
049200     MOVE DIRU-FLCARRCNTL-4  TO TAB-FLCARRCNTL(4)                         
049300     MOVE DIRU-FLCARRCNTL-5  TO TAB-FLCARRCNTL(5)                         
049400     MOVE DIRU-FLCARRCNTL-6  TO TAB-FLCARRCNTL(6)                         
049500     MOVE DIRU-FLCARRCNTL-7  TO TAB-FLCARRCNTL(7)                         
049600     MOVE DIRU-FLCARRCNTL-8  TO TAB-FLCARRCNTL(8)                         
049700     MOVE DIRU-FLCARRCNTL-9  TO TAB-FLCARRCNTL(9)                         
049800     MOVE DIRU-FLCARRCNTL-10 TO TAB-FLCARRCNTL(10)                        
049900     MOVE DIRU-FLCARRCNTL-11 TO TAB-FLCARRCNTL(11)                        
050000     MOVE DIRU-FLCARRCNTL-12 TO TAB-FLCARRCNTL(12)                        
050100     MOVE DIRU-FLCARRCNTL-13 TO TAB-FLCARRCNTL(13)                        
050200     MOVE DIRU-FLCARRCNTL-14 TO TAB-FLCARRCNTL(14)                        
050300     MOVE DIRU-FLCARRCNTL-15 TO TAB-FLCARRCNTL(15)                        
050400     MOVE DIRU-FLACIF-1      TO TAB-FLACIF(1)                             
050500     MOVE DIRU-FLACIF-2      TO TAB-FLACIF(2)                             
050600     MOVE DIRU-FLACIF-3      TO TAB-FLACIF(3)                             
050700     MOVE DIRU-FLACIF-4      TO TAB-FLACIF(4)                             
050800     MOVE DIRU-FLACIF-5      TO TAB-FLACIF(5)                             
050900     MOVE DIRU-FLACIF-6      TO TAB-FLACIF(6)                             
051000     MOVE DIRU-FLACIF-7      TO TAB-FLACIF(7)                             
051100     MOVE DIRU-FLACIF-8      TO TAB-FLACIF(8)                             
051200     MOVE DIRU-FLACIF-9      TO TAB-FLACIF(9)                             
051300     MOVE DIRU-FLACIF-10     TO TAB-FLACIF(10)                            
051400     MOVE DIRU-FLACIF-11     TO TAB-FLACIF(11)                            
051500     MOVE DIRU-FLACIF-12     TO TAB-FLACIF(12)                            
051600     MOVE DIRU-FLACIF-13     TO TAB-FLACIF(13)                            
051700     MOVE DIRU-FLACIF-14     TO TAB-FLACIF(14)                            
051800     MOVE DIRU-FLACIF-15     TO TAB-FLACIF(15)                            
051900     MOVE DIRU-TEVCOMST-1    TO TAB-TEVCOMST(1)                           
052000     MOVE DIRU-TEVCOMST-2    TO TAB-TEVCOMST(2)                           
052100     MOVE DIRU-TEVCOMST-3    TO TAB-TEVCOMST(3)                           
052200     MOVE DIRU-TEVCOMST-4    TO TAB-TEVCOMST(4)                           
052300     MOVE DIRU-TEVCOMST-5    TO TAB-TEVCOMST(5)                           
052400     MOVE DIRU-TEVCOMST-6    TO TAB-TEVCOMST(6)                           
052500     MOVE DIRU-TEVCOMST-7    TO TAB-TEVCOMST(7)                           
052600     MOVE DIRU-TEVCOMST-8    TO TAB-TEVCOMST(8)                           
052700     MOVE DIRU-TEVCOMST-9    TO TAB-TEVCOMST(9)                           
052800     MOVE DIRU-TEVCOMST-10   TO TAB-TEVCOMST(10)                          
052900     MOVE DIRU-TEVCOMST-11   TO TAB-TEVCOMST(11)                          
053000     MOVE DIRU-TEVCOMST-12   TO TAB-TEVCOMST(12)                          
053100     MOVE DIRU-TEVCOMST-13   TO TAB-TEVCOMST(13)                          
053200     MOVE DIRU-TEVCOMST-14   TO TAB-TEVCOMST(14)                          
053300     MOVE DIRU-TEVCOMST-15   TO TAB-TEVCOMST(15)                          
053400     MOVE DIRU-IDVCINIT-1    TO TAB-IDVCINIT(1)                           
053500     MOVE DIRU-IDVCINIT-2    TO TAB-IDVCINIT(2)                           
053600     MOVE DIRU-IDVCINIT-3    TO TAB-IDVCINIT(3)                           
053700     MOVE DIRU-IDVCINIT-4    TO TAB-IDVCINIT(4)                           
053800     MOVE DIRU-IDVCINIT-5    TO TAB-IDVCINIT(5)                           
053900     MOVE DIRU-IDVCINIT-6    TO TAB-IDVCINIT(6)                           
054000     MOVE DIRU-IDVCINIT-7    TO TAB-IDVCINIT(7)                           
054100     MOVE DIRU-IDVCINIT-8    TO TAB-IDVCINIT(8)                           
054200     MOVE DIRU-IDVCINIT-9    TO TAB-IDVCINIT(9)                           
054300     MOVE DIRU-IDVCINIT-10   TO TAB-IDVCINIT(10)                          
054400     MOVE DIRU-IDVCINIT-11   TO TAB-IDVCINIT(11)                          
054500     MOVE DIRU-IDVCINIT-12   TO TAB-IDVCINIT(12)                          
054600     MOVE DIRU-IDVCINIT-13   TO TAB-IDVCINIT(13)                          
054700     MOVE DIRU-IDVCINIT-14   TO TAB-IDVCINIT(14)                          
054800     MOVE DIRU-IDVCINIT-15   TO TAB-IDVCINIT(15)                          
054900                                                                          
055000     PERFORM CBA-SUBSTITUTE-SYMBOLS                                       
055100     .                                                                    
055200     EJECT                                                                
055300 CBA-SUBSTITUTE-SYMBOLS    SECTION.                                       
055400                                                                          
055500     IF WS-TEFAX-1 NOT = SPACE                                            
055600       MOVE WS-TEFAX-1 TO FROM-TEXT                                       
055700       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
055800       IF SUBST-DONE = YES                                                
055900         MOVE TO-TEXT TO WS-TEFAX-1                                       
056000       END-IF                                                             
056100     END-IF                                                               
056200                                                                          
056300     IF WS-TEFAX-2 NOT = SPACE                                            
056400       MOVE WS-TEFAX-2 TO FROM-TEXT                                       
056500       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
056600       IF SUBST-DONE = YES                                                
056700         MOVE TO-TEXT TO WS-TEFAX-2                                       
056800       END-IF                                                             
056900     END-IF                                                               
057000                                                                          
057100     IF WS-TEFAX-3 NOT = SPACE                                            
057200       MOVE WS-TEFAX-3 TO FROM-TEXT                                       
057300       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
057400       IF SUBST-DONE = YES                                                
057500         MOVE TO-TEXT TO WS-TEFAX-3                                       
057600       END-IF                                                             
057700     END-IF                                                               
057800                                                                          
057900     IF WS-TEFAX-4 NOT = SPACE                                            
058000       MOVE WS-TEFAX-4 TO FROM-TEXT                                       
058100       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
058200       IF SUBST-DONE = YES                                                
058300         MOVE TO-TEXT TO WS-TEFAX-4                                       
058400       END-IF                                                             
058500     END-IF                                                               
058600                                                                          
058700     IF WS-TEFAX-5 NOT = SPACE                                            
058800       MOVE WS-TEFAX-5 TO FROM-TEXT                                       
058900       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
059000       IF SUBST-DONE = YES                                                
059100         MOVE TO-TEXT TO WS-TEFAX-5                                       
059200       END-IF                                                             
059300     END-IF                                                               
059400                                                                          
059500     MOVE 1 TO IX                                                         
059600     PERFORM UNTIL IX > MAX-INDEX                                         
059700       IF TAB-TEVCOMST(IX) NOT = SPACE                                    
059800         MOVE TAB-TEVCOMST(IX) TO FROM-TEXT                               
059900         PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                
060000         IF SUBST-DONE = YES                                              
060100           MOVE TO-TEXT TO TAB-TEVCOMST(IX)                               
060200         END-IF                                                           
060300       END-IF                                                             
060400       IF TAB-IDPFDEF(IX) NOT = SPACE                                     
060500         MOVE TAB-IDPFDEF(IX) TO FROM-TEXT                                
060600         PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                
060700         IF SUBST-DONE = YES                                              
060800           MOVE TO-TEXT TO TAB-IDPFDEF(IX)                                
060900         END-IF                                                           
061000       END-IF                                                             
061100       IF TAB-IDOUTDEST(IX) NOT = SPACE                                   
061200         MOVE TAB-IDOUTDEST(IX) TO FROM-TEXT                              
061300         PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                
061400         IF SUBST-DONE = YES                                              
061500           MOVE TO-TEXT TO TAB-IDOUTDEST(IX)                              
061600         END-IF                                                           
061700       END-IF                                                             
061800       ADD 1 TO IX                                                        
061900     END-PERFORM                                                          
062000                                                                          
062100     .                                                                    
062200     EJECT                                                                
062300 CBAA-CHECK-SUBSTITUTE-ONE   SECTION.                                     
062400                                                                          
062500*    -- SYMBOLIC EXPRESSIONS IN THE FORM &A OR &A(S-E) MAY OCCUR          
062600*    -- "&A" MAY BE ONE OF THE FOLLOWING:                                 
062700*    --      &I = THE VALUE OF IDLIST (OUTPUT ID)                         
062800*    --      &S = THE VALUE OF IDOUTREC (OUTPUT SUB TYPE)                 
062900*    --      &O = THE VALUE OF IDOUTTYPE (OUTPUT TYPE)                    
063000*    --      &D = THE VALUE OF CURRENT DATE (YYMMDD)                      
063100*    --      &T = THE VALUE OF CURRENT TIME STAMP (HHMMSSHH)              
063200*    -- (S-E) SPECIFIES A SUB-STRING OF THE SPECIFIED VALUE               
063300*    -- WHERE "S" IS THE START POSITION AND "E" IS THE END                
063400*    -- POSITION OF THE SUB-STRING                                        
063500     MOVE NOO TO SUBST-DONE                                               
063600     MOVE 1 TO TIX1                                                       
063700     INSPECT FROM-TEXT TALLYING TIX1                                      
063800             FOR CHARACTERS BEFORE INITIAL '&'                            
063900     PERFORM UNTIL TIX1 >= LENGTH OF FROM-TEXT                            
064000       MOVE FROM-TEXT TO TEMP-TEXT                                        
064100       IF FROM-TEXT(TIX1:2) = '&I'                                        
064200         MOVE HDR-IDLIST TO SYMBVAL-TEXT                                  
064300         MOVE LENGTH OF HDR-IDLIST TO TIXL                                
064400         PERFORM CBAAA-SUBST-SYMBOL                                       
064500       END-IF                                                             
064600       IF FROM-TEXT(TIX1:2) = '&S'                                        
064700         MOVE HDR-IDOUTREC    TO SYMBVAL-TEXT                             
064800         MOVE LENGTH OF HDR-IDOUTREC TO TIXL                              
064900         PERFORM CBAAA-SUBST-SYMBOL                                       
065000       END-IF                                                             
065100       IF FROM-TEXT(TIX1:2) = '&O'                                        
065200         MOVE HDR-IDOUTTYPE   TO SYMBVAL-TEXT                             
065300         MOVE LENGTH OF HDR-IDOUTTYPE TO TIXL                             
065400         PERFORM CBAAA-SUBST-SYMBOL                                       
065500       END-IF                                                             
065600       IF FROM-TEXT(TIX1:2) = '&D'                                        
065700         MOVE WS-CURRENT-DATE TO TDATE                                    
065800         MOVE TDATE           TO SYMBVAL-TEXT                             
065900         MOVE 6               TO TIXL                                     
066000         PERFORM CBAAA-SUBST-SYMBOL                                       
066100       END-IF                                                             
066200       IF FROM-TEXT(TIX1:2) = '&T'                                        
066300         MOVE WS-CURRENT-TIME TO TTIME                                    
066400         MOVE TTIME           TO SYMBVAL-TEXT                             
066500         MOVE 8               TO TIXL                                     
066600         PERFORM CBAAA-SUBST-SYMBOL                                       
066700       END-IF                                                             
066800                                                                          
066900       ADD 1 TO TIX1                                                      
067000       INSPECT FROM-TEXT(TIX1:) TALLYING TIX1                             
067100               FOR CHARACTERS BEFORE INITIAL '&'                          
067200     END-PERFORM                                                          
067300                                                                          
067400     .                                                                    
067500     EJECT                                                                
067600 CBAAA-SUBST-SYMBOL        SECTION.                                       
067700                                                                          
067800     IF TEMP-TEXT(TIX1 + 2:1) = '('                                       
067900       COMPUTE TIX2 = TIX1 + 3                                            
068000       INSPECT TEMP-TEXT(TIX2:) TALLYING TIX2                             
068100               FOR CHARACTERS BEFORE ')'                                  
068200       MOVE TEMP-TEXT(TIX1 + 3:TIX2 - TIX1 - 3) TO TINTERVAL              
068300       MOVE ZERO TO TIX3                                                  
068400       INSPECT TINTERVAL TALLYING TIX3                                    
068500               FOR CHARACTERS BEFORE '-'                                  
068600       COMPUTE TIXS = FUNCTION NUMVAL(TINTERVAL(1:TIX3))                  
068700       COMPUTE TIXL = FUNCTION NUMVAL(TINTERVAL(TIX3 + 2:))               
068800       COMPUTE TIXL = TIXL - TIXS + 1                                     
068900     ELSE                                                                 
069000       MOVE 1 TO TIXS                                                     
069100*      -- STRIP TRAILING BLANKS                                           
069200       PERFORM UNTIL SYMBVAL-TEXT(TIXL:1) NOT = SPACE                     
069300         SUBTRACT 1 FROM TIXL                                             
069400       END-PERFORM                                                        
069500       COMPUTE TIX2 = TIX1 + 1                                            
069600     END-IF                                                               
069700     IF TIXS > 0 AND TIXL > 0                                             
069800       IF TIX1 > 1                                                        
069900         STRING TEMP-TEXT(1:TIX1 - 1) DELIMITED BY SIZE                   
070000                SYMBVAL-TEXT(TIXS:TIXL) DELIMITED BY SIZE                 
070100                TEMP-TEXT(TIX2 + 1:)  DELIMITED BY SIZE                   
070200           INTO TO-TEXT                                                   
070300       ELSE                                                               
070400         STRING SYMBVAL-TEXT(TIXS:TIXL) DELIMITED BY SIZE                 
070500                TEMP-TEXT(TIX2 + 1:)  DELIMITED BY SIZE                   
070600           INTO TO-TEXT                                                   
070700       END-IF                                                             
070800     END-IF                                                               
070900                                                                          
071000     MOVE TO-TEXT TO FROM-TEXT                                            
071100     MOVE YES TO SUBST-DONE                                               
071200     .                                                                    
071300     EJECT                                                                
071400 CC-OPEN-DISTRIB-CHANNELS  SECTION.                                       
071500                                                                          
071600     PERFORM CCA-COMPUTE-RESTART-KEYS                                     
071700                                                                          
071800     MOVE ZERO                  TO WS-KDRC                                
071900     MOVE YES                   TO WZ11OUTX-OPEN-SW                       
072000     MOVE 'OPEN'                TO WS-KDFUNC                              
072100     MOVE 1 TO IX                                                         
072200     PERFORM UNTIL IX > MAX-INDEX OR TAB-KDOUTMETH(IX) = SPACE            
072300     OR WS-KDRC > 0                                                       
072400       MOVE IX                  TO WS-IDCALL                              
072500       PERFORM S04-DISTRIBUTE                                             
072600       ADD 1 TO IX                                                        
072700     END-PERFORM                                                          
072800                                                                          
072900*    -- REMEMBER IF AN OPEN ERROR OCCURRED                                
073000     IF WS-KDRC > 0                                                       
073100       MOVE NOO TO WZ11OUTX-OPEN-SW                                       
073200     END-IF                                                               
073300                                                                          
073400*    -- SAVE NUMBER OF RULE LINES ACTUALLY USED IN THIS RULE              
073500     COMPUTE IX-SAVE-NBR-OF-RULES = IX - 1                                
073600     .                                                                    
073700     EJECT                                                                
073800 CCA-COMPUTE-RESTART-KEYS SECTION.                                        
073900                                                                          
074000     MOVE HDR-IDOUTTYPE         TO REKY-IDOUTTYPE                         
074100     MOVE HDR-IDOUTREC          TO REKY-IDOUTREC                          
074200     MOVE HDR-IDLIST            TO REKY-IDLIST                            
074300     MOVE WS-CURRENT-DATE       TO REKY-TIREGDAT                          
074400     MOVE WS-CURRENT-TIME       TO REKY-TIKLOCK                           
074500                                                                          
074600     MOVE ZERO TO REKY-IDLOPNR                                            
074700     PERFORM DB2-SELECT-TZ4REKY-TAB                                       
074800     PERFORM UNTIL LINES-MISSING                                          
074900       ADD 1 TO REKY-IDLOPNR                                              
075000       PERFORM DB2-SELECT-TZ4REKY-TAB                                     
075100     END-PERFORM                                                          
075200*    -- SAVE ORIGINAL IDLOPNR (THIS VALUE IS USED ONLY FOR                
075300*    -- THE FIRST RULE IN THE SET)                                        
075400     MOVE REKY-IDLOPNR          TO W-SAVE-IDLOPNR                         
075500     .                                                                    
075600     EJECT                                                                
075700 F-RECEIVE-DISTRIBUTE-SAVE-DATA SECTION.                                  
075800                                                                          
075900*    - DATA IS SAVED, BUT NOT DISTRIBUTED, IF RULE IS MISSING             
076000                                                                          
076100     PERFORM S03-RECEIVE-MESSAGE-DATA                                     
076200     PERFORM UNTIL RECV-KDRC > ZERO                                       
076300       IF RULE-FOUND                                                      
076400       AND WZ11OUTX-OPEN-OK                                               
076500         PERFORM FA-SEND-DISTRIBUTE-DATA                                  
076600       END-IF                                                             
076700       PERFORM FB-HANDLE-RESTART-TABLES                                   
076800       PERFORM S03-RECEIVE-MESSAGE-DATA                                   
076900     END-PERFORM                                                          
077000                                                                          
077100     IF WZ11OUTX-OPEN-OK                                                  
077200       PERFORM FC-CLOSE-DISTRIB-CHANNELS                                  
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600 FA-SEND-DISTRIBUTE-DATA SECTION.                                         
077700                                                                          
077800     MOVE 'PUT'                     TO WS-KDFUNC                          
077900     MOVE 1 TO IX                                                         
078000     PERFORM UNTIL IX > IX-SAVE-NBR-OF-RULES OR WS-KDRC > 0               
078100       MOVE IX                        TO WS-IDCALL                        
078200       MOVE WS-RECV-KVDLEN            TO WS-TEOUTDATA-L                   
078300       MOVE IN-DATA(1:WS-RECV-KVDLEN) TO WS-TEOUTDATA                     
078400       PERFORM S04-DISTRIBUTE                                             
078500       ADD 1 TO IX                                                        
078600     END-PERFORM                                                          
078700                                                                          
078800*    -- REMEMBER IF A PUT ERROR OCCURRED                                  
078900     IF WS-KDRC > 0                                                       
079000       MOVE NOO TO WZ11OUTX-PUT-SW                                        
079100     END-IF                                                               
079200     .                                                                    
079300     EJECT                                                                
079400 FB-HANDLE-RESTART-TABLES SECTION.                                        
079500                                                                          
079600     IF RULE-FOUND                                                        
079700       MOVE DIRU-KVDAGAR-RESEND         TO WS-KVDAGAR-RESEND              
079800     ELSE                                                                 
079900       MOVE WS-DEFAULT-RESTARTABLE-DAYS TO WS-KVDAGAR-RESEND              
080000     END-IF                                                               
080100                                                                          
080200     IF WS-KVDAGAR-RESEND > ZERO                                          
080300       MOVE 1 TO IX                                                       
080400       PERFORM UNTIL IX > IX-SAVE-NBR-OF-RULES                            
080500         IF FIRST-LOOP                                                    
080600           PERFORM FBA-INS-RESTART-KEYS                                   
080700         END-IF                                                           
080800         PERFORM FBB-INS-RESTART-DATA                                     
080900         ADD 1 TO IX                                                      
081000       END-PERFORM                                                        
081100       MOVE NOO TO FIRST-LOOP-SW                                          
081200     END-IF                                                               
081300     .                                                                    
081400     EJECT                                                                
081500 FBA-INS-RESTART-KEYS SECTION.                                            
081600                                                                          
081700*    -- MOST KEYS TO REKY ARE SET IN CCA- SECTION.                        
081800*    -- THEY ARE SET HERE TOO, IN CASE THE RULE WAS MISSING               
081900*    -- AND CCA SECTION WAS SKIPPED.                                      
082000     MOVE HDR-IDOUTTYPE         TO REKY-IDOUTTYPE                         
082100     MOVE HDR-IDOUTREC          TO REKY-IDOUTREC                          
082200     MOVE HDR-IDLIST            TO REKY-IDLIST                            
082300     MOVE WS-CURRENT-DATE       TO REKY-TIREGDAT                          
082400     MOVE WS-CURRENT-TIME       TO REKY-TIKLOCK                           
082500*    -- USE SAVED IDLOPNR FROM SECTION CCA TO GENERATE THE CORRECT        
082600*    -- NUMBER FOR THIS RULE. RULE 1 USE IDLOPNR 0-99,                    
082700*    -- RULE 2 USE 100-199 ETC                                            
082800     IF REKY-TIREGDAT < 151012                                            
082900       COMPUTE REKY-IDLOPNR = (IX - 1) * 100 + W-SAVE-IDLOPNR             
083000     ELSE                                                                 
083100*    -- USE SAVED IDLOPNR FROM SECTION CCA TO GENERATE THE CORRECT        
083200*    -- NUMBER FOR THIS RULE. RULE 1 USE IDLOPNR 0-49,                    
083300*    -- RULE 2 USE 50-99 ETC                                              
083400       COMPUTE REKY-IDLOPNR = (IX - 1) * 50 + W-SAVE-IDLOPNR              
083500     END-IF                                                               
083600                                                                          
083700     MOVE TAB-KDOUTMETH(IX)     TO REKY-KDOUTMETH                         
083800     MOVE TAB-IDOUTDEST(IX)     TO REKY-IDOUTDEST                         
083900     MOVE TAB-KVCOPIES(IX)      TO REKY-KVCOPIES                          
084000     MOVE TAB-FLCARRCNTL(IX)    TO REKY-FLCARRCNTL                        
084100     IF REKY-FLCARRCNTL = SPACE                                           
084200       MOVE NOO                 TO REKY-FLCARRCNTL                        
084300     END-IF                                                               
084400     MOVE TAB-FLACIF(IX)        TO REKY-FLACIF                            
084500     IF REKY-FLACIF = SPACE                                               
084600       MOVE NOO                 TO REKY-FLACIF                            
084700     END-IF                                                               
084800     MOVE TAB-IDPFDEF(IX)       TO REKY-IDPFDEF                           
084900     MOVE TAB-IDFORMSNM(IX)     TO REKY-IDFORMSNM                         
085000     MOVE TAB-TEVCOMST(IX)      TO REKY-TEVCOMST                          
085100     MOVE TAB-IDVCINIT(IX)      TO REKY-IDVCINIT                          
085200     MOVE WS-FLRULEMISS         TO REKY-FLRULEMISS                        
085300     MOVE WS-IDMAIL-SENDER      TO REKY-IDMAIL-SENDER                     
085400     MOVE ZERO                  TO REKY-KVANTEX-PRINTAD                   
085500     IF RULE-FOUND                                                        
085600     AND TAB-KDOUTMETH(IX) NOT = WS-SAVE AND NOT = WS-GET-IT              
085700     AND TAB-KDOUTMETH(IX) NOT = WS-WEB                                   
085800       MOVE 1                   TO REKY-KVANTEX-PRINTAD                   
085900     END-IF                                                               
086000                                                                          
086100     MOVE WS-CURRENT-DATE-X6 TO DAYS-TIDATE1                              
086200     MOVE 'YYMMDD' TO DAYS-KDDATFMT1                                      
086300     MOVE WS-KVDAGAR-RESEND TO DAYS-KVDAYS                                
086400     MOVE 'WEEKDAYS' TO DAYS-IDCALEND                                     
086500     MOVE SPACE TO DAYS-TIDATE2                                           
086600                   WS-TIDATE2                                             
086700     MOVE 'YYMMDD' TO DAYS-KDDATFMT2                                      
086800     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
086900     IF DAYS-KDRC = ZERO                                                  
087000       MOVE DAYS-TIDATE2(1:6) TO WS-TIDATE2                               
087100       MOVE WS-TIDATE2 TO REKY-TIAAMMDD-RENS                              
087200     END-IF                                                               
087300                                                                          
087400     PERFORM DB2-INSERT-TZ4REKY-TAB                                       
087500     .                                                                    
087600     EJECT                                                                
087700 FBB-INS-RESTART-DATA SECTION.                                            
087800                                                                          
087900     MOVE HDR-IDOUTTYPE   TO REDA-IDOUTTYPE                               
088000     MOVE HDR-IDOUTREC    TO REDA-IDOUTREC                                
088100     MOVE HDR-IDLIST      TO REDA-IDLIST                                  
088200     MOVE WS-CURRENT-DATE TO REDA-TIREGDAT                                
088300     MOVE WS-CURRENT-TIME TO REDA-TIKLOCK                                 
088400     MOVE W-SAVE-IDLOPNR  TO REDA-IDLOPNR                                 
088500     ADD 1 TO TAB-KVPOST(IX)                                              
088600     MOVE TAB-KVPOST(IX)  TO REDA-KVPOST                                  
088700                                                                          
088800     PERFORM DB2-SELECT-TZ4REDA-TAB                                       
088900     IF LINES-MISSING                                                     
089000       MOVE WS-RECV-KVDLEN            TO REDA-TEOUTDATA-L                 
089100       MOVE IN-DATA(1:WS-RECV-KVDLEN) TO REDA-TEOUTDATA-D                 
089200       PERFORM DB2-INSERT-TZ4REDA-TAB                                     
089300     END-IF                                                               
089400     .                                                                    
089500     EJECT                                                                
089600 FC-CLOSE-DISTRIB-CHANNELS  SECTION.                                      
089700                                                                          
089800     MOVE 'CLOSE' TO WS-KDFUNC                                            
089900     MOVE 1 TO IX                                                         
090000     PERFORM UNTIL IX > IX-SAVE-NBR-OF-RULES                              
090100       MOVE IX TO WS-IDCALL                                               
090200       PERFORM S04-DISTRIBUTE                                             
090300       ADD 1 TO IX                                                        
090400     END-PERFORM                                                          
090500     .                                                                    
090600     EJECT                                                                
090700 G-HANDLE-ERROR SECTION.                                                  
090800                                                                          
090900     IF RECV-ADDISPABS-RETURN NOT = SPACE  AND NOT = LOW-VALUE            
091000*      -- A RETURN ADRESS WAS SPECIFIED BY THE SENDER.                    
091100*      -- USE THIS ADDRESS TO RETURN AN ERROR MESSAGE                     
091200       PERFORM GA-SEND-ERROR-RESPONSE                                     
091300     ELSE                                                                 
091400       IF HDR-IDOUTTYPE NOT = WS-DP-ERROR                                 
091500*        -- NORMAL NON-D&P DISTRIBUTION AND NO RETURN-ADDRESS.            
091600*        -- SEARCH IN RULE-TABLE FOR AN ERROR-RULE WITH                   
091700*        -- OUTPUT TYPE "DP-ERROR" AND RECEIVER = ORIGINAL                
091800*        -- OUTPUT TYPE AND AND RECEIVER CONCATENATED.                    
091900*        -- EXAMPLE: IF ORIGINAL KEY WAS ("AAA", "BBB") -                 
092000*        -- SEARCH FOR ("DP-ERROR, "AAA+BBB") IF SUCH A                   
092100*        -- RULE EXISTS, SEND ERROR MESSAGE TO THIS PROGRAM IN            
092200*        -- THE NORMAL WAY, AND LET THE RULE HANDLE THE                   
092300*        -- CORRECT DISTRIBUTION.                                         
092400         MOVE HDR-IDOUTTYPE TO WS-IDOUTTYPE-SAVE                          
092500         MOVE HDR-IDOUTREC  TO WS-IDOUTREC-SAVE                           
092600                                                                          
092700         MOVE WS-DP-ERROR   TO HDR-IDOUTTYPE                              
092800         MOVE SPACE         TO ERR-HDR-IDOUTREC                           
092900         STRING WS-IDOUTTYPE-SAVE '+' HDR-IDOUTREC                        
093000           DELIMITED BY SPACE INTO ERR-HDR-IDOUTREC                       
093100         MOVE ERR-HDR-IDOUTREC TO HDR-IDOUTREC                            
093200                                                                          
093300         PERFORM DB2-SELECT-TZ4DIRU-TAB                                   
093400         IF LINES-FOUND                                                   
093500*          -- USE D&P ITSELF TO SIGNAL ERROR                              
093600           PERFORM GB-SEND-DP-ERROR                                       
093700         ELSE                                                             
093800           MOVE 'DAP-ERROR'       TO HDR-IDOUTTYPE                        
093900           MOVE 'DAP-ERROR'       TO HDR-IDOUTREC                         
094000           PERFORM DB2-SELECT-TZ4DIRU-TAB                                 
094100           IF LINES-FOUND                                                 
094200*            -- USE D&P TO SEND MAIL TO SUPPORT MAILBOX                   
094300             PERFORM GB-SEND-DP-ERROR                                     
094400           ELSE                                                           
094500*            -- NO ERROR RULE FOUND, USE HARD-CODED SIGNALLING            
094600             MOVE WS-IDOUTTYPE-SAVE TO HDR-IDOUTTYPE                      
094700             MOVE WS-IDOUTREC-SAVE TO HDR-IDOUTREC                        
094800             PERFORM GC-SEND-HARDCODED-ERROR                              
094900           END-IF                                                         
095000         END-IF                                                           
095100       ELSE                                                               
095200*        -- ERROR IN D&P-ERROR SIGNALLING, USE HARD-CODED SIGNAL          
095300         PERFORM GC-SEND-HARDCODED-ERROR                                  
095400       END-IF                                                             
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800 GA-SEND-ERROR-RESPONSE SECTION.                                          
095900                                                                          
096000*    -- RETURN AN ERROR MESSAGE VIA THE SPECIFIED                         
096100*    -- RETURN ADDRESS.                                                   
096200                                                                          
096300     IF WZ04-SEND-IDCOM = ZERO                                            
096400       PERFORM S30-OPEN-ERROR-RESPONSE                                    
096500       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
096600     END-IF                                                               
096700                                                                          
096800     MOVE '001'           TO ERR-RESP-IDMSGVER                            
096900     MOVE SPACE           TO ERR-RESP-IDMSG-INFO                          
097000     MOVE HDR-IDOUTTYPE   TO ERR-HDR-IDOUTTYPE                            
097100     MOVE HDR-IDOUTREC    TO ERR-HDR-IDOUTREC                             
097200     MOVE HDR-IDLIST      TO ERR-HDR-IDLIST                               
097300     PERFORM S30-PUT-ERROR-RESPONSE                                       
097400                                                                          
097500     IF WZ04-SEND-IDCOM > ZERO                                            
097600       PERFORM S30-CLOSE-ERROR-RESPONSE                                   
097700       MOVE ZERO TO WZ04-SEND-IDCOM                                       
097800     END-IF                                                               
097900     .                                                                    
098000     EJECT                                                                
098100                                                                          
098200 GB-SEND-DP-ERROR SECTION.                                                
098300                                                                          
098400*    -- USE D&P ITSELF TO SEND AN ERROR MESSAGE VIA A SUITABLE            
098500*    -- "DP-ERROR" OR "DAP-ERROR" DISTRIBUTION RULE                       
098600                                                                          
098700                                                                          
098800     IF WZ04-SEND-IDCOM = ZERO                                            
098900       PERFORM S40-OPEN-DP-ERROR                                          
099000       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
099100     END-IF                                                               
099200                                                                          
099300     PERFORM S40-PUT-DP-ERROR-HDR                                         
099400                                                                          
099500     MOVE ' D&P DISTRIBUTION FAILED' TO REDA-TEOUTDATA-D                  
099600     PERFORM S40-PUT-DP-ERROR-DATA                                        
099700                                                                          
099800     CALL VIMSID  USING WS-VIMSID                                         
099900     MOVE SPACE                         TO REDA-TEOUTDATA-D               
100000     STRING ' DATE & TIME: '             DELIMITED BY SIZE                
100100          FUNCTION CURRENT-DATE (3:6)    DELIMITED BY SIZE                
100200          ' '                            DELIMITED BY SIZE                
100300          FUNCTION CURRENT-DATE (9:8)    DELIMITED BY SIZE                
100400          ' IMS-SYS: '                   DELIMITED BY SIZE                
100500          WS-VIMSID                      DELIMITED BY SIZE                
100600     INTO REDA-TEOUTDATA-D                                                
100700     PERFORM S40-PUT-DP-ERROR-DATA                                        
100800     MOVE SPACE                          TO REDA-TEOUTDATA-D              
100900                                                                          
101000     MOVE ' (SENT VIA DP-ERROR CHANNEL)' TO REDA-TEOUTDATA-D              
101100     PERFORM S40-PUT-DP-ERROR-DATA                                        
101200                                                                          
101300     MOVE SPACE TO REDA-TEOUTDATA-D                                       
101400     STRING ' TYPE: '  WS-IDOUTTYPE-SAVE                                  
101500       DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                            
101600     PERFORM S40-PUT-DP-ERROR-DATA                                        
101700                                                                          
101800     MOVE SPACE TO REDA-TEOUTDATA-D                                       
101900     STRING ' SUB-TYPE: ' WS-IDOUTREC-SAVE                                
102000       DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                            
102100     PERFORM S40-PUT-DP-ERROR-DATA                                        
102200                                                                          
102300     MOVE SPACE TO REDA-TEOUTDATA-D                                       
102400     STRING ' ID: ' HDR-IDLIST                                            
102500       DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                            
102600     PERFORM S40-PUT-DP-ERROR-DATA                                        
102700                                                                          
102800     MOVE SPACE TO REDA-TEOUTDATA-D                                       
102900     STRING ' ERROR: '  ERR-RESP-IDMSG-ERROR                              
103000            ' ' ERR-RESP-IDELMT-ERROR                                     
103100       DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                            
103200     PERFORM S40-PUT-DP-ERROR-DATA                                        
103300                                                                          
103400     IF WS-OUT-ERROR-TEXT NOT = SPACE                                     
103500       MOVE SPACE TO REDA-TEOUTDATA-D                                     
103600       STRING ' ' WS-OUT-ERROR-TEXT                                       
103700         DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                          
103800       PERFORM S40-PUT-DP-ERROR-DATA                                      
103900     END-IF                                                               
104000                                                                          
104100     IF WZ04-SEND-IDCOM > ZERO                                            
104200       PERFORM S40-CLOSE-DP-ERROR                                         
104300       MOVE ZERO TO WZ04-SEND-IDCOM                                       
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 GC-SEND-HARDCODED-ERROR SECTION.                                         
104800                                                                          
104900*    -- NO VALID ADDRESS FOR ERROR SIGNALLING SPECIFIED.                  
105000*    -- HANDLE SIGNALLING INTERNALLY WITHIN D&P                           
105100*    -- TEMP SOLUTION: USE THE OUT0 PCB AND "CHANNEL" 9                   
105200*    -- TO PRINT TO THE TEST PRINTER QSE10316                             
105300                                                                          
105400     MOVE 'OPEN'                TO OUTP-KDFUNC                            
105500     MOVE 9                     TO OUTP-IDCALL                            
105600     MOVE 'OUT0'                TO OUTP-IDPCB                             
105700     MOVE 'QSE10316'            TO OUTP-IDOUTDEST                         
105800     MOVE SPACE                 TO OUTP-KVCOPIES                          
105900     MOVE SPACE                 TO OUTP-IDPFDEF                           
106000     MOVE SPACE                 TO OUTP-IDFORMSNM                         
106100     MOVE NOO                   TO OUTP-FLCARRCNTL                        
106200     MOVE NOO                   TO OUTP-FLACIF                            
106300     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
106400     IF OUTP-KDRC > 0                                                     
106500        STRING 'GC OUTP ERROR. '                                          
106600                WS-OUT-ERROR-TEXT DELIMITED BY SIZE                       
106700           INTO ERROR-TEXT                                                
106800        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
106900     END-IF                                                               
107000                                                                          
107100     CALL VIMSID  USING WS-VIMSID                                         
107200                                                                          
107300     MOVE 'PUT'                      TO OUTP-KDFUNC                       
107400     MOVE 60                         TO OUTP-TEOUTDATA-L                  
107500                                                                          
107600     MOVE ' D&P DISTRIBUTION FAILED'    TO OUTP-TEOUTDATA                 
107700     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
107800                                                                          
107900     MOVE SPACE                         TO OUTP-TEOUTDATA                 
108000     STRING ' DATE & TIME: '             DELIMITED BY SIZE                
108100          FUNCTION CURRENT-DATE (3:6)    DELIMITED BY SIZE                
108200          ' '                            DELIMITED BY SIZE                
108300          FUNCTION CURRENT-DATE (9:8)    DELIMITED BY SIZE                
108400          ' IMS-SYS: '                   DELIMITED BY SIZE                
108500          WS-VIMSID                      DELIMITED BY SIZE                
108600     INTO OUTP-TEOUTDATA                                                  
108700     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
108800     IF OUTP-KDRC > 0                                                     
108900        STRING 'GC OUTP ERROR. '                                          
109000                WS-OUT-ERROR-TEXT DELIMITED BY SIZE                       
109100           INTO ERROR-TEXT                                                
109200        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
109300     END-IF                                                               
109400                                                                          
109500     MOVE SPACE TO OUTP-TEOUTDATA                                         
109600     MOVE ' (SENT VIA DEFAULT CHANNEL)' TO OUTP-TEOUTDATA                 
109700     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
109800                                                                          
109900     MOVE SPACE TO OUTP-TEOUTDATA                                         
110000     STRING ' TYPE: '  HDR-IDOUTTYPE                                      
110100     DELIMITED BY SIZE INTO OUTP-TEOUTDATA                                
110200     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
110300                                                                          
110400     MOVE SPACE TO OUTP-TEOUTDATA                                         
110500     STRING ' SUB-TYPE: '  HDR-IDOUTREC                                   
110600     DELIMITED BY SIZE INTO OUTP-TEOUTDATA                                
110700     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
110800                                                                          
110900     MOVE SPACE TO OUTP-TEOUTDATA                                         
111000     STRING ' ID: '  HDR-IDLIST                                           
111100       DELIMITED BY SIZE INTO OUTP-TEOUTDATA                              
111200     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
111300                                                                          
111400     MOVE SPACE TO OUTP-TEOUTDATA                                         
111500     STRING ' ERROR: '  ERR-RESP-IDMSG-ERROR                              
111600            ' ' ERR-RESP-IDELMT-ERROR                                     
111700       DELIMITED BY SIZE INTO OUTP-TEOUTDATA                              
111800     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
111900                                                                          
112000     IF WS-OUT-ERROR-TEXT NOT = SPACE                                     
112100       MOVE SPACE TO OUTP-TEOUTDATA                                       
112200       STRING ' ' WS-OUT-ERROR-TEXT                                       
112300         DELIMITED BY SIZE INTO OUTP-TEOUTDATA                            
112400       CALL WZ11OUTP USING OUTP-WZ11OUT                                   
112500     END-IF                                                               
112600                                                                          
112700     MOVE 'CLOSE'                   TO OUTP-KDFUNC                        
112800     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
112900     .                                                                    
113000     EJECT                                                                
113100*    --- DISPATCHER SECTIONS                                              
113200 S03-RECEIVE-OPEN SECTION.                                                
113300                                                                          
113400     MOVE 'OPEN'                  TO RECV-KDFUNC                          
113500     MOVE WS-MY-OWN-ADRESS        TO RECV-ADDISPABS                       
113600     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-OPEN-AREA                 
113700                                                                          
113800     MOVE RECV-KDTRANS            TO WS-KDTRANS                           
113900                                                                          
114000     IF RECV-KDRC > 0                                                     
114100       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
114200       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
114300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
114400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
114500     END-IF                                                               
114600     .                                                                    
114700     SKIP3                                                                
114800 S03-RECEIVE-MESSAGE-HEADER SECTION.                                      
114900                                                                          
115000*    -- TRUNCATION IS ALLOWED TO BE ABLE TO HANDLE TRAILING               
115100*    -- BLANKS IN HEADER RECORD. SHORTER RECORDS ARE ALSO                 
115200*    -- ACCEPTED AND MISSING FIELDS ARE ASSUMED TO BE = SPACE.            
115300                                                                          
115400     MOVE 'GET'                      TO RECV-KDFUNC                       
115500     MOVE SPACE                      TO RECV-AREA                         
115600     MOVE LENGTH OF RECV-AREA        TO RECV-KVDLEN                       
115700     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN RECV-AREA          
115800                                                                          
115900     IF RECV-KDRC > 1 AND NOT = 21                                        
116000       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
116100       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
116200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
116300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
116400     ELSE                                                                 
116500       MOVE RECV-KVDLEN TO WS-RECV-KVDLEN                                 
116600     END-IF                                                               
116700     .                                                                    
116800     SKIP3                                                                
116900 S03-RECEIVE-MESSAGE-DATA SECTION.                                        
117000                                                                          
117100     MOVE 'GET'                      TO RECV-KDFUNC                       
117200     MOVE LENGTH OF IN-TEOUTDATA     TO RECV-KVDLEN                       
117300     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN                    
117400                                           IN-TEOUTDATA                   
117500                                                                          
117600     IF RECV-KDRC > 1                                                     
117700       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
117800       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
117900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
118000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
118100     ELSE                                                                 
118200       MOVE RECV-KVDLEN TO WS-RECV-KVDLEN                                 
118300     END-IF                                                               
118400     .                                                                    
118500     SKIP3                                                                
118600 S03-RECEIVE-CLOSE SECTION.                                               
118700                                                                          
118800     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
118900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
119000                                                                          
119100     IF RECV-KDRC > 0                                                     
119200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
119300       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
119400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
119500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
119600     END-IF                                                               
119700     .                                                                    
119800                                                                          
119900     EJECT                                                                
120000 S04-DISTRIBUTE SECTION.                                                  
120100                                                                          
120200     EVALUATE TAB-KDOUTMETH(IX)                                           
120300       WHEN WS-PRINT                                                      
120400         MOVE WS-IDCALL        TO OUTP-IDCALL                             
120500         MOVE WS-KDFUNC        TO OUTP-KDFUNC                             
120600         IF WS-KDFUNC = 'OPEN'                                            
120700*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
120800*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
120900           MOVE REKY-IDOUTTYPE      TO OUTP-IDOUTTYPE                     
121000           MOVE REKY-IDOUTREC       TO OUTP-IDOUTREC                      
121100           MOVE REKY-IDLIST         TO OUTP-IDLIST                        
121200           MOVE REKY-TIREGDAT       TO OUTP-TIREGDAT                      
121300           MOVE REKY-TIKLOCK        TO OUTP-TIKLOCK                       
121400                                                                          
121500           MOVE TAB-IDOUTDEST(IX)   TO OUTP-IDOUTDEST                     
121600           MOVE TAB-KVCOPIES(IX)    TO OUTP-KVCOPIES                      
121700           MOVE TAB-IDPFDEF(IX)     TO OUTP-IDPFDEF                       
121800           MOVE TAB-IDFORMSNM(IX)   TO OUTP-IDFORMSNM                     
121900           MOVE TAB-FLCARRCNTL(IX)  TO OUTP-FLCARRCNTL                    
122000           MOVE TAB-FLACIF(IX)      TO OUTP-FLACIF                        
122100*          -- OLD RULES MAY LACK FLACIF, AND PAGEDEF/FORMDEF              
122200*          -- PLUS NO CARRIAGE CONTROL CHARACTERS USED TO INDICATE        
122300*          -- ACIF SHOULD BE USED (EXAMPLE: BILL-IT INVOICES)             
122400           IF TAB-FLACIF(IX) = SPACE                                      
122500             MOVE NOO               TO OUTP-FLACIF                        
122600           END-IF                                                         
122700           IF TAB-FLCARRCNTL(IX) = NOO                                    
122800           AND TAB-IDPFDEF(IX) NOT = SPACE                                
122900             MOVE YES               TO OUTP-FLACIF                        
123000           END-IF                                                         
123100           MOVE SPACE               TO OUTP-IDPCB                         
123200         END-IF                                                           
123300         IF WS-KDFUNC = 'PUT'                                             
123400           MOVE WS-TEOUTDATA-L   TO OUTP-TEOUTDATA-L                      
123500           MOVE WS-TEOUTDATA     TO OUTP-TEOUTDATA                        
123600         END-IF                                                           
123700         CALL WZ11OUTP USING OUTP-WZ11OUT                                 
123800         MOVE ERR-PRINT-FAILED TO WS-ERROR-TYPE                           
123900         MOVE OUTP-KDRC        TO WS-KDRC                                 
124000         MOVE OUTP-TEOUTDATA   TO WS-TEOUTDATA                            
124100                                                                          
124200       WHEN WS-FAX                                                        
124300         MOVE WS-IDCALL        TO OUTF-IDCALL                             
124400         MOVE WS-KDFUNC        TO OUTF-KDFUNC                             
124500         IF WS-KDFUNC = 'OPEN'                                            
124600*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
124700*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
124800           MOVE REKY-IDOUTTYPE      TO OUTF-IDOUTTYPE                     
124900           MOVE REKY-IDOUTREC       TO OUTF-IDOUTREC                      
125000           MOVE REKY-IDLIST         TO OUTF-IDLIST                        
125100           MOVE REKY-TIREGDAT       TO OUTF-TIREGDAT                      
125200           MOVE REKY-TIKLOCK        TO OUTF-TIKLOCK                       
125300                                                                          
125400           MOVE TAB-IDOUTDEST(IX)   TO OUTF-IDOUTDEST                     
125500           MOVE TAB-IDPFDEF(IX)     TO OUTF-IDPFDEF                       
125600           MOVE TAB-FLCARRCNTL(IX)  TO OUTF-FLCARRCNTL                    
125700           MOVE WS-IDMAIL-SENDER    TO OUTF-IDMAIL-SENDER                 
125800           MOVE WS-TEFAX-1          TO OUTF-TEFAX(1)                      
125900           MOVE WS-TEFAX-2          TO OUTF-TEFAX(2)                      
126000           MOVE WS-TEFAX-3          TO OUTF-TEFAX(3)                      
126100           MOVE WS-TEFAX-4          TO OUTF-TEFAX(4)                      
126200           MOVE WS-TEFAX-5          TO OUTF-TEFAX(5)                      
126300         END-IF                                                           
126400         IF WS-KDFUNC = 'PUT'                                             
126500           MOVE WS-TEOUTDATA-L   TO OUTF-TEOUTDATA-L                      
126600           MOVE WS-TEOUTDATA     TO OUTF-TEOUTDATA                        
126700         END-IF                                                           
126800                                                                          
126900         CALL WZ11OUTF USING OUTF-WZ11OUT                                 
127000         MOVE ERR-FAX-FAILED   TO WS-ERROR-TYPE                           
127100         MOVE OUTF-KDRC        TO WS-KDRC                                 
127200         MOVE OUTF-TEOUTDATA   TO WS-TEOUTDATA                            
127300                                                                          
127400       WHEN WS-MAIL                                                       
127500         MOVE WS-IDCALL        TO OUTM-IDCALL                             
127600         MOVE WS-KDFUNC        TO OUTM-KDFUNC                             
127700         IF WS-KDFUNC = 'OPEN'                                            
127800*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
127900*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
128000           MOVE REKY-IDOUTTYPE      TO OUTM-IDOUTTYPE                     
128100           MOVE REKY-IDOUTREC       TO OUTM-IDOUTREC                      
128200           MOVE REKY-IDLIST         TO OUTM-IDLIST                        
128300           MOVE REKY-TIREGDAT       TO OUTM-TIREGDAT                      
128400           MOVE REKY-TIKLOCK        TO OUTM-TIKLOCK                       
128500                                                                          
128600           MOVE TAB-IDOUTDEST(IX)   TO OUTM-IDOUTDEST                     
128700           MOVE TAB-IDPFDEF(IX)     TO OUTM-IDPFDEF                       
128800           MOVE TAB-FLCARRCNTL(IX)  TO OUTM-FLCARRCNTL                    
128900           MOVE WS-IDMAIL-SENDER    TO OUTM-IDMAIL-SENDER                 
129000*          -- USE THE FIRST TEFAX LINE AS TITLE, AND                      
129100*          -- SHIFT THE OTHERS UP ONE STEP.                               
129200           IF WS-TEFAX-1 NOT = SPACE                                      
129300             MOVE WS-TEFAX-1        TO OUTM-IDMAILTTL                     
129400           ELSE                                                           
129500             MOVE SPACE             TO OUTM-IDMAILTTL                     
129600             STRING 'D&P Mail ('                                          
129700                    HDR-IDOUTTYPE                                         
129800                    ')'                                                   
129900                    DELIMITED BY SIZE INTO OUTM-IDMAILTTL                 
130000           END-IF                                                         
130100           MOVE WS-TEFAX-2          TO OUTM-TEFAX(1)                      
130200           MOVE WS-TEFAX-3          TO OUTM-TEFAX(2)                      
130300           MOVE WS-TEFAX-4          TO OUTM-TEFAX(3)                      
130400           MOVE WS-TEFAX-5          TO OUTM-TEFAX(4)                      
130500           MOVE SPACE               TO OUTM-TEFAX(5)                      
130600                                                                          
130700         END-IF                                                           
130800         IF WS-KDFUNC = 'PUT'                                             
130900           MOVE WS-TEOUTDATA-L   TO OUTM-TEOUTDATA-L                      
131000           MOVE WS-TEOUTDATA     TO OUTM-TEOUTDATA                        
131100         END-IF                                                           
131200                                                                          
131300         CALL WZ11OUTM USING OUTM-WZ11OUT                                 
131400                                                                          
131500         MOVE ERR-MAIL-FAILED  TO WS-ERROR-TYPE                           
131600         MOVE OUTM-KDRC        TO WS-KDRC                                 
131700         MOVE OUTM-TEOUTDATA   TO WS-TEOUTDATA                            
131800                                                                          
131900       WHEN WS-EDI                                                        
132000*        -- NOTE: THE VCOM SUBROUTINE IS USED FOR EDI TOO                 
132100         MOVE WS-IDCALL        TO OUTV-IDCALL                             
132200         MOVE WS-KDFUNC        TO OUTV-KDFUNC                             
132300         IF WS-KDFUNC = 'OPEN'                                            
132400           MOVE TAB-IDOUTDEST(IX)   TO OUTV-IDOUTDEST                     
132500           MOVE TAB-TEVCOMST(IX)    TO OUTV-TEVCOMST                      
132600           MOVE TAB-IDVCINIT(IX)    TO OUTV-IDVCINIT                      
132700         END-IF                                                           
132800         IF WS-KDFUNC = 'PUT'                                             
132900           MOVE WS-TEOUTDATA-L   TO OUTV-TEOUTDATA-L                      
133000           MOVE WS-TEOUTDATA     TO OUTV-TEOUTDATA                        
133100         END-IF                                                           
133200         CALL WZ11OUTV USING OUTV-WZ11OUT                                 
133300         MOVE ERR-EDI-FAILED   TO WS-ERROR-TYPE                           
133400         MOVE OUTV-KDRC        TO WS-KDRC                                 
133500         MOVE OUTV-TEOUTDATA   TO WS-TEOUTDATA                            
133600                                                                          
133700       WHEN WS-VCOM                                                       
133800         MOVE WS-IDCALL        TO OUTV-IDCALL                             
133900         MOVE WS-KDFUNC        TO OUTV-KDFUNC                             
134000         IF WS-KDFUNC = 'OPEN'                                            
134100           MOVE TAB-IDOUTDEST(IX)   TO OUTV-IDOUTDEST                     
134200           MOVE TAB-TEVCOMST(IX)    TO OUTV-TEVCOMST                      
134300           MOVE TAB-IDVCINIT(IX)    TO OUTV-IDVCINIT                      
134400         END-IF                                                           
134500         IF WS-KDFUNC = 'PUT'                                             
134600           MOVE WS-TEOUTDATA-L   TO OUTV-TEOUTDATA-L                      
134700           MOVE WS-TEOUTDATA     TO OUTV-TEOUTDATA                        
134800         END-IF                                                           
134900         CALL WZ11OUTV USING OUTV-WZ11OUT                                 
135000         MOVE ERR-VCOM-FAILED  TO WS-ERROR-TYPE                           
135100         MOVE OUTV-KDRC        TO WS-KDRC                                 
135200         MOVE OUTV-TEOUTDATA   TO WS-TEOUTDATA                            
135300                                                                          
135400       WHEN WS-ONDEMAND                                                   
135500         MOVE WS-IDCALL        TO OUTO-IDCALL                             
135600         MOVE WS-KDFUNC        TO OUTO-KDFUNC                             
135700         IF WS-KDFUNC = 'OPEN'                                            
135800*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
135900*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
136000           MOVE REKY-IDOUTTYPE      TO OUTO-IDOUTTYPE                     
136100           MOVE REKY-IDOUTREC       TO OUTO-IDOUTREC                      
136200           MOVE REKY-IDLIST         TO OUTO-IDLIST                        
136300           MOVE REKY-TIREGDAT       TO OUTO-TIREGDAT                      
136400           MOVE REKY-TIKLOCK        TO OUTO-TIKLOCK                       
136500                                                                          
136600           MOVE TAB-IDOUTDEST(IX)   TO OUTO-IDOUTDEST                     
136700           MOVE TAB-IDPFDEF(IX)     TO OUTO-IDPFDEF                       
136800           MOVE TAB-IDFORMSNM(IX)   TO OUTO-IDFORMSNM                     
136900           MOVE TAB-FLCARRCNTL(IX)  TO OUTO-FLCARRCNTL                    
137000           MOVE TAB-FLACIF(IX)      TO OUTO-FLACIF                        
137100*          -- OLD RULES MAY LACK FLACIF, AND PAGEDEF/FORMDEF              
137200*          -- PLUS NO CARRIAGE CONTROL CHARACTERS USED TO INDICATE        
137300*          -- ACIF SHOULD BE USED (EXAMPLE: BILL-IT INVOICES)             
137400           IF TAB-FLACIF(IX) = SPACE                                      
137500             MOVE NOO               TO OUTO-FLACIF                        
137600           END-IF                                                         
137700           IF TAB-FLCARRCNTL(IX) = NOO                                    
137800           AND TAB-IDPFDEF(IX) NOT = SPACE                                
137900             MOVE YES               TO OUTO-FLACIF                        
138000           END-IF                                                         
138100           MOVE SPACE            TO OUTO-IDPCB                            
138200         END-IF                                                           
138300         IF WS-KDFUNC = 'PUT'                                             
138400           MOVE WS-TEOUTDATA-L   TO OUTO-TEOUTDATA-L                      
138500           MOVE WS-TEOUTDATA     TO OUTO-TEOUTDATA                        
138600         END-IF                                                           
138700         CALL WZ11OUTO USING OUTO-WZ11OUT                                 
138800         MOVE ERR-ONDEMAND-FAILED TO WS-ERROR-TYPE                        
138900         MOVE OUTO-KDRC        TO WS-KDRC                                 
139000         MOVE OUTO-TEOUTDATA   TO WS-TEOUTDATA                            
139100                                                                          
139200       WHEN WS-GET-IT                                                     
139300         MOVE WS-IDCALL        TO OUTA-IDCALL                             
139400         MOVE WS-KDFUNC        TO OUTA-KDFUNC                             
139500         IF WS-KDFUNC = 'OPEN'                                            
139600*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
139700*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
139800           MOVE REKY-IDOUTTYPE      TO OUTA-IDOUTTYPE                     
139900           MOVE REKY-IDOUTREC       TO OUTA-IDOUTREC                      
140000           MOVE REKY-IDLIST         TO OUTA-IDLIST                        
140100           MOVE REKY-TIREGDAT       TO OUTA-TIREGDAT                      
140200           MOVE REKY-TIKLOCK        TO OUTA-TIKLOCK                       
140300                                                                          
140400           MOVE TAB-IDOUTDEST(IX)   TO OUTA-IDOUTDEST                     
140500           MOVE TAB-IDPFDEF(IX)     TO OUTA-IDPFDEF                       
140600           MOVE TAB-FLCARRCNTL(IX)  TO OUTA-FLCARRCNTL                    
140700           MOVE WS-CURRENT-DATE     TO OUTA-TIREGDAT                      
140800           MOVE WS-CURRENT-TIME2    TO OUTA-TIREGTID                      
140900         END-IF                                                           
141000         IF WS-KDFUNC = 'PUT'                                             
141100           MOVE WS-TEOUTDATA-L   TO OUTA-TEOUTDATA-L                      
141200           MOVE WS-TEOUTDATA     TO OUTA-TEOUTDATA                        
141300         END-IF                                                           
141400         CALL WZ11OUTA USING OUTA-WZ11OUT                                 
141500         MOVE ERR-GET-IT-FAILED  TO WS-ERROR-TYPE                         
141600         MOVE OUTA-KDRC          TO WS-KDRC                               
141700         MOVE OUTA-TEOUTDATA     TO WS-TEOUTDATA                          
141800                                                                          
141900       WHEN WS-WEB                                                        
142000         MOVE WS-IDCALL        TO OUTW-IDCALL                             
142100         MOVE WS-KDFUNC        TO OUTW-KDFUNC                             
142200         IF WS-KDFUNC = 'OPEN'                                            
142300           MOVE REKY-IDOUTTYPE    TO OUTW-IDOUTTYPE                       
142400           MOVE REKY-IDOUTREC     TO OUTW-IDOUTREC                        
142500           MOVE REKY-IDLIST       TO OUTW-IDLIST                          
142600           MOVE REKY-TIREGDAT     TO OUTW-TIREGDAT                        
142700           MOVE REKY-TIKLOCK      TO OUTW-TIKLOCK                         
142800*          -- USE SAVED IDLOPNR TO GENERATE THE CORRECT IDLOPNR           
142900*          -- FOR THIS RULE. RULE 1 USE IDLOPNR 0-99,                     
143000*          -- RULE 2 USE 100-199 ETC                                      
143100           IF REKY-TIREGDAT < 151012                                      
143200            COMPUTE OUTW-IDLOPNR = (IX - 1) * 100 + W-SAVE-IDLOPNR        
143300           ELSE                                                           
143400            COMPUTE OUTW-IDLOPNR = (IX - 1) * 50 + W-SAVE-IDLOPNR         
143500           END-IF                                                         
143600         END-IF                                                           
143700         IF WS-KDFUNC = 'PUT'                                             
143800           MOVE WS-TEOUTDATA-L    TO OUTW-TEOUTDATA-L                     
143900           MOVE WS-TEOUTDATA      TO OUTW-TEOUTDATA                       
144000         END-IF                                                           
144100*        WZ0420Y transaction is used in a special way.                    
144200*        Normally, when a DAP rule has dist method WEB,                   
144300*        the main program doesn't send response back to IMS.              
144400*        Instead WZ0420 sends response using WZ11OUTW.                    
144500*        However, when the main program is triggered from API             
144600*        and the main program is sending back the response and is         
144700*        calling WZ0420 to only save the data in DAP tables, then         
144800*        WZ0420Y transaction is used and in that case, we do not          
144900*        call WZ11OUTW.                                                   
145000         IF WS-KDTRANS = 'WZ0420Y '                                       
145100           CONTINUE                                                       
145200         ELSE                                                             
145300           CALL WZ11OUTW       USING OUTW-WZ11OUT                         
145400         END-IF                                                           
145500         MOVE ERR-WEB-FAILED      TO WS-ERROR-TYPE                        
145600         MOVE OUTW-KDRC           TO WS-KDRC                              
145700         MOVE OUTW-TEOUTDATA      TO WS-TEOUTDATA                         
145800                                                                          
145900     END-EVALUATE                                                         
146000                                                                          
146100     IF WS-KDRC > ZERO                                                    
146200       MOVE WS-ERROR-TYPE         TO ERR-RESP-IDMSG-ERROR                 
146300       MOVE WS-TEOUTDATA          TO WS-OUT-ERROR-TEXT                    
146400     END-IF                                                               
146500     .                                                                    
146600     EJECT                                                                
146700 S30-OPEN-ERROR-RESPONSE SECTION.                                         
146800                                                                          
146900     MOVE 'OPEN'                  TO SEND-KDFUNC                          
147000     MOVE RECV-ADDISPABS-RETURN   TO SEND-ADDISPABS                       
147100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
147200                                                                          
147300     IF SEND-KDRC > 0                                                     
147400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
147500       STRING 'S30 WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                  
147600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
147700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
147800     END-IF                                                               
147900     .                                                                    
148000     SKIP3                                                                
148100 S30-PUT-ERROR-RESPONSE SECTION.                                          
148200                                                                          
148300     MOVE 'PUT'                           TO SEND-KDFUNC                  
148400     MOVE  WZ04-SEND-IDCOM                TO SEND-IDCOM                   
148500     MOVE LENGTH OF ERROR-RESP-AREA       TO SEND-KVDLEN                  
148600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
148700                         SEND-KVDLEN                                      
148800                         ERROR-RESP-AREA                                  
148900     IF SEND-KDRC > ZERO                                                  
149000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
149100       STRING 'S30 WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
149200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
149300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
149400     END-IF                                                               
149500     .                                                                    
149600     SKIP3                                                                
149700 S30-CLOSE-ERROR-RESPONSE SECTION.                                        
149800                                                                          
149900     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
150000     MOVE  WZ04-SEND-IDCOM                TO SEND-IDCOM                   
150100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
150200     .                                                                    
150300                                                                          
150400     EJECT                                                                
150500 S40-OPEN-DP-ERROR SECTION.                                               
150600                                                                          
150700     MOVE WS-MY-OWN-ADRESS TO SEND-ADDISPABS                              
150800     MOVE 'OPEN'           TO SEND-KDFUNC                                 
150900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
151000                         SEND-OPEN-AREA                                   
151100     IF SEND-KDRC > ZERO                                                  
151200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
151300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
151400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
151500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
151600     END-IF                                                               
151700     .                                                                    
151800                                                                          
151900 S40-PUT-DP-ERROR-HDR SECTION.                                            
152000                                                                          
152100     MOVE SPACE                     TO ERR2-REQU-AREA                     
152200     MOVE 001                       TO ERR2-REQU-IDMSGVER                 
152300     MOVE IDPGM                     TO ERR2-REQU-IDUSER                   
152400     MOVE HDR-IDOUTTYPE             TO ERR2-HDR-IDOUTTYPE                 
152500     MOVE HDR-IDOUTREC              TO ERR2-HDR-IDOUTREC                  
152600                                                                          
152700     MOVE 'PUT'                     TO SEND-KDFUNC                        
152800     MOVE  WZ04-SEND-IDCOM          TO SEND-IDCOM                         
152900     MOVE LENGTH OF ERR2-REQU-AREA  TO SEND-KVDLEN                        
153000     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
153100                         ERR2-REQU-AREA                                   
153200     IF SEND-KDRC > ZERO                                                  
153300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
153400       STRING 'S40 WZ01SEND PUT HDR ERROR RC=' KDRC-DISPLAY               
153500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
153600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
153700     END-IF                                                               
153800     .                                                                    
153900     SKIP3                                                                
154000 S40-PUT-DP-ERROR-DATA SECTION.                                           
154100                                                                          
154200     MOVE 'PUT'            TO SEND-KDFUNC                                 
154300     MOVE  WZ04-SEND-IDCOM TO SEND-IDCOM                                  
154400     MOVE 120              TO SEND-KVDLEN                                 
154500     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
154600                                      REDA-TEOUTDATA-D                    
154700     IF SEND-KDRC > ZERO                                                  
154800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
154900       STRING 'S40 WZ01SEND PUT DATA ERROR RC=' KDRC-DISPLAY              
155000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
155100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
155200     END-IF                                                               
155300     .                                                                    
155400     SKIP3                                                                
155500 S40-CLOSE-DP-ERROR SECTION.                                              
155600                                                                          
155700     MOVE 'CLOSE'          TO SEND-KDFUNC                                 
155800     MOVE  WZ04-SEND-IDCOM TO SEND-IDCOM                                  
155900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
156000     .                                                                    
156100     EJECT                                                                
156200                                                                          
156300* -- DB2-SECTIONS                                                         
156400 DB2-SELECT-TZ4DIRU-TAB  SECTION.                                         
156500                                                                          
156600     MOVE '000100811'  TO GOOD-SQLCODECODES                               
156700                                                                          
156800     EXEC SQL                                                             
156900         SELECT  KDOUTMETH_1                                              
157000               , KDOUTMETH_2                                              
157100               , KDOUTMETH_3                                              
157200               , KDOUTMETH_4                                              
157300               , KDOUTMETH_5                                              
157400               , KDOUTMETH_6                                              
157500               , KDOUTMETH_7                                              
157600               , KDOUTMETH_8                                              
157700               , KDOUTMETH_9                                              
157800               , KDOUTMETH_10                                             
157900               , KDOUTMETH_11                                             
158000               , KDOUTMETH_12                                             
158100               , KDOUTMETH_13                                             
158200               , KDOUTMETH_14                                             
158300               , KDOUTMETH_15                                             
158400               , IDOUTDEST_1                                              
158500               , IDOUTDEST_2                                              
158600               , IDOUTDEST_3                                              
158700               , IDOUTDEST_4                                              
158800               , IDOUTDEST_5                                              
158900               , IDOUTDEST_6                                              
159000               , IDOUTDEST_7                                              
159100               , IDOUTDEST_8                                              
159200               , IDOUTDEST_9                                              
159300               , IDOUTDEST_10                                             
159400               , IDOUTDEST_11                                             
159500               , IDOUTDEST_12                                             
159600               , IDOUTDEST_13                                             
159700               , IDOUTDEST_14                                             
159800               , IDOUTDEST_15                                             
159900               , KVCOPIES_1                                               
160000               , KVCOPIES_2                                               
160100               , KVCOPIES_3                                               
160200               , KVCOPIES_4                                               
160300               , KVCOPIES_5                                               
160400               , KVCOPIES_6                                               
160500               , KVCOPIES_7                                               
160600               , KVCOPIES_8                                               
160700               , KVCOPIES_9                                               
160800               , KVCOPIES_10                                              
160900               , KVCOPIES_11                                              
161000               , KVCOPIES_12                                              
161100               , KVCOPIES_13                                              
161200               , KVCOPIES_14                                              
161300               , KVCOPIES_15                                              
161400               , FLCARRCNTL_1                                             
161500               , FLCARRCNTL_2                                             
161600               , FLCARRCNTL_3                                             
161700               , FLCARRCNTL_4                                             
161800               , FLCARRCNTL_5                                             
161900               , FLCARRCNTL_6                                             
162000               , FLCARRCNTL_7                                             
162100               , FLCARRCNTL_8                                             
162200               , FLCARRCNTL_9                                             
162300               , FLCARRCNTL_10                                            
162400               , FLCARRCNTL_11                                            
162500               , FLCARRCNTL_12                                            
162600               , FLCARRCNTL_13                                            
162700               , FLCARRCNTL_14                                            
162800               , FLCARRCNTL_15                                            
162900               , IDPFDEF_1                                                
163000               , IDPFDEF_2                                                
163100               , IDPFDEF_3                                                
163200               , IDPFDEF_4                                                
163300               , IDPFDEF_5                                                
163400               , IDPFDEF_6                                                
163500               , IDPFDEF_7                                                
163600               , IDPFDEF_8                                                
163700               , IDPFDEF_9                                                
163800               , IDPFDEF_10                                               
163900               , IDPFDEF_11                                               
164000               , IDPFDEF_12                                               
164100               , IDPFDEF_13                                               
164200               , IDPFDEF_14                                               
164300               , IDPFDEF_15                                               
164400               , IDFORMSNM_1                                              
164500               , IDFORMSNM_2                                              
164600               , IDFORMSNM_3                                              
164700               , IDFORMSNM_4                                              
164800               , IDFORMSNM_5                                              
164900               , IDFORMSNM_6                                              
165000               , IDFORMSNM_7                                              
165100               , IDFORMSNM_8                                              
165200               , IDFORMSNM_9                                              
165300               , IDFORMSNM_10                                             
165400               , IDFORMSNM_11                                             
165500               , IDFORMSNM_12                                             
165600               , IDFORMSNM_13                                             
165700               , IDFORMSNM_14                                             
165800               , IDFORMSNM_15                                             
165900               , TEVCOMST_1                                               
166000               , TEVCOMST_2                                               
166100               , TEVCOMST_3                                               
166200               , TEVCOMST_4                                               
166300               , TEVCOMST_5                                               
166400               , TEVCOMST_6                                               
166500               , TEVCOMST_7                                               
166600               , TEVCOMST_8                                               
166700               , TEVCOMST_9                                               
166800               , TEVCOMST_10                                              
166900               , TEVCOMST_11                                              
167000               , TEVCOMST_12                                              
167100               , TEVCOMST_13                                              
167200               , TEVCOMST_14                                              
167300               , TEVCOMST_15                                              
167400               , IDVCINIT_1                                               
167500               , IDVCINIT_2                                               
167600               , IDVCINIT_3                                               
167700               , IDVCINIT_4                                               
167800               , IDVCINIT_5                                               
167900               , IDVCINIT_6                                               
168000               , IDVCINIT_7                                               
168100               , IDVCINIT_8                                               
168200               , IDVCINIT_9                                               
168300               , IDVCINIT_10                                              
168400               , IDVCINIT_11                                              
168500               , IDVCINIT_12                                              
168600               , IDVCINIT_13                                              
168700               , IDVCINIT_14                                              
168800               , IDVCINIT_15                                              
168900               , TEFAX_1                                                  
169000               , TEFAX_2                                                  
169100               , TEFAX_3                                                  
169200               , TEFAX_4                                                  
169300               , TEFAX_5                                                  
169400               , KVDAGAR_RESEND                                           
169500               , IDMAIL_SENDER                                            
169600               , FLACIF_1                                                 
169700               , FLACIF_2                                                 
169800               , FLACIF_3                                                 
169900               , FLACIF_4                                                 
170000               , FLACIF_5                                                 
170100               , FLACIF_6                                                 
170200               , FLACIF_7                                                 
170300               , FLACIF_8                                                 
170400               , FLACIF_9                                                 
170500               , FLACIF_10                                                
170600               , FLACIF_11                                                
170700               , FLACIF_12                                                
170800               , FLACIF_13                                                
170900               , FLACIF_14                                                
171000               , FLACIF_15                                                
171100               , IDOUTTYPE                                                
171200               , IDOUTREC_FROM                                            
171300               , IDOUTREC_TO                                              
171400                                                                          
171500         INTO   :DIRU-KDOUTMETH-1                                         
171600              , :DIRU-KDOUTMETH-2                                         
171700              , :DIRU-KDOUTMETH-3                                         
171800              , :DIRU-KDOUTMETH-4                                         
171900              , :DIRU-KDOUTMETH-5                                         
172000              , :DIRU-KDOUTMETH-6                                         
172100              , :DIRU-KDOUTMETH-7                                         
172200              , :DIRU-KDOUTMETH-8                                         
172300              , :DIRU-KDOUTMETH-9                                         
172400              , :DIRU-KDOUTMETH-10                                        
172500              , :DIRU-KDOUTMETH-11                                        
172600              , :DIRU-KDOUTMETH-12                                        
172700              , :DIRU-KDOUTMETH-13                                        
172800              , :DIRU-KDOUTMETH-14                                        
172900              , :DIRU-KDOUTMETH-15                                        
173000              , :DIRU-IDOUTDEST-1                                         
173100              , :DIRU-IDOUTDEST-2                                         
173200              , :DIRU-IDOUTDEST-3                                         
173300              , :DIRU-IDOUTDEST-4                                         
173400              , :DIRU-IDOUTDEST-5                                         
173500              , :DIRU-IDOUTDEST-6                                         
173600              , :DIRU-IDOUTDEST-7                                         
173700              , :DIRU-IDOUTDEST-8                                         
173800              , :DIRU-IDOUTDEST-9                                         
173900              , :DIRU-IDOUTDEST-10                                        
174000              , :DIRU-IDOUTDEST-11                                        
174100              , :DIRU-IDOUTDEST-12                                        
174200              , :DIRU-IDOUTDEST-13                                        
174300              , :DIRU-IDOUTDEST-14                                        
174400              , :DIRU-IDOUTDEST-15                                        
174500              , :DIRU-KVCOPIES-1                                          
174600              , :DIRU-KVCOPIES-2                                          
174700              , :DIRU-KVCOPIES-3                                          
174800              , :DIRU-KVCOPIES-4                                          
174900              , :DIRU-KVCOPIES-5                                          
175000              , :DIRU-KVCOPIES-6                                          
175100              , :DIRU-KVCOPIES-7                                          
175200              , :DIRU-KVCOPIES-8                                          
175300              , :DIRU-KVCOPIES-9                                          
175400              , :DIRU-KVCOPIES-10                                         
175500              , :DIRU-KVCOPIES-11                                         
175600              , :DIRU-KVCOPIES-12                                         
175700              , :DIRU-KVCOPIES-13                                         
175800              , :DIRU-KVCOPIES-14                                         
175900              , :DIRU-KVCOPIES-15                                         
176000              , :DIRU-FLCARRCNTL-1                                        
176100              , :DIRU-FLCARRCNTL-2                                        
176200              , :DIRU-FLCARRCNTL-3                                        
176300              , :DIRU-FLCARRCNTL-4                                        
176400              , :DIRU-FLCARRCNTL-5                                        
176500              , :DIRU-FLCARRCNTL-6                                        
176600              , :DIRU-FLCARRCNTL-7                                        
176700              , :DIRU-FLCARRCNTL-8                                        
176800              , :DIRU-FLCARRCNTL-9                                        
176900              , :DIRU-FLCARRCNTL-10                                       
177000              , :DIRU-FLCARRCNTL-11                                       
177100              , :DIRU-FLCARRCNTL-12                                       
177200              , :DIRU-FLCARRCNTL-13                                       
177300              , :DIRU-FLCARRCNTL-14                                       
177400              , :DIRU-FLCARRCNTL-15                                       
177500              , :DIRU-IDPFDEF-1                                           
177600              , :DIRU-IDPFDEF-2                                           
177700              , :DIRU-IDPFDEF-3                                           
177800              , :DIRU-IDPFDEF-4                                           
177900              , :DIRU-IDPFDEF-5                                           
178000              , :DIRU-IDPFDEF-6                                           
178100              , :DIRU-IDPFDEF-7                                           
178200              , :DIRU-IDPFDEF-8                                           
178300              , :DIRU-IDPFDEF-9                                           
178400              , :DIRU-IDPFDEF-10                                          
178500              , :DIRU-IDPFDEF-11                                          
178600              , :DIRU-IDPFDEF-12                                          
178700              , :DIRU-IDPFDEF-13                                          
178800              , :DIRU-IDPFDEF-14                                          
178900              , :DIRU-IDPFDEF-15                                          
179000              , :DIRU-IDFORMSNM-1                                         
179100              , :DIRU-IDFORMSNM-2                                         
179200              , :DIRU-IDFORMSNM-3                                         
179300              , :DIRU-IDFORMSNM-4                                         
179400              , :DIRU-IDFORMSNM-5                                         
179500              , :DIRU-IDFORMSNM-6                                         
179600              , :DIRU-IDFORMSNM-7                                         
179700              , :DIRU-IDFORMSNM-8                                         
179800              , :DIRU-IDFORMSNM-9                                         
179900              , :DIRU-IDFORMSNM-10                                        
180000              , :DIRU-IDFORMSNM-11                                        
180100              , :DIRU-IDFORMSNM-12                                        
180200              , :DIRU-IDFORMSNM-13                                        
180300              , :DIRU-IDFORMSNM-14                                        
180400              , :DIRU-IDFORMSNM-15                                        
180500              , :DIRU-TEVCOMST-1                                          
180600              , :DIRU-TEVCOMST-2                                          
180700              , :DIRU-TEVCOMST-3                                          
180800              , :DIRU-TEVCOMST-4                                          
180900              , :DIRU-TEVCOMST-5                                          
181000              , :DIRU-TEVCOMST-6                                          
181100              , :DIRU-TEVCOMST-7                                          
181200              , :DIRU-TEVCOMST-8                                          
181300              , :DIRU-TEVCOMST-9                                          
181400              , :DIRU-TEVCOMST-10                                         
181500              , :DIRU-TEVCOMST-11                                         
181600              , :DIRU-TEVCOMST-12                                         
181700              , :DIRU-TEVCOMST-13                                         
181800              , :DIRU-TEVCOMST-14                                         
181900              , :DIRU-TEVCOMST-15                                         
182000              , :DIRU-IDVCINIT-1                                          
182100              , :DIRU-IDVCINIT-2                                          
182200              , :DIRU-IDVCINIT-3                                          
182300              , :DIRU-IDVCINIT-4                                          
182400              , :DIRU-IDVCINIT-5                                          
182500              , :DIRU-IDVCINIT-6                                          
182600              , :DIRU-IDVCINIT-7                                          
182700              , :DIRU-IDVCINIT-8                                          
182800              , :DIRU-IDVCINIT-9                                          
182900              , :DIRU-IDVCINIT-10                                         
183000              , :DIRU-IDVCINIT-11                                         
183100              , :DIRU-IDVCINIT-12                                         
183200              , :DIRU-IDVCINIT-13                                         
183300              , :DIRU-IDVCINIT-14                                         
183400              , :DIRU-IDVCINIT-15                                         
183500              , :DIRU-TEFAX-1                                             
183600              , :DIRU-TEFAX-2                                             
183700              , :DIRU-TEFAX-3                                             
183800              , :DIRU-TEFAX-4                                             
183900              , :DIRU-TEFAX-5                                             
184000              , :DIRU-KVDAGAR-RESEND                                      
184100              , :DIRU-IDMAIL-SENDER                                       
184200              , :DIRU-FLACIF-1                                            
184300              , :DIRU-FLACIF-2                                            
184400              , :DIRU-FLACIF-3                                            
184500              , :DIRU-FLACIF-4                                            
184600              , :DIRU-FLACIF-5                                            
184700              , :DIRU-FLACIF-6                                            
184800              , :DIRU-FLACIF-7                                            
184900              , :DIRU-FLACIF-8                                            
185000              , :DIRU-FLACIF-9                                            
185100              , :DIRU-FLACIF-10                                           
185200              , :DIRU-FLACIF-11                                           
185300              , :DIRU-FLACIF-12                                           
185400              , :DIRU-FLACIF-13                                           
185500              , :DIRU-FLACIF-14                                           
185600              , :DIRU-FLACIF-15                                           
185700              , :DIRU-IDOUTTYPE                                           
185800              , :DIRU-IDOUTREC-FROM                                       
185900              , :DIRU-IDOUTREC-TO                                         
186000                                                                          
186100         FROM    TZ4DIRU                                                  
186200                                                                          
186300         WHERE   IDOUTTYPE      = :HDR-IDOUTTYPE                          
186400         AND     IDOUTREC_FROM  = :HDR-IDOUTREC                           
186500         AND     IDOUTREC_TO    = :HDR-IDOUTREC                           
186600         OR      IDOUTTYPE      = :HDR-IDOUTTYPE                          
186700         AND     IDOUTREC_FROM <= :HDR-IDOUTREC                           
186800         AND     IDOUTREC_TO   >= :HDR-IDOUTREC                           
186900         AND NOT EXISTS                                                   
187000               (SELECT  *                                                 
187100                FROM    TZ4DIRU                                           
187200                WHERE   IDOUTTYPE     = :HDR-IDOUTTYPE                    
187300                AND     IDOUTREC_FROM = :HDR-IDOUTREC                     
187400                AND     IDOUTREC_TO   = :HDR-IDOUTREC)                    
187500     END-EXEC                                                             
187600                                                                          
187700     MOVE SQLCODE TO SQLCODE-WS                                           
187800     PERFORM DB2-STATUS-CHECK                                             
187900                                                                          
188000     IF SQLCODE-WS = 811                                                  
188100       MOVE YES TO WS-FLRULEMULT                                          
188200     ELSE                                                                 
188300       MOVE NOO TO WS-FLRULEMULT                                          
188400     END-IF                                                               
188500     .                                                                    
188600     EJECT                                                                
188700 DB2-UPDATE-TZ4DIRU-TIANVDAT  SECTION.                                    
188800                                                                          
188900     MOVE 000     TO GOOD-SQLCODECODES                                    
189000     EXEC SQL                                                             
189100         UPDATE TZ4DIRU                                                   
189200           SET   TIANVDAT      = :WS-CURRENT-DATE                         
189300                                                                          
189400         WHERE   IDOUTTYPE     = :DIRU-IDOUTTYPE                          
189500          AND    IDOUTREC_FROM = :DIRU-IDOUTREC-FROM                      
189600          AND    IDOUTREC_TO   = :DIRU-IDOUTREC-TO                        
189700     END-EXEC                                                             
189800                                                                          
189900     MOVE SQLCODE TO SQLCODE-WS                                           
190000     PERFORM DB2-STATUS-CHECK                                             
190100     .                                                                    
190200                                                                          
190300 DB2-SELECT-TZ4REKY-TAB  SECTION.                                         
190400                                                                          
190500     MOVE 000100  TO GOOD-SQLCODECODES                                    
190600                                                                          
190700     EXEC SQL                                                             
190800         SELECT  IDLIST                                                   
190900                                                                          
191000         INTO   :WS-TZ4REKY-DUMMY                                         
191100                                                                          
191200         FROM    TZ4REKY                                                  
191300                                                                          
191400         WHERE   IDOUTTYPE = :REKY-IDOUTTYPE                              
191500         AND     IDOUTREC  = :REKY-IDOUTREC                               
191600         AND     IDLIST    = :REKY-IDLIST                                 
191700         AND     TIREGDAT  = :REKY-TIREGDAT                               
191800         AND     TIKLOCK   = :REKY-TIKLOCK                                
191900         AND     IDLOPNR   = :REKY-IDLOPNR                                
192000     END-EXEC                                                             
192100                                                                          
192200     MOVE SQLCODE TO SQLCODE-WS                                           
192300     PERFORM DB2-STATUS-CHECK                                             
192400     .                                                                    
192500     EJECT                                                                
192600 DB2-INSERT-TZ4REKY-TAB  SECTION.                                         
192700     SKIP2                                                                
192800     MOVE 000   TO GOOD-SQLCODECODES                                      
192900     EXEC SQL                                                             
193000       INSERT INTO TZ4REKY                                                
193100         (IDOUTTYPE,IDOUTREC,IDLIST,TIREGDAT,TIKLOCK,IDLOPNR              
193200         ,KDOUTMETH,IDOUTDEST,TIAAMMDD_RENS,KVCOPIES,FLCARRCNTL           
193300         ,IDPFDEF,IDFORMSNM,TEVCOMST,IDVCINIT                             
193400         ,TEFAX_1,TEFAX_2,TEFAX_3,TEFAX_4,TEFAX_5                         
193500         ,IDMAIL_SENDER,KVANTEX_PRINTAD,FLRULEMISS,FLACIF)                
193600                                                                          
193700       VALUES                                                             
193800         (:REKY-IDOUTTYPE,:REKY-IDOUTREC,:REKY-IDLIST                     
193900         ,:REKY-TIREGDAT,:REKY-TIKLOCK,:REKY-IDLOPNR                      
194000         ,:REKY-KDOUTMETH,:REKY-IDOUTDEST,:REKY-TIAAMMDD-RENS             
194100         ,:REKY-KVCOPIES,:REKY-FLCARRCNTL,:REKY-IDPFDEF                   
194200         ,:REKY-IDFORMSNM,:REKY-TEVCOMST,:REKY-IDVCINIT                   
194300         ,:WS-TEFAX-1,:WS-TEFAX-2,:WS-TEFAX-3,:WS-TEFAX-4                 
194400         ,:WS-TEFAX-5,:REKY-IDMAIL-SENDER                                 
194500         ,:REKY-KVANTEX-PRINTAD,:REKY-FLRULEMISS,:REKY-FLACIF)            
194600     END-EXEC                                                             
194700                                                                          
194800     MOVE SQLCODE TO SQLCODE-WS                                           
194900     PERFORM DB2-STATUS-CHECK                                             
195000     .                                                                    
195100     EJECT                                                                
195200 DB2-SELECT-TZ4REDA-TAB  SECTION.                                         
195300                                                                          
195400     MOVE 000100  TO GOOD-SQLCODECODES                                    
195500                                                                          
195600     EXEC SQL                                                             
195700         SELECT  IDLIST                                                   
195800                                                                          
195900         INTO   :WS-TZ4REDA-DUMMY                                         
196000                                                                          
196100         FROM    TZ4REDA                                                  
196200                                                                          
196300         WHERE   IDOUTTYPE = :REDA-IDOUTTYPE                              
196400         AND     IDOUTREC  = :REDA-IDOUTREC                               
196500         AND     IDLIST    = :REDA-IDLIST                                 
196600         AND     TIREGDAT  = :REDA-TIREGDAT                               
196700         AND     TIKLOCK   = :REDA-TIKLOCK                                
196800         AND     IDLOPNR   = :REDA-IDLOPNR                                
196900         AND     KVPOST    = :REDA-KVPOST                                 
197000     END-EXEC                                                             
197100                                                                          
197200     MOVE SQLCODE TO SQLCODE-WS                                           
197300     PERFORM DB2-STATUS-CHECK                                             
197400     .                                                                    
197500     EJECT                                                                
197600 DB2-INSERT-TZ4REDA-TAB  SECTION.                                         
197700     SKIP2                                                                
197800     MOVE 000   TO GOOD-SQLCODECODES                                      
197900     EXEC SQL                                                             
198000       INSERT INTO TZ4REDA                                                
198100         (IDOUTTYPE,IDOUTREC,IDLIST,TIREGDAT,TIKLOCK                      
198200         ,IDLOPNR,KVPOST,TEOUTDATA)                                       
198300                                                                          
198400       VALUES                                                             
198500         (:REDA-IDOUTTYPE,:REDA-IDOUTREC,:REDA-IDLIST                     
198600         ,:REDA-TIREGDAT,:REDA-TIKLOCK,:REDA-IDLOPNR                      
198700         ,:REDA-KVPOST,:REDA-TEOUTDATA)                                   
198800     END-EXEC                                                             
198900                                                                          
199000     MOVE SQLCODE TO SQLCODE-WS                                           
199100     PERFORM DB2-STATUS-CHECK                                             
199200     .                                                                    
199300     EJECT                                                                
199400 DB2-STATUS-CHECK  SECTION.                                               
199500                                                                          
199600     SET SQLCODE-IX TO 1                                                  
199700     SEARCH GOOD-SQLCODE                                                  
199800       AT END                                                             
199900          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
200000          DELIMITED BY SIZE INTO ERROR-TEXT                               
200100          CALL ABEND USING RKOD-ABEND-DB2                                 
200200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
200300     END-SEARCH                                                           
200400     .                                                                    
