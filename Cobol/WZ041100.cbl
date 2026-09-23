000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ041100.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/09/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.DAP.DISTRRESTARTMAINTENANCE                             
001000*    FUNCTION:                                                            
001100*        SEARCH/RESTART DISTRIBUTION OF OUTPUT                            
001200*        DEPENDING ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)           
001300*        KDPGMACT = 'S' SEARCH                                            
001400*        KDPGMACT = 'R' RESTART                                           
001500*                                                                         
001600*        THE PROGRAM READS  TABLE TZ4DIRU                                 
001700*        THE PROGRAM UPDATE TABLE TZ4REKY                                 
001800*        THE PROGRAM READS  TABLE TZ4REDA                                 
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: WZ0411U                                             
002200*        REQUEST:     WZ0411I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    WZ0411O1                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'WZ041100'.            
004000 77  IDSYSTEM                    PIC X(4)    VALUE 'WZ04'.                
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600*    --- CONSTANT WORK FIELDS                                             
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  YES                         PIC X       VALUE 'Y'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000 77  WS-PRINT                    PIC X(4)    VALUE 'PRT '.                
005100 77  WS-FAX                      PIC X(4)    VALUE 'FAX '.                
005200 77  WS-VCOM                     PIC X(4)    VALUE 'VCOM'.                
005300 77  WS-EDI                      PIC X(4)    VALUE 'EDI '.                
005400 77  WS-MAIL                     PIC X(4)    VALUE 'MAIL'.                
005500 77  WS-ONDEMAND                 PIC X(4)    VALUE 'ONDE'.                
005600 77  WS-GET-IT                   PIC X(4)    VALUE 'GETI'.                
005700 77  WS-WEB                      PIC X(4)    VALUE 'WEB '.                
005800 77  WS-DATE-FORMAT              PIC X(6)    VALUE 'YYMMDD'.              
005900 77  WS-MAX-KVANTEX-PRINTAD      PIC S9(1)   VALUE +9 COMP-3.             
006000 77  WS-ADRESS                   PIC X(50)                                
006100                    VALUE 'CARPARTS.DAP.DISTRRESTARTMAINTENANCE'.         
006200 77  WS-ADRESS-SEND              PIC X(50)                                
006300                                    VALUE 'CARPARTS.DAP.DISTRDOC'.        
006400 77  KEYS-SW                     PIC X       VALUE SPACE.                 
006500     88  KEYS-OK                             VALUE 'Y'.                   
006600     88  KEYS-WRONG                          VALUE 'N'.                   
006700                                                                          
006800 77  MAIL-TYPE-SW                PIC X       VALUE SPACE.                 
006900     88  MAIL-TYPE-M                         VALUE 'M'.                   
007000     88  MAIL-TYPE-N                         VALUE 'N'.                   
007100                                                                          
007200 77  ACTION-CODE-SW              PIC X   VALUE SPACE.                     
007300     88  ACT-CODE-VALID                  VALUE 'S', 'R'.                  
007400     88  ACT-CODE-SEARCH                 VALUE 'S'.                       
007500     88  ACT-CODE-RESTART                VALUE 'R'.                       
007600                                                                          
007700 77  KDOUTMETH-SW                PIC X(4)    VALUE SPACE.                 
007800     88  KDOUTMETH-VALID             VALUE 'PRT ', 'EDI ', 'MAIL',        
007900                                           'VCOM', 'FAX ', 'ONDE',        
008000                                           'GETI', 'WEB '.                
008100 77  WS-VALID-IDPFDEF            PIC X(36)                                
008200                     VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.        
008300 77  WS-TEST-IDPFDEF             PIC X(36)                                
008400                     VALUE '                                    '.        
008500                                                                          
008600 77  WS-TEFAX-MAY-BE-SPECIFIED   PIC X(1)    VALUE 'N'.                   
008700 77  WS-IDMAIL-MAY-BE-SPECIFIED  PIC X(1)    VALUE 'N'.                   
008800                                                                          
008900*    --- WORK-FIELDS                                                      
009000 01  IX                          PIC 9       VALUE ZERO.                  
009100 01  WS-IDPFDEF                  PIC X(8)    VALUE SPACE.                 
009200 01  TZ4DIRU-COUNTER             PIC S9(7)   VALUE ZERO COMP-3.           
009300 01  WS-IDLIST-KEY               PIC X(10)   VALUE SPACES.                
009400 01  WS-TIREGDAT-KEY             PIC S9(7)   VALUE ZERO COMP-3.           
009500 01  WS-TIKLOCK-KEY              PIC S9(9)   VALUE ZERO COMP-3.           
009600 01  WS-REKY-IDLOPNR-KEY         PIC S9(3)   VALUE ZERO COMP-3.           
009700 01  WS-REDA-IDLOPNR-KEY         PIC S9(3)   VALUE ZERO COMP-3.           
009800 01  WS-ERROR-TYPE               PIC X(3)    VALUE SPACE.                 
009900 01  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
010000                                                                          
010100 01  WS-IDCALL                   PIC S9(9)   COMP VALUE +0.               
010200 01  WS-KDFUNC                   PIC X(10)   VALUE SPACE.                 
010300 01  WS-KDRC                     PIC S9(9)   COMP VALUE +0.               
010400 01  WS-TEOUTDATA-L              PIC S9(9)   COMP VALUE +0.               
010500 01  WS-TEOUTDATA                PIC X(3000) VALUE SPACE.                 
010600                                                                          
010700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
010800 01  GENERAL-SUBPROGRAMS.                                                 
010900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
011100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011200     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
011300     03  WZ20DATE                PIC X(8)    VALUE 'WZ20DATE'.            
011400     03  W009EMAD                PIC X(8)    VALUE 'W009EMAD'.            
011500     03  WZ20TEFA                PIC X(8)    VALUE 'WZ20TEFA'.            
011600     03  WZ11OUTP                PIC X(8)    VALUE 'WZ11OUTP'.            
011700     03  WZ11OUTF                PIC X(8)    VALUE 'WZ11OUTF'.            
011800     03  WZ11OUTM                PIC X(8)    VALUE 'WZ11OUTM'.            
011900     03  WZ11OUTV                PIC X(8)    VALUE 'WZ11OUTV'.            
012000     03  WZ11OUTO                PIC X(8)    VALUE 'WZ11OUTO'.            
012100     03  WZ11OUTA                PIC X(8)    VALUE 'WZ11OUTA'.            
012200     03  WZ11OUTW                PIC X(8)    VALUE 'WZ11OUTW'.            
012300     SKIP3                                                                
012400                                                                          
012500*    --- PARAMETERS TO ABEND                                              
012600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
013000                                                                          
013100 01  MESSAGE-CODES.                                                       
013200     03  ERROR-CODES.                                                     
013300         05  ERR-AUTHORIZATION-MISSING PIC X(3)    VALUE '00A'.           
013400         05  ERR-INVALID-KEY           PIC X(3)    VALUE '022'.           
013500         05  ERR-INVALID-FIELD         PIC X(3)    VALUE '023'.           
013600         05  ERR-MUST-BE-NUMERIC       PIC X(3)    VALUE '024'.           
013700         05  ERR-FIELD-NOT-FOUND       PIC X(3)    VALUE '025'.           
013800         05  ERR-MUST-BE-ENTERED       PIC X(3)    VALUE '026'.           
013900         05  ERR-NOT-CHANGEABLE        PIC X(3)    VALUE '031'.           
014000         05  ERR-SYSTEM-ERROR          PIC X(3)    VALUE '099'.           
014100         05  ERR-TEFAX-NOT-ALLOWED     PIC X(3)    VALUE '102'.           
014200         05  ERR-IDMAIL-NOT-ALLOWED    PIC X(3)    VALUE '103'.           
014300         05  ERR-DISTR-RULES-MISSING   PIC X(3)    VALUE '201'.           
014400         03  ERR-PRINT-FAILED          PIC X(3)    VALUE '202'.           
014500         03  ERR-FAX-FAILED            PIC X(3)    VALUE '203'.           
014600         03  ERR-MAIL-FAILED           PIC X(3)    VALUE '204'.           
014700         03  ERR-EDI-FAILED            PIC X(3)    VALUE '205'.           
014800         03  ERR-VCOM-FAILED           PIC X(3)    VALUE '206'.           
014900         03  ERR-ONDEMAND-FAILED       PIC X(3)    VALUE '207'.           
015000         03  ERR-GET-IT-FAILED         PIC X(3)    VALUE '208'.           
015100         03  ERR-WEB-FAILED            PIC X(3)    VALUE '209'.           
015200     03  INFO-CODES.                                                      
015300         05  INF-RESTARTED             PIC X(3)    VALUE '105'.           
015400     EJECT                                                                
015500                                                                          
015600*     --- PARAMETRAR TILL SUBPROGRAM                                      
015700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
015800     SKIP3                                                                
015900*01  -COPY WZ01SUB                                                        
016000     EJECT                                                                
016100                                                                          
016200 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
016300     SKIP3                                                                
016400*01  -COPY WZ01SEND                                                       
016500     EJECT                                                                
016600                                                                          
016700 01  FILLER                      PIC X(16)   VALUE 'DATE-CONTROL'.        
016800     SKIP3                                                                
016900*01  -COPY WZ20DATE                                                       
017000                                                                          
017100 01  FILLER                      PIC X(16)  VALUE 'W009EMAD-AREA'.        
017200     SKIP3                                                                
017300*01 -COPY W009EMAD                                                        
017400     EJECT                                                                
017500                                                                          
017600 01  FILLER                      PIC X(16)  VALUE 'WZ20TEFA-AREA'.        
017700     SKIP3                                                                
017800*01 -COPY WZ20TEFA                                                        
017900     EJECT                                                                
018000                                                                          
018100 01  FILLER                      PIC X(16)   VALUE 'SEC-AREA'.            
018200     SKIP3                                                                
018300*01 -COPY WSECAREA                                                        
018400     EJECT                                                                
018500                                                                          
018600 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTF-AREA'.        
018700     SKIP3                                                                
018800*01 -COPY WZ11OUTF                                                        
018900 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTM-AREA'.        
019000     SKIP3                                                                
019100*01 -COPY WZ11OUTM                                                        
019200 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTO-AREA'.        
019300     SKIP3                                                                
019400*01 -COPY WZ11OUTO                                                        
019500 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTP-AREA'.        
019600     SKIP3                                                                
019700*01 -COPY WZ11OUTP                                                        
019800 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTV-AREA'.        
019900     SKIP3                                                                
020000*01 -COPY WZ11OUTV                                                        
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTA-AREA'.        
020300     SKIP3                                                                
020400*01 -COPY WZ11OUTA                                                        
020500     EJECT                                                                
020600 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTW-AREA'.        
020700     SKIP3                                                                
020800*01 -COPY WZ11OUTW                                                        
020900     EJECT                                                                
021000                                                                          
021100*     --- PARAMETRAR TILL OLIKA AREOR                                     
021200 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
021300 01  HDR-AREA.                                                            
021400*    03  -COPY WZ01REQU   -PRE HDR-                                       
021500*    03  -COPY WZ04HDR    -PRE HDR-                                       
021600     EJECT                                                                
021700                                                                          
021800 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
021900     SKIP3                                                                
022000 01  REQU-AREA.                                                           
022100*    03  -COPY WZ01REQU                                                   
022200*    03  -COPY WZ0411I1                                                   
022300     EJECT                                                                
022400                                                                          
022500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
022600     SKIP3                                                                
022700 01  RESP-AREA.                                                           
022800*    03  -COPY WZ01RESP                                                   
022900*    03  -COPY WZ0411O1                                                   
023000     EJECT                                                                
023100                                                                          
023200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
023300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
023400                                                                          
023500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
023600 01  DB2-WS.                                                              
023700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
023800         88  LINES-FOUND                     VALUE 000.                   
023900         88  LINES-MISSING                   VALUE 100.                   
024000         88  RESOURCE-WRONG                  VALUE 904.                   
024100     03  GOOD-SQLCODECODES.                                               
024200         05  GOOD-SQLCODE OCCURS 5                                        
024300             INDEXED BY SQLCODE-IX PIC 9(3).                              
024400     EJECT                                                                
024500 01  FILLER                      PIC X(16)   VALUE 'TZ4DIRU-AREA'.        
024600*01  -COPY TZ4DIRU -PRE DIRU-                                             
024700                                                                          
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)   VALUE 'TZ4REKY-AREA'.        
025000*01  -COPY TZ4REKY -PRE REKY-                                             
025100                                                                          
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'TZ4REDA-AREA'.        
025400*01  -COPY TZ4REDA -PRE REDA-                                             
025500                                                                          
025600     EXEC SQL INCLUDE TZ4DIRU END-EXEC.                                   
025700     EXEC SQL INCLUDE TZ4REKY END-EXEC.                                   
025800     EXEC SQL INCLUDE TZ4REDA END-EXEC.                                   
025900     EJECT                                                                
026000 LINKAGE SECTION.                                                         
026100     EJECT                                                                
026200 PROCEDURE DIVISION.                                                      
026300 MAIN SECTION.                                                            
026400                                                                          
026500     PERFORM S80-FETCH-REQUEST-ARGUMENT                                   
026600     IF SUB-KDRC = 0                                                      
026700       PERFORM A-INIT                                                     
026800       PERFORM B-CHECK-KEYS                                               
026900       IF KEYS-OK                                                         
027000         PERFORM F-READ-SHOW-INFO                                         
027100       END-IF                                                             
027200       PERFORM S80-RETURN-RESPONSE                                        
027300     END-IF                                                               
027400                                                                          
027500     MOVE ZERO TO RETURN-CODE                                             
027600     GOBACK                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 A-INIT SECTION.                                                          
028000                                                                          
028100     INITIALIZE GOOD-SQLCODECODES                                         
028200     MOVE ALL '+' TO RESP-AREA                                            
028300     MOVE ZERO    TO RESP-TIREGDAT-KEY                                    
028400                     RESP-TIKLOCK-KEY                                     
028500                     RESP-IDLOPNR-KEY                                     
028600                     RESP-KVANTEX-PRINTAD                                 
028700     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
028800                     RESP-IDELMT-ERROR                                    
028900                     RESP-IDMSG-INFO                                      
029000     .                                                                    
029100     EJECT                                                                
029200*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
029300 B-CHECK-KEYS SECTION.                                                    
029400                                                                          
029500     MOVE YES TO KEYS-SW                                                  
029600     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
029700                                                                          
029800     IF REQU-IDMSGVER NUMERIC                                             
029900     AND ACT-CODE-VALID                                                   
030000     AND REQU-IDOUTTYPE-KEY > SPACE AND NOT = ALL '+'                     
030100     AND REQU-IDOUTREC-KEY >= SPACE                                       
030200     AND REQU-IDLIST-KEY >= SPACE                                         
030300     AND REQU-TIREGDAT-KEY > ZERO                                         
030400     AND REQU-TIKLOCK-KEY NUMERIC                                         
030500     AND REQU-IDLOPNR-KEY NUMERIC                                         
030600     AND REQU-IDUSER > SPACE AND NOT = ALL '+'                            
030700       CONTINUE                                                           
030800     ELSE                                                                 
030900       MOVE NOO TO KEYS-SW                                                
031000     END-IF                                                               
031100     IF REQU-IDOUTREC-KEY = ALL '+'                                       
031200       MOVE SPACE TO REQU-IDOUTREC-KEY                                    
031300     END-IF                                                               
031400     IF REQU-IDLIST-KEY = ALL '+'                                         
031500       MOVE SPACE TO REQU-IDLIST-KEY                                      
031600     END-IF                                                               
031700                                                                          
031800     MOVE FUNCTION TRIM (REQU-IDLIST-KEY)                                 
031900                               TO WS-IDLIST-KEY                           
032000                                                                          
032100     IF KEYS-WRONG                                                        
032200       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
032300                                                                          
032400       IF ACT-CODE-VALID                                                  
032500         CONTINUE                                                         
032600       ELSE                                                               
032700         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
032800         MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                       
032900       END-IF                                                             
033000                                                                          
033100       IF REQU-IDMSGVER NUMERIC                                           
033200         CONTINUE                                                         
033300       ELSE                                                               
033400         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
033500         MOVE 'IDMSGVER'       TO RESP-IDELMT-ERROR                       
033600       END-IF                                                             
033700                                                                          
033800       IF  REQU-IDUSER > SPACE                                            
033900       AND REQU-IDUSER NOT = ALL '+'                                      
034000         CONTINUE                                                         
034100       ELSE                                                               
034200         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
034300         MOVE 'IDUSER' TO RESP-IDELMT-ERROR                               
034400       END-IF                                                             
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034800*** - MOVE SEARCHING KEYS TO RESPONSE                                     
034900 F-READ-SHOW-INFO SECTION.                                                
035000                                                                          
035100     MOVE REQU-IDOUTTYPE-KEY TO RESP-IDOUTTYPE-KEY                        
035200     MOVE REQU-IDOUTREC-KEY  TO RESP-IDOUTREC-KEY                         
035300     MOVE REQU-IDLIST-KEY    TO RESP-IDLIST-KEY                           
035400     MOVE REQU-TIREGDAT-KEY  TO RESP-TIREGDAT-KEY                         
035500     MOVE REQU-TIKLOCK-KEY   TO RESP-TIKLOCK-KEY                          
035600     MOVE REQU-IDLOPNR-KEY   TO RESP-IDLOPNR-KEY                          
035700     MOVE REQU-TIREGDAT-KEY  TO WS-TIREGDAT-KEY                           
035800     MOVE REQU-TIKLOCK-KEY   TO WS-TIKLOCK-KEY                            
035900                                                                          
036000*    -- REKY AND REDA USE DIFFERENT VALUES OF IDLOPNR                     
036100*    -- REKY USE THE SPECIFIELD VALUE, WHICH MAY BE > 100                 
036200*    -- BUT REDA ALWAYS USE A VALUE < 100.                                
036300*    -- IF REKY-IDLOPNR IS 000, 100, 200 ETC, REDA-IDLOPNR = 000          
036400*    -- IF REKY-IDLOPNR IS 001, 101, 201 ETC, REDA-IDLOPNR = 001          
036500*    -- AND SO ON.                                                        
036600     MOVE REQU-IDLOPNR-KEY   TO WS-REKY-IDLOPNR-KEY                       
036700     IF WS-TIREGDAT-KEY < 151012                                          
036800       COMPUTE WS-REDA-IDLOPNR-KEY =                                      
036900             FUNCTION REM (WS-REKY-IDLOPNR-KEY, 100)                      
037000     ELSE                                                                 
037100       COMPUTE WS-REDA-IDLOPNR-KEY =                                      
037200             FUNCTION REM (WS-REKY-IDLOPNR-KEY, 50)                       
037300     END-IF                                                               
037400                                                                          
037500     PERFORM FA-READ-BASICDATA                                            
037600     .                                                                    
037700     EJECT                                                                
037800*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
037900 FA-READ-BASICDATA SECTION.                                               
038000                                                                          
038100     IF ACT-CODE-SEARCH                                                   
038200       PERFORM FAA-SEARCH-TZ4REKY                                         
038300     ELSE                                                                 
038400       IF ACT-CODE-RESTART                                                
038500         PERFORM S01-CHECK-SECURITY                                       
038600         IF RESP-IDMSG-ERROR = SPACE                                      
038700           PERFORM FAB-RESTART-DISTRIBUTION                               
038800           IF RESP-IDMSG-ERROR = SPACE                                    
038900             PERFORM FAC-UPDATE-TZ4REKY                                   
039000           END-IF                                                         
039100         END-IF                                                           
039200       END-IF                                                             
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600*** - SEARCH FOR RESTARTING KEYS                                          
039700 FAA-SEARCH-TZ4REKY SECTION.                                              
039800                                                                          
039900     PERFORM DB2-SELECT-TZ4REKY-TAB                                       
040000                                                                          
040100     IF LINES-FOUND                                                       
040200       PERFORM FAAA-MOVE-SEARCH-TO-RESPOND                                
040300     ELSE                                                                 
040400       MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                       
040500       MOVE 'KEY'               TO RESP-IDELMT-ERROR                      
040600       PERFORM S03-SCRATCH-RESPOND                                        
040700     END-IF                                                               
040800     .                                                                    
040900     SKIP3                                                                
041000 FAAA-MOVE-SEARCH-TO-RESPOND SECTION.                                     
041100                                                                          
041200     MOVE REKY-KDOUTMETH        TO RESP-KDOUTMETH                         
041300     MOVE REKY-IDOUTDEST        TO RESP-IDOUTDEST                         
041400     MOVE REKY-KVCOPIES         TO RESP-KVCOPIES                          
041500     MOVE REKY-FLCARRCNTL       TO RESP-FLCARRCNTL                        
041600     MOVE REKY-FLACIF           TO RESP-FLACIF                            
041700     MOVE REKY-IDPFDEF          TO RESP-IDPFDEF                           
041800     MOVE REKY-IDFORMSNM        TO RESP-IDFORMSNM                         
041900     MOVE REKY-TEVCOMST         TO RESP-TEVCOMST                          
042000     MOVE REKY-IDVCINIT         TO RESP-IDVCINIT                          
042100     MOVE REKY-IDMAIL-SENDER    TO RESP-IDMAIL-SENDER                     
042200     MOVE REKY-TIAAMMDD-RENS    TO RESP-TIAAMMDD-RENS                     
042300     MOVE REKY-TEFAX-1          TO RESP-TEFAX(1)                          
042400     MOVE REKY-TEFAX-2          TO RESP-TEFAX(2)                          
042500     MOVE REKY-TEFAX-3          TO RESP-TEFAX(3)                          
042600     MOVE REKY-TEFAX-4          TO RESP-TEFAX(4)                          
042700     MOVE REKY-TEFAX-5          TO RESP-TEFAX(5)                          
042800     MOVE REKY-KVANTEX-PRINTAD  TO RESP-KVANTEX-PRINTAD                   
042900     IF REKY-FLRULEMISS = JA                                              
043000       MOVE JA                  TO RESP-FLRULEUSE                         
043100     ELSE                                                                 
043200       MOVE NOO                 TO RESP-FLRULEUSE                         
043300     END-IF                                                               
043400     .                                                                    
043500     EJECT                                                                
043600*** - RESTART DISTRIBUTION                                                
043700 FAB-RESTART-DISTRIBUTION SECTION.                                        
043800                                                                          
043900     IF REQU-FLRULEUSE = JA                                               
044000       PERFORM DB2-COUNT-TZ4DIRU-TAB                                      
044100       IF TZ4DIRU-COUNTER > 0                                             
044200         PERFORM FABA-RESTART-FROM-RULES                                  
044300       ELSE                                                               
044400         MOVE ERR-DISTR-RULES-MISSING TO RESP-IDMSG-ERROR                 
044500         MOVE 'RULES MISSING'         TO RESP-IDELMT-ERROR                
044600       END-IF                                                             
044700     ELSE                                                                 
044800       IF REQU-FLRULEUSE = NOO                                            
044900         PERFORM S02-CHECK-REQU-DATA                                      
045000         IF RESP-IDMSG-ERROR = SPACE                                      
045100           PERFORM FABB-RESTART-FROM-SCREEN                               
045200         END-IF                                                           
045300       ELSE                                                               
045400         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
045500         MOVE 'FLRULEUSE'         TO RESP-IDELMT-ERROR                    
045600       END-IF                                                             
045700     END-IF                                                               
045800     .                                                                    
045900     SKIP3                                                                
046000 FABA-RESTART-FROM-RULES SECTION.                                         
046100                                                                          
046200     MOVE 1                  TO HDR-REQU-IDMSGVER                         
046300     MOVE 'R'                TO HDR-REQU-KDPGMACT                         
046400     MOVE IDPGM              TO HDR-REQU-IDUSER                           
046500                                                                          
046600     MOVE REQU-IDOUTTYPE-KEY TO HDR-HDR-IDOUTTYPE                         
046700     MOVE REQU-IDOUTREC-KEY  TO HDR-HDR-IDOUTREC                          
046800     MOVE REQU-IDLIST-KEY    TO HDR-HDR-IDLIST                            
046900                                                                          
047000     IF WZ04-SEND-IDCOM = ZERO                                            
047100       PERFORM S90-SEND-OPEN                                              
047200       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
047300     END-IF                                                               
047400                                                                          
047500     PERFORM S90-PUT-HEADER                                               
047600                                                                          
047700     PERFORM DB2-DCL-OPN-TZ4REDA-CRS                                      
047800     PERFORM DB2-FETCH-TZ4REDA-CRS                                        
047900     PERFORM UNTIL LINES-MISSING                                          
048000       PERFORM S90-PUT-DATA                                               
048100       PERFORM DB2-FETCH-TZ4REDA-CRS                                      
048200     END-PERFORM                                                          
048300     PERFORM DB2-CLOSE-TZ4REDA-CRS                                        
048400                                                                          
048500     IF WZ04-SEND-IDCOM > ZERO                                            
048600       PERFORM S90-SEND-CLOSE                                             
048700       MOVE ZERO TO WZ04-SEND-IDCOM                                       
048800     END-IF                                                               
048900     .                                                                    
049000     SKIP3                                                                
049100 FABB-RESTART-FROM-SCREEN SECTION.                                        
049200                                                                          
049300     MOVE 1                  TO WS-IDCALL                                 
049400     MOVE 'OPEN'             TO WS-KDFUNC                                 
049500     PERFORM S04-CALL-WZ11OUTX                                            
049600                                                                          
049700     IF RESP-IDMSG-ERROR = SPACE                                          
049800       PERFORM DB2-DCL-OPN-TZ4REDA-CRS                                    
049900       PERFORM DB2-FETCH-TZ4REDA-CRS                                      
050000       PERFORM UNTIL LINES-MISSING                                        
050100         MOVE 1                         TO WS-IDCALL                      
050200         MOVE 'PUT'                     TO WS-KDFUNC                      
050300         MOVE REDA-TEOUTDATA-L          TO WS-TEOUTDATA-L                 
050400         MOVE REDA-TEOUTDATA-D(1:REDA-TEOUTDATA-L)                        
050500                                        TO WS-TEOUTDATA                   
050600         PERFORM S04-CALL-WZ11OUTX                                        
050700         PERFORM DB2-FETCH-TZ4REDA-CRS                                    
050800       END-PERFORM                                                        
050900       PERFORM DB2-CLOSE-TZ4REDA-CRS                                      
051000     END-IF                                                               
051100                                                                          
051200     MOVE 1 TO       WS-IDCALL                                            
051300     MOVE 'CLOSE' TO WS-KDFUNC                                            
051400     PERFORM  S04-CALL-WZ11OUTX                                           
051500     .                                                                    
051600     EJECT                                                                
051700 FAC-UPDATE-TZ4REKY SECTION.                                              
051800                                                                          
051900     PERFORM DB2-SELECT-TZ4REKY-TAB                                       
052000                                                                          
052100     IF LINES-FOUND                                                       
052200       IF REKY-KVANTEX-PRINTAD < WS-MAX-KVANTEX-PRINTAD                   
052300         ADD 1 TO REKY-KVANTEX-PRINTAD                                    
052400         PERFORM DB2-UPDATE-TZ4REKY-TAB                                   
052500       END-IF                                                             
052600       MOVE INF-RESTARTED TO RESP-IDMSG-INFO                              
052700     END-IF                                                               
052800     .                                                                    
052900                                                                          
053000     EJECT                                                                
053100*    --- DISPATCHER SECTIONS                                              
053200 S80-FETCH-REQUEST-ARGUMENT SECTION.                                      
053300                                                                          
053400     MOVE 'GETARG'                   TO SUB-KDFUNC                        
053500     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
053600     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
053700                                                                          
053800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
053900                                                                          
054000     IF SUB-KDRC > 0                                                      
054100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
054200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
054300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
054500     END-IF                                                               
054600     .                                                                    
054700     SKIP3                                                                
054800 S80-RETURN-RESPONSE SECTION.                                             
054900                                                                          
055000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
055100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
055200                                                                          
055300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
055400                                                                          
055500     IF SUB-KDRC > 0                                                      
055600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
055700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
055800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
055900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056000     END-IF                                                               
056100     .                                                                    
056200 S01-CHECK-SECURITY SECTION.                                              
056300                                                                          
056400     MOVE REQU-IDUSER       TO SEC-IDUSER                                 
056500     MOVE IDSYSTEM          TO SEC-IDTRANS                                
056600     MOVE REQU-IDOUTREC-KEY TO SEC-IDKEY                                  
056700                                                                          
056800     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
056900                         SEC-IDKEY  SEC-KDSVAR                            
057000                                                                          
057100     IF SEC-KDSVAR > SPACE                                                
057200       MOVE ERR-AUTHORIZATION-MISSING TO RESP-IDMSG-ERROR                 
057300     END-IF                                                               
057400     .                                                                    
057500     SKIP3                                                                
057600*** - VALIDATE REQUESTED FIELDS FOR UPDATE/INSERT ON DISTRIBUTION         
057700***   RULES.                                                              
057800 S02-CHECK-REQU-DATA SECTION.                                             
057900                                                                          
058000     PERFORM DB2-SELECT-TZ4REKY-TAB                                       
058100                                                                          
058200     IF LINES-MISSING                                                     
058300       MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                       
058400       MOVE 'RESTART TABLE'     TO RESP-IDELMT-ERROR                      
058500     END-IF                                                               
058600                                                                          
058700     IF RESP-IDMSG-ERROR = SPACE                                          
058800       IF REQU-KDOUTMETH = ALL '+'                                        
058900         MOVE SPACE TO REQU-KDOUTMETH                                     
059000       END-IF                                                             
059100       IF REQU-FLRULEUSE = NOO                                            
059200         PERFORM S02A-VALIDATE-KDOUTMETH                                  
059300       END-IF                                                             
059400     END-IF                                                               
059500                                                                          
059600     IF RESP-IDMSG-ERROR = SPACE                                          
059700       IF REQU-IDOUTDEST = ALL '+'                                        
059800         MOVE SPACE TO REQU-IDOUTDEST                                     
059900       END-IF                                                             
060000       IF REQU-FLRULEUSE = NOO                                            
060100         PERFORM S02B-RELATION-CNTL-IDOUTDEST                             
060200       END-IF                                                             
060300     END-IF                                                               
060400                                                                          
060500     IF RESP-IDMSG-ERROR = SPACE                                          
060600       IF REQU-KVCOPIES = ALL '+'                                         
060700         MOVE SPACE TO REQU-KVCOPIES                                      
060800       END-IF                                                             
060900       IF REQU-FLRULEUSE = NOO                                            
061000         PERFORM S02C-RELATION-CNTL-KVCOPIES                              
061100       END-IF                                                             
061200     END-IF                                                               
061300                                                                          
061400     IF RESP-IDMSG-ERROR = SPACE                                          
061500       IF REQU-FLCARRCNTL = ALL '+'                                       
061600         MOVE NOO   TO REQU-FLCARRCNTL                                    
061700       END-IF                                                             
061800       IF REQU-FLRULEUSE = NOO                                            
061900         PERFORM S02D-RELATION-CNTL-FLCARRCNTL                            
062000       END-IF                                                             
062100     END-IF                                                               
062200                                                                          
062300     IF RESP-IDMSG-ERROR = SPACE                                          
062400       IF REQU-IDPFDEF = ALL '+'                                          
062500         MOVE SPACE TO REQU-IDPFDEF                                       
062600       END-IF                                                             
062700       IF REQU-FLRULEUSE = NOO                                            
062800         PERFORM S02E-RELATION-CNTL-IDPFDEF                               
062900       END-IF                                                             
063000     END-IF                                                               
063100                                                                          
063200     IF RESP-IDMSG-ERROR = SPACE                                          
063300       IF REQU-IDFORMSNM = ALL '+'                                        
063400         MOVE SPACE TO REQU-IDFORMSNM                                     
063500       END-IF                                                             
063600       IF REQU-FLRULEUSE = NOO                                            
063700         PERFORM S02F-RELATION-CNTL-IDFORMSNM                             
063800       END-IF                                                             
063900     END-IF                                                               
064000                                                                          
064100     IF RESP-IDMSG-ERROR = SPACE                                          
064200       IF REQU-TEVCOMST = ALL '+'                                         
064300         MOVE SPACE TO REQU-TEVCOMST                                      
064400       END-IF                                                             
064500       IF REQU-FLRULEUSE = NOO                                            
064600         PERFORM S02G-RELATION-CNTL-TEVCOMST                              
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000     IF RESP-IDMSG-ERROR = SPACE                                          
065100       IF REQU-IDVCINIT = ALL '+'                                         
065200         MOVE SPACE TO REQU-IDVCINIT                                      
065300       END-IF                                                             
065400       IF REQU-FLRULEUSE = NOO                                            
065500         PERFORM S02H-RELATION-CNTL-IDVCINIT                              
065600       END-IF                                                             
065700     END-IF                                                               
065800                                                                          
065900     IF RESP-IDMSG-ERROR = SPACE                                          
066000       MOVE 1 TO IX                                                       
066100       PERFORM UNTIL IX > 5                                               
066200         IF REQU-TEFAX(IX) = ALL '+'                                      
066300           MOVE SPACE TO REQU-TEFAX(IX)                                   
066400         END-IF                                                           
066500         ADD 1 TO IX                                                      
066600       END-PERFORM                                                        
066700       IF REQU-FLRULEUSE = NOO                                            
066800         PERFORM S02I-RELATION-CNTL-TEFAX                                 
066900       END-IF                                                             
067000     END-IF                                                               
067100                                                                          
067200     IF RESP-IDMSG-ERROR = SPACE                                          
067300       IF REQU-IDMAIL-SENDER = ALL '+'                                    
067400         MOVE SPACE TO REQU-IDMAIL-SENDER                                 
067500       END-IF                                                             
067600       IF REQU-FLRULEUSE = NOO                                            
067700         PERFORM S02J-RELATION-CNTL-IDMAIL                                
067800       END-IF                                                             
067900     END-IF                                                               
068000                                                                          
068100     IF RESP-IDMSG-ERROR = SPACE                                          
068200       IF REQU-FLACIF     = ALL '+' OR = SPACE                            
068300         MOVE NOO   TO REQU-FLACIF                                        
068400       END-IF                                                             
068500       IF REQU-FLRULEUSE = NOO                                            
068600         PERFORM S02M-RELATION-CNTL-FLACIF                                
068700       END-IF                                                             
068800     END-IF                                                               
068900                                                                          
069000*    IF RESP-IDMSG-ERROR = SPACE                                          
069100*      IF REQU-FLRULEUSE = NOO                                            
069200*        PERFORM S02K-VALIDATE-TIAAMMDD-RENS                              
069300*      END-IF                                                             
069400*    END-IF                                                               
069500                                                                          
069600     PERFORM S02L-MOVE-REQU-TO-RESPOND                                    
069700     .                                                                    
069800     SKIP3                                                                
069900*** - VALIDATE KDOUTMETH                                                  
070000 S02A-VALIDATE-KDOUTMETH SECTION.                                         
070100                                                                          
070200     MOVE REQU-KDOUTMETH TO KDOUTMETH-SW                                  
070300     IF KDOUTMETH-VALID                                                   
070400       CONTINUE                                                           
070500     ELSE                                                                 
070600       MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                         
070700       MOVE 'KDOUTMETH'       TO RESP-IDELMT-ERROR                        
070800     END-IF                                                               
070900                                                                          
071000     MOVE REQU-KDOUTMETH TO RESP-KDOUTMETH                                
071100     .                                                                    
071200     SKIP3                                                                
071300*** - IDOUTDEST RELATION CONTROL                                          
071400 S02B-RELATION-CNTL-IDOUTDEST SECTION.                                    
071500                                                                          
071600     IF REQU-IDOUTDEST > SPACE                                            
071700       IF REQU-KDOUTMETH = SPACE                                          
071800         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
071900         MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                            
072000       END-IF                                                             
072100* CONTROL WHEN METHOD PRINT, VCOM OR EDI - MAX 8 CHAR DEST                
072200       IF REQU-KDOUTMETH = WS-PRINT                                       
072300       OR WS-VCOM OR WS-EDI                                               
072400         IF REQU-IDOUTDEST(9:1) > SPACE                                   
072500           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
072600           MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                          
072700         END-IF                                                           
072800       END-IF                                                             
072900* CONTROL WHEN METHOD FAX                                                 
073000       IF REQU-KDOUTMETH = WS-FAX                                         
073100         MOVE REQU-IDOUTDEST TO TEFA-IDTFN-IDTFX                          
073200         CALL WZ20TEFA USING TEFA-WZ20TEFA                                
073300         IF TEFA-KDRC > 4                                                 
073400           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
073500           MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                          
073600         ELSE                                                             
073700           MOVE TEFA-IDTFN-IDTFX TO REQU-IDOUTDEST                        
073800         END-IF                                                           
073900       END-IF                                                             
074000* CONTROL WHEN METHOD MAIL                                                
074100       IF REQU-KDOUTMETH = WS-MAIL                                        
074200         MOVE REQU-IDOUTDEST TO EMAD-IDMAIL                               
074300         CALL W009EMAD USING EMAD-W009EMAD                                
074400         IF EMAD-KDSVAR > SPACE                                           
074500           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
074600           MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                          
074700         END-IF                                                           
074800       END-IF                                                             
074900* CONTROL WHEN METHOD ONDEMAND                                            
075000       IF REQU-KDOUTMETH = WS-ONDEMAND                                    
075100         IF REQU-IDFORMSNM > SPACE AND NOT = ALL '+'                      
075200           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
075300           MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                          
075400         END-IF                                                           
075500       END-IF                                                             
075600     ELSE                                                                 
075700       IF (REQU-KDOUTMETH > SPACE AND NOT = WS-ONDEMAND)                  
075800         MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                     
075900         MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                            
076000       END-IF                                                             
076100     END-IF                                                               
076200                                                                          
076300     MOVE REQU-IDOUTDEST TO RESP-IDOUTDEST                                
076400     .                                                                    
076500*** - KVCOPIES RELATION CONTROL                                           
076600 S02C-RELATION-CNTL-KVCOPIES SECTION.                                     
076700                                                                          
076800     IF REQU-KDOUTMETH = WS-PRINT                                         
076900       IF REQU-KVCOPIES NUMERIC                                           
077000         IF REQU-KVCOPIES = '0'                                           
077100           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
077200           MOVE 'KVCOPIES' TO RESP-IDELMT-ERROR                           
077300         END-IF                                                           
077400       ELSE                                                               
077500         MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
077600         MOVE 'KVCOPIES' TO RESP-IDELMT-ERROR                             
077700       END-IF                                                             
077800     ELSE                                                                 
077900       IF REQU-KVCOPIES > '0'                                             
078000         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
078100         MOVE 'KVCOPIES' TO RESP-IDELMT-ERROR                             
078200       END-IF                                                             
078300     END-IF                                                               
078400                                                                          
078500     MOVE REQU-KVCOPIES TO RESP-KVCOPIES                                  
078600     .                                                                    
078700     SKIP3                                                                
078800*** - FLCARRCNTL RELATION CONTROL                                         
078900 S02D-RELATION-CNTL-FLCARRCNTL SECTION.                                   
079000                                                                          
079100     IF REQU-FLCARRCNTL = JA                                              
079200       IF REQU-KDOUTMETH = WS-PRINT OR WS-FAX OR WS-ONDEMAND              
079300       OR WS-GET-IT OR WS-MAIL                                            
079400         CONTINUE                                                         
079500       ELSE                                                               
079600         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
079700         MOVE 'FLCARRCNTL' TO RESP-IDELMT-ERROR                           
079800       END-IF                                                             
079900     ELSE                                                                 
080000       IF REQU-FLCARRCNTL = NOO                                           
080100         CONTINUE                                                         
080200       ELSE                                                               
080300         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
080400         MOVE 'FLCARRCNTL' TO RESP-IDELMT-ERROR                           
080500       END-IF                                                             
080600     END-IF                                                               
080700                                                                          
080800     MOVE REQU-FLCARRCNTL TO RESP-FLCARRCNTL                              
080900     .                                                                    
081000     SKIP3                                                                
081100*** - IDPFDEF RELATION CONTROL                                            
081200 S02E-RELATION-CNTL-IDPFDEF SECTION.                                      
081300                                                                          
081400     IF REQU-IDPFDEF > SPACE                                              
081500       IF REQU-KDOUTMETH = WS-PRINT OR WS-FAX OR WS-MAIL                  
081600       OR WS-GET-IT OR WS-ONDEMAND                                        
081700         MOVE SPACE TO WS-IDPFDEF                                         
081800         MOVE REQU-IDPFDEF TO WS-IDPFDEF                                  
081900         INSPECT WS-IDPFDEF CONVERTING                                    
082000                            WS-VALID-IDPFDEF TO WS-TEST-IDPFDEF           
082100         IF WS-IDPFDEF = SPACE                                            
082200*        -- ALSO ALLOW FILE SUFFIX (.XXX) FOR MAIL                        
082300         OR (REQU-KDOUTMETH = WS-MAIL AND WS-IDPFDEF = '.')               
082400*          -- ALSO ALLOW AN *-PREFIX TO FIX PDF LANDSCAPE PROBLEM         
082500         OR (REQU-KDOUTMETH = WS-MAIL AND WS-IDPFDEF = '*')               
082600           CONTINUE                                                       
082700         ELSE                                                             
082800           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
082900           MOVE 'IDPFDEF' TO RESP-IDELMT-ERROR                            
083000         END-IF                                                           
083100       ELSE                                                               
083200         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
083300         MOVE 'IDPFDEF' TO RESP-IDELMT-ERROR                              
083400       END-IF                                                             
083500     END-IF                                                               
083600                                                                          
083700     MOVE REQU-IDPFDEF TO RESP-IDPFDEF                                    
083800     .                                                                    
083900     SKIP3                                                                
084000*** - IDFORMSNM RELATION CONTROL                                          
084100 S02F-RELATION-CNTL-IDFORMSNM SECTION.                                    
084200                                                                          
084300     IF REQU-IDFORMSNM > SPACE                                            
084400       IF REQU-KDOUTMETH = WS-PRINT OR WS-ONDEMAND OR WS-MAIL             
084500         CONTINUE                                                         
084600       ELSE                                                               
084700         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
084800         MOVE 'IDFORMSNM' TO RESP-IDELMT-ERROR                            
084900       END-IF                                                             
085000     END-IF                                                               
085100                                                                          
085200     MOVE REQU-IDFORMSNM TO RESP-IDFORMSNM                                
085300     .                                                                    
085400     SKIP3                                                                
085500*** - TEVCOMST RELATION CONTROL                                           
085600 S02G-RELATION-CNTL-TEVCOMST SECTION.                                     
085700                                                                          
085800     IF REQU-TEVCOMST > SPACE                                             
085900       IF REQU-KDOUTMETH = WS-VCOM OR WS-EDI                              
086000         CONTINUE                                                         
086100       ELSE                                                               
086200         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
086300         MOVE 'TEVCOMST' TO RESP-IDELMT-ERROR                             
086400       END-IF                                                             
086500     END-IF                                                               
086600                                                                          
086700     MOVE REQU-TEVCOMST TO RESP-TEVCOMST                                  
086800     .                                                                    
086900     SKIP3                                                                
087000*** - IDVCINIT RELATION CONTROL                                           
087100 S02H-RELATION-CNTL-IDVCINIT SECTION.                                     
087200                                                                          
087300     IF REQU-IDVCINIT > SPACE                                             
087400       IF REQU-KDOUTMETH = WS-VCOM OR WS-EDI                              
087500         CONTINUE                                                         
087600       ELSE                                                               
087700         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
087800         MOVE 'IDVCINIT' TO RESP-IDELMT-ERROR                             
087900       END-IF                                                             
088000     END-IF                                                               
088100                                                                          
088200     MOVE REQU-IDVCINIT TO RESP-IDVCINIT                                  
088300     .                                                                    
088400     SKIP3                                                                
088500*** - TEFAX RELATION CONTROL                                              
088600 S02I-RELATION-CNTL-TEFAX SECTION.                                        
088700                                                                          
088800     MOVE NOO TO WS-TEFAX-MAY-BE-SPECIFIED                                
088900     IF REQU-KDOUTMETH = WS-FAX                                           
089000     OR REQU-KDOUTMETH = WS-MAIL                                          
089100       MOVE YES TO WS-TEFAX-MAY-BE-SPECIFIED                              
089200     END-IF                                                               
089300                                                                          
089400     MOVE 1 TO IX                                                         
089500     PERFORM UNTIL IX > 5                                                 
089600     OR RESP-IDMSG-ERROR > SPACE                                          
089700       IF REQU-TEFAX(IX) > SPACE                                          
089800         IF WS-TEFAX-MAY-BE-SPECIFIED = YES                               
089900           CONTINUE                                                       
090000         ELSE                                                             
090100           MOVE ERR-TEFAX-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
090200           MOVE 'TEFAX' TO RESP-IDELMT-ERROR                              
090300         END-IF                                                           
090400       END-IF                                                             
090500       ADD 1 TO IX                                                        
090600     END-PERFORM                                                          
090700                                                                          
090800     MOVE 1 TO IX                                                         
090900     PERFORM UNTIL IX > 5                                                 
091000       MOVE REQU-TEFAX(IX) TO RESP-TEFAX(IX)                              
091100       ADD 1 TO IX                                                        
091200     END-PERFORM                                                          
091300     .                                                                    
091400     SKIP3                                                                
091500*** - IDMAIL-SENDER RELATION CONTROL                                      
091600 S02J-RELATION-CNTL-IDMAIL SECTION.                                       
091700                                                                          
091800     MOVE NOO TO WS-IDMAIL-MAY-BE-SPECIFIED                               
091900     IF REQU-KDOUTMETH = WS-FAX                                           
092000     OR REQU-KDOUTMETH = WS-MAIL                                          
092100       MOVE YES TO WS-IDMAIL-MAY-BE-SPECIFIED                             
092200     END-IF                                                               
092300                                                                          
092400     IF REQU-IDMAIL-SENDER > SPACE                                        
092500       IF WS-IDMAIL-MAY-BE-SPECIFIED = YES                                
092600         MOVE REQU-IDMAIL-SENDER TO EMAD-IDMAIL                           
092700         CALL W009EMAD USING EMAD-W009EMAD                                
092800         IF EMAD-KDSVAR > SPACE                                           
092900           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
093000           MOVE 'IDMAIL-SENDER' TO RESP-IDELMT-ERROR                      
093100         END-IF                                                           
093200       ELSE                                                               
093300         MOVE ERR-IDMAIL-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
093400         MOVE 'IDMAIL-SENDER' TO RESP-IDELMT-ERROR                        
093500       END-IF                                                             
093600     END-IF                                                               
093700                                                                          
093800     MOVE REQU-IDMAIL-SENDER TO RESP-IDMAIL-SENDER                        
093900     .                                                                    
094000     SKIP3                                                                
094100*** - VALIDATE CLEANING-DATE                                              
094200*S02K-VALIDATE-TIAAMMDD-RENS SECTION.                                     
094300*                                                                         
094400*   IF REQU-TIAAMMDD-RENS NUMERIC                                         
094500*     MOVE REQU-TIAAMMDD-RENS TO DATE-TIDATE                              
094600*     MOVE WS-DATE-FORMAT TO DATE-KDDATFMT                                
094700*     CALL WZ20DATE USING DATE-WZ20DATE                                   
094800*     IF DATE-KDRC > ZERO                                                 
094900*       MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                        
095000*       MOVE 'TIAAMMDD-RENS'   TO RESP-IDELMT-ERROR                       
095100*     END-IF                                                              
095200*   ELSE                                                                  
095300*     MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                        
095400*     MOVE 'TIAAMMDD-RENS'     TO RESP-IDELMT-ERROR                       
095500*   END-IF                                                                
095600*                                                                         
095700*   MOVE REQU-TIAAMMDD-RENS TO RESP-TIAAMMDD-RENS                         
095800*    .                                                                    
095900     SKIP3                                                                
096000 S02L-MOVE-REQU-TO-RESPOND SECTION.                                       
096100                                                                          
096200     MOVE REQU-KDOUTMETH        TO RESP-KDOUTMETH                         
096300     MOVE REQU-IDOUTDEST        TO RESP-IDOUTDEST                         
096400     MOVE REQU-KVCOPIES         TO RESP-KVCOPIES                          
096500     MOVE REQU-FLCARRCNTL       TO RESP-FLCARRCNTL                        
096600     MOVE REQU-FLACIF           TO RESP-FLACIF                            
096700     MOVE REQU-IDPFDEF          TO RESP-IDPFDEF                           
096800     MOVE REQU-IDFORMSNM        TO RESP-IDFORMSNM                         
096900     MOVE REQU-TEVCOMST         TO RESP-TEVCOMST                          
097000     MOVE REQU-IDVCINIT         TO RESP-IDVCINIT                          
097100     MOVE REQU-IDMAIL-SENDER    TO RESP-IDMAIL-SENDER                     
097200     MOVE REQU-TIAAMMDD-RENS    TO RESP-TIAAMMDD-RENS                     
097300     MOVE REQU-TEFAX(1)         TO RESP-TEFAX(1)                          
097400     MOVE REQU-TEFAX(2)         TO RESP-TEFAX(2)                          
097500     MOVE REQU-TEFAX(3)         TO RESP-TEFAX(3)                          
097600     MOVE REQU-TEFAX(4)         TO RESP-TEFAX(4)                          
097700     MOVE REQU-TEFAX(5)         TO RESP-TEFAX(5)                          
097800     MOVE REQU-FLRULEUSE        TO RESP-FLRULEUSE                         
097900     .                                                                    
098000     SKIP3                                                                
098100*** - FLACIF  RELATION CONTROL                                            
098200 S02M-RELATION-CNTL-FLACIF     SECTION.                                   
098300                                                                          
098400     IF REQU-FLACIF = JA                                                  
098500       IF (REQU-KDOUTMETH = WS-PRINT OR WS-ONDEMAND)                      
098600*      AND REQU-IDPFDEF NOT = ALL '+'                                     
098700*      AND REQU-IDPFDEF NOT = SPACE                                       
098800         CONTINUE                                                         
098900       ELSE                                                               
099000         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
099100         MOVE 'FLACIF' TO RESP-IDELMT-ERROR                               
099200       END-IF                                                             
099300     ELSE                                                                 
099400       IF REQU-FLACIF = NOO                                               
099500         CONTINUE                                                         
099600       ELSE                                                               
099700         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
099800         MOVE 'FLACIF' TO RESP-IDELMT-ERROR                               
099900       END-IF                                                             
100000     END-IF                                                               
100100                                                                          
100200     MOVE REQU-FLACIF TO RESP-FLACIF                                      
100300     .                                                                    
100400     EJECT                                                                
100500 S03-SCRATCH-RESPOND SECTION.                                             
100600                                                                          
100700     MOVE SPACE TO RESP-KDOUTMETH                                         
100800     MOVE SPACE TO RESP-IDOUTDEST                                         
100900     MOVE SPACE TO RESP-KVCOPIES                                          
101000     MOVE SPACE TO RESP-FLCARRCNTL                                        
101100     MOVE SPACE TO RESP-FLACIF                                            
101200     MOVE SPACE TO RESP-IDPFDEF                                           
101300     MOVE SPACE TO RESP-IDFORMSNM                                         
101400     MOVE SPACE TO RESP-TEVCOMST                                          
101500     MOVE SPACE TO RESP-IDVCINIT                                          
101600     MOVE SPACE TO RESP-IDMAIL-SENDER                                     
101700     MOVE ZERO  TO RESP-TIAAMMDD-RENS                                     
101800     MOVE SPACE TO RESP-TEFAX(1)                                          
101900     MOVE SPACE TO RESP-TEFAX(2)                                          
102000     MOVE SPACE TO RESP-TEFAX(3)                                          
102100     MOVE SPACE TO RESP-TEFAX(4)                                          
102200     MOVE SPACE TO RESP-TEFAX(5)                                          
102300     MOVE ZERO  TO RESP-KVANTEX-PRINTAD                                   
102400     MOVE SPACE TO RESP-FLRULEUSE                                         
102500     .                                                                    
102600 S04-CALL-WZ11OUTX SECTION.                                               
102700                                                                          
102800     EVALUATE REQU-KDOUTMETH                                              
102900       WHEN WS-PRINT                                                      
103000         MOVE WS-IDCALL        TO OUTP-IDCALL                             
103100         MOVE WS-KDFUNC        TO OUTP-KDFUNC                             
103200         IF WS-KDFUNC = 'OPEN'                                            
103300*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
103400*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
103500           MOVE REQU-IDOUTTYPE-KEY                                        
103600                                 TO OUTP-IDOUTTYPE                        
103700           MOVE REQU-IDOUTREC-KEY                                         
103800                                 TO OUTP-IDOUTREC                         
103900           MOVE REQU-IDLIST-KEY  TO OUTP-IDLIST                           
104000           MOVE WS-TIREGDAT-KEY  TO OUTP-TIREGDAT                         
104100           MOVE WS-TIKLOCK-KEY   TO OUTP-TIKLOCK                          
104200                                                                          
104300           MOVE REQU-IDOUTDEST   TO OUTP-IDOUTDEST                        
104400           MOVE REQU-KVCOPIES    TO OUTP-KVCOPIES                         
104500           MOVE REQU-IDPFDEF     TO OUTP-IDPFDEF                          
104600           MOVE REQU-IDFORMSNM   TO OUTP-IDFORMSNM                        
104700           MOVE REQU-FLCARRCNTL  TO OUTP-FLCARRCNTL                       
104800           MOVE REQU-FLACIF      TO OUTP-FLACIF                           
104900*          -- OLD RULES MAY LACK FLACIF, AND PAGEDEF/FORMDEF              
105000*          -- PLUS NO CARRIAGE CONTROL CHARACTERS USED TO INDICATE        
105100*          -- ACIF SHOULD BE USED (EXAMPLE: BILL-IT INVOICES)             
105200           IF REQU-FLACIF = SPACE                                         
105300             MOVE NOO            TO OUTP-FLACIF                           
105400           END-IF                                                         
105500           IF REQU-FLCARRCNTL = NOO AND REQU-IDPFDEF NOT = SPACE          
105600             MOVE JA             TO OUTP-FLACIF                           
105700           END-IF                                                         
105800           MOVE SPACE            TO OUTP-IDPCB                            
105900         END-IF                                                           
106000         IF WS-KDFUNC = 'PUT'                                             
106100           MOVE WS-TEOUTDATA-L   TO OUTP-TEOUTDATA-L                      
106200           MOVE WS-TEOUTDATA     TO OUTP-TEOUTDATA                        
106300         END-IF                                                           
106400         CALL WZ11OUTP USING OUTP-WZ11OUT                                 
106500         MOVE ERR-PRINT-FAILED TO WS-ERROR-TYPE                           
106600         MOVE OUTP-KDRC        TO WS-KDRC                                 
106700         MOVE OUTP-TEOUTDATA   TO WS-TEOUTDATA                            
106800                                                                          
106900       WHEN WS-FAX                                                        
107000         MOVE WS-IDCALL        TO OUTF-IDCALL                             
107100         MOVE WS-KDFUNC        TO OUTF-KDFUNC                             
107200         IF WS-KDFUNC = 'OPEN'                                            
107300*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
107400*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
107500           MOVE REQU-IDOUTTYPE-KEY                                        
107600                                 TO OUTF-IDOUTTYPE                        
107700           MOVE REQU-IDOUTREC-KEY                                         
107800                                 TO OUTF-IDOUTREC                         
107900           MOVE REQU-IDLIST-KEY  TO OUTF-IDLIST                           
108000           MOVE WS-TIREGDAT-KEY  TO OUTF-TIREGDAT                         
108100           MOVE WS-TIKLOCK-KEY   TO OUTF-TIKLOCK                          
108200                                                                          
108300           MOVE REQU-IDOUTDEST   TO OUTF-IDOUTDEST                        
108400           MOVE REQU-IDPFDEF     TO OUTF-IDPFDEF                          
108500           MOVE REQU-FLCARRCNTL  TO OUTF-FLCARRCNTL                       
108600           MOVE REQU-IDMAIL-SENDER TO OUTF-IDMAIL-SENDER                  
108700           MOVE REQU-TEFAX(1)    TO OUTF-TEFAX(1)                         
108800           MOVE REQU-TEFAX(2)    TO OUTF-TEFAX(2)                         
108900           MOVE REQU-TEFAX(3)    TO OUTF-TEFAX(3)                         
109000           MOVE REQU-TEFAX(4)    TO OUTF-TEFAX(4)                         
109100           MOVE REQU-TEFAX(5)    TO OUTF-TEFAX(5)                         
109200         END-IF                                                           
109300         IF WS-KDFUNC = 'PUT'                                             
109400           MOVE WS-TEOUTDATA-L   TO OUTF-TEOUTDATA-L                      
109500           MOVE WS-TEOUTDATA     TO OUTF-TEOUTDATA                        
109600         END-IF                                                           
109700         CALL WZ11OUTF USING OUTF-WZ11OUT                                 
109800         MOVE ERR-FAX-FAILED   TO WS-ERROR-TYPE                           
109900         MOVE OUTF-KDRC        TO WS-KDRC                                 
110000         MOVE OUTF-TEOUTDATA   TO WS-TEOUTDATA                            
110100                                                                          
110200       WHEN WS-MAIL                                                       
110300         MOVE WS-IDCALL        TO OUTM-IDCALL                             
110400         MOVE WS-KDFUNC        TO OUTM-KDFUNC                             
110500         IF WS-KDFUNC = 'OPEN'                                            
110600*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
110700*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
110800           MOVE REQU-IDOUTTYPE-KEY                                        
110900                                 TO OUTM-IDOUTTYPE                        
111000           MOVE REQU-IDOUTREC-KEY                                         
111100                                 TO OUTM-IDOUTREC                         
111200           MOVE REQU-IDLIST-KEY  TO OUTM-IDLIST                           
111300           MOVE WS-TIREGDAT-KEY  TO OUTM-TIREGDAT                         
111400           MOVE WS-TIKLOCK-KEY   TO OUTM-TIKLOCK                          
111500                                                                          
111600           MOVE REQU-IDOUTDEST   TO OUTM-IDOUTDEST                        
111700           MOVE REQU-IDPFDEF     TO OUTM-IDPFDEF                          
111800           MOVE REQU-FLCARRCNTL  TO OUTM-FLCARRCNTL                       
111900           MOVE REQU-IDMAIL-SENDER TO OUTM-IDMAIL-SENDER                  
112000*          -- USE FIRST TEFAX LINE AS TITLE, AND SHIFT                    
112100*          -- THE OTHERS UP ONE STEP                                      
112200           IF REQU-TEFAX(1) NOT = SPACE                                   
112300             MOVE REQU-TEFAX(1)  TO OUTM-IDMAILTTL                        
112400           ELSE                                                           
112500             MOVE SPACE TO OUTM-IDMAILTTL                                 
112600             STRING 'D&P Mail ('                                          
112700                    REQU-IDOUTTYPE-KEY                                    
112800                    ')'                                                   
112900                    DELIMITED BY SIZE INTO OUTM-IDMAILTTL                 
113000           END-IF                                                         
113100           MOVE REQU-TEFAX(2)    TO OUTM-TEFAX(1)                         
113200           MOVE REQU-TEFAX(3)    TO OUTM-TEFAX(2)                         
113300           MOVE REQU-TEFAX(4)    TO OUTM-TEFAX(3)                         
113400           MOVE REQU-TEFAX(5)    TO OUTM-TEFAX(4)                         
113500           MOVE SPACE            TO OUTM-TEFAX(5)                         
113600                                                                          
113700         END-IF                                                           
113800         IF WS-KDFUNC = 'PUT'                                             
113900           MOVE WS-TEOUTDATA-L   TO OUTM-TEOUTDATA-L                      
114000           MOVE WS-TEOUTDATA     TO OUTM-TEOUTDATA                        
114100         END-IF                                                           
114200                                                                          
114300         CALL WZ11OUTM USING OUTM-WZ11OUT                                 
114400                                                                          
114500         MOVE ERR-MAIL-FAILED  TO WS-ERROR-TYPE                           
114600         MOVE OUTM-KDRC        TO WS-KDRC                                 
114700         MOVE OUTM-TEOUTDATA   TO WS-TEOUTDATA                            
114800                                                                          
114900       WHEN WS-EDI                                                        
115000*        -- NOTE: THE VCOM SUBROUTINE IS USED FOR EDI TOO                 
115100         MOVE WS-IDCALL        TO OUTV-IDCALL                             
115200         MOVE WS-KDFUNC        TO OUTV-KDFUNC                             
115300         IF WS-KDFUNC = 'OPEN'                                            
115400           MOVE REQU-IDOUTDEST   TO OUTV-IDOUTDEST                        
115500           MOVE REQU-TEVCOMST    TO OUTV-TEVCOMST                         
115600           MOVE REQU-IDVCINIT    TO OUTV-IDVCINIT                         
115700         END-IF                                                           
115800         IF WS-KDFUNC = 'PUT'                                             
115900           MOVE WS-TEOUTDATA-L   TO OUTV-TEOUTDATA-L                      
116000           MOVE WS-TEOUTDATA     TO OUTV-TEOUTDATA                        
116100         END-IF                                                           
116200         CALL WZ11OUTV USING OUTV-WZ11OUT                                 
116300         MOVE ERR-EDI-FAILED   TO WS-ERROR-TYPE                           
116400         MOVE OUTV-KDRC        TO WS-KDRC                                 
116500         MOVE OUTV-TEOUTDATA   TO WS-TEOUTDATA                            
116600                                                                          
116700       WHEN WS-VCOM                                                       
116800         MOVE WS-IDCALL        TO OUTV-IDCALL                             
116900         MOVE WS-KDFUNC        TO OUTV-KDFUNC                             
117000         IF WS-KDFUNC = 'OPEN'                                            
117100           MOVE REQU-IDOUTDEST   TO OUTV-IDOUTDEST                        
117200           MOVE REQU-TEVCOMST    TO OUTV-TEVCOMST                         
117300           MOVE REQU-IDVCINIT    TO OUTV-IDVCINIT                         
117400         END-IF                                                           
117500         IF WS-KDFUNC = 'PUT'                                             
117600           MOVE WS-TEOUTDATA-L   TO OUTV-TEOUTDATA-L                      
117700           MOVE WS-TEOUTDATA     TO OUTV-TEOUTDATA                        
117800         END-IF                                                           
117900         CALL WZ11OUTV USING OUTV-WZ11OUT                                 
118000         MOVE ERR-VCOM-FAILED  TO WS-ERROR-TYPE                           
118100         MOVE OUTV-KDRC        TO WS-KDRC                                 
118200         MOVE OUTV-TEOUTDATA   TO WS-TEOUTDATA                            
118300                                                                          
118400       WHEN WS-ONDEMAND                                                   
118500         MOVE WS-IDCALL        TO OUTO-IDCALL                             
118600         MOVE WS-KDFUNC        TO OUTO-KDFUNC                             
118700         IF WS-KDFUNC = 'OPEN'                                            
118800*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
118900*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
119000           MOVE REQU-IDOUTTYPE-KEY                                        
119100                                 TO OUTO-IDOUTTYPE                        
119200           MOVE REQU-IDOUTREC-KEY                                         
119300                                 TO OUTO-IDOUTREC                         
119400           MOVE REQU-IDLIST-KEY  TO OUTO-IDLIST                           
119500           MOVE WS-TIREGDAT-KEY  TO OUTO-TIREGDAT                         
119600           MOVE WS-TIKLOCK-KEY   TO OUTO-TIKLOCK                          
119700                                                                          
119800           MOVE REQU-IDOUTDEST   TO OUTO-IDOUTDEST                        
119900           MOVE REQU-IDPFDEF     TO OUTO-IDPFDEF                          
120000           MOVE REQU-IDFORMSNM   TO OUTO-IDFORMSNM                        
120100           MOVE REQU-FLCARRCNTL  TO OUTO-FLCARRCNTL                       
120200           MOVE REQU-FLACIF      TO OUTO-FLACIF                           
120300*          -- OLD RULES MAY LACK FLACIF, AND PAGEDEF/FORMDEF              
120400*          -- PLUS NO CARRIAGE CONTROL CHARACTERS USED TO INDICATE        
120500*          -- ACIF SHOULD BE USED (EXAMPLE: BILL-IT INVOICES)             
120600           IF REQU-FLACIF = SPACE                                         
120700             MOVE NOO            TO OUTO-FLACIF                           
120800           END-IF                                                         
120900           IF REQU-FLCARRCNTL = NOO AND REQU-IDPFDEF NOT = SPACE          
121000             MOVE JA             TO OUTO-FLACIF                           
121100           END-IF                                                         
121200           MOVE SPACE            TO OUTO-IDPCB                            
121300         END-IF                                                           
121400         IF WS-KDFUNC = 'PUT'                                             
121500           MOVE WS-TEOUTDATA-L   TO OUTO-TEOUTDATA-L                      
121600           MOVE WS-TEOUTDATA     TO OUTO-TEOUTDATA                        
121700         END-IF                                                           
121800         CALL WZ11OUTO USING OUTO-WZ11OUT                                 
121900         MOVE ERR-ONDEMAND-FAILED TO WS-ERROR-TYPE                        
122000         MOVE OUTO-KDRC        TO WS-KDRC                                 
122100         MOVE OUTO-TEOUTDATA   TO WS-TEOUTDATA                            
122200                                                                          
122300       WHEN WS-GET-IT                                                     
122400         MOVE WS-IDCALL        TO OUTA-IDCALL                             
122500         MOVE WS-KDFUNC        TO OUTA-KDFUNC                             
122600         IF WS-KDFUNC = 'OPEN'                                            
122700*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
122800*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
122900           MOVE REQU-IDOUTTYPE-KEY                                        
123000                                 TO OUTA-IDOUTTYPE                        
123100           MOVE REQU-IDOUTREC-KEY                                         
123200                                 TO OUTA-IDOUTREC                         
123300           MOVE REQU-IDLIST-KEY  TO OUTA-IDLIST                           
123400           MOVE WS-TIREGDAT-KEY  TO OUTA-TIREGDAT                         
123500           MOVE WS-TIKLOCK-KEY   TO OUTA-TIKLOCK                          
123600                                                                          
123700           MOVE REQU-IDOUTDEST   TO OUTA-IDOUTDEST                        
123800           MOVE REQU-IDPFDEF     TO OUTA-IDPFDEF                          
123900           MOVE REQU-FLCARRCNTL  TO OUTA-FLCARRCNTL                       
124000           MOVE ZERO             TO OUTA-TIREGDAT                         
124100           MOVE ZERO             TO OUTA-TIREGTID                         
124200         END-IF                                                           
124300         IF WS-KDFUNC = 'PUT'                                             
124400           MOVE WS-TEOUTDATA-L   TO OUTA-TEOUTDATA-L                      
124500           MOVE WS-TEOUTDATA     TO OUTA-TEOUTDATA                        
124600         END-IF                                                           
124700         CALL WZ11OUTA USING OUTA-WZ11OUT                                 
124800         MOVE ERR-GET-IT-FAILED  TO WS-ERROR-TYPE                         
124900         MOVE OUTA-KDRC          TO WS-KDRC                               
125000         MOVE OUTA-TEOUTDATA     TO WS-TEOUTDATA                          
125100                                                                          
125200       WHEN WS-WEB                                                        
125300         MOVE WS-IDCALL        TO OUTW-IDCALL                             
125400         MOVE WS-KDFUNC        TO OUTW-KDFUNC                             
125500         IF WS-KDFUNC = 'OPEN'                                            
125600           MOVE REQU-IDOUTTYPE-KEY TO OUTW-IDOUTTYPE                      
125700           MOVE REQU-IDOUTREC-KEY  TO OUTW-IDOUTREC                       
125800           MOVE REQU-IDLIST-KEY    TO OUTW-IDLIST                         
125900           MOVE REQU-TIREGDAT-KEY  TO OUTW-TIREGDAT                       
126000           MOVE REQU-TIKLOCK-KEY   TO OUTW-TIKLOCK                        
126100           MOVE REQU-IDLOPNR-KEY   TO OUTW-IDLOPNR                        
126200         END-IF                                                           
126300         IF WS-KDFUNC = 'PUT'                                             
126400           MOVE WS-TEOUTDATA-L   TO OUTW-TEOUTDATA-L                      
126500           MOVE WS-TEOUTDATA     TO OUTW-TEOUTDATA                        
126600         END-IF                                                           
126700         CALL WZ11OUTW USING OUTW-WZ11OUT                                 
126800         MOVE ERR-WEB-FAILED     TO WS-ERROR-TYPE                         
126900         MOVE OUTW-KDRC          TO WS-KDRC                               
127000         MOVE OUTW-TEOUTDATA     TO WS-TEOUTDATA                          
127100                                                                          
127200     END-EVALUATE                                                         
127300                                                                          
127400     IF WS-KDRC > ZERO                                                    
127500       MOVE WS-ERROR-TYPE         TO RESP-IDMSG-ERROR                     
127600       MOVE WS-TEOUTDATA(1:16)    TO RESP-IDELMT-ERROR                    
127700     END-IF                                                               
127800     .                                                                    
127900 S90-SEND-OPEN  SECTION.                                                  
128000                                                                          
128100     MOVE WS-ADRESS-SEND TO SEND-ADDISPABS                                
128200     MOVE 'OPEN'         TO SEND-KDFUNC                                   
128300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
128400                         SEND-OPEN-AREA                                   
128500     IF SEND-KDRC > ZERO                                                  
128600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
128700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
128800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
128900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
129000     END-IF                                                               
129100     .                                                                    
129200 S90-PUT-HEADER SECTION.                                                  
129300                                                                          
129400     MOVE 'PUT'              TO SEND-KDFUNC                               
129500     MOVE  WZ04-SEND-IDCOM   TO SEND-IDCOM                                
129600     MOVE LENGTH OF HDR-AREA TO SEND-KVDLEN                               
129700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
129800                                           HDR-AREA                       
129900     IF SEND-KDRC > ZERO                                                  
130000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
130100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
130200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
130300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
130400     END-IF                                                               
130500     .                                                                    
130600 S90-PUT-DATA SECTION.                                                    
130700                                                                          
130800     MOVE 'PUT'            TO SEND-KDFUNC                                 
130900     MOVE  WZ04-SEND-IDCOM TO SEND-IDCOM                                  
131000     MOVE REDA-TEOUTDATA-L TO SEND-KVDLEN                                 
131100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
131200                                      REDA-TEOUTDATA-D                    
131300     IF SEND-KDRC > ZERO                                                  
131400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
131500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
131600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
131700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
131800     END-IF                                                               
131900     .                                                                    
132000 S90-SEND-CLOSE SECTION.                                                  
132100                                                                          
132200     MOVE 'CLOSE'          TO SEND-KDFUNC                                 
132300     MOVE  WZ04-SEND-IDCOM TO SEND-IDCOM                                  
132400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
132500     .                                                                    
132600                                                                          
132700*    --- DB2 SECTIONS                                                     
132800 DB2-SELECT-TZ4REKY-TAB SECTION.                                          
132900                                                                          
133000     MOVE 000100  TO GOOD-SQLCODECODES                                    
133100     EXEC SQL                                                             
133200          SELECT KDOUTMETH                                                
133300               , IDOUTDEST                                                
133400               , KVCOPIES                                                 
133500               , FLCARRCNTL                                               
133600               , IDPFDEF                                                  
133700               , IDFORMSNM                                                
133800               , TEVCOMST                                                 
133900               , IDVCINIT                                                 
134000               , IDMAIL_SENDER                                            
134100               , TIAAMMDD_RENS                                            
134200               , TEFAX_1                                                  
134300               , TEFAX_2                                                  
134400               , TEFAX_3                                                  
134500               , TEFAX_4                                                  
134600               , TEFAX_5                                                  
134700               , KVANTEX_PRINTAD                                          
134800               , FLRULEMISS                                               
134900               , FLACIF                                                   
135000                                                                          
135100          INTO  :REKY-KDOUTMETH                                           
135200              , :REKY-IDOUTDEST                                           
135300              , :REKY-KVCOPIES                                            
135400              , :REKY-FLCARRCNTL                                          
135500              , :REKY-IDPFDEF                                             
135600              , :REKY-IDFORMSNM                                           
135700              , :REKY-TEVCOMST                                            
135800              , :REKY-IDVCINIT                                            
135900              , :REKY-IDMAIL-SENDER                                       
136000              , :REKY-TIAAMMDD-RENS                                       
136100              , :REKY-TEFAX-1                                             
136200              , :REKY-TEFAX-2                                             
136300              , :REKY-TEFAX-3                                             
136400              , :REKY-TEFAX-4                                             
136500              , :REKY-TEFAX-5                                             
136600              , :REKY-KVANTEX-PRINTAD                                     
136700              , :REKY-FLRULEMISS                                          
136800              , :REKY-FLACIF                                              
136900                                                                          
137000          FROM   TZ4REKY                                                  
137100                                                                          
137200          WHERE  IDOUTTYPE = :REQU-IDOUTTYPE-KEY                          
137300           AND   IDOUTREC  = :REQU-IDOUTREC-KEY                           
137400           AND   TRIM(IDLIST) = :WS-IDLIST-KEY                            
137500           AND   TIREGDAT  = :WS-TIREGDAT-KEY                             
137600           AND   TIKLOCK   = :WS-TIKLOCK-KEY                              
137700           AND   IDLOPNR   = :WS-REKY-IDLOPNR-KEY                         
137800     END-EXEC                                                             
137900                                                                          
138000     MOVE SQLCODE TO SQLCODE-WS                                           
138100     PERFORM DB2-STATUS-CHECK                                             
138200     .                                                                    
138300 DB2-UPDATE-TZ4REKY-TAB  SECTION.                                         
138400                                                                          
138500     MOVE 000     TO GOOD-SQLCODECODES                                    
138600     EXEC SQL                                                             
138700         UPDATE TZ4REKY                                                   
138800           SET   KVANTEX_PRINTAD = :REKY-KVANTEX-PRINTAD                  
138900                                                                          
139000         WHERE   IDOUTTYPE = :REQU-IDOUTTYPE-KEY                          
139100          AND    IDOUTREC  = :REQU-IDOUTREC-KEY                           
139200          AND    TRIM(IDLIST) = :WS-IDLIST-KEY                            
139300          AND    TIREGDAT  = :WS-TIREGDAT-KEY                             
139400          AND    TIKLOCK   = :WS-TIKLOCK-KEY                              
139500          AND    IDLOPNR   = :WS-REKY-IDLOPNR-KEY                         
139600     END-EXEC                                                             
139700                                                                          
139800     MOVE SQLCODE TO SQLCODE-WS                                           
139900     PERFORM DB2-STATUS-CHECK                                             
140000     .                                                                    
140100 DB2-COUNT-TZ4DIRU-TAB SECTION.                                           
140200                                                                          
140300     MOVE 000100  TO GOOD-SQLCODECODES                                    
140400                                                                          
140500     EXEC SQL                                                             
140600          SELECT COUNT(*)                                                 
140700                                                                          
140800          INTO  :TZ4DIRU-COUNTER                                          
140900                                                                          
141000          FROM   TZ4DIRU                                                  
141100                                                                          
141200          WHERE  IDOUTTYPE      = :REQU-IDOUTTYPE-KEY                     
141300           AND   IDOUTREC_FROM  = :REQU-IDOUTREC-KEY                      
141400           AND   IDOUTREC_TO    = :REQU-IDOUTREC-KEY                      
141500          OR     IDOUTTYPE      = :REQU-IDOUTTYPE-KEY                     
141600           AND   IDOUTREC_FROM <= :REQU-IDOUTREC-KEY                      
141700           AND   IDOUTREC_TO   >= :REQU-IDOUTREC-KEY                      
141800           AND NOT EXISTS                                                 
141900                 (SELECT  *                                               
142000                  FROM    TZ4DIRU                                         
142100                  WHERE   IDOUTTYPE     = :REQU-IDOUTTYPE-KEY             
142200                  AND     IDOUTREC_FROM = :REQU-IDOUTREC-KEY              
142300                  AND     IDOUTREC_TO   = :REQU-IDOUTREC-KEY)             
142400     END-EXEC                                                             
142500                                                                          
142600     MOVE SQLCODE TO SQLCODE-WS                                           
142700     PERFORM DB2-STATUS-CHECK                                             
142800     .                                                                    
142900 DB2-DCL-OPN-TZ4REDA-CRS SECTION.                                         
143000                                                                          
143100     MOVE 000100 TO GOOD-SQLCODECODES                                     
143200                                                                          
143300     EXEC SQL                                                             
143400       DECLARE TZ4REDA-CRS CURSOR WITH HOLD FOR                           
143500                                                                          
143600       SELECT  TEOUTDATA                                                  
143700                                                                          
143800       FROM    TZ4REDA                                                    
143900                                                                          
144000       WHERE  IDOUTTYPE = :REQU-IDOUTTYPE-KEY                             
144100        AND   IDOUTREC  = :REQU-IDOUTREC-KEY                              
144200        AND   TRIM(IDLIST) = :WS-IDLIST-KEY                               
144300        AND   TIREGDAT  = :WS-TIREGDAT-KEY                                
144400        AND   TIKLOCK   = :WS-TIKLOCK-KEY                                 
144500        AND   IDLOPNR   = :WS-REDA-IDLOPNR-KEY                            
144600                                                                          
144700       ORDER BY IDOUTTYPE                                                 
144800              , IDOUTREC                                                  
144900              , IDLIST                                                    
145000              , TIREGDAT                                                  
145100              , TIKLOCK                                                   
145200              , IDLOPNR                                                   
145300              , KVPOST                                                    
145400     END-EXEC                                                             
145500                                                                          
145600     MOVE 000100  TO GOOD-SQLCODECODES                                    
145700                                                                          
145800     EXEC SQL                                                             
145900       OPEN TZ4REDA-CRS                                                   
146000     END-EXEC                                                             
146100                                                                          
146200     MOVE SQLCODE TO SQLCODE-WS                                           
146300     PERFORM DB2-STATUS-CHECK                                             
146400     .                                                                    
146500 DB2-FETCH-TZ4REDA-CRS SECTION.                                           
146600                                                                          
146700     MOVE 000100  TO GOOD-SQLCODECODES                                    
146800                                                                          
146900     EXEC SQL                                                             
147000                                                                          
147100       FETCH TZ4REDA-CRS                                                  
147200                                                                          
147300       INTO :REDA-TEOUTDATA                                               
147400                                                                          
147500     END-EXEC                                                             
147600                                                                          
147700     MOVE SQLCODE TO SQLCODE-WS                                           
147800     PERFORM DB2-STATUS-CHECK                                             
147900     .                                                                    
148000 DB2-CLOSE-TZ4REDA-CRS SECTION.                                           
148100                                                                          
148200     EXEC SQL                                                             
148300        CLOSE TZ4REDA-CRS                                                 
148400     END-EXEC                                                             
148500     .                                                                    
148600 DB2-STATUS-CHECK  SECTION.                                               
148700                                                                          
148800     SET SQLCODE-IX TO 1                                                  
148900     SEARCH GOOD-SQLCODE                                                  
149000       AT END                                                             
149100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
149200          DELIMITED BY SIZE INTO ERROR-TEXT                               
149300          CALL ABEND USING RKOD-ABEND-DB2                                 
149400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
149500     END-SEARCH                                                           
149600     .                                                                    
