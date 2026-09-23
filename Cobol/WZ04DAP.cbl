000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ04DAP.                                                 
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   08/04/28.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        - SUBROUTINE INTERFACE FOR DISTRIBUTION AND PRINT.               
001100*        - (MODFICATION OF SOURCE FROM ORIGINAL WZ042000 PGM)             
001200*        - CALLS SUBPROGRAMS FOR DISTRIBUTION DEPENDING ON VALUES         
001300*               IN TABLE TZ4DIRU FIELD KDOUTMETH.                         
001400*                                                                         
001500*        THE PROGRAM READS   TABLE TZ4DIRU                                
001600*        THE PROGRAM UPDATES TABLE TZ4REKY                                
001700*        THE PROGRAM UPDATES TABLE TZ4REDA                                
001800*                                                                         
001900*    CALL FORMAT:                                                         
002000*        CALL WZ04DAP USING DAP-WZ04DAP                                   
002100*                                                                         
002200*    ARGUMENT COPYTEXT:                                                   
002300*        WZ04DAP                                                          
002400*    ARGUMENTS:                                                           
002500*        KDFUNC = "OPEN", "PUT" OR "CLOSE"                                
002600*        KDRC = 0 RETURNED IF CALL OK; = 8 IF CALL FAILS                  
002700*        IDCALL=001-9 ID SEQUENCE NUMBER USED WHEN MULTIPLE               
002800*            PARALLEL DISTRIBUTIONS ARE ACTIVE. THE VALUE IS              
002900*            CREATED BY THE OPEN CALL AND USED LATER IN THE OTHER         
003000*            CALLS.                                                       
003100*            NOTE: THIS IS FOR FUTURE USE. CURRENTLY MULTIPLE             
003200*            DISTRIBUTIONS ARE NOT SUPPORTED.                             
003300*                                                                         
003400*        1. KDFUNC = "OPEN"                                               
003500*            SPECIFY IDOUTTYPE, IDOUTREC TO SELECT THE APPROPRITE         
003600*            D&P RULE. IDLIST IS AN OPTIONAL ID FOR THIS                  
003700*            DISTRIBUTION.                                                
003800*            SPECIFY IDOUTDEST TO OVERRIDE VALUE GIVEN IN THE             
003900*            RULE.                                                        
004000*                                                                         
004100*        2. KDFUNC = "PUT"                                                
004200*            SPECIFY TEOUTDATA AND SET KVDLEN TO ACTUAL LENGTH OF         
004300*            DATA.                                                        
004400*                                                                         
004500*        3. KDFUNC = "CLOSE"                                              
004600*            NO ADDITIONAL ARGUMENTS USED.                                
004700*            MARK THIS DISTRIBUTION AS COMPLETE. THE PYSICAL              
004800*            DISTRIBUTION WILL BE INITIATED AND EXECUTED IN               
004900*            THE BACKGROUND.                                              
005000*                                                                         
005100     SKIP3                                                                
005200 ENVIRONMENT DIVISION.                                                    
005300     SKIP2                                                                
005400 INPUT-OUTPUT SECTION.                                                    
005500                                                                          
005600 FILE-CONTROL.                                                            
005700     EJECT                                                                
005800 DATA DIVISION.                                                           
005900     SKIP3                                                                
006000 FILE SECTION.                                                            
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300 77  IDPGM                       PIC X(08)   VALUE 'WZ04DAP'.             
006400*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
006500 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
006600 77  KDRC-DISPLAY                PIC Z(5).                                
006700 77  WS-OUT-ERROR-TEXT           PIC X(80)   VALUE SPACE.                 
006800                                                                          
006900 77  WS-MY-OWN-ADRESS            PIC X(50)   VALUE                        
007000                                          'CARPARTS.DAP.DISTRDOC'.        
007100 77  YES                         PIC X       VALUE 'J'.                   
007200 77  NOO                         PIC X       VALUE 'N'.                   
007300 77  WS-PRINT                    PIC X(4)    VALUE 'PRT '.                
007400 77  WS-EDI                      PIC X(4)    VALUE 'EDI '.                
007500 77  WS-FAX                      PIC X(4)    VALUE 'FAX '.                
007600 77  WS-VCOM                     PIC X(4)    VALUE 'VCOM'.                
007700 77  WS-MAIL                     PIC X(4)    VALUE 'MAIL'.                
007800 77  WS-ONDEMAND                 PIC X(4)    VALUE 'ONDE'.                
007900 77  WS-SAVE                     PIC X(4)    VALUE 'SAVE'.                
008000 77  WS-GET-IT                   PIC X(4)    VALUE 'GETI'.                
008100 77  WS-WEB                      PIC X(4)    VALUE 'WEB '.                
008200 77  WS-DEFAULT-RESTARTABLE-DAYS PIC S9(3)   VALUE 20 COMP-3.             
008300 77  WS-DP-ERROR                 PIC X(8)    VALUE 'DP-ERROR'.            
008400                                                                          
008500 77  ARGUMENTS-SW                PIC X       VALUE 'J'.                   
008600     88  ARGUMENTS-OK                        VALUE 'J'.                   
008700     88  ARGUMENTS-WRONG                     VALUE 'N'.                   
008800                                                                          
008900 77  FIRST-LOOP-SW               PIC X       VALUE 'J'.                   
009000     88  FIRST-LOOP                          VALUE 'J'.                   
009100     88  OTHER-LOOP                          VALUE 'N'.                   
009200                                                                          
009300 77  WS-FLRULEOK                 PIC X       VALUE SPACE.                 
009400     88  RULE-NOT-OK                         VALUE 'J'.                   
009500     88  RULE-OK                             VALUE 'N'.                   
009600                                                                          
009700 77  WS-FLRULEMULT               PIC X       VALUE SPACE.                 
009800     88  MULTIPLE-RULES                      VALUE 'J'.                   
009900     88  SINGLE-RULE                         VALUE 'N'.                   
010000                                                                          
010100 77  WZ11OUTX-OPEN-SW            PIC X       VALUE 'J'.                   
010200     88  WZ11OUTX-OPEN-OK                    VALUE 'J'.                   
010300     88  WZ11OUTX-OPEN-FAILED                VALUE 'N'.                   
010400                                                                          
010500 77  WZ11OUTX-PUT-SW             PIC X       VALUE 'J'.                   
010600     88  WZ11OUTX-PUT-OK                     VALUE 'J'.                   
010700     88  WZ11OUTX-PUT-FAILED                 VALUE 'N'.                   
010800                                                                          
010900 01  WS-IDCALL                   PIC S9(9)   COMP VALUE +0.               
011000 01  WS-KDFUNC                   PIC X(10)   VALUE SPACE.                 
011100 01  WS-KDRC                     PIC S9(9)   COMP VALUE +0.               
011200 01  WS-TEOUTDATA-L              PIC S9(9)   COMP VALUE +0.               
011300 01  WS-TEOUTDATA                PIC X(3000) VALUE SPACE.                 
011400                                                                          
011500 01  WS-CURRENT-DATE-X6          PIC X(6)    VALUE SPACE.                 
011600 01  WS-CURRENT-DATE             PIC S9(7)   VALUE ZERO COMP-3.           
011700 01  WS-TIDATE2                  PIC X(6)    VALUE SPACE.                 
011800 01  WS-CURRENT-TIME             PIC S9(9)   VALUE ZERO COMP-3.           
011900 01  WS-CURRENT-TIME2            PIC S9(7)   VALUE ZERO COMP-3.           
012000 01  WS-TZ4REKY-DUMMY            PIC X(15)   VALUE SPACE.                 
012100 01  WS-TZ4REDA-DUMMY            PIC X(15)   VALUE SPACE.                 
012200 01  WS-KVDAGAR-RESEND           PIC S9(3)   VALUE ZERO COMP-3.           
012300 01  WS-TEFAX-1                  PIC X(50)   VALUE SPACE.                 
012400 01  WS-TEFAX-2                  PIC X(50)   VALUE SPACE.                 
012500 01  WS-TEFAX-3                  PIC X(50)   VALUE SPACE.                 
012600 01  WS-TEFAX-4                  PIC X(50)   VALUE SPACE.                 
012700 01  WS-TEFAX-5                  PIC X(50)   VALUE SPACE.                 
012800 01  WS-IDMAIL-SENDER            PIC X(60)   VALUE SPACE.                 
012900 01  WS-IDOUTTYPE-SAVE           PIC X(15)   VALUE SPACE.                 
013000 01  WS-IDOUTREC-SAVE            PIC X(30)   VALUE SPACE.                 
013100 01  WS-IDOUTREC-TEMP            PIC X(30)   VALUE SPACE.                 
013200 01  WS-IDLIST-SAVE              PIC X(10)   VALUE SPACE.                 
013300 01  WS-IDOUTDEST-SAVE           PIC X(60)   VALUE SPACE.                 
013400                                                                          
013500 01  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
013600                                                                          
013700 01  WS-IDOUTTYPE-KEY            PIC X(15)   VALUE SPACE.                 
013800 01  WS-IDOUTREC-KEY             PIC X(30)   VALUE SPACE.                 
013900 01  WS-IDLIST-KEY               PIC X(10)   VALUE SPACE.                 
014000                                                                          
014100 01  TAB-DATA.                                                            
014200     03 TAB-OUT-DATA OCCURS 15.                                           
014300        05 TAB-KDOUTMETH         PIC X(4)    VALUE SPACE.                 
014400        05 TAB-IDOUTDEST         PIC X(60)   VALUE SPACE.                 
014500        05 TAB-KVCOPIES          PIC X       VALUE SPACE.                 
014600        05 TAB-IDPFDEF           PIC X(8)    VALUE SPACE.                 
014700        05 TAB-IDFORMSNM         PIC X(8)    VALUE SPACE.                 
014800        05 TAB-FLCARRCNTL        PIC X       VALUE SPACE.                 
014900        05 TAB-FLACIF            PIC X       VALUE SPACE.                 
015000        05 TAB-TEVCOMST          PIC X(20)   VALUE SPACE.                 
015100        05 TAB-IDVCINIT          PIC X(8)    VALUE SPACE.                 
015200        05 TAB-KVPOST            PIC S9(7)   VALUE ZERO COMP-3.           
015300                                                                          
015400 01  W-SAVE-IDLOPNR              PIC S9(3)   COMP-3.                      
015500                                                                          
015600 01  IX                          PIC 99      VALUE ZERO.                  
015700 01  IX-SAVE-NBR-OF-RULES        PIC 99      VALUE ZERO.                  
015800 01  MAX-INDEX                   PIC 99      VALUE 15.                    
015900                                                                          
016000*    --- WORK FIELDS FOR VARIABLE SUBSTITUTION IN RULE DATA               
016100 01  TIX1                        PIC S9(4)   BINARY.                      
016200 01  TIX2                        PIC S9(4)   BINARY.                      
016300 01  TIX3                        PIC S9(4)   BINARY.                      
016400 01  TIXS                        PIC S9(4)   BINARY.                      
016500 01  TIXL                        PIC S9(4)   BINARY.                      
016600 01  TINTERVAL                   PIC X(10).                               
016700 01  TDATE                       PIC 9(6)    BINARY.                      
016800 01  TTIME                       PIC 9(8)    BINARY.                      
016900 01  FROM-TEXT                   PIC X(100).                              
017000 01  TO-TEXT                     PIC X(100).                              
017100 01  TEMP-TEXT                   PIC X(100).                              
017200 01  SYMBVAL-TEXT                PIC X(50).                               
017300 01  SUBST-DONE                  PIC X.                                   
017400                                                                          
017500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
017600 01  GENERAL-SUBPROGRAMS.                                                 
017700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017900     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
018000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
018100     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
018200     03  WZ11OUTP                PIC X(8)    VALUE 'WZ11OUTP'.            
018300     03  WZ11OUTF                PIC X(8)    VALUE 'WZ11OUTF'.            
018400     03  WZ11OUTM                PIC X(8)    VALUE 'WZ11OUTM'.            
018500     03  WZ11OUTV                PIC X(8)    VALUE 'WZ11OUTV'.            
018600     03  WZ11OUTO                PIC X(8)    VALUE 'WZ11OUTO'.            
018700     03  WZ11OUTA                PIC X(8)    VALUE 'WZ11OUTA'.            
018800     03  WZ11OUTW                PIC X(8)    VALUE 'WZ11OUTW'.            
018900     SKIP3                                                                
019000                                                                          
019100*    --- PARAMETERS TO ABEND                                              
019200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
019300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
019400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
019500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
019600                                                                          
019700*    --- PARAMETERS TO VIMSID                                             
019800 77  WS-VIMSID                   PIC X(8)    VALUE SPACE.                 
019900     SKIP3                                                                
020000 01  MESSAGE-CODES.                                                       
020100     03  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.                 
020200     03  ERR-MULTIPLE-RULES      PIC X(3)    VALUE '200'.                 
020300     03  ERR-RULE-MISSING        PIC X(3)    VALUE '201'.                 
020400     03  ERR-PRINT-FAILED        PIC X(3)    VALUE '202'.                 
020500     03  ERR-FAX-FAILED          PIC X(3)    VALUE '203'.                 
020600     03  ERR-MAIL-FAILED         PIC X(3)    VALUE '204'.                 
020700     03  ERR-EDI-FAILED          PIC X(3)    VALUE '205'.                 
020800     03  ERR-VCOM-FAILED         PIC X(3)    VALUE '206'.                 
020900     03  ERR-ONDEMAND-FAILED     PIC X(3)    VALUE '207'.                 
021000     03  ERR-GET-IT-FAILED       PIC X(3)    VALUE '208'.                 
021100     03  ERR-WEB-FAILED          PIC X(3)    VALUE '209'.                 
021200 01  WS-ERROR-TYPE               PIC X(3)    VALUE SPACE.                 
021300                                                                          
021400 01  ERR-IDMSG-ERROR             PIC X(3).                                
021500 01  ERR-IDELMT-ERROR            PIC X(16).                               
021600     EJECT                                                                
021700                                                                          
021800 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
021900     SKIP3                                                                
022000*01  -COPY WZ01SEND                                                       
022100     EJECT                                                                
022200 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTF-AREA'.        
022300     SKIP3                                                                
022400*01 -COPY WZ11OUTF                                                        
022500 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTM-AREA'.        
022600     SKIP3                                                                
022700*01 -COPY WZ11OUTM                                                        
022800 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTO-AREA'.        
022900     SKIP3                                                                
023000*01 -COPY WZ11OUTO                                                        
023100 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTP-AREA'.        
023200     SKIP3                                                                
023300*01 -COPY WZ11OUTP                                                        
023400 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTV-AREA'.        
023500     SKIP3                                                                
023600*01 -COPY WZ11OUTV                                                        
023700 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTA-AREA'.        
023800     SKIP3                                                                
023900*01 -COPY WZ11OUTA                                                        
024000 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTW-AREA'.        
024100     SKIP3                                                                
024200*01 -COPY WZ11OUTW                                                        
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)   VALUE 'ERROR-AREA'.          
024500     SKIP3                                                                
024600 01  ERR2-REQU-AREA.                                                      
024700*    03  -COPY WZ01REQU   -PRE ERR2-                                      
024800*    03  -COPY WZ04HDR    -PRE ERR2-                                      
024900     EJECT                                                                
025000 01  FILLER                      PIC X(16)  VALUE 'WZ20DAYS-AREA'.        
025100     SKIP3                                                                
025200*    -COPY WZ20DAYS                                                       
025300     EJECT                                                                
025400                                                                          
025500 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
025600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
025700                                                                          
025800 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
025900 01  DB2-WS.                                                              
026000     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
026100         88  CURSOR-OK                       VALUE 000.                   
026200         88  LINES-FOUND                     VALUE 000.                   
026300         88  LINES-MISSING                   VALUE 100.                   
026400         88  RESOURCE-WRONG                  VALUE 904.                   
026500         88  MULTIPLE-LINES                  VALUE 811.                   
026600     03  GOOD-SQLCODECODES.                                               
026700         05  GOOD-SQLCODE OCCURS 5                                        
026800             INDEXED BY SQLCODE-IX PIC 9(3).                              
026900                                                                          
027000     EJECT                                                                
027100 01  FILLER                      PIC X(16)  VALUE 'TZ4DIRU-AREA'.         
027200                                                                          
027300*01  -COPY TZ4DIRU -PRE DIRU-                                             
027400     EJECT                                                                
027500 01  FILLER                      PIC X(16)  VALUE 'TZ4REKY-AREA'.         
027600                                                                          
027700*01  -COPY TZ4REKY -PRE REKY-                                             
027800     EJECT                                                                
027900 01  FILLER                      PIC X(16)  VALUE 'TZ4REDA-AREA'.         
028000                                                                          
028100*01  -COPY TZ4REDA -PRE REDA-                                             
028200     EJECT                                                                
028300                                                                          
028400     EJECT                                                                
028500     EXEC SQL INCLUDE TZ4DIRU END-EXEC.                                   
028600     EJECT                                                                
028700     EXEC SQL INCLUDE TZ4REKY END-EXEC.                                   
028800     EJECT                                                                
028900     EXEC SQL INCLUDE TZ4REDA END-EXEC.                                   
029000     EJECT                                                                
029100                                                                          
029200 LINKAGE SECTION.                                                         
029300                                                                          
029400*01  -COPY WZ04DAP                                                        
029500     EJECT                                                                
029600                                                                          
029700 PROCEDURE DIVISION USING DAP-WZ04DAP.                                    
029800 MAIN SECTION.                                                            
029900                                                                          
030000     PERFORM A-INIT-CHECK-ARGUMENTS                                       
030100     IF ARGUMENTS-OK                                                      
030200                                                                          
030300       EVALUATE DAP-KDFUNC                                                
030400       WHEN 'OPEN'                                                        
030500         PERFORM B-OPEN-INIT                                              
030600         PERFORM C-FETCH-DISTRIB-RULE                                     
030700                                                                          
030800       WHEN 'PUT'                                                         
030900*        - DATA IS SAVED, BUT NOT DISTRIBUTED, IF RULE IS MISSING         
031000         IF RULE-OK                                                       
031100         AND WZ11OUTX-OPEN-OK                                             
031200           PERFORM FA-SEND-DISTRIBUTE-DATA                                
031300         END-IF                                                           
031400         PERFORM FB-HANDLE-RESTART-TABLES                                 
031500                                                                          
031600       WHEN 'CLOSE'                                                       
031700         IF RULE-NOT-OK                                                   
031800           MOVE ERR-RULE-MISSING       TO ERR-IDMSG-ERROR                 
031900           MOVE 'MISSING RULE'         TO ERR-IDELMT-ERROR                
032000           IF MULTIPLE-RULES                                              
032100             MOVE ERR-MULTIPLE-RULES   TO ERR-IDMSG-ERROR                 
032200             MOVE 'MULTIPLE RULES'     TO ERR-IDELMT-ERROR                
032300           END-IF                                                         
032400           PERFORM G-HANDLE-ERROR                                         
032500         END-IF                                                           
032600         IF WZ11OUTX-OPEN-FAILED OR WZ11OUTX-PUT-FAILED                   
032700*          - TYPE OF ERROR IS SET IN S04-DISTRIBUTE                       
032800           PERFORM G-HANDLE-ERROR                                         
032900         END-IF                                                           
033000         IF WZ11OUTX-OPEN-OK                                              
033100           PERFORM FC-CLOSE-DISTRIB-CHANNELS                              
033200         END-IF                                                           
033300                                                                          
033400       END-EVALUATE                                                       
033500                                                                          
033600     ELSE                                                                 
033700       MOVE ERR-INVALID-KEY TO ERR-IDMSG-ERROR                            
033800       MOVE 'INVALID ARG DATA' TO ERR-IDELMT-ERROR                        
033900       PERFORM G-HANDLE-ERROR                                             
034000     END-IF                                                               
034100                                                                          
034200     MOVE ZERO TO RETURN-CODE                                             
034300     GOBACK                                                               
034400     .                                                                    
034500     EJECT                                                                
034600 A-INIT-CHECK-ARGUMENTS SECTION.                                          
034700                                                                          
034800     MOVE YES TO ARGUMENTS-SW                                             
034900     MOVE SPACE TO DAP-BEFEL                                              
035000                                                                          
035100     EVALUATE DAP-KDFUNC                                                  
035200     WHEN 'OPEN'                                                          
035300*      -- GENERATE A DUMMY IDCALL NUMBER                                  
035400*      -- MULTIPLE PARALLEL DISTRIBUTIONS NOT YET SUPPORTED.              
035500       MOVE 1 TO DAP-IDCALL                                               
035600                                                                          
035700*      -- OUTPUT TYPE MUST BE SPECIFIED, OTHER FIELDS ARE OPTIONAL        
035800       IF DAP-IDOUTTYPE = SPACE                                           
035900         MOVE NOO TO ARGUMENTS-SW                                         
036000         MOVE 8 TO DAP-KDRC                                               
036100       END-IF                                                             
036200                                                                          
036300*      -- SET SWITCH TO YES ON 'OPEN' AS ITS A NEW RULE/KEY NOW           
036400       MOVE YES TO FIRST-LOOP-SW                                          
036500                                                                          
036600     WHEN 'PUT'                                                           
036700       IF DAP-KVDLEN < ZERO OR > LENGTH OF DAP-TEOUTDATA                  
036800         MOVE NOO TO ARGUMENTS-SW                                         
036900         MOVE 8 TO DAP-KDRC                                               
037000       END-IF                                                             
037100       IF DAP-IDCALL NOT = 1                                              
037200*        -- ONLY SINGLE DISTRIBUTIONS CURRENTLY SUPPORTED                 
037300         MOVE NOO TO ARGUMENTS-SW                                         
037400         MOVE 8 TO DAP-KDRC                                               
037500       END-IF                                                             
037600                                                                          
037700     WHEN 'CLOSE'                                                         
037800       IF DAP-IDCALL NOT = 1                                              
037900*        -- ONLY SINGLE DISTRIBUTIONS CURRENTLY SUPPORTED                 
038000         MOVE NOO TO ARGUMENTS-SW                                         
038100         MOVE 8 TO DAP-KDRC                                               
038200       END-IF                                                             
038300                                                                          
038400     WHEN OTHER                                                           
038500       MOVE NOO TO ARGUMENTS-SW                                           
038600       MOVE 8 TO DAP-KDRC                                                 
038700     END-EVALUATE                                                         
038800     .                                                                    
038900     EJECT                                                                
039000 B-OPEN-INIT SECTION.                                                     
039100                                                                          
039200     INITIALIZE GOOD-SQLCODECODES                                         
039300     INITIALIZE REKY-TZ4REKY                                              
039400     INITIALIZE REDA-TZ4REDA                                              
039500     MOVE FUNCTION CURRENT-DATE (3:6) TO WS-CURRENT-DATE                  
039600                                         WS-CURRENT-DATE-X6               
039700     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-CURRENT-TIME                  
039800     MOVE FUNCTION CURRENT-DATE (9:6) TO WS-CURRENT-TIME2                 
039900                                                                          
040000     .                                                                    
040100     EJECT                                                                
040200 C-FETCH-DISTRIB-RULE SECTION.                                            
040300                                                                          
040400*    -- SAVE KEY-DATA FOR LATER CALLS                                     
040500     MOVE DAP-IDOUTTYPE   TO WS-IDOUTTYPE-KEY                             
040600     MOVE DAP-IDOUTREC    TO WS-IDOUTREC-KEY                              
040700     MOVE DAP-IDLIST      TO WS-IDLIST-KEY                                
040800                                                                          
040900*    -- SAVE ANY OVERRIDING VALUE FOR DEST                                
041000     MOVE DAP-IDOUTDEST   TO WS-IDOUTDEST-SAVE                            
041100                                                                          
041200*    -- USE KEY-DATA TO SEARCH FOR A DISTRIBUTION RULE                    
041300     PERFORM CA-CLEAR-RULE-TABLE                                          
041400     PERFORM DB2-SELECT-TZ4DIRU-TAB                                       
041500     IF LINES-FOUND                                                       
041600       MOVE NOO TO WS-FLRULEOK                                            
041700       PERFORM CB-BUILD-RULE-TABLE                                        
041800       PERFORM CC-OPEN-DISTRIB-CHANNELS                                   
041900       PERFORM DB2-UPDATE-TZ4DIRU-TIANVDAT                                
042000     ELSE                                                                 
042100       MOVE YES TO WS-FLRULEOK                                            
042200       MOVE 1 TO IX-SAVE-NBR-OF-RULES                                     
042300     END-IF                                                               
042400     .                                                                    
042500     SKIP3                                                                
042600 CA-CLEAR-RULE-TABLE   SECTION.                                           
042700                                                                          
042800     MOVE SPACE   TO WS-TEFAX-1                                           
042900                     WS-TEFAX-2                                           
043000                     WS-TEFAX-3                                           
043100                     WS-TEFAX-4                                           
043200                     WS-TEFAX-5                                           
043300                                                                          
043400     MOVE 1 TO IX                                                         
043500     PERFORM UNTIL IX > MAX-INDEX                                         
043600       MOVE SPACE TO TAB-KDOUTMETH(IX)                                    
043700                     TAB-IDOUTDEST(IX)                                    
043800                     TAB-KVCOPIES(IX)                                     
043900                     TAB-IDPFDEF(IX)                                      
044000                     TAB-IDFORMSNM(IX)                                    
044100                     TAB-FLCARRCNTL(IX)                                   
044200                     TAB-FLACIF(IX)                                       
044300                     TAB-TEVCOMST(IX)                                     
044400                     TAB-IDVCINIT(IX)                                     
044500       MOVE ZERO TO  TAB-KVPOST(IX)                                       
044600       ADD 1 TO IX                                                        
044700     END-PERFORM                                                          
044800     MOVE ZERO TO IX-SAVE-NBR-OF-RULES                                    
044900     MOVE ZERO TO W-SAVE-IDLOPNR                                          
045000     .                                                                    
045100     EJECT                                                                
045200 CB-BUILD-RULE-TABLE SECTION.                                             
045300                                                                          
045400*    -- COPY RULE DATA TO AN INTERNAL TABLE TO MAKE IT POSSIBLE           
045500*    -- TO LOOP OVER THE RULE LINES.                                      
045600*    -- (TEFAX AND IDMAIL-ENDER DATA ARE DIFFERENT -                      
045700*    --  THEY ARE USED WITH MANY LINES)                                   
045800                                                                          
045900     MOVE DIRU-TEFAX-1       TO WS-TEFAX-1                                
046000     MOVE DIRU-TEFAX-2       TO WS-TEFAX-2                                
046100     MOVE DIRU-TEFAX-3       TO WS-TEFAX-3                                
046200     MOVE DIRU-TEFAX-4       TO WS-TEFAX-4                                
046300     MOVE DIRU-TEFAX-5       TO WS-TEFAX-5                                
046400     MOVE DIRU-IDMAIL-SENDER TO WS-IDMAIL-SENDER                          
046500                                                                          
046600     MOVE DIRU-KDOUTMETH-1   TO TAB-KDOUTMETH(1)                          
046700     MOVE DIRU-KDOUTMETH-2   TO TAB-KDOUTMETH(2)                          
046800     MOVE DIRU-KDOUTMETH-3   TO TAB-KDOUTMETH(3)                          
046900     MOVE DIRU-KDOUTMETH-4   TO TAB-KDOUTMETH(4)                          
047000     MOVE DIRU-KDOUTMETH-5   TO TAB-KDOUTMETH(5)                          
047100     MOVE DIRU-KDOUTMETH-6   TO TAB-KDOUTMETH(6)                          
047200     MOVE DIRU-KDOUTMETH-7   TO TAB-KDOUTMETH(7)                          
047300     MOVE DIRU-KDOUTMETH-8   TO TAB-KDOUTMETH(8)                          
047400     MOVE DIRU-KDOUTMETH-9   TO TAB-KDOUTMETH(9)                          
047500     MOVE DIRU-KDOUTMETH-10  TO TAB-KDOUTMETH(10)                         
047600     MOVE DIRU-KDOUTMETH-11  TO TAB-KDOUTMETH(11)                         
047700     MOVE DIRU-KDOUTMETH-12  TO TAB-KDOUTMETH(12)                         
047800     MOVE DIRU-KDOUTMETH-13  TO TAB-KDOUTMETH(13)                         
047900     MOVE DIRU-KDOUTMETH-14  TO TAB-KDOUTMETH(14)                         
048000     MOVE DIRU-KDOUTMETH-15  TO TAB-KDOUTMETH(15)                         
048100                                                                          
048200*    -- DESTINATION IN RULE WILL BE REPLACED BY VALUE                     
048300*    -- GIVEN IN THE CALL - IF THE VALUE IN RULE IS "?"                   
048400*    -- AND A REPLACING VALUE HAS BEEN SPECIFIED.                         
048500*    -- THIS MAKES IT POSSIBLE TO HAVE FIXED DEESTINATIONS ON             
048600*    -- SOME RULE LINES AND A DYNAMIC VALUE VIA CALL ON OTHER             
048700*    -- LINES.                                                            
048800     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-1 = '?'          
048900       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(1)                         
049000     ELSE                                                                 
049100       MOVE DIRU-IDOUTDEST-1  TO TAB-IDOUTDEST(1)                         
049200     END-IF                                                               
049300     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-2 = '?'          
049400       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(2)                         
049500     ELSE                                                                 
049600       MOVE DIRU-IDOUTDEST-2  TO TAB-IDOUTDEST(2)                         
049700     END-IF                                                               
049800     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-3 = '?'          
049900       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(3)                         
050000     ELSE                                                                 
050100       MOVE DIRU-IDOUTDEST-3  TO TAB-IDOUTDEST(3)                         
050200     END-IF                                                               
050300     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-4 = '?'          
050400       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(4)                         
050500     ELSE                                                                 
050600       MOVE DIRU-IDOUTDEST-4  TO TAB-IDOUTDEST(4)                         
050700     END-IF                                                               
050800     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-5 = '?'          
050900       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(5)                         
051000     ELSE                                                                 
051100       MOVE DIRU-IDOUTDEST-5  TO TAB-IDOUTDEST(5)                         
051200     END-IF                                                               
051300     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-6 = '?'          
051400       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(6)                         
051500     ELSE                                                                 
051600       MOVE DIRU-IDOUTDEST-6  TO TAB-IDOUTDEST(6)                         
051700     END-IF                                                               
051800     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-7 = '?'          
051900       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(7)                         
052000     ELSE                                                                 
052100       MOVE DIRU-IDOUTDEST-7  TO TAB-IDOUTDEST(7)                         
052200     END-IF                                                               
052300     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-8 = '?'          
052400       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(8)                         
052500     ELSE                                                                 
052600       MOVE DIRU-IDOUTDEST-8  TO TAB-IDOUTDEST(8)                         
052700     END-IF                                                               
052800     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-9 = '?'          
052900       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(9)                         
053000     ELSE                                                                 
053100       MOVE DIRU-IDOUTDEST-9  TO TAB-IDOUTDEST(9)                         
053200     END-IF                                                               
053300     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-10 = '?'         
053400       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(10)                        
053500     ELSE                                                                 
053600       MOVE DIRU-IDOUTDEST-10 TO TAB-IDOUTDEST(10)                        
053700     END-IF                                                               
053800     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-11 = '?'         
053900       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(11)                        
054000     ELSE                                                                 
054100       MOVE DIRU-IDOUTDEST-11 TO TAB-IDOUTDEST(11)                        
054200     END-IF                                                               
054300     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-12 = '?'         
054400       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(12)                        
054500     ELSE                                                                 
054600       MOVE DIRU-IDOUTDEST-12 TO TAB-IDOUTDEST(12)                        
054700     END-IF                                                               
054800     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-13 = '?'         
054900       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(13)                        
055000     ELSE                                                                 
055100       MOVE DIRU-IDOUTDEST-13 TO TAB-IDOUTDEST(13)                        
055200     END-IF                                                               
055300     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-14 = '?'         
055400       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(14)                        
055500     ELSE                                                                 
055600       MOVE DIRU-IDOUTDEST-14 TO TAB-IDOUTDEST(14)                        
055700     END-IF                                                               
055800     IF WS-IDOUTDEST-SAVE NOT = SPACE AND DIRU-IDOUTDEST-15 = '?'         
055900       MOVE WS-IDOUTDEST-SAVE TO TAB-IDOUTDEST(15)                        
056000     ELSE                                                                 
056100       MOVE DIRU-IDOUTDEST-15 TO TAB-IDOUTDEST(15)                        
056200     END-IF                                                               
056300                                                                          
056400     MOVE DIRU-KVCOPIES-1    TO TAB-KVCOPIES(1)                           
056500     MOVE DIRU-KVCOPIES-2    TO TAB-KVCOPIES(2)                           
056600     MOVE DIRU-KVCOPIES-3    TO TAB-KVCOPIES(3)                           
056700     MOVE DIRU-KVCOPIES-4    TO TAB-KVCOPIES(4)                           
056800     MOVE DIRU-KVCOPIES-5    TO TAB-KVCOPIES(5)                           
056900     MOVE DIRU-KVCOPIES-6    TO TAB-KVCOPIES(6)                           
057000     MOVE DIRU-KVCOPIES-7    TO TAB-KVCOPIES(7)                           
057100     MOVE DIRU-KVCOPIES-8    TO TAB-KVCOPIES(8)                           
057200     MOVE DIRU-KVCOPIES-9    TO TAB-KVCOPIES(9)                           
057300     MOVE DIRU-KVCOPIES-10   TO TAB-KVCOPIES(10)                          
057400     MOVE DIRU-KVCOPIES-11   TO TAB-KVCOPIES(11)                          
057500     MOVE DIRU-KVCOPIES-12   TO TAB-KVCOPIES(12)                          
057600     MOVE DIRU-KVCOPIES-13   TO TAB-KVCOPIES(13)                          
057700     MOVE DIRU-KVCOPIES-14   TO TAB-KVCOPIES(14)                          
057800     MOVE DIRU-KVCOPIES-15   TO TAB-KVCOPIES(15)                          
057900     MOVE DIRU-IDPFDEF-1     TO TAB-IDPFDEF(1)                            
058000     MOVE DIRU-IDPFDEF-2     TO TAB-IDPFDEF(2)                            
058100     MOVE DIRU-IDPFDEF-3     TO TAB-IDPFDEF(3)                            
058200     MOVE DIRU-IDPFDEF-4     TO TAB-IDPFDEF(4)                            
058300     MOVE DIRU-IDPFDEF-5     TO TAB-IDPFDEF(5)                            
058400     MOVE DIRU-IDPFDEF-6     TO TAB-IDPFDEF(6)                            
058500     MOVE DIRU-IDPFDEF-7     TO TAB-IDPFDEF(7)                            
058600     MOVE DIRU-IDPFDEF-8     TO TAB-IDPFDEF(8)                            
058700     MOVE DIRU-IDPFDEF-9     TO TAB-IDPFDEF(9)                            
058800     MOVE DIRU-IDPFDEF-10    TO TAB-IDPFDEF(10)                           
058900     MOVE DIRU-IDPFDEF-11    TO TAB-IDPFDEF(11)                           
059000     MOVE DIRU-IDPFDEF-12    TO TAB-IDPFDEF(12)                           
059100     MOVE DIRU-IDPFDEF-13    TO TAB-IDPFDEF(13)                           
059200     MOVE DIRU-IDPFDEF-14    TO TAB-IDPFDEF(14)                           
059300     MOVE DIRU-IDPFDEF-15    TO TAB-IDPFDEF(15)                           
059400     MOVE DIRU-IDFORMSNM-1   TO TAB-IDFORMSNM(1)                          
059500     MOVE DIRU-IDFORMSNM-2   TO TAB-IDFORMSNM(2)                          
059600     MOVE DIRU-IDFORMSNM-3   TO TAB-IDFORMSNM(3)                          
059700     MOVE DIRU-IDFORMSNM-4   TO TAB-IDFORMSNM(4)                          
059800     MOVE DIRU-IDFORMSNM-5   TO TAB-IDFORMSNM(5)                          
059900     MOVE DIRU-IDFORMSNM-6   TO TAB-IDFORMSNM(6)                          
060000     MOVE DIRU-IDFORMSNM-7   TO TAB-IDFORMSNM(7)                          
060100     MOVE DIRU-IDFORMSNM-8   TO TAB-IDFORMSNM(8)                          
060200     MOVE DIRU-IDFORMSNM-9   TO TAB-IDFORMSNM(9)                          
060300     MOVE DIRU-IDFORMSNM-10  TO TAB-IDFORMSNM(10)                         
060400     MOVE DIRU-IDFORMSNM-11  TO TAB-IDFORMSNM(11)                         
060500     MOVE DIRU-IDFORMSNM-12  TO TAB-IDFORMSNM(12)                         
060600     MOVE DIRU-IDFORMSNM-13  TO TAB-IDFORMSNM(13)                         
060700     MOVE DIRU-IDFORMSNM-14  TO TAB-IDFORMSNM(14)                         
060800     MOVE DIRU-IDFORMSNM-15  TO TAB-IDFORMSNM(15)                         
060900     MOVE DIRU-FLCARRCNTL-1  TO TAB-FLCARRCNTL(1)                         
061000     MOVE DIRU-FLCARRCNTL-2  TO TAB-FLCARRCNTL(2)                         
061100     MOVE DIRU-FLCARRCNTL-3  TO TAB-FLCARRCNTL(3)                         
061200     MOVE DIRU-FLCARRCNTL-4  TO TAB-FLCARRCNTL(4)                         
061300     MOVE DIRU-FLCARRCNTL-5  TO TAB-FLCARRCNTL(5)                         
061400     MOVE DIRU-FLCARRCNTL-6  TO TAB-FLCARRCNTL(6)                         
061500     MOVE DIRU-FLCARRCNTL-7  TO TAB-FLCARRCNTL(7)                         
061600     MOVE DIRU-FLCARRCNTL-8  TO TAB-FLCARRCNTL(8)                         
061700     MOVE DIRU-FLCARRCNTL-9  TO TAB-FLCARRCNTL(9)                         
061800     MOVE DIRU-FLCARRCNTL-10 TO TAB-FLCARRCNTL(10)                        
061900     MOVE DIRU-FLCARRCNTL-11 TO TAB-FLCARRCNTL(11)                        
062000     MOVE DIRU-FLCARRCNTL-12 TO TAB-FLCARRCNTL(12)                        
062100     MOVE DIRU-FLCARRCNTL-13 TO TAB-FLCARRCNTL(13)                        
062200     MOVE DIRU-FLCARRCNTL-14 TO TAB-FLCARRCNTL(14)                        
062300     MOVE DIRU-FLCARRCNTL-15 TO TAB-FLCARRCNTL(15)                        
062400     MOVE DIRU-FLACIF-1      TO TAB-FLACIF(1)                             
062500     MOVE DIRU-FLACIF-2      TO TAB-FLACIF(2)                             
062600     MOVE DIRU-FLACIF-3      TO TAB-FLACIF(3)                             
062700     MOVE DIRU-FLACIF-4      TO TAB-FLACIF(4)                             
062800     MOVE DIRU-FLACIF-5      TO TAB-FLACIF(5)                             
062900     MOVE DIRU-FLACIF-6      TO TAB-FLACIF(6)                             
063000     MOVE DIRU-FLACIF-7      TO TAB-FLACIF(7)                             
063100     MOVE DIRU-FLACIF-8      TO TAB-FLACIF(8)                             
063200     MOVE DIRU-FLACIF-9      TO TAB-FLACIF(9)                             
063300     MOVE DIRU-FLACIF-10     TO TAB-FLACIF(10)                            
063400     MOVE DIRU-FLACIF-11     TO TAB-FLACIF(11)                            
063500     MOVE DIRU-FLACIF-12     TO TAB-FLACIF(12)                            
063600     MOVE DIRU-FLACIF-13     TO TAB-FLACIF(13)                            
063700     MOVE DIRU-FLACIF-14     TO TAB-FLACIF(14)                            
063800     MOVE DIRU-FLACIF-15     TO TAB-FLACIF(15)                            
063900     MOVE DIRU-TEVCOMST-1    TO TAB-TEVCOMST(1)                           
064000     MOVE DIRU-TEVCOMST-2    TO TAB-TEVCOMST(2)                           
064100     MOVE DIRU-TEVCOMST-3    TO TAB-TEVCOMST(3)                           
064200     MOVE DIRU-TEVCOMST-4    TO TAB-TEVCOMST(4)                           
064300     MOVE DIRU-TEVCOMST-5    TO TAB-TEVCOMST(5)                           
064400     MOVE DIRU-TEVCOMST-6    TO TAB-TEVCOMST(6)                           
064500     MOVE DIRU-TEVCOMST-7    TO TAB-TEVCOMST(7)                           
064600     MOVE DIRU-TEVCOMST-8    TO TAB-TEVCOMST(8)                           
064700     MOVE DIRU-TEVCOMST-9    TO TAB-TEVCOMST(9)                           
064800     MOVE DIRU-TEVCOMST-10   TO TAB-TEVCOMST(10)                          
064900     MOVE DIRU-TEVCOMST-11   TO TAB-TEVCOMST(11)                          
065000     MOVE DIRU-TEVCOMST-12   TO TAB-TEVCOMST(12)                          
065100     MOVE DIRU-TEVCOMST-13   TO TAB-TEVCOMST(13)                          
065200     MOVE DIRU-TEVCOMST-14   TO TAB-TEVCOMST(14)                          
065300     MOVE DIRU-TEVCOMST-15   TO TAB-TEVCOMST(15)                          
065400     MOVE DIRU-IDVCINIT-1    TO TAB-IDVCINIT(1)                           
065500     MOVE DIRU-IDVCINIT-2    TO TAB-IDVCINIT(2)                           
065600     MOVE DIRU-IDVCINIT-3    TO TAB-IDVCINIT(3)                           
065700     MOVE DIRU-IDVCINIT-4    TO TAB-IDVCINIT(4)                           
065800     MOVE DIRU-IDVCINIT-5    TO TAB-IDVCINIT(5)                           
065900     MOVE DIRU-IDVCINIT-6    TO TAB-IDVCINIT(6)                           
066000     MOVE DIRU-IDVCINIT-7    TO TAB-IDVCINIT(7)                           
066100     MOVE DIRU-IDVCINIT-8    TO TAB-IDVCINIT(8)                           
066200     MOVE DIRU-IDVCINIT-9    TO TAB-IDVCINIT(9)                           
066300     MOVE DIRU-IDVCINIT-10   TO TAB-IDVCINIT(10)                          
066400     MOVE DIRU-IDVCINIT-11   TO TAB-IDVCINIT(11)                          
066500     MOVE DIRU-IDVCINIT-12   TO TAB-IDVCINIT(12)                          
066600     MOVE DIRU-IDVCINIT-13   TO TAB-IDVCINIT(13)                          
066700     MOVE DIRU-IDVCINIT-14   TO TAB-IDVCINIT(14)                          
066800     MOVE DIRU-IDVCINIT-15   TO TAB-IDVCINIT(15)                          
066900                                                                          
067000     PERFORM CBA-SUBSTITUTE-SYMBOLS                                       
067100     .                                                                    
067200     EJECT                                                                
067300 CBA-SUBSTITUTE-SYMBOLS    SECTION.                                       
067400                                                                          
067500     IF WS-TEFAX-1 NOT = SPACE                                            
067600       MOVE WS-TEFAX-1 TO FROM-TEXT                                       
067700       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
067800       IF SUBST-DONE = YES                                                
067900         MOVE TO-TEXT TO WS-TEFAX-1                                       
068000       END-IF                                                             
068100     END-IF                                                               
068200                                                                          
068300     IF WS-TEFAX-2 NOT = SPACE                                            
068400       MOVE WS-TEFAX-2 TO FROM-TEXT                                       
068500       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
068600       IF SUBST-DONE = YES                                                
068700         MOVE TO-TEXT TO WS-TEFAX-2                                       
068800       END-IF                                                             
068900     END-IF                                                               
069000                                                                          
069100     IF WS-TEFAX-3 NOT = SPACE                                            
069200       MOVE WS-TEFAX-3 TO FROM-TEXT                                       
069300       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
069400       IF SUBST-DONE = YES                                                
069500         MOVE TO-TEXT TO WS-TEFAX-3                                       
069600       END-IF                                                             
069700     END-IF                                                               
069800                                                                          
069900     IF WS-TEFAX-4 NOT = SPACE                                            
070000       MOVE WS-TEFAX-4 TO FROM-TEXT                                       
070100       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
070200       IF SUBST-DONE = YES                                                
070300         MOVE TO-TEXT TO WS-TEFAX-4                                       
070400       END-IF                                                             
070500     END-IF                                                               
070600                                                                          
070700     IF WS-TEFAX-5 NOT = SPACE                                            
070800       MOVE WS-TEFAX-5 TO FROM-TEXT                                       
070900       PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                  
071000       IF SUBST-DONE = YES                                                
071100         MOVE TO-TEXT TO WS-TEFAX-5                                       
071200       END-IF                                                             
071300     END-IF                                                               
071400                                                                          
071500     MOVE 1 TO IX                                                         
071600     PERFORM UNTIL IX > MAX-INDEX                                         
071700       IF TAB-TEVCOMST(IX) NOT = SPACE                                    
071800         MOVE TAB-TEVCOMST(IX) TO FROM-TEXT                               
071900         PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                
072000         IF SUBST-DONE = YES                                              
072100           MOVE TO-TEXT TO TAB-TEVCOMST(IX)                               
072200         END-IF                                                           
072300       END-IF                                                             
072400       IF TAB-IDPFDEF(IX) NOT = SPACE                                     
072500         MOVE TAB-IDPFDEF(IX) TO FROM-TEXT                                
072600         PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                
072700         IF SUBST-DONE = YES                                              
072800           MOVE TO-TEXT TO TAB-IDPFDEF(IX)                                
072900         END-IF                                                           
073000       END-IF                                                             
073100       IF TAB-IDOUTDEST(IX) NOT = SPACE                                   
073200         MOVE TAB-IDOUTDEST(IX) TO FROM-TEXT                              
073300         PERFORM CBAA-CHECK-SUBSTITUTE-ONE                                
073400         IF SUBST-DONE = YES                                              
073500           MOVE TO-TEXT TO TAB-IDOUTDEST(IX)                              
073600         END-IF                                                           
073700       END-IF                                                             
073800       ADD 1 TO IX                                                        
073900     END-PERFORM                                                          
074000                                                                          
074100     .                                                                    
074200     EJECT                                                                
074300 CBAA-CHECK-SUBSTITUTE-ONE   SECTION.                                     
074400                                                                          
074500*    -- SYMBOLIC EXPRESSIONS IN THE FORM &A OR &A(S-E) MAY OCCUR          
074600*    -- "&A" MAY BE ONE OF THE FOLLOWING:                                 
074700*    --      &I = THE VALUE OF IDLIST (OUTPUT ID)                         
074800*    --      &S = THE VALUE OF IDOUTREC (OUTPUT SUB TYPE)                 
074900*    --      &O = THE VALUE OF IDOUTTYPE (OUTPUT TYPE)                    
075000*    --      &D = THE VALUE OF CURRENT DATE (YYMMDD)                      
075100*    --      &T = THE VALUE OF CURRENT TIME STAMP (HHMMSSHH)              
075200*    -- (S-E) SPECIFIES A SUB-STRING OF THE SPECIFIED VALUE               
075300*    -- WHERE "S" IS THE START POSITION AND "E" IS THE END                
075400*    -- POSITION OF THE SUB-STRING                                        
075500     MOVE NOO TO SUBST-DONE                                               
075600     MOVE 1 TO TIX1                                                       
075700     INSPECT FROM-TEXT TALLYING TIX1                                      
075800             FOR CHARACTERS BEFORE INITIAL '&'                            
075900     PERFORM UNTIL TIX1 >= LENGTH OF FROM-TEXT                            
076000       MOVE FROM-TEXT TO TEMP-TEXT                                        
076100       IF FROM-TEXT(TIX1:2) = '&I'                                        
076200         MOVE WS-IDLIST-KEY TO SYMBVAL-TEXT                               
076300         MOVE LENGTH OF WS-IDLIST-KEY TO TIXL                             
076400         PERFORM CBAAA-SUBST-SYMBOL                                       
076500       END-IF                                                             
076600       IF FROM-TEXT(TIX1:2) = '&S'                                        
076700         MOVE WS-IDOUTREC-KEY TO SYMBVAL-TEXT                             
076800         MOVE LENGTH OF WS-IDOUTREC-KEY TO TIXL                           
076900         PERFORM CBAAA-SUBST-SYMBOL                                       
077000       END-IF                                                             
077100       IF FROM-TEXT(TIX1:2) = '&O'                                        
077200         MOVE WS-IDOUTTYPE-KEY   TO SYMBVAL-TEXT                          
077300         MOVE LENGTH OF WS-IDOUTTYPE-KEY TO TIXL                          
077400         PERFORM CBAAA-SUBST-SYMBOL                                       
077500       END-IF                                                             
077600       IF FROM-TEXT(TIX1:2) = '&D'                                        
077700         MOVE WS-CURRENT-DATE TO TDATE                                    
077800         MOVE TDATE           TO SYMBVAL-TEXT                             
077900         MOVE 6               TO TIXL                                     
078000         PERFORM CBAAA-SUBST-SYMBOL                                       
078100       END-IF                                                             
078200       IF FROM-TEXT(TIX1:2) = '&T'                                        
078300         MOVE WS-CURRENT-TIME TO TTIME                                    
078400         MOVE TTIME           TO SYMBVAL-TEXT                             
078500         MOVE 8               TO TIXL                                     
078600         PERFORM CBAAA-SUBST-SYMBOL                                       
078700       END-IF                                                             
078800                                                                          
078900       ADD 1 TO TIX1                                                      
079000       INSPECT FROM-TEXT(TIX1:) TALLYING TIX1                             
079100               FOR CHARACTERS BEFORE INITIAL '&'                          
079200     END-PERFORM                                                          
079300                                                                          
079400     .                                                                    
079500     EJECT                                                                
079600 CBAAA-SUBST-SYMBOL        SECTION.                                       
079700                                                                          
079800     IF TEMP-TEXT(TIX1 + 2:1) = '('                                       
079900       COMPUTE TIX2 = TIX1 + 3                                            
080000       INSPECT TEMP-TEXT(TIX2:) TALLYING TIX2                             
080100               FOR CHARACTERS BEFORE ')'                                  
080200       MOVE TEMP-TEXT(TIX1 + 3:TIX2 - TIX1 - 3) TO TINTERVAL              
080300       MOVE ZERO TO TIX3                                                  
080400       INSPECT TINTERVAL TALLYING TIX3                                    
080500               FOR CHARACTERS BEFORE '-'                                  
080600       COMPUTE TIXS = FUNCTION NUMVAL(TINTERVAL(1:TIX3))                  
080700       COMPUTE TIXL = FUNCTION NUMVAL(TINTERVAL(TIX3 + 2:))               
080800       COMPUTE TIXL = TIXL - TIXS + 1                                     
080900     ELSE                                                                 
081000       MOVE 1 TO TIXS                                                     
081100*      -- STRIP TRAILING BLANKS                                           
081200       PERFORM UNTIL SYMBVAL-TEXT(TIXL:1) NOT = SPACE                     
081300         SUBTRACT 1 FROM TIXL                                             
081400       END-PERFORM                                                        
081500       COMPUTE TIX2 = TIX1 + 1                                            
081600     END-IF                                                               
081700     IF TIXS > 0 AND TIXL > 0                                             
081800       IF TIX1 > 1                                                        
081900         STRING TEMP-TEXT(1:TIX1 - 1) DELIMITED BY SIZE                   
082000                SYMBVAL-TEXT(TIXS:TIXL) DELIMITED BY SIZE                 
082100                TEMP-TEXT(TIX2 + 1:)  DELIMITED BY SIZE                   
082200           INTO TO-TEXT                                                   
082300       ELSE                                                               
082400         STRING SYMBVAL-TEXT(TIXS:TIXL) DELIMITED BY SIZE                 
082500                TEMP-TEXT(TIX2 + 1:)  DELIMITED BY SIZE                   
082600           INTO TO-TEXT                                                   
082700       END-IF                                                             
082800     END-IF                                                               
082900                                                                          
083000     MOVE TO-TEXT TO FROM-TEXT                                            
083100     MOVE YES TO SUBST-DONE                                               
083200     .                                                                    
083300     EJECT                                                                
083400 CC-OPEN-DISTRIB-CHANNELS  SECTION.                                       
083500                                                                          
083600     PERFORM CCA-COMPUTE-RESTART-KEYS                                     
083700                                                                          
083800     MOVE ZERO                  TO WS-KDRC                                
083900     MOVE YES                   TO WZ11OUTX-OPEN-SW                       
084000     MOVE 'OPEN'                TO WS-KDFUNC                              
084100     MOVE 1 TO IX                                                         
084200     PERFORM UNTIL IX > MAX-INDEX OR TAB-KDOUTMETH(IX) = SPACE            
084300     OR WS-KDRC > 0                                                       
084400       MOVE IX                  TO WS-IDCALL                              
084500       PERFORM S04-DISTRIBUTE                                             
084600       ADD 1 TO IX                                                        
084700     END-PERFORM                                                          
084800                                                                          
084900*    -- REMEMBER IF AN OPEN ERROR OCCURRED                                
085000     IF WS-KDRC > 0                                                       
085100       MOVE NOO TO WZ11OUTX-OPEN-SW                                       
085200     END-IF                                                               
085300                                                                          
085400*    -- SAVE NUMBER OF RULE LINES ACTUALLY USED IN THIS RULE              
085500     COMPUTE IX-SAVE-NBR-OF-RULES = IX - 1                                
085600     .                                                                    
085700     EJECT                                                                
085800 CCA-COMPUTE-RESTART-KEYS SECTION.                                        
085900                                                                          
086000     MOVE WS-IDOUTTYPE-KEY      TO REKY-IDOUTTYPE                         
086100     MOVE WS-IDOUTREC-KEY       TO REKY-IDOUTREC                          
086200     MOVE WS-IDLIST-KEY         TO REKY-IDLIST                            
086300     MOVE WS-CURRENT-DATE       TO REKY-TIREGDAT                          
086400     MOVE WS-CURRENT-TIME       TO REKY-TIKLOCK                           
086500                                                                          
086600     MOVE ZERO TO REKY-IDLOPNR                                            
086700     PERFORM DB2-SELECT-TZ4REKY-TAB                                       
086800     PERFORM UNTIL LINES-MISSING                                          
086900       ADD 1 TO REKY-IDLOPNR                                              
087000       PERFORM DB2-SELECT-TZ4REKY-TAB                                     
087100     END-PERFORM                                                          
087200*    -- SAVE ORIGINAL IDLOPNR (THIS VALUE IS USED ONLY FOR                
087300*    -- THE FIRST RULE IN THE SET)                                        
087400     MOVE REKY-IDLOPNR          TO W-SAVE-IDLOPNR                         
087500     .                                                                    
087600     EJECT                                                                
087700                                                                          
087800 FA-SEND-DISTRIBUTE-DATA SECTION.                                         
087900                                                                          
088000     MOVE 'PUT'                     TO WS-KDFUNC                          
088100     MOVE 1 TO IX                                                         
088200     PERFORM UNTIL IX > IX-SAVE-NBR-OF-RULES OR WS-KDRC > 0               
088300       MOVE IX                        TO WS-IDCALL                        
088400       MOVE DAP-KVDLEN                TO WS-TEOUTDATA-L                   
088500       MOVE DAP-TEOUTDATA(1:DAP-KVDLEN)                                   
088600            TO WS-TEOUTDATA                                               
088700       PERFORM S04-DISTRIBUTE                                             
088800       ADD 1 TO IX                                                        
088900     END-PERFORM                                                          
089000                                                                          
089100*    -- REMEMBER IF A PUT ERROR OCCURRED                                  
089200     IF WS-KDRC > 0                                                       
089300       MOVE NOO TO WZ11OUTX-PUT-SW                                        
089400     END-IF                                                               
089500     .                                                                    
089600     EJECT                                                                
089700 FB-HANDLE-RESTART-TABLES SECTION.                                        
089800                                                                          
089900     IF RULE-OK                                                           
090000       MOVE DIRU-KVDAGAR-RESEND         TO WS-KVDAGAR-RESEND              
090100     ELSE                                                                 
090200       MOVE WS-DEFAULT-RESTARTABLE-DAYS TO WS-KVDAGAR-RESEND              
090300     END-IF                                                               
090400                                                                          
090500     IF WS-KVDAGAR-RESEND > ZERO                                          
090600       MOVE 1 TO IX                                                       
090700       PERFORM UNTIL IX > IX-SAVE-NBR-OF-RULES                            
090800*      PERFORM UNTIL IX > 1                                               
090900         IF FIRST-LOOP                                                    
091000           PERFORM FBA-INS-RESTART-KEYS                                   
091100         END-IF                                                           
091200         PERFORM FBB-INS-RESTART-DATA                                     
091300         ADD 1 TO IX                                                      
091400       END-PERFORM                                                        
091500       MOVE NOO TO FIRST-LOOP-SW                                          
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900 FBA-INS-RESTART-KEYS SECTION.                                            
092000                                                                          
092100*    -- MOST KEYS TO REKY ARE SET IN CCA- SECTION.                        
092200*    -- THEY ARE SET HERE TOO, IN CASE THE RULE WAS MISSING               
092300*    -- AND CCA SECTION WAS SKIPPED.                                      
092400     MOVE WS-IDOUTTYPE-KEY      TO REKY-IDOUTTYPE                         
092500     MOVE WS-IDOUTREC-KEY       TO REKY-IDOUTREC                          
092600     MOVE WS-IDLIST-KEY         TO REKY-IDLIST                            
092700     MOVE WS-CURRENT-DATE       TO REKY-TIREGDAT                          
092800     MOVE WS-CURRENT-TIME       TO REKY-TIKLOCK                           
092900*    -- USE SAVED IDLOPNR FROM SECTION CCA TO GENERATE THE CORRECT        
093000*    -- NUMBER FOR THIS RULE. RULE 1 USE IDLOPNR 0-99,                    
093100*    -- RULE 2 USE 100-199 ETC                                            
093200     IF REKY-TIREGDAT < 151012                                            
093300       COMPUTE REKY-IDLOPNR = (IX - 1) * 100 + W-SAVE-IDLOPNR             
093400     ELSE                                                                 
093500       COMPUTE REKY-IDLOPNR = (IX - 1) * 50 + W-SAVE-IDLOPNR              
093600     END-IF                                                               
093700                                                                          
093800     MOVE TAB-KDOUTMETH(IX)     TO REKY-KDOUTMETH                         
093900     MOVE TAB-IDOUTDEST(IX)     TO REKY-IDOUTDEST                         
094000     MOVE TAB-KVCOPIES(IX)      TO REKY-KVCOPIES                          
094100     MOVE TAB-FLCARRCNTL(IX)    TO REKY-FLCARRCNTL                        
094200     IF REKY-FLCARRCNTL = SPACE                                           
094300       MOVE NOO                 TO REKY-FLCARRCNTL                        
094400     END-IF                                                               
094500     MOVE TAB-FLACIF(IX)        TO REKY-FLACIF                            
094600     IF REKY-FLACIF = SPACE                                               
094700       MOVE NOO                 TO REKY-FLACIF                            
094800     END-IF                                                               
094900     MOVE TAB-IDPFDEF(IX)       TO REKY-IDPFDEF                           
095000     MOVE TAB-IDFORMSNM(IX)     TO REKY-IDFORMSNM                         
095100     MOVE TAB-TEVCOMST(IX)      TO REKY-TEVCOMST                          
095200     MOVE TAB-IDVCINIT(IX)      TO REKY-IDVCINIT                          
095300     MOVE WS-FLRULEOK           TO REKY-FLRULEMISS                        
095400     MOVE WS-IDMAIL-SENDER      TO REKY-IDMAIL-SENDER                     
095500     MOVE ZERO                  TO REKY-KVANTEX-PRINTAD                   
095600     IF RULE-OK                                                           
095700     AND TAB-KDOUTMETH(IX) NOT = WS-SAVE AND NOT = WS-GET-IT              
095800     AND TAB-KDOUTMETH(IX) NOT = WS-WEB                                   
095900       MOVE 1                   TO REKY-KVANTEX-PRINTAD                   
096000     END-IF                                                               
096100                                                                          
096200     MOVE WS-CURRENT-DATE-X6 TO DAYS-TIDATE1                              
096300     MOVE 'YYMMDD' TO DAYS-KDDATFMT1                                      
096400     MOVE WS-KVDAGAR-RESEND TO DAYS-KVDAYS                                
096500     MOVE 'WEEKDAYS' TO DAYS-IDCALEND                                     
096600     MOVE SPACE TO DAYS-TIDATE2                                           
096700                   WS-TIDATE2                                             
096800     MOVE 'YYMMDD' TO DAYS-KDDATFMT2                                      
096900     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
097000     IF DAYS-KDRC = ZERO                                                  
097100       MOVE DAYS-TIDATE2(1:6) TO WS-TIDATE2                               
097200       MOVE WS-TIDATE2 TO REKY-TIAAMMDD-RENS                              
097300     END-IF                                                               
097400                                                                          
097500     PERFORM DB2-INSERT-TZ4REKY-TAB                                       
097600     .                                                                    
097700     EJECT                                                                
097800 FBB-INS-RESTART-DATA SECTION.                                            
097900                                                                          
098000     MOVE REKY-IDOUTTYPE  TO REDA-IDOUTTYPE                               
098100     MOVE REKY-IDOUTREC   TO REDA-IDOUTREC                                
098200     MOVE REKY-IDLIST     TO REDA-IDLIST                                  
098300     MOVE WS-CURRENT-DATE TO REDA-TIREGDAT                                
098400     MOVE WS-CURRENT-TIME TO REDA-TIKLOCK                                 
098500     MOVE W-SAVE-IDLOPNR  TO REDA-IDLOPNR                                 
098600     ADD 1 TO TAB-KVPOST(IX)                                              
098700     MOVE TAB-KVPOST(IX)  TO REDA-KVPOST                                  
098800                                                                          
098900     PERFORM DB2-SELECT-TZ4REDA-TAB                                       
099000     IF LINES-MISSING                                                     
099100       MOVE DAP-KVDLEN        TO REDA-TEOUTDATA-L                         
099200       MOVE DAP-TEOUTDATA(1:DAP-KVDLEN)                                   
099300                              TO REDA-TEOUTDATA-D                         
099400       PERFORM DB2-INSERT-TZ4REDA-TAB                                     
099500     END-IF                                                               
099600     .                                                                    
099700     EJECT                                                                
099800 FC-CLOSE-DISTRIB-CHANNELS  SECTION.                                      
099900                                                                          
100000     MOVE 'CLOSE' TO WS-KDFUNC                                            
100100     MOVE 1 TO IX                                                         
100200     PERFORM UNTIL IX > IX-SAVE-NBR-OF-RULES                              
100300       MOVE IX TO WS-IDCALL                                               
100400       PERFORM S04-DISTRIBUTE                                             
100500       ADD 1 TO IX                                                        
100600     END-PERFORM                                                          
100700     .                                                                    
100800     EJECT                                                                
100900 G-HANDLE-ERROR SECTION.                                                  
101000                                                                          
101100     IF WS-IDOUTTYPE-KEY NOT = WS-DP-ERROR                                
101200*      -- NORMAL NON-D&P DISTRIBUTION AND NO RETURN-ADDRESS.              
101300*      -- SEARCH IN RULE-TABLE FOR AN ERROR-RULE WITH                     
101400*      -- OUTPUT TYPE "DP-ERROR" AND RECEIVER = ORIGINAL                  
101500*      -- OUTPUT TYPE AND AND RECEIVER CONCATENATED.                      
101600*      -- EXAMPLE: IF ORIGINAL KEY WAS ("AAA", "BBB") -                   
101700*      -- SEARCH FOR ("DP-ERROR, "AAA+BBB") IF SUCH A                     
101800*      -- RULE EXISTS, SEND ERROR MESSAGE TO THIS PROGRAM IN              
101900*      -- THE NORMAL WAY, AND LET THE RULE HANDLE THE                     
102000*      -- CORRECT DISTRIBUTION.                                           
102100       MOVE WS-IDOUTTYPE-KEY TO WS-IDOUTTYPE-SAVE                         
102200       MOVE WS-IDOUTREC-KEY  TO WS-IDOUTREC-SAVE                          
102300                                                                          
102400       MOVE WS-DP-ERROR   TO WS-IDOUTTYPE-KEY                             
102500       MOVE SPACE         TO WS-IDOUTREC-TEMP                             
102600       STRING WS-IDOUTTYPE-SAVE '+' WS-IDOUTREC-KEY                       
102700         DELIMITED BY SPACE INTO WS-IDOUTREC-TEMP                         
102800       MOVE WS-IDOUTREC-TEMP TO WS-IDOUTREC-KEY                           
102900                                                                          
103000       PERFORM DB2-SELECT-TZ4DIRU-TAB                                     
103100       IF LINES-FOUND                                                     
103200*        -- USE D&P ITSELF TO SIGNAL ERROR                                
103300         PERFORM GB-SEND-DP-ERROR                                         
103400       ELSE                                                               
103500         MOVE 'DAP-ERROR'       TO WS-IDOUTTYPE-KEY                       
103600         MOVE 'DAP-ERROR'       TO WS-IDOUTREC-KEY                        
103700         PERFORM DB2-SELECT-TZ4DIRU-TAB                                   
103800         IF LINES-FOUND                                                   
103900*          -- USE D&P TO SEND MAIL TO SUPPORT MAILBOX                     
104000           PERFORM GB-SEND-DP-ERROR                                       
104100         ELSE                                                             
104200*          -- NO ERROR RULE FOUND, USE HARD-CODED SIGNALLING              
104300           MOVE WS-IDOUTTYPE-SAVE TO WS-IDOUTTYPE-KEY                     
104400           MOVE WS-IDOUTREC-SAVE TO WS-IDOUTREC-KEY                       
104500           PERFORM GC-SEND-HARDCODED-ERROR                                
104600         END-IF                                                           
104700       END-IF                                                             
104800     ELSE                                                                 
104900*      -- ERROR IN D&P-ERROR SIGNALLING, USE HARD-CODED SIGNAL            
105000       PERFORM GC-SEND-HARDCODED-ERROR                                    
105100     END-IF                                                               
105200                                                                          
105300*    -- ALSO RETURN ERROR MSG TO CALLER                                   
105400     STRING ERR-IDMSG-ERROR ' ' WS-OUT-ERROR-TEXT                         
105500       DELIMITED BY SIZE                                                  
105600       INTO DAP-BEFEL                                                     
105700     .                                                                    
105800     EJECT                                                                
105900                                                                          
106000 GB-SEND-DP-ERROR SECTION.                                                
106100                                                                          
106200*    -- USE D&P ITSELF TO SEND AN ERROR MESSAGE VIA A SUITABLE            
106300*    -- "DP-ERROR" OR "DAP-ERROR" DISTRIBUTION RULE                       
106400                                                                          
106500                                                                          
106600     IF WZ04-SEND-IDCOM = ZERO                                            
106700       PERFORM S40-OPEN-DP-ERROR                                          
106800       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
106900     END-IF                                                               
107000                                                                          
107100     PERFORM S40-PUT-DP-ERROR-HDR                                         
107200                                                                          
107300     MOVE ' D&P DISTRIBUTION FAILED' TO REDA-TEOUTDATA-D                  
107400     PERFORM S40-PUT-DP-ERROR-DATA                                        
107500                                                                          
107600     CALL VIMSID  USING WS-VIMSID                                         
107700     MOVE SPACE                         TO REDA-TEOUTDATA-D               
107800     STRING ' DATE & TIME: '             DELIMITED BY SIZE                
107900          FUNCTION CURRENT-DATE (3:6)    DELIMITED BY SIZE                
108000          ' '                            DELIMITED BY SIZE                
108100          FUNCTION CURRENT-DATE (9:8)    DELIMITED BY SIZE                
108200          ' IMS-SYS: '                   DELIMITED BY SIZE                
108300          WS-VIMSID                      DELIMITED BY SIZE                
108400     INTO REDA-TEOUTDATA-D                                                
108500     PERFORM S40-PUT-DP-ERROR-DATA                                        
108600     MOVE SPACE                          TO REDA-TEOUTDATA-D              
108700                                                                          
108800     MOVE ' (SENT VIA DP-ERROR CHANNEL)' TO REDA-TEOUTDATA-D              
108900     PERFORM S40-PUT-DP-ERROR-DATA                                        
109000                                                                          
109100     MOVE SPACE TO REDA-TEOUTDATA-D                                       
109200     STRING ' TYPE: '  WS-IDOUTTYPE-SAVE                                  
109300       DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                            
109400     PERFORM S40-PUT-DP-ERROR-DATA                                        
109500                                                                          
109600     MOVE SPACE TO REDA-TEOUTDATA-D                                       
109700     STRING ' SUB-TYPE: ' WS-IDOUTREC-SAVE                                
109800       DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                            
109900     PERFORM S40-PUT-DP-ERROR-DATA                                        
110000                                                                          
110100     MOVE SPACE TO REDA-TEOUTDATA-D                                       
110200     STRING ' ID: ' WS-IDLIST-KEY                                         
110300       DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                            
110400     PERFORM S40-PUT-DP-ERROR-DATA                                        
110500                                                                          
110600     MOVE SPACE TO REDA-TEOUTDATA-D                                       
110700     STRING ' ERROR: '  ERR-IDMSG-ERROR                                   
110800            ' ' ERR-IDELMT-ERROR                                          
110900       DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                            
111000     PERFORM S40-PUT-DP-ERROR-DATA                                        
111100                                                                          
111200     IF WS-OUT-ERROR-TEXT NOT = SPACE                                     
111300       MOVE SPACE TO REDA-TEOUTDATA-D                                     
111400       STRING ' ' WS-OUT-ERROR-TEXT                                       
111500         DELIMITED BY SIZE INTO REDA-TEOUTDATA-D                          
111600       PERFORM S40-PUT-DP-ERROR-DATA                                      
111700     END-IF                                                               
111800                                                                          
111900     IF WZ04-SEND-IDCOM > ZERO                                            
112000       PERFORM S40-CLOSE-DP-ERROR                                         
112100       MOVE ZERO TO WZ04-SEND-IDCOM                                       
112200     END-IF                                                               
112300     .                                                                    
112400     EJECT                                                                
112500 GC-SEND-HARDCODED-ERROR SECTION.                                         
112600                                                                          
112700*    -- NO VALID ADDRESS FOR ERROR SIGNALLING SPECIFIED.                  
112800*    -- HANDLE SIGNALLING INTERNALLY WITHIN D&P                           
112900*    -- TEMP SOLUTION: USE THE OUT0 PCB AND "CHANNEL" 9                   
113000*    -- TO PRINT TO THE TEST PRINTER QSE10316                             
113100                                                                          
113200     MOVE 'OPEN'                TO OUTP-KDFUNC                            
113300     MOVE 9                     TO OUTP-IDCALL                            
113400     MOVE 'OUT0'                TO OUTP-IDPCB                             
113500     MOVE 'QSE10316'            TO OUTP-IDOUTDEST                         
113600     MOVE SPACE                 TO OUTP-KVCOPIES                          
113700     MOVE SPACE                 TO OUTP-IDPFDEF                           
113800     MOVE SPACE                 TO OUTP-IDFORMSNM                         
113900     MOVE NOO                   TO OUTP-FLCARRCNTL                        
114000     MOVE NOO                   TO OUTP-FLACIF                            
114100     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
114200     IF OUTP-KDRC > 0                                                     
114300        STRING 'GC OUTP ERROR. '                                          
114400                WS-OUT-ERROR-TEXT DELIMITED BY SIZE                       
114500           INTO ERROR-TEXT                                                
114600        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
114700     END-IF                                                               
114800                                                                          
114900     CALL VIMSID  USING WS-VIMSID                                         
115000                                                                          
115100     MOVE 'PUT'                      TO OUTP-KDFUNC                       
115200     MOVE 60                         TO OUTP-TEOUTDATA-L                  
115300                                                                          
115400     MOVE ' D&P DISTRIBUTION FAILED'    TO OUTP-TEOUTDATA                 
115500     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
115600                                                                          
115700     MOVE SPACE                         TO OUTP-TEOUTDATA                 
115800     STRING ' DATE & TIME: '             DELIMITED BY SIZE                
115900          FUNCTION CURRENT-DATE (3:6)    DELIMITED BY SIZE                
116000          ' '                            DELIMITED BY SIZE                
116100          FUNCTION CURRENT-DATE (9:8)    DELIMITED BY SIZE                
116200          ' IMS-SYS: '                   DELIMITED BY SIZE                
116300          WS-VIMSID                      DELIMITED BY SIZE                
116400     INTO OUTP-TEOUTDATA                                                  
116500     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
116600     IF OUTP-KDRC > 0                                                     
116700        STRING 'GC OUTP ERROR. '                                          
116800                WS-OUT-ERROR-TEXT DELIMITED BY SIZE                       
116900           INTO ERROR-TEXT                                                
117000        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
117100     END-IF                                                               
117200                                                                          
117300     MOVE SPACE TO OUTP-TEOUTDATA                                         
117400     MOVE ' (SENT VIA DEFAULT CHANNEL)' TO OUTP-TEOUTDATA                 
117500     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
117600                                                                          
117700     MOVE SPACE TO OUTP-TEOUTDATA                                         
117800     STRING ' TYPE: '  WS-IDOUTTYPE-KEY                                   
117900     DELIMITED BY SIZE INTO OUTP-TEOUTDATA                                
118000     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
118100                                                                          
118200     MOVE SPACE TO OUTP-TEOUTDATA                                         
118300     STRING ' SUB-TYPE: '  WS-IDOUTREC-KEY                                
118400     DELIMITED BY SIZE INTO OUTP-TEOUTDATA                                
118500     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
118600                                                                          
118700     MOVE SPACE TO OUTP-TEOUTDATA                                         
118800     STRING ' ID: '  WS-IDLIST-KEY                                        
118900       DELIMITED BY SIZE INTO OUTP-TEOUTDATA                              
119000     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
119100                                                                          
119200     MOVE SPACE TO OUTP-TEOUTDATA                                         
119300     STRING ' ERROR: '  ERR-IDMSG-ERROR                                   
119400            ' ' ERR-IDELMT-ERROR                                          
119500       DELIMITED BY SIZE INTO OUTP-TEOUTDATA                              
119600     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
119700                                                                          
119800     IF WS-OUT-ERROR-TEXT NOT = SPACE                                     
119900       MOVE SPACE TO OUTP-TEOUTDATA                                       
120000       STRING ' ' WS-OUT-ERROR-TEXT                                       
120100         DELIMITED BY SIZE INTO OUTP-TEOUTDATA                            
120200       CALL WZ11OUTP USING OUTP-WZ11OUT                                   
120300     END-IF                                                               
120400                                                                          
120500     MOVE 'CLOSE'                   TO OUTP-KDFUNC                        
120600     CALL WZ11OUTP USING OUTP-WZ11OUT                                     
120700     .                                                                    
120800     EJECT                                                                
120900 S04-DISTRIBUTE SECTION.                                                  
121000                                                                          
121100     EVALUATE TAB-KDOUTMETH(IX)                                           
121200       WHEN WS-PRINT                                                      
121300         MOVE WS-IDCALL        TO OUTP-IDCALL                             
121400         MOVE WS-KDFUNC        TO OUTP-KDFUNC                             
121500         IF WS-KDFUNC = 'OPEN'                                            
121600*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
121700*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
121800           MOVE REKY-IDOUTTYPE      TO OUTP-IDOUTTYPE                     
121900           MOVE REKY-IDOUTREC       TO OUTP-IDOUTREC                      
122000           MOVE REKY-IDLIST         TO OUTP-IDLIST                        
122100           MOVE REKY-TIREGDAT       TO OUTP-TIREGDAT                      
122200           MOVE REKY-TIKLOCK        TO OUTP-TIKLOCK                       
122300                                                                          
122400           MOVE TAB-IDOUTDEST(IX)   TO OUTP-IDOUTDEST                     
122500           MOVE TAB-KVCOPIES(IX)    TO OUTP-KVCOPIES                      
122600           MOVE TAB-IDPFDEF(IX)     TO OUTP-IDPFDEF                       
122700           MOVE TAB-IDFORMSNM(IX)   TO OUTP-IDFORMSNM                     
122800           MOVE TAB-FLCARRCNTL(IX)  TO OUTP-FLCARRCNTL                    
122900           MOVE TAB-FLACIF(IX)      TO OUTP-FLACIF                        
123000*          -- OLD RULES MAY LACK FLACIF, AND PAGEDEF/FORMDEF              
123100*          -- PLUS NO CARRIAGE CONTROL CHARACTERS USED TO INDICATE        
123200*          -- ACIF SHOULD BE USED (EXAMPLE: BILL-IT INVOICES)             
123300           IF TAB-FLACIF(IX) = SPACE                                      
123400             MOVE NOO               TO OUTP-FLACIF                        
123500           END-IF                                                         
123600           IF TAB-FLCARRCNTL(IX) = NOO                                    
123700           AND TAB-IDPFDEF(IX) NOT = SPACE                                
123800             MOVE YES               TO OUTP-FLACIF                        
123900           END-IF                                                         
124000           MOVE SPACE               TO OUTP-IDPCB                         
124100         END-IF                                                           
124200         IF WS-KDFUNC = 'PUT'                                             
124300           MOVE WS-TEOUTDATA-L   TO OUTP-TEOUTDATA-L                      
124400           MOVE WS-TEOUTDATA     TO OUTP-TEOUTDATA                        
124500         END-IF                                                           
124600         CALL WZ11OUTP USING OUTP-WZ11OUT                                 
124700         MOVE ERR-PRINT-FAILED TO WS-ERROR-TYPE                           
124800         MOVE OUTP-KDRC        TO WS-KDRC                                 
124900         MOVE OUTP-TEOUTDATA   TO WS-TEOUTDATA                            
125000                                                                          
125100       WHEN WS-FAX                                                        
125200         MOVE WS-IDCALL        TO OUTF-IDCALL                             
125300         MOVE WS-KDFUNC        TO OUTF-KDFUNC                             
125400         IF WS-KDFUNC = 'OPEN'                                            
125500*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
125600*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
125700           MOVE REKY-IDOUTTYPE      TO OUTF-IDOUTTYPE                     
125800           MOVE REKY-IDOUTREC       TO OUTF-IDOUTREC                      
125900           MOVE REKY-IDLIST         TO OUTF-IDLIST                        
126000           MOVE REKY-TIREGDAT       TO OUTF-TIREGDAT                      
126100           MOVE REKY-TIKLOCK        TO OUTF-TIKLOCK                       
126200                                                                          
126300           MOVE TAB-IDOUTDEST(IX)   TO OUTF-IDOUTDEST                     
126400           MOVE TAB-IDPFDEF(IX)     TO OUTF-IDPFDEF                       
126500           MOVE TAB-FLCARRCNTL(IX)  TO OUTF-FLCARRCNTL                    
126600           MOVE WS-IDMAIL-SENDER    TO OUTF-IDMAIL-SENDER                 
126700           MOVE WS-TEFAX-1          TO OUTF-TEFAX(1)                      
126800           MOVE WS-TEFAX-2          TO OUTF-TEFAX(2)                      
126900           MOVE WS-TEFAX-3          TO OUTF-TEFAX(3)                      
127000           MOVE WS-TEFAX-4          TO OUTF-TEFAX(4)                      
127100           MOVE WS-TEFAX-5          TO OUTF-TEFAX(5)                      
127200         END-IF                                                           
127300         IF WS-KDFUNC = 'PUT'                                             
127400           MOVE WS-TEOUTDATA-L   TO OUTF-TEOUTDATA-L                      
127500           MOVE WS-TEOUTDATA     TO OUTF-TEOUTDATA                        
127600         END-IF                                                           
127700                                                                          
127800         CALL WZ11OUTF USING OUTF-WZ11OUT                                 
127900         MOVE ERR-FAX-FAILED   TO WS-ERROR-TYPE                           
128000         MOVE OUTF-KDRC        TO WS-KDRC                                 
128100         MOVE OUTF-TEOUTDATA   TO WS-TEOUTDATA                            
128200                                                                          
128300       WHEN WS-MAIL                                                       
128400         MOVE WS-IDCALL        TO OUTM-IDCALL                             
128500         MOVE WS-KDFUNC        TO OUTM-KDFUNC                             
128600         IF WS-KDFUNC = 'OPEN'                                            
128700*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
128800*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
128900           MOVE REKY-IDOUTTYPE      TO OUTM-IDOUTTYPE                     
129000           MOVE REKY-IDOUTREC       TO OUTM-IDOUTREC                      
129100           MOVE REKY-IDLIST         TO OUTM-IDLIST                        
129200           MOVE REKY-TIREGDAT       TO OUTM-TIREGDAT                      
129300           MOVE REKY-TIKLOCK        TO OUTM-TIKLOCK                       
129400                                                                          
129500           MOVE TAB-IDOUTDEST(IX)   TO OUTM-IDOUTDEST                     
129600           MOVE TAB-IDPFDEF(IX)     TO OUTM-IDPFDEF                       
129700           MOVE TAB-FLCARRCNTL(IX)  TO OUTM-FLCARRCNTL                    
129800           MOVE WS-IDMAIL-SENDER    TO OUTM-IDMAIL-SENDER                 
129900*          -- USE THE FIRST TEFAX LINE AS TITLE, AND                      
130000*          -- SHIFT THE OTHERS UP ONE STEP.                               
130100           IF WS-TEFAX-1 NOT = SPACE                                      
130200             MOVE WS-TEFAX-1        TO OUTM-IDMAILTTL                     
130300           ELSE                                                           
130400             MOVE SPACE             TO OUTM-IDMAILTTL                     
130500             STRING 'D&P Mail ('                                          
130600                    WS-IDOUTTYPE-KEY                                      
130700                    ')'                                                   
130800                    DELIMITED BY SIZE INTO OUTM-IDMAILTTL                 
130900           END-IF                                                         
131000           MOVE WS-TEFAX-2          TO OUTM-TEFAX(1)                      
131100           MOVE WS-TEFAX-3          TO OUTM-TEFAX(2)                      
131200           MOVE WS-TEFAX-4          TO OUTM-TEFAX(3)                      
131300           MOVE WS-TEFAX-5          TO OUTM-TEFAX(4)                      
131400           MOVE SPACE               TO OUTM-TEFAX(5)                      
131500                                                                          
131600         END-IF                                                           
131700         IF WS-KDFUNC = 'PUT'                                             
131800           MOVE WS-TEOUTDATA-L   TO OUTM-TEOUTDATA-L                      
131900           MOVE WS-TEOUTDATA     TO OUTM-TEOUTDATA                        
132000         END-IF                                                           
132100                                                                          
132200         CALL WZ11OUTM USING OUTM-WZ11OUT                                 
132300                                                                          
132400         MOVE ERR-MAIL-FAILED  TO WS-ERROR-TYPE                           
132500         MOVE OUTM-KDRC        TO WS-KDRC                                 
132600         MOVE OUTM-TEOUTDATA   TO WS-TEOUTDATA                            
132700                                                                          
132800       WHEN WS-EDI                                                        
132900*        -- NOTE: THE VCOM SUBROUTINE IS USED FOR EDI TOO                 
133000         MOVE WS-IDCALL        TO OUTV-IDCALL                             
133100         MOVE WS-KDFUNC        TO OUTV-KDFUNC                             
133200         IF WS-KDFUNC = 'OPEN'                                            
133300           MOVE TAB-IDOUTDEST(IX)   TO OUTV-IDOUTDEST                     
133400           MOVE TAB-TEVCOMST(IX)    TO OUTV-TEVCOMST                      
133500           MOVE TAB-IDVCINIT(IX)    TO OUTV-IDVCINIT                      
133600         END-IF                                                           
133700         IF WS-KDFUNC = 'PUT'                                             
133800           MOVE WS-TEOUTDATA-L   TO OUTV-TEOUTDATA-L                      
133900           MOVE WS-TEOUTDATA     TO OUTV-TEOUTDATA                        
134000         END-IF                                                           
134100         CALL WZ11OUTV USING OUTV-WZ11OUT                                 
134200         MOVE ERR-EDI-FAILED   TO WS-ERROR-TYPE                           
134300         MOVE OUTV-KDRC        TO WS-KDRC                                 
134400         MOVE OUTV-TEOUTDATA   TO WS-TEOUTDATA                            
134500                                                                          
134600       WHEN WS-VCOM                                                       
134700         MOVE WS-IDCALL        TO OUTV-IDCALL                             
134800         MOVE WS-KDFUNC        TO OUTV-KDFUNC                             
134900         IF WS-KDFUNC = 'OPEN'                                            
135000           MOVE TAB-IDOUTDEST(IX)   TO OUTV-IDOUTDEST                     
135100           MOVE TAB-TEVCOMST(IX)    TO OUTV-TEVCOMST                      
135200           MOVE TAB-IDVCINIT(IX)    TO OUTV-IDVCINIT                      
135300         END-IF                                                           
135400         IF WS-KDFUNC = 'PUT'                                             
135500           MOVE WS-TEOUTDATA-L   TO OUTV-TEOUTDATA-L                      
135600           MOVE WS-TEOUTDATA     TO OUTV-TEOUTDATA                        
135700         END-IF                                                           
135800         CALL WZ11OUTV USING OUTV-WZ11OUT                                 
135900         MOVE ERR-VCOM-FAILED  TO WS-ERROR-TYPE                           
136000         MOVE OUTV-KDRC        TO WS-KDRC                                 
136100         MOVE OUTV-TEOUTDATA   TO WS-TEOUTDATA                            
136200                                                                          
136300       WHEN WS-ONDEMAND                                                   
136400         MOVE WS-IDCALL        TO OUTO-IDCALL                             
136500         MOVE WS-KDFUNC        TO OUTO-KDFUNC                             
136600         IF WS-KDFUNC = 'OPEN'                                            
136700*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
136800*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
136900           MOVE REKY-IDOUTTYPE      TO OUTO-IDOUTTYPE                     
137000           MOVE REKY-IDOUTREC       TO OUTO-IDOUTREC                      
137100           MOVE REKY-IDLIST         TO OUTO-IDLIST                        
137200           MOVE REKY-TIREGDAT       TO OUTO-TIREGDAT                      
137300           MOVE REKY-TIKLOCK        TO OUTO-TIKLOCK                       
137400                                                                          
137500           MOVE TAB-IDOUTDEST(IX)   TO OUTO-IDOUTDEST                     
137600           MOVE TAB-IDPFDEF(IX)     TO OUTO-IDPFDEF                       
137700           MOVE TAB-IDFORMSNM(IX)   TO OUTO-IDFORMSNM                     
137800           MOVE TAB-FLCARRCNTL(IX)  TO OUTO-FLCARRCNTL                    
137900           MOVE TAB-FLACIF(IX)      TO OUTO-FLACIF                        
138000*          -- OLD RULES MAY LACK FLACIF, AND PAGEDEF/FORMDEF              
138100*          -- PLUS NO CARRIAGE CONTROL CHARACTERS USED TO INDICATE        
138200*          -- ACIF SHOULD BE USED (EXAMPLE: BILL-IT INVOICES)             
138300           IF TAB-FLACIF(IX) = SPACE                                      
138400             MOVE NOO               TO OUTO-FLACIF                        
138500           END-IF                                                         
138600           IF TAB-FLCARRCNTL(IX) = NOO                                    
138700           AND TAB-IDPFDEF(IX) NOT = SPACE                                
138800             MOVE YES               TO OUTO-FLACIF                        
138900           END-IF                                                         
139000           MOVE SPACE            TO OUTO-IDPCB                            
139100         END-IF                                                           
139200         IF WS-KDFUNC = 'PUT'                                             
139300           MOVE WS-TEOUTDATA-L   TO OUTO-TEOUTDATA-L                      
139400           MOVE WS-TEOUTDATA     TO OUTO-TEOUTDATA                        
139500         END-IF                                                           
139600         CALL WZ11OUTO USING OUTO-WZ11OUT                                 
139700         MOVE ERR-ONDEMAND-FAILED TO WS-ERROR-TYPE                        
139800         MOVE OUTO-KDRC        TO WS-KDRC                                 
139900         MOVE OUTO-TEOUTDATA   TO WS-TEOUTDATA                            
140000                                                                          
140100       WHEN WS-GET-IT                                                     
140200         MOVE WS-IDCALL        TO OUTA-IDCALL                             
140300         MOVE WS-KDFUNC        TO OUTA-KDFUNC                             
140400         IF WS-KDFUNC = 'OPEN'                                            
140500*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
140600*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
140700           MOVE REKY-IDOUTTYPE      TO OUTA-IDOUTTYPE                     
140800           MOVE REKY-IDOUTREC       TO OUTA-IDOUTREC                      
140900           MOVE REKY-IDLIST         TO OUTA-IDLIST                        
141000           MOVE REKY-TIREGDAT       TO OUTA-TIREGDAT                      
141100           MOVE REKY-TIKLOCK        TO OUTA-TIKLOCK                       
141200                                                                          
141300           MOVE TAB-IDOUTDEST(IX)   TO OUTA-IDOUTDEST                     
141400           MOVE TAB-IDPFDEF(IX)     TO OUTA-IDPFDEF                       
141500           MOVE TAB-FLCARRCNTL(IX)  TO OUTA-FLCARRCNTL                    
141600           MOVE WS-CURRENT-DATE     TO OUTA-TIREGDAT                      
141700           MOVE WS-CURRENT-TIME2    TO OUTA-TIREGTID                      
141800         END-IF                                                           
141900         IF WS-KDFUNC = 'PUT'                                             
142000           MOVE WS-TEOUTDATA-L   TO OUTA-TEOUTDATA-L                      
142100           MOVE WS-TEOUTDATA     TO OUTA-TEOUTDATA                        
142200         END-IF                                                           
142300         CALL WZ11OUTA USING OUTA-WZ11OUT                                 
142400         MOVE ERR-GET-IT-FAILED  TO WS-ERROR-TYPE                         
142500         MOVE OUTA-KDRC          TO WS-KDRC                               
142600         MOVE OUTA-TEOUTDATA     TO WS-TEOUTDATA                          
142700                                                                          
142800       WHEN WS-WEB                                                        
142900         MOVE WS-IDCALL        TO OUTW-IDCALL                             
143000         MOVE WS-KDFUNC        TO OUTW-KDFUNC                             
143100         IF WS-KDFUNC = 'OPEN'                                            
143200           MOVE REKY-IDOUTTYPE    TO OUTW-IDOUTTYPE                       
143300           MOVE REKY-IDOUTREC     TO OUTW-IDOUTREC                        
143400           MOVE REKY-IDLIST       TO OUTW-IDLIST                          
143500           MOVE REKY-TIREGDAT     TO OUTW-TIREGDAT                        
143600           MOVE REKY-TIKLOCK      TO OUTW-TIKLOCK                         
143700*          -- USE SAVED IDLOPNR TO GENERATE THE CORRECT IDLOPNR           
143800*          -- FOR THIS RULE. RULE 1 USE IDLOPNR 0-99,                     
143900*          -- RULE 2 USE 100-199 ETC                                      
144000           IF REKY-TIREGDAT < 151012                                      
144100           COMPUTE OUTW-IDLOPNR = (IX - 1) * 100 + W-SAVE-IDLOPNR         
144200           ELSE                                                           
144300           COMPUTE OUTW-IDLOPNR = (IX - 1) * 50 + W-SAVE-IDLOPNR          
144400           END-IF                                                         
144500         END-IF                                                           
144600         IF WS-KDFUNC = 'PUT'                                             
144700           MOVE WS-TEOUTDATA-L    TO OUTW-TEOUTDATA-L                     
144800           MOVE WS-TEOUTDATA      TO OUTW-TEOUTDATA                       
144900         END-IF                                                           
145000         CALL WZ11OUTW USING OUTW-WZ11OUT                                 
145100         MOVE ERR-WEB-FAILED      TO WS-ERROR-TYPE                        
145200         MOVE OUTW-KDRC           TO WS-KDRC                              
145300         MOVE OUTW-TEOUTDATA      TO WS-TEOUTDATA                         
145400                                                                          
145500     END-EVALUATE                                                         
145600                                                                          
145700     IF WS-KDRC > ZERO                                                    
145800       MOVE WS-ERROR-TYPE         TO ERR-IDMSG-ERROR                      
145900       MOVE WS-TEOUTDATA          TO WS-OUT-ERROR-TEXT                    
146000     END-IF                                                               
146100     .                                                                    
146200     EJECT                                                                
146300 S40-OPEN-DP-ERROR SECTION.                                               
146400                                                                          
146500     MOVE WS-MY-OWN-ADRESS TO SEND-ADDISPABS                              
146600     MOVE 'OPEN'           TO SEND-KDFUNC                                 
146700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
146800                         SEND-OPEN-AREA                                   
146900     IF SEND-KDRC > ZERO                                                  
147000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
147100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
147200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
147300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
147400     END-IF                                                               
147500     .                                                                    
147600                                                                          
147700 S40-PUT-DP-ERROR-HDR SECTION.                                            
147800                                                                          
147900     MOVE SPACE                     TO ERR2-REQU-AREA                     
148000     MOVE 001                       TO ERR2-REQU-IDMSGVER                 
148100     MOVE IDPGM                     TO ERR2-REQU-IDUSER                   
148200     MOVE WS-IDOUTTYPE-KEY          TO ERR2-HDR-IDOUTTYPE                 
148300     MOVE WS-IDOUTREC-KEY           TO ERR2-HDR-IDOUTREC                  
148400                                                                          
148500     MOVE 'PUT'                     TO SEND-KDFUNC                        
148600     MOVE  WZ04-SEND-IDCOM          TO SEND-IDCOM                         
148700     MOVE LENGTH OF ERR2-REQU-AREA  TO SEND-KVDLEN                        
148800     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
148900                         ERR2-REQU-AREA                                   
149000     IF SEND-KDRC > ZERO                                                  
149100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
149200       STRING 'S40 WZ01SEND PUT HDR ERROR RC=' KDRC-DISPLAY               
149300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
149400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
149500     END-IF                                                               
149600     .                                                                    
149700     SKIP3                                                                
149800 S40-PUT-DP-ERROR-DATA SECTION.                                           
149900                                                                          
150000     MOVE 'PUT'            TO SEND-KDFUNC                                 
150100     MOVE  WZ04-SEND-IDCOM TO SEND-IDCOM                                  
150200     MOVE 120              TO SEND-KVDLEN                                 
150300     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN                    
150400                                      REDA-TEOUTDATA-D                    
150500     IF SEND-KDRC > ZERO                                                  
150600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
150700       STRING 'S40 WZ01SEND PUT DATA ERROR RC=' KDRC-DISPLAY              
150800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
150900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
151000     END-IF                                                               
151100     .                                                                    
151200     SKIP3                                                                
151300 S40-CLOSE-DP-ERROR SECTION.                                              
151400                                                                          
151500     MOVE 'CLOSE'          TO SEND-KDFUNC                                 
151600     MOVE  WZ04-SEND-IDCOM TO SEND-IDCOM                                  
151700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
151800     .                                                                    
151900     EJECT                                                                
152000                                                                          
152100* -- DB2-SECTIONS                                                         
152200 DB2-SELECT-TZ4DIRU-TAB  SECTION.                                         
152300                                                                          
152400     MOVE '000100811'  TO GOOD-SQLCODECODES                               
152500                                                                          
152600     EXEC SQL                                                             
152700         SELECT  KDOUTMETH_1                                              
152800               , KDOUTMETH_2                                              
152900               , KDOUTMETH_3                                              
153000               , KDOUTMETH_4                                              
153100               , KDOUTMETH_5                                              
153200               , KDOUTMETH_6                                              
153300               , KDOUTMETH_7                                              
153400               , KDOUTMETH_8                                              
153500               , KDOUTMETH_9                                              
153600               , KDOUTMETH_10                                             
153700               , KDOUTMETH_11                                             
153800               , KDOUTMETH_12                                             
153900               , KDOUTMETH_13                                             
154000               , KDOUTMETH_14                                             
154100               , KDOUTMETH_15                                             
154200               , IDOUTDEST_1                                              
154300               , IDOUTDEST_2                                              
154400               , IDOUTDEST_3                                              
154500               , IDOUTDEST_4                                              
154600               , IDOUTDEST_5                                              
154700               , IDOUTDEST_6                                              
154800               , IDOUTDEST_7                                              
154900               , IDOUTDEST_8                                              
155000               , IDOUTDEST_9                                              
155100               , IDOUTDEST_10                                             
155200               , IDOUTDEST_11                                             
155300               , IDOUTDEST_12                                             
155400               , IDOUTDEST_13                                             
155500               , IDOUTDEST_14                                             
155600               , IDOUTDEST_15                                             
155700               , KVCOPIES_1                                               
155800               , KVCOPIES_2                                               
155900               , KVCOPIES_3                                               
156000               , KVCOPIES_4                                               
156100               , KVCOPIES_5                                               
156200               , KVCOPIES_6                                               
156300               , KVCOPIES_7                                               
156400               , KVCOPIES_8                                               
156500               , KVCOPIES_9                                               
156600               , KVCOPIES_10                                              
156700               , KVCOPIES_11                                              
156800               , KVCOPIES_12                                              
156900               , KVCOPIES_13                                              
157000               , KVCOPIES_14                                              
157100               , KVCOPIES_15                                              
157200               , FLCARRCNTL_1                                             
157300               , FLCARRCNTL_2                                             
157400               , FLCARRCNTL_3                                             
157500               , FLCARRCNTL_4                                             
157600               , FLCARRCNTL_5                                             
157700               , FLCARRCNTL_6                                             
157800               , FLCARRCNTL_7                                             
157900               , FLCARRCNTL_8                                             
158000               , FLCARRCNTL_9                                             
158100               , FLCARRCNTL_10                                            
158200               , FLCARRCNTL_11                                            
158300               , FLCARRCNTL_12                                            
158400               , FLCARRCNTL_13                                            
158500               , FLCARRCNTL_14                                            
158600               , FLCARRCNTL_15                                            
158700               , IDPFDEF_1                                                
158800               , IDPFDEF_2                                                
158900               , IDPFDEF_3                                                
159000               , IDPFDEF_4                                                
159100               , IDPFDEF_5                                                
159200               , IDPFDEF_6                                                
159300               , IDPFDEF_7                                                
159400               , IDPFDEF_8                                                
159500               , IDPFDEF_9                                                
159600               , IDPFDEF_10                                               
159700               , IDPFDEF_11                                               
159800               , IDPFDEF_12                                               
159900               , IDPFDEF_13                                               
160000               , IDPFDEF_14                                               
160100               , IDPFDEF_15                                               
160200               , IDFORMSNM_1                                              
160300               , IDFORMSNM_2                                              
160400               , IDFORMSNM_3                                              
160500               , IDFORMSNM_4                                              
160600               , IDFORMSNM_5                                              
160700               , IDFORMSNM_6                                              
160800               , IDFORMSNM_7                                              
160900               , IDFORMSNM_8                                              
161000               , IDFORMSNM_9                                              
161100               , IDFORMSNM_10                                             
161200               , IDFORMSNM_11                                             
161300               , IDFORMSNM_12                                             
161400               , IDFORMSNM_13                                             
161500               , IDFORMSNM_14                                             
161600               , IDFORMSNM_15                                             
161700               , TEVCOMST_1                                               
161800               , TEVCOMST_2                                               
161900               , TEVCOMST_3                                               
162000               , TEVCOMST_4                                               
162100               , TEVCOMST_5                                               
162200               , TEVCOMST_6                                               
162300               , TEVCOMST_7                                               
162400               , TEVCOMST_8                                               
162500               , TEVCOMST_9                                               
162600               , TEVCOMST_10                                              
162700               , TEVCOMST_11                                              
162800               , TEVCOMST_12                                              
162900               , TEVCOMST_13                                              
163000               , TEVCOMST_14                                              
163100               , TEVCOMST_15                                              
163200               , IDVCINIT_1                                               
163300               , IDVCINIT_2                                               
163400               , IDVCINIT_3                                               
163500               , IDVCINIT_4                                               
163600               , IDVCINIT_5                                               
163700               , IDVCINIT_6                                               
163800               , IDVCINIT_7                                               
163900               , IDVCINIT_8                                               
164000               , IDVCINIT_9                                               
164100               , IDVCINIT_10                                              
164200               , IDVCINIT_11                                              
164300               , IDVCINIT_12                                              
164400               , IDVCINIT_13                                              
164500               , IDVCINIT_14                                              
164600               , IDVCINIT_15                                              
164700               , TEFAX_1                                                  
164800               , TEFAX_2                                                  
164900               , TEFAX_3                                                  
165000               , TEFAX_4                                                  
165100               , TEFAX_5                                                  
165200               , KVDAGAR_RESEND                                           
165300               , IDMAIL_SENDER                                            
165400               , FLACIF_1                                                 
165500               , FLACIF_2                                                 
165600               , FLACIF_3                                                 
165700               , FLACIF_4                                                 
165800               , FLACIF_5                                                 
165900               , FLACIF_6                                                 
166000               , FLACIF_7                                                 
166100               , FLACIF_8                                                 
166200               , FLACIF_9                                                 
166300               , FLACIF_10                                                
166400               , FLACIF_11                                                
166500               , FLACIF_12                                                
166600               , FLACIF_13                                                
166700               , FLACIF_14                                                
166800               , FLACIF_15                                                
166900               , IDOUTTYPE                                                
167000               , IDOUTREC_FROM                                            
167100               , IDOUTREC_TO                                              
167200                                                                          
167300         INTO   :DIRU-KDOUTMETH-1                                         
167400              , :DIRU-KDOUTMETH-2                                         
167500              , :DIRU-KDOUTMETH-3                                         
167600              , :DIRU-KDOUTMETH-4                                         
167700              , :DIRU-KDOUTMETH-5                                         
167800              , :DIRU-KDOUTMETH-6                                         
167900              , :DIRU-KDOUTMETH-7                                         
168000              , :DIRU-KDOUTMETH-8                                         
168100              , :DIRU-KDOUTMETH-9                                         
168200              , :DIRU-KDOUTMETH-10                                        
168300              , :DIRU-KDOUTMETH-11                                        
168400              , :DIRU-KDOUTMETH-12                                        
168500              , :DIRU-KDOUTMETH-13                                        
168600              , :DIRU-KDOUTMETH-14                                        
168700              , :DIRU-KDOUTMETH-15                                        
168800              , :DIRU-IDOUTDEST-1                                         
168900              , :DIRU-IDOUTDEST-2                                         
169000              , :DIRU-IDOUTDEST-3                                         
169100              , :DIRU-IDOUTDEST-4                                         
169200              , :DIRU-IDOUTDEST-5                                         
169300              , :DIRU-IDOUTDEST-6                                         
169400              , :DIRU-IDOUTDEST-7                                         
169500              , :DIRU-IDOUTDEST-8                                         
169600              , :DIRU-IDOUTDEST-9                                         
169700              , :DIRU-IDOUTDEST-10                                        
169800              , :DIRU-IDOUTDEST-11                                        
169900              , :DIRU-IDOUTDEST-12                                        
170000              , :DIRU-IDOUTDEST-13                                        
170100              , :DIRU-IDOUTDEST-14                                        
170200              , :DIRU-IDOUTDEST-15                                        
170300              , :DIRU-KVCOPIES-1                                          
170400              , :DIRU-KVCOPIES-2                                          
170500              , :DIRU-KVCOPIES-3                                          
170600              , :DIRU-KVCOPIES-4                                          
170700              , :DIRU-KVCOPIES-5                                          
170800              , :DIRU-KVCOPIES-6                                          
170900              , :DIRU-KVCOPIES-7                                          
171000              , :DIRU-KVCOPIES-8                                          
171100              , :DIRU-KVCOPIES-9                                          
171200              , :DIRU-KVCOPIES-10                                         
171300              , :DIRU-KVCOPIES-11                                         
171400              , :DIRU-KVCOPIES-12                                         
171500              , :DIRU-KVCOPIES-13                                         
171600              , :DIRU-KVCOPIES-14                                         
171700              , :DIRU-KVCOPIES-15                                         
171800              , :DIRU-FLCARRCNTL-1                                        
171900              , :DIRU-FLCARRCNTL-2                                        
172000              , :DIRU-FLCARRCNTL-3                                        
172100              , :DIRU-FLCARRCNTL-4                                        
172200              , :DIRU-FLCARRCNTL-5                                        
172300              , :DIRU-FLCARRCNTL-6                                        
172400              , :DIRU-FLCARRCNTL-7                                        
172500              , :DIRU-FLCARRCNTL-8                                        
172600              , :DIRU-FLCARRCNTL-9                                        
172700              , :DIRU-FLCARRCNTL-10                                       
172800              , :DIRU-FLCARRCNTL-11                                       
172900              , :DIRU-FLCARRCNTL-12                                       
173000              , :DIRU-FLCARRCNTL-13                                       
173100              , :DIRU-FLCARRCNTL-14                                       
173200              , :DIRU-FLCARRCNTL-15                                       
173300              , :DIRU-IDPFDEF-1                                           
173400              , :DIRU-IDPFDEF-2                                           
173500              , :DIRU-IDPFDEF-3                                           
173600              , :DIRU-IDPFDEF-4                                           
173700              , :DIRU-IDPFDEF-5                                           
173800              , :DIRU-IDPFDEF-6                                           
173900              , :DIRU-IDPFDEF-7                                           
174000              , :DIRU-IDPFDEF-8                                           
174100              , :DIRU-IDPFDEF-9                                           
174200              , :DIRU-IDPFDEF-10                                          
174300              , :DIRU-IDPFDEF-11                                          
174400              , :DIRU-IDPFDEF-12                                          
174500              , :DIRU-IDPFDEF-13                                          
174600              , :DIRU-IDPFDEF-14                                          
174700              , :DIRU-IDPFDEF-15                                          
174800              , :DIRU-IDFORMSNM-1                                         
174900              , :DIRU-IDFORMSNM-2                                         
175000              , :DIRU-IDFORMSNM-3                                         
175100              , :DIRU-IDFORMSNM-4                                         
175200              , :DIRU-IDFORMSNM-5                                         
175300              , :DIRU-IDFORMSNM-6                                         
175400              , :DIRU-IDFORMSNM-7                                         
175500              , :DIRU-IDFORMSNM-8                                         
175600              , :DIRU-IDFORMSNM-9                                         
175700              , :DIRU-IDFORMSNM-10                                        
175800              , :DIRU-IDFORMSNM-11                                        
175900              , :DIRU-IDFORMSNM-12                                        
176000              , :DIRU-IDFORMSNM-13                                        
176100              , :DIRU-IDFORMSNM-14                                        
176200              , :DIRU-IDFORMSNM-15                                        
176300              , :DIRU-TEVCOMST-1                                          
176400              , :DIRU-TEVCOMST-2                                          
176500              , :DIRU-TEVCOMST-3                                          
176600              , :DIRU-TEVCOMST-4                                          
176700              , :DIRU-TEVCOMST-5                                          
176800              , :DIRU-TEVCOMST-6                                          
176900              , :DIRU-TEVCOMST-7                                          
177000              , :DIRU-TEVCOMST-8                                          
177100              , :DIRU-TEVCOMST-9                                          
177200              , :DIRU-TEVCOMST-10                                         
177300              , :DIRU-TEVCOMST-11                                         
177400              , :DIRU-TEVCOMST-12                                         
177500              , :DIRU-TEVCOMST-13                                         
177600              , :DIRU-TEVCOMST-14                                         
177700              , :DIRU-TEVCOMST-15                                         
177800              , :DIRU-IDVCINIT-1                                          
177900              , :DIRU-IDVCINIT-2                                          
178000              , :DIRU-IDVCINIT-3                                          
178100              , :DIRU-IDVCINIT-4                                          
178200              , :DIRU-IDVCINIT-5                                          
178300              , :DIRU-IDVCINIT-6                                          
178400              , :DIRU-IDVCINIT-7                                          
178500              , :DIRU-IDVCINIT-8                                          
178600              , :DIRU-IDVCINIT-9                                          
178700              , :DIRU-IDVCINIT-10                                         
178800              , :DIRU-IDVCINIT-11                                         
178900              , :DIRU-IDVCINIT-12                                         
179000              , :DIRU-IDVCINIT-13                                         
179100              , :DIRU-IDVCINIT-14                                         
179200              , :DIRU-IDVCINIT-15                                         
179300              , :DIRU-TEFAX-1                                             
179400              , :DIRU-TEFAX-2                                             
179500              , :DIRU-TEFAX-3                                             
179600              , :DIRU-TEFAX-4                                             
179700              , :DIRU-TEFAX-5                                             
179800              , :DIRU-KVDAGAR-RESEND                                      
179900              , :DIRU-IDMAIL-SENDER                                       
180000              , :DIRU-FLACIF-1                                            
180100              , :DIRU-FLACIF-2                                            
180200              , :DIRU-FLACIF-3                                            
180300              , :DIRU-FLACIF-4                                            
180400              , :DIRU-FLACIF-5                                            
180500              , :DIRU-FLACIF-6                                            
180600              , :DIRU-FLACIF-7                                            
180700              , :DIRU-FLACIF-8                                            
180800              , :DIRU-FLACIF-9                                            
180900              , :DIRU-FLACIF-10                                           
181000              , :DIRU-FLACIF-11                                           
181100              , :DIRU-FLACIF-12                                           
181200              , :DIRU-FLACIF-13                                           
181300              , :DIRU-FLACIF-14                                           
181400              , :DIRU-FLACIF-15                                           
181500              , :DIRU-IDOUTTYPE                                           
181600              , :DIRU-IDOUTREC-FROM                                       
181700              , :DIRU-IDOUTREC-TO                                         
181800                                                                          
181900         FROM    TZ4DIRU                                                  
182000                                                                          
182100         WHERE   IDOUTTYPE      = :WS-IDOUTTYPE-KEY                       
182200         AND     IDOUTREC_FROM  = :WS-IDOUTREC-KEY                        
182300         AND     IDOUTREC_TO    = :WS-IDOUTREC-KEY                        
182400         OR      IDOUTTYPE      = :WS-IDOUTTYPE-KEY                       
182500         AND     IDOUTREC_FROM <= :WS-IDOUTREC-KEY                        
182600         AND     IDOUTREC_TO   >= :WS-IDOUTREC-KEY                        
182700         AND NOT EXISTS                                                   
182800               (SELECT  *                                                 
182900                FROM    TZ4DIRU                                           
183000                WHERE   IDOUTTYPE     = :WS-IDOUTTYPE-KEY                 
183100                AND     IDOUTREC_FROM = :WS-IDOUTREC-KEY                  
183200                AND     IDOUTREC_TO   = :WS-IDOUTREC-KEY)                 
183300     END-EXEC                                                             
183400                                                                          
183500     MOVE SQLCODE TO SQLCODE-WS                                           
183600     PERFORM DB2-STATUS-CHECK                                             
183700                                                                          
183800     IF SQLCODE-WS = 811                                                  
183900       MOVE YES TO WS-FLRULEMULT                                          
184000     ELSE                                                                 
184100       MOVE NOO TO WS-FLRULEMULT                                          
184200     END-IF                                                               
184300     .                                                                    
184400     EJECT                                                                
184500 DB2-UPDATE-TZ4DIRU-TIANVDAT  SECTION.                                    
184600                                                                          
184700     MOVE 000     TO GOOD-SQLCODECODES                                    
184800     EXEC SQL                                                             
184900         UPDATE TZ4DIRU                                                   
185000           SET   TIANVDAT      = :WS-CURRENT-DATE                         
185100                                                                          
185200         WHERE   IDOUTTYPE     = :DIRU-IDOUTTYPE                          
185300          AND    IDOUTREC_FROM = :DIRU-IDOUTREC-FROM                      
185400          AND    IDOUTREC_TO   = :DIRU-IDOUTREC-TO                        
185500     END-EXEC                                                             
185600                                                                          
185700     MOVE SQLCODE TO SQLCODE-WS                                           
185800     PERFORM DB2-STATUS-CHECK                                             
185900     .                                                                    
186000                                                                          
186100 DB2-SELECT-TZ4REKY-TAB  SECTION.                                         
186200                                                                          
186300     MOVE 000100  TO GOOD-SQLCODECODES                                    
186400                                                                          
186500     EXEC SQL                                                             
186600         SELECT  IDLIST                                                   
186700                                                                          
186800         INTO   :WS-TZ4REKY-DUMMY                                         
186900                                                                          
187000         FROM    TZ4REKY                                                  
187100                                                                          
187200         WHERE   IDOUTTYPE = :REKY-IDOUTTYPE                              
187300         AND     IDOUTREC  = :REKY-IDOUTREC                               
187400         AND     IDLIST    = :REKY-IDLIST                                 
187500         AND     TIREGDAT  = :REKY-TIREGDAT                               
187600         AND     TIKLOCK   = :REKY-TIKLOCK                                
187700         AND     IDLOPNR   = :REKY-IDLOPNR                                
187800     END-EXEC                                                             
187900                                                                          
188000     MOVE SQLCODE TO SQLCODE-WS                                           
188100     PERFORM DB2-STATUS-CHECK                                             
188200     .                                                                    
188300     EJECT                                                                
188400 DB2-INSERT-TZ4REKY-TAB  SECTION.                                         
188500     SKIP2                                                                
188600     MOVE 000   TO GOOD-SQLCODECODES                                      
188700     EXEC SQL                                                             
188800       INSERT INTO TZ4REKY                                                
188900         (IDOUTTYPE,IDOUTREC,IDLIST,TIREGDAT,TIKLOCK,IDLOPNR              
189000         ,KDOUTMETH,IDOUTDEST,TIAAMMDD_RENS,KVCOPIES,FLCARRCNTL           
189100         ,IDPFDEF,IDFORMSNM,TEVCOMST,IDVCINIT                             
189200         ,TEFAX_1,TEFAX_2,TEFAX_3,TEFAX_4,TEFAX_5                         
189300         ,IDMAIL_SENDER,KVANTEX_PRINTAD,FLRULEMISS,FLACIF)                
189400                                                                          
189500       VALUES                                                             
189600         (:REKY-IDOUTTYPE,:REKY-IDOUTREC,:REKY-IDLIST                     
189700         ,:REKY-TIREGDAT,:REKY-TIKLOCK,:REKY-IDLOPNR                      
189800         ,:REKY-KDOUTMETH,:REKY-IDOUTDEST,:REKY-TIAAMMDD-RENS             
189900         ,:REKY-KVCOPIES,:REKY-FLCARRCNTL,:REKY-IDPFDEF                   
190000         ,:REKY-IDFORMSNM,:REKY-TEVCOMST,:REKY-IDVCINIT                   
190100         ,:WS-TEFAX-1,:WS-TEFAX-2,:WS-TEFAX-3,:WS-TEFAX-4                 
190200         ,:WS-TEFAX-5,:REKY-IDMAIL-SENDER                                 
190300         ,:REKY-KVANTEX-PRINTAD,:REKY-FLRULEMISS,:REKY-FLACIF)            
190400     END-EXEC                                                             
190500                                                                          
190600     MOVE SQLCODE TO SQLCODE-WS                                           
190700     PERFORM DB2-STATUS-CHECK                                             
190800     .                                                                    
190900     EJECT                                                                
191000 DB2-SELECT-TZ4REDA-TAB  SECTION.                                         
191100                                                                          
191200     MOVE 000100  TO GOOD-SQLCODECODES                                    
191300                                                                          
191400     EXEC SQL                                                             
191500         SELECT  IDLIST                                                   
191600                                                                          
191700         INTO   :WS-TZ4REDA-DUMMY                                         
191800                                                                          
191900         FROM    TZ4REDA                                                  
192000                                                                          
192100         WHERE   IDOUTTYPE = :REDA-IDOUTTYPE                              
192200         AND     IDOUTREC  = :REDA-IDOUTREC                               
192300         AND     IDLIST    = :REDA-IDLIST                                 
192400         AND     TIREGDAT  = :REDA-TIREGDAT                               
192500         AND     TIKLOCK   = :REDA-TIKLOCK                                
192600         AND     IDLOPNR   = :REDA-IDLOPNR                                
192700         AND     KVPOST    = :REDA-KVPOST                                 
192800     END-EXEC                                                             
192900                                                                          
193000     MOVE SQLCODE TO SQLCODE-WS                                           
193100     PERFORM DB2-STATUS-CHECK                                             
193200     .                                                                    
193300     EJECT                                                                
193400 DB2-INSERT-TZ4REDA-TAB  SECTION.                                         
193500     SKIP2                                                                
193600     MOVE 000   TO GOOD-SQLCODECODES                                      
193700     EXEC SQL                                                             
193800       INSERT INTO TZ4REDA                                                
193900         (IDOUTTYPE,IDOUTREC,IDLIST,TIREGDAT,TIKLOCK                      
194000         ,IDLOPNR,KVPOST,TEOUTDATA)                                       
194100                                                                          
194200       VALUES                                                             
194300         (:REDA-IDOUTTYPE,:REDA-IDOUTREC,:REDA-IDLIST                     
194400         ,:REDA-TIREGDAT,:REDA-TIKLOCK,:REDA-IDLOPNR                      
194500         ,:REDA-KVPOST,:REDA-TEOUTDATA)                                   
194600     END-EXEC                                                             
194700                                                                          
194800     MOVE SQLCODE TO SQLCODE-WS                                           
194900     PERFORM DB2-STATUS-CHECK                                             
195000     .                                                                    
195100     EJECT                                                                
195200 DB2-STATUS-CHECK  SECTION.                                               
195300                                                                          
195400     SET SQLCODE-IX TO 1                                                  
195500     SEARCH GOOD-SQLCODE                                                  
195600       AT END                                                             
195700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
195800          DELIMITED BY SIZE INTO ERROR-TEXT                               
195900          CALL ABEND USING RKOD-ABEND-DB2                                 
196000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
196100     END-SEARCH                                                           
196200     .                                                                    
