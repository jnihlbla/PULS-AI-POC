000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5013300.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   15/11/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES UPDATES OF FINANCIAL STEERING               
000900*        PARAMETERS.                                                      
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDB6                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W5T133                                              
001500*        MID:         W5I13301                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W5O133N1                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W5013300'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  YES                         PIC X       VALUE 'J'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300 77  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
003400 77  WS-START-DATE               PIC 9(6)    VALUE ZERO.                  
003500 77  WS-PERCENT-FROM             PIC 9(3)V9(3) VALUE ZERO.                
003600 77  WS-PERCENT                  PIC 9(3)V9(2) VALUE ZERO.                
003700 77  WS-RELANDCO-EXP             PIC 9(3)V9(1) VALUE ZERO.                
003800 77  WS-RELANDCO-PERC            PIC 9(3)V9(3) VALUE ZERO.                
003900 77  WS-RELANDCO-FROM-PERC       PIC 9(3)V9(3) VALUE ZERO.                
004000 77  WS-RELANDCO-FROM            PIC 9(3)V9(1) VALUE ZERO.                
004100 77  WS-RELANDCO-TO              PIC 9(3)V9(1) VALUE ZERO.                
004200 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004300 77  WS-IDFRIDATA                PIC X(25).                               
004400 77  WS-CNT                      PIC 9(2)    VALUE ZERO.                  
004500 77  WS-IX                       PIC S9(3)   COMP-3 VALUE ZERO.           
004600 77  MAX-IX                      PIC S9(3)   COMP-3 VALUE 46.             
004700 77  MAX-IX-DC                   PIC S9(3)   COMP-3 VALUE 314.            
004800 77  WS-IDDC-SEND                PIC X(2)    VALUE SPACE.                 
004900                                                                          
005000 77  WS-SEND-IDFTG               PIC 9(2)    VALUE 0.                     
005100     88 SEND-FTG-US                          VALUE 53.                    
005200     88 SEND-FTG-CA                          VALUE 54.                    
005300                                                                          
005400 77  WS-REC-IDFTG                PIC 9(2)    VALUE 0.                     
005500     88 REC-FTG-US                           VALUE 53.                    
005600     88 REC-FTG-CA                           VALUE 54.                    
005700                                                                          
005800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-WRONG                        VALUE 'N'.                   
006300                                                                          
006400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006500     88  KEYS-OK                             VALUE 'J'.                   
006600     88  KEYS-WRONG                          VALUE 'N'.                   
006700                                                                          
006800 77  IDFTG-SW                    PIC X       VALUE 'J'.                   
006900     88  IDFTG-YES                           VALUE 'J'.                   
007000     88  IDFTG-NO                            VALUE 'N'.                   
007100                                                                          
007200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007300     88  OWN-MID                             VALUE '5133'.                
007400     88  GOOD-MID                            VALUE '5131' '5132'          
007500                                                   '5133' '5134'          
007600                                                   '5135' '5136'          
007700                                                   '5137' '5138'          
007800                                                   '5139'.                
007900     88  HELP-MID                            VALUE '0551'.                
008000     EJECT                                                                
008100 01  W-DAGENS-DATUM-ONE-YEAR            PIC 9(6) VALUE ZERO.              
008200 01  FILLER REDEFINES W-DAGENS-DATUM-ONE-YEAR.                            
008300     03  W-DAGENS-DATUM-ONE-YEAR-YY     PIC 9(2).                         
008400     03  W-DAGENS-DATUM-ONE-YEAR-MM     PIC 9(2).                         
008500     03  W-DAGENS-DATUM-ONE-YEAR-DD     PIC 9(2).                         
008600                                                                          
008700 01  WORK-DATE                   PIC 9(6)   VALUE ZERO.                   
008800 01  FILLER REDEFINES WORK-DATE.                                          
008900     03  WORK-DATUM-YY                  PIC 9(2).                         
009000     03  WORK-DATUM-MM                  PIC 9(2).                         
009100     03  WORK-DATUM-DD                  PIC 9(2).                         
009200                                                                          
009300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009400 01  GENERAL-SUBPROGRAMS.                                                 
009500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010000     EJECT                                                                
010100*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
010200*01  -COPY WDECAREA                                                       
010300*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010400*01 -COPY WMEDAREA                                                        
010500*01 -COPY WWDC03                                                          
010600*01 -COPY WWDC99                                                          
010700*01 -COPY WWDCKONS                                                        
010800*01  -COPY WDATAREA                                                       
010900 01  WZ20DATE PIC X(8) VALUE 'WZ20DATE'.                                  
011000     SKIP3                                                                
011100*    -COPY WZ20DATE                                                       
011200     EJECT                                                                
011300*                                                                         
011400 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
011500     SKIP3                                                                
011600*    -COPY WZ20DAYS                                                       
011700     EJECT                                                                
011800*                                                                         
011900     EJECT                                                                
012000     SKIP3                                                                
012100 01  MESSAGE-CODES.                                                       
012200     03  ERR-CORR-HILITE-FLDS     PIC X(3)    VALUE '001'.                
012300     03  INF-PRESS-PF11           PIC X(3)    VALUE '003'.                
012400     03  ERR-PF11-AND-NO-DATA     PIC X(3)    VALUE '011'.                
012500     03  INF-UPDATE-DONE          PIC X(3)    VALUE '101'.                
012600     03  ERR-WRONG-KEY            PIC X(3)    VALUE '401'.                
012700     03  ERR-FUTURE-DATE          PIC X(3)    VALUE '363'.                
012800     03  ERR-ONE-YEAR-IN-FUTURE   PIC X(3)    VALUE '364'.                
012900     03  ERR-INFO-MISSING         PIC X(3)    VALUE '413'.                
013000     EJECT                                                                
013100*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
013200*                                                                         
013300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013400     SKIP3                                                                
013500*01 -COPY WMSGINIT                                                        
013600     EJECT                                                                
013700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
013800*                                                                         
013900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014000     SKIP3                                                                
014100*01  MID -COPY W5I13301                                                   
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014400     SKIP3                                                                
014500*01  -COPY WMSGAREA                                                       
014600     EJECT                                                                
014700     03  MOD REDEFINES MSG-AREA.                                          
014800*      05  -COPY W5O13301                                                 
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015100     SKIP3                                                                
015200*01  -COPY WMFSAREA                                                       
015300     EJECT                                                                
015400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015700     SKIP3                                                                
015800 01  KEYS-FOR-DLI.                                                        
015900     03  W-KDSEGKEY-X.                                                    
016000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016100     03  W-IDDC-X.                                                        
016200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016300     SKIP2                                                                
016400*    --- STATUS CODES FROM IMS                                            
016500 01  STATUS-WS                   PIC XX.                                  
016600     88  SEGMENT-FOUND                       VALUE '  '.                  
016700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016900     SKIP2                                                                
017000 01  GOOD-STATUSCODES.                                                    
017100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200     SKIP3                                                                
017300 01  SSA1                        PIC X(64).                               
017400 01  SSA2                        PIC X(64).                               
017500     EJECT                                                                
017600*    --- IMS FUNCTION CODES                                               
017700*01  -COPY W0003                                                          
017800     EJECT                                                                
017900*    ---  DLI INPUT-OUTPUT AREA                                           
018000                                                                          
018100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
018200 01  DLI-IO-WDB601.                                                       
018300*    03  -COPY WDB601                                                     
018400     EJECT                                                                
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB617'.                      
018600 01  DLI-IO-WDB617.                                                       
018700*    03  -COPY WDB617                                                     
018800     EJECT                                                                
018900 LINKAGE SECTION.                                                         
019000*01  -COPY W0009   -PRE MSG-                                              
019100*01  -COPY W0008   -PRE WDP7-                                             
019200     05  FILLER                  PIC X.                                   
019300                                                                          
019400*01  -COPY W0008  -PRE WDB6-                                              
019500     05  FILLER                  PIC X.                                   
019600     EJECT                                                                
019700 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
019800 MAIN SECTION.                                                            
019900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
020000                                                                          
020100     PERFORM IMS-GET-MSG                                                  
020200     IF SEGMENT-FOUND                                                     
020300       PERFORM A-INIT                                                     
020400       PERFORM B-CHECK-KEYS                                               
020500       IF KEYS-OK                                                         
020600         IF MFS-UPDATE                                                    
020700           PERFORM G-CHECK-INPUT                                          
020800           IF INDATA-OK                                                   
020900             PERFORM H-UPDATE                                             
021000           END-IF                                                         
021100         ELSE                                                             
021200           IF MFS-FIRST                                                   
021300             PERFORM C-FIRST-PAGE                                         
021400           ELSE                                                           
021500             PERFORM E-SAME-PAGE                                          
021600           END-IF                                                         
021700         END-IF                                                           
021800         PERFORM F-READ-SHOW-INFO                                         
021900       END-IF                                                             
022000       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O13301 + 4                      
022100       PERFORM IMS-INSERT-MSG                                             
022200     END-IF                                                               
022300                                                                          
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900     IF MSG-DOUBLE-TRANSACTIONS                                           
023000       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W5I13301                 
023100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
023200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023300     ELSE                                                                 
023400       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W5I13301                  
023500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
023600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023700     END-IF                                                               
023800                                                                          
023900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
024100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024200                                                                          
024300     MOVE LOW-VALUE TO MSG-AREA                                           
024400     MOVE 'W5O133N1' TO MFS-IDMOD                                         
024500     MOVE '5133' TO MOD-IDTRANS                                           
024600     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
024700                                                                          
024800     IF OWN-MID OR HELP-MID                                               
024900       CONTINUE                                                           
025000     ELSE                                                                 
025100       MOVE SPACE TO MFS-KDTRTYP                                          
025200       MOVE '7' TO MFS-IDPFK                                              
025300     END-IF                                                               
025400     ACCEPT TODAYS-DATE FROM DATE                                         
025500     ACCEPT WORK-DATE    FROM DATE                                        
025600                                                                          
025700     MOVE WORK-DATE             TO DATE-TIDATE                            
025800     .                                                                    
025900     EJECT                                                                
026000 B-CHECK-KEYS SECTION.                                                    
026100                                                                          
026200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026300     MOVE '001'             TO MSGI-KDCALL                                
026400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026600     MOVE '5133'            TO MSGI-IDTRANS                               
026700     IF GOOD-MID                                                          
026800        MOVE MID-IDDC-IN    TO MSGI-IDDC-KEY                              
026900     END-IF                                                               
027000     CALL W005INIT       USING MSGI-WMSGINIT WDP7-PCB                     
027100                                                                          
027200*    - LANGUAGE TO BE USED BY MEDKONV                                     
027300     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
027400                                                                          
027500     MOVE YES               TO KEYS-SW                                    
027600                               IDFTG-SW                                   
027700     PERFORM BA-CHECK-IDDC                                                
027800                                                                          
027900     IF KEYS-WRONG                                                        
028000       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
028100       CALL WMEDKONV     USING MED-WMEDAREA                               
028200       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
028300       PERFORM MFS-ERASE-FIELD-IN                                         
028400       PERFORM MFS-ERASE-FIELD-OUT                                        
028500     END-IF                                                               
028600     .                                                                    
028700     EJECT                                                                
028800 BA-CHECK-IDDC SECTION.                                                   
028900                                                                          
029000     IF MID-IDDC-IN NOT = ALL '+'                                         
029100        MOVE MID-IDDC-IN    TO W-IDDC                                     
029200                               MOD-IDDC                                   
029300     ELSE                                                                 
029400        MOVE MSGI-IDDC-KEY  TO W-IDDC                                     
029500                               MOD-IDDC                                   
029600     END-IF                                                               
029700** DC WITH FTG CODE 57 NOT ALLOWED **                                     
029800     PERFORM IMS-GU-WDB601                                                
029900     IF SEGMENT-FOUND                                                     
030000        IF NOT DCS-FTG-PV                                                 
030100         CONTINUE                                                         
030200        ELSE                                                              
030300         MOVE NOO    TO KEYS-SW                                           
030400        END-IF                                                            
030500     ELSE                                                                 
030600        MOVE NOO    TO KEYS-SW                                            
030700     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 C-FIRST-PAGE SECTION.                                                    
031100                                                                          
031200     PERFORM MFS-ERASE-FIELD-IN                                           
031300     .                                                                    
031400     EJECT                                                                
031500 E-SAME-PAGE SECTION.                                                     
031600                                                                          
031700     IF MID-INPUT = ALL '+'                                               
031800       PERFORM MFS-ERASE-FIELD-IN                                         
031900     ELSE                                                                 
032000       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
032100       CALL WMEDKONV    USING MED-WMEDAREA                                
032200       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
032300       PERFORM MFS-READ-IN-AGAIN                                          
032400       PERFORM EA-MID-INDATA-TILL-MOD                                     
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 EA-MID-INDATA-TILL-MOD SECTION.                                          
032900                                                                          
033000     IF MID-RELANDCO-EXP-IN = ALL '+'                                     
033100       MOVE MFS-ERASE-FIELD        TO MOD-RELANDCO-EXP-IN                 
033200     ELSE                                                                 
033300       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELANDCO-EXP-IN                 
033400     END-IF                                                               
033500                                                                          
033600     IF MID-TILANDCO-IN = ALL '+'                                         
033700       MOVE MFS-ERASE-FIELD        TO MOD-TILANDCO-IN                     
033800     ELSE                                                                 
033900       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TILANDCO-IN                     
034000     END-IF                                                               
034100                                                                          
034200     IF MID-RELANDCO-FROM-IN = ALL '+'                                    
034300       MOVE MFS-ERASE-FIELD        TO MOD-RELANDCO-FROM-IN                
034400     ELSE                                                                 
034500       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELANDCO-FROM-IN                
034600     END-IF                                                               
034700                                                                          
034800     IF MID-REDMTRL-IN = ALL '+'                                          
034900       MOVE MFS-ERASE-FIELD        TO MOD-REDMTRL-IN                      
035000     ELSE                                                                 
035100       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-REDMTRL-IN                      
035200     END-IF                                                               
035300                                                                          
035400     IF MID-REDIRLON-IN = ALL '+'                                         
035500       MOVE MFS-ERASE-FIELD        TO MOD-REDIRLON-IN                     
035600     ELSE                                                                 
035700       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-REDIRLON-IN                     
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 F-READ-SHOW-INFO SECTION.                                                
036200                                                                          
036300     PERFORM FA-GET-IDDISTR                                               
036400                                                                          
036500     PERFORM IMS-GU-WDB617                                                
036600     IF SEGMENT-FOUND                                                     
036700                                                                          
036800        IF PROC-RELANDCO-EXP > 0                                          
036900          COMPUTE WS-RELANDCO-EXP =                                       
037000                  (PROC-RELANDCO-EXP - 1) * 100                           
037100          MOVE WS-RELANDCO-EXP   TO MOD-RELANDCO-EXP                      
037200        ELSE                                                              
037300           MOVE 0                 TO MOD-RELANDCO-EXP                     
037400        END-IF                                                            
037500                                                                          
037600        COMPUTE WS-PERCENT = PROC-REDIRLON     * 100                      
037700        MOVE WS-PERCENT (1:3)     TO MOD-REDIRLON                         
037800                                                                          
037900        COMPUTE WS-PERCENT = PROC-REDMTRL      * 100                      
038000        MOVE WS-PERCENT (1:3)     TO MOD-REDMTRL                          
038100                                                                          
038200        IF PROC-RELANDCO-FROM > 0                                         
038300          COMPUTE WS-RELANDCO-FROM =                                      
038400                  (PROC-RELANDCO-FROM) * 100                              
038500          MOVE WS-RELANDCO-FROM  TO MOD-RELANDCO-FROM                     
038600        ELSE                                                              
038700           MOVE 0                 TO MOD-RELANDCO-FROM                    
038800        END-IF                                                            
038900                                                                          
039000        IF PROC-RELANDCO-TO > 0                                           
039100          COMPUTE WS-RELANDCO-TO   = PROC-RELANDCO-TO * 100               
039200          MOVE WS-RELANDCO-TO       TO MOD-RELANDCO-TO                    
039300          MOVE WS-RELANDCO-TO    TO MOD-RELANDCO-TO                       
039400        ELSE                                                              
039500          MOVE 0                 TO MOD-RELANDCO-TO                       
039600        END-IF                                                            
039700                                                                          
039800        MOVE PROC-IDUSER          TO MOD-IDUSER                           
039900        IF PROC-TIUPPDAT = 0                                              
040000          MOVE MFS-ERASE-FIELD   TO MOD-TIUPPDAT                          
040100        ELSE                                                              
040200          MOVE PROC-TIUPPDAT     TO MOD-TIUPPDAT                          
040300        END-IF                                                            
040400                                                                          
040500        IF PROC-TILANDCO = 0                                              
040600          MOVE MFS-ERASE-FIELD   TO MOD-TILANDCO                          
040700        ELSE                                                              
040800          MOVE PROC-TILANDCO     TO MOD-TILANDCO                          
040900        END-IF                                                            
041000                                                                          
041100     ELSE                                                                 
041200        PERFORM MFS-ERASE-FIELD-OUT                                       
041300     END-IF                                                               
041400     .                                                                    
041500     EJECT                                                                
041600                                                                          
041700 FA-GET-IDDISTR SECTION.                                                  
041800                                                                          
041900     MOVE +1 TO WS-IX                                                     
042000     PERFORM UNTIL WS-IX = MAX-IX                                         
042100       MOVE ZERO     TO MOD-IDDISTR(WS-IX)                                
042200       ADD +1 TO WS-IX                                                    
042300     END-PERFORM                                                          
042400**************************************************************            
042500* SHOW EXPORT DIST ONLY WHEN FTG CODE NOT = FOR SENDING DC AND            
042600* RECEIVING DC EXCEPT FOR US AND CA PER WWDC03.                           
042700                                                                          
042800     MOVE W-IDDC              TO WS-IDDC-SEND                             
042900     MOVE +1 TO WS-IX                                                     
043000     SET WWDC03-IX TO +1                                                  
043100     PERFORM UNTIL WWDC03-IX = MAX-IX-DC                                  
043110                OR WS-IX = MAX-IX                                         
043200       IF WWDC03-IDDC-SEND(WWDC03-IX) = WS-IDDC-SEND                      
043400         PERFORM FAA-GET-IDDISTR                                          
043500       END-IF                                                             
043600       SET WWDC03-IX UP BY +1                                             
043700     END-PERFORM                                                          
043800     MOVE WS-IDDC-SEND        TO W-IDDC                                   
043900**************************************************************            
044000     .                                                                    
044100     EJECT                                                                
044200 FAA-GET-IDDISTR SECTION.                                                 
044300                                                                          
044400     IF IDFTG-YES                                                         
044500       MOVE WWDC03-IDDC-SEND(WWDC03-IX) TO W-IDDC                         
044700       PERFORM IMS-GU-WDB601                                              
044800        IF SEGMENT-FOUND                                                  
044900          MOVE DCS-IDFTG      TO WS-SEND-IDFTG                            
045000          MOVE NOO            TO IDFTG-SW                                 
045100        END-IF                                                            
045200     END-IF                                                               
045300     MOVE WWDC03-IDDC-REC(WWDC03-IX)                                      
045400                              TO W-IDDC                                   
045500     PERFORM IMS-GU-WDB601                                                
045600     IF SEGMENT-FOUND                                                     
045700       MOVE DCS-IDFTG         TO WS-REC-IDFTG                             
045800       IF WS-SEND-IDFTG NOT = WS-REC-IDFTG                                
045900         IF (SEND-FTG-US AND REC-FTG-CA)                                  
046000         OR (SEND-FTG-CA AND REC-FTG-US)                                  
046200           CONTINUE                                                       
046300         ELSE                                                             
046400           MOVE WWDC03-SOK-IDDISTR(WWDC03-IX)                             
046500                            TO MOD-IDDISTR(WS-IX)                         
046600           ADD +1           TO WS-IX                                      
046700         END-IF                                                           
046800       END-IF                                                             
046900     END-IF                                                               
047000     .                                                                    
047100     EJECT                                                                
047200 G-CHECK-INPUT SECTION.                                                   
047300                                                                          
047400     MOVE YES  TO INDATA-SW                                               
047500     IF MID-INPUT = ALL '+'                                               
047600        MOVE ERR-PF11-AND-NO-DATA    TO MED-IDMFSFEL                      
047700        CALL WMEDKONV             USING MED-WMEDAREA                      
047800        MOVE MED-TEMFSFEL            TO MOD-TEMFSFEL                      
047900        PERFORM MFS-ERASE-FIELD-IN                                        
048000        PERFORM MFS-ERASE-FIELD-OUT                                       
048100        MOVE NOO                     TO INDATA-SW                         
048200     ELSE                                                                 
048300        PERFORM GA-CHECK-RELANDCO                                         
048400        PERFORM GD-CHECK-TILANDCO                                         
048500        PERFORM GB-CHECK-REDIRLON                                         
048600        PERFORM GC-CHECK-REDMTRL                                          
048700        IF INDATA-OK                                                      
048800           CONTINUE                                                       
048900        ELSE                                                              
049000           IF MED-TEMFSFEL = ERR-FUTURE-DATE                              
049100             MOVE ERR-FUTURE-DATE        TO MED-IDMFSFEL                  
049200           END-IF                                                         
049300                                                                          
049400           IF MED-TEMFSFEL = ERR-ONE-YEAR-IN-FUTURE                       
049500             MOVE ERR-ONE-YEAR-IN-FUTURE TO MED-IDMFSFEL                  
049600           END-IF                                                         
049700                                                                          
049800           IF MED-TEMFSFEL = ERR-INFO-MISSING                             
049900             MOVE ERR-INFO-MISSING       TO MED-IDMFSFEL                  
050000           END-IF                                                         
050100                                                                          
050200           IF MED-TEMFSFEL NOT = ERR-ONE-YEAR-IN-FUTURE  AND              
050300                                 ERR-FUTURE-DATE AND                      
050400                                 ERR-INFO-MISSING                         
050500             MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                  
050600           END-IF                                                         
050700                                                                          
050800           CALL WMEDKONV      USING MED-WMEDAREA                          
050900           MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                             
051000           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
051100           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
051200        END-IF                                                            
051300     END-IF.                                                              
051400     .                                                                    
051500     EJECT                                                                
051600 GA-CHECK-RELANDCO SECTION.                                               
051700                                                                          
051800     IF MID-RELANDCO-EXP-IN = ALL '+'                                     
051900       MOVE MFS-NUM-FIELD-OK          TO MOD-RELANDCO-EXP-IN-ATTR         
052000     ELSE                                                                 
052100        INSPECT MID-RELANDCO-EXP-IN TALLYING WS-CNT                       
052200        FOR ALL '-'                                                       
052300        IF WS-CNT > 0                                                     
052400          MOVE MFS-NUM-FIELD-WRONG    TO MOD-RELANDCO-EXP-IN-ATTR         
052500          MOVE NOO                    TO INDATA-SW                        
052600        ELSE                                                              
052700          MOVE MID-RELANDCO-EXP-IN    TO WS-IDFRIDATA                     
052800          MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                    
052900          MOVE 3                      TO DEC-KVHELTAL                     
053000          MOVE 1                      TO DEC-KVDECIMAL                    
053100          CALL WDECEDIT            USING DEC-WDECAREA                     
053200                                                                          
053300           IF DEC-KDSVAR-OK                                               
053400             MOVE DEC-IDEDITDATA      TO WS-RELANDCO-EXP                  
053500             MOVE WS-RELANDCO-EXP     TO MOD-RELANDCO-EXP                 
053600             MOVE MFS-NUM-FIELD-OK    TO MOD-RELANDCO-EXP-IN-ATTR         
053700           ELSE                                                           
053800             MOVE MFS-NUM-FIELD-WRONG TO MOD-RELANDCO-EXP-IN-ATTR         
053900             MOVE NOO                 TO INDATA-SW                        
054000           END-IF                                                         
054100        END-IF                                                            
054200     END-IF                                                               
054300                                                                          
054400     IF MID-RELANDCO-FROM-IN = ALL '+'                                    
054500       MOVE MFS-NUM-FIELD-OK      TO MOD-RELANDCO-FROM-IN-ATTR            
054600     ELSE                                                                 
054700       INSPECT MID-RELANDCO-FROM-IN TALLYING WS-CNT                       
054800        FOR ALL '-'                                                       
054900        IF WS-CNT > 0                                                     
055000          MOVE MFS-NUM-FIELD-WRONG   TO MOD-RELANDCO-FROM-IN-ATTR         
055100          MOVE NOO                    TO INDATA-SW                        
055200        ELSE                                                              
055300          MOVE MID-RELANDCO-FROM-IN   TO WS-IDFRIDATA                     
055400          MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                    
055500          MOVE 3                      TO DEC-KVHELTAL                     
055600          MOVE 1                      TO DEC-KVDECIMAL                    
055700           CALL WDECEDIT            USING DEC-WDECAREA                    
055800                                                                          
055900           IF DEC-KDSVAR-OK                                               
056000             MOVE DEC-IDEDITDATA      TO WS-RELANDCO-FROM                 
056100             MOVE WS-RELANDCO-FROM    TO MOD-RELANDCO-FROM                
056200             MOVE MFS-NUM-FIELD-OK   TO MOD-RELANDCO-FROM-IN-ATTR         
056300           ELSE                                                           
056400             MOVE MFS-NUM-FIELD-WRONG TO MOD-RELANDCO-FROM-IN-ATTR        
056500             MOVE NOO                TO INDATA-SW                         
056600           END-IF                                                         
056700        END-IF                                                            
056800     END-IF                                                               
056900     .                                                                    
057000     EJECT                                                                
057100 GD-CHECK-TILANDCO SECTION.                                               
057200                                                                          
057300     IF MID-TILANDCO-IN NOT = ALL '+'                                     
057400       INSPECT MID-TILANDCO-IN REPLACING LEADING SPACE                    
057500                                   BY ZERO                                
057600       IF MID-TILANDCO-IN NUMERIC                                         
057700         MOVE MID-TILANDCO-IN       TO WS-START-DATE                      
057800         MOVE WORK-DATE             TO DATE-TIDATE                        
057900         MOVE 'YYMMDD'              TO DATE-KDDATFMT                      
058000         CALL WZ20DATE USING DATE-WZ20DATE                                
058100                                                                          
058200          IF DATE-KDRC = 0                                                
058300            IF WS-START-DATE    <= DATE-TIDATE                            
058400              MOVE ERR-FUTURE-DATE     TO MED-TEMFSFEL                    
058500              MOVE MFS-NUM-FIELD-WRONG TO MOD-TILANDCO-IN-ATTR            
058600              MOVE NOO                 TO INDATA-SW                       
058700            ELSE                                                          
058800              MOVE MFS-NUM-FIELD-OK    TO MOD-TILANDCO-IN-ATTR            
058900            END-IF                                                        
059000          END-IF                                                          
059100                                                                          
059200         MOVE MID-TILANDCO-IN       TO DAYS-TIDATE2                       
059300         MOVE WORK-DATE             TO DAYS-TIDATE1                       
059400         MOVE 'YYMMDD'              TO DAYS-KDDATFMT2                     
059500         MOVE ' '                   TO DAYS-IDCALEND                      
059600         MOVE 'YYMMDD'              TO DAYS-KDDATFMT1                     
059700         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
059800                                                                          
059900          IF DATE-KDRC = 0                                                
060000            IF DAYS-KVDAYS     > 365                                      
060100              MOVE ERR-ONE-YEAR-IN-FUTURE TO MED-TEMFSFEL                 
060200              MOVE MFS-NUM-FIELD-WRONG TO MOD-TILANDCO-IN-ATTR            
060300              MOVE NOO                 TO INDATA-SW                       
060400            ELSE                                                          
060500              MOVE MFS-NUM-FIELD-OK    TO MOD-TILANDCO-IN-ATTR            
060600            END-IF                                                        
060700          END-IF                                                          
060800                                                                          
060900         MOVE WS-START-DATE           TO DATE-TIDATE                      
061000         MOVE 'YYMMDD'                TO DATE-KDDATFMT                    
061100         CALL WZ20DATE USING DATE-WZ20DATE                                
061200                                                                          
061300          IF DATE-KDRC > 0                                                
061400            MOVE ERR-CORR-HILITE-FLDS  TO MED-TEMFSFEL                    
061500            MOVE MFS-NUM-FIELD-WRONG   TO MOD-TILANDCO-IN-ATTR            
061600            MOVE NOO                   TO INDATA-SW                       
061700          ELSE                                                            
061800            MOVE MFS-NUM-FIELD-OK      TO MOD-TILANDCO-IN-ATTR            
061900          END-IF                                                          
062000       END-IF                                                             
062100     END-IF                                                               
062200          IF(MID-TILANDCO-IN = ALL '+' OR  SPACES) AND                    
062300             MID-RELANDCO-FROM-IN NOT = ALL '+'                           
062400            MOVE ERR-INFO-MISSING      TO MED-TEMFSFEL                    
062500            MOVE MFS-NUM-FIELD-WRONG   TO MOD-TILANDCO-IN-ATTR            
062600            MOVE NOO                   TO INDATA-SW                       
062700          ELSE                                                            
062800            MOVE MFS-NUM-FIELD-OK      TO MOD-TILANDCO-IN-ATTR            
062900          END-IF                                                          
063000     .                                                                    
063100     EJECT                                                                
063200 GB-CHECK-REDIRLON SECTION.                                               
063300                                                                          
063400     IF MID-REDIRLON-IN NOT = ALL '+'                                     
063500       INSPECT MID-REDIRLON-IN REPLACING LEADING SPACE                    
063600                               BY ZERO                                    
063700       IF MID-REDIRLON-IN NUMERIC                                         
063800         MOVE MFS-NUM-FIELD-OK    TO MOD-REDIRLON-IN-ATTR                 
063900       ELSE                                                               
064000         MOVE MFS-NUM-FIELD-WRONG TO MOD-REDIRLON-IN-ATTR                 
064100         MOVE NOO                 TO INDATA-SW                            
064200       END-IF                                                             
064300     END-IF                                                               
064400     .                                                                    
064500     EJECT                                                                
064600 GC-CHECK-REDMTRL SECTION.                                                
064700                                                                          
064800     IF MID-REDMTRL-IN NOT = ALL '+'                                      
064900       INSPECT MID-REDMTRL-IN REPLACING LEADING SPACE                     
065000                              BY ZERO                                     
065100       IF MID-REDMTRL-IN NUMERIC                                          
065200         MOVE MFS-NUM-FIELD-OK    TO MOD-REDMTRL-IN-ATTR                  
065300       ELSE                                                               
065400         MOVE MFS-NUM-FIELD-OK    TO MOD-REDMTRL-IN-ATTR                  
065500         MOVE NOO                 TO INDATA-SW                            
065600       END-IF                                                             
065700     END-IF                                                               
065800     .                                                                    
065900     EJECT                                                                
066000 H-UPDATE SECTION.                                                        
066100                                                                          
066200     PERFORM IMS-GHU-WDB617                                               
066300     IF SEGMENT-FOUND                                                     
066400        PERFORM HB-MOVE-FIELDS                                            
066500        PERFORM IMS-REPL-WDB617                                           
066600     ELSE                                                                 
066700        PERFORM HA-INIT-FIELDS                                            
066800        PERFORM HB-MOVE-FIELDS-ISRT                                       
066900        PERFORM IMS-ISRT-WDB617                                           
067000     END-IF                                                               
067100     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
067200     CALL WMEDKONV USING MED-WMEDAREA                                     
067300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
067400     PERFORM MFS-FORM-ATTR                                                
067500     PERFORM MFS-ERASE-FIELD-IN                                           
067600     .                                                                    
067700     EJECT                                                                
067800 HA-INIT-FIELDS SECTION.                                                  
067900     INITIALIZE    PROC-REDIRLON                                          
068000                   PROC-REDMTRL                                           
068100                   PROC-RELANDCO-TO                                       
068200                   PROC-TILANDCO                                          
068300                   PROC-RELANDCO-FROM                                     
068400                   PROC-RELANDCO-EXP                                      
068500                   PROC-RELANDCO-ITX-FROM                                 
068600                   PROC-RELANDCO-ITX-TO                                   
068700                   PROC-RELANDCO-GTX-FROM                                 
068800                   PROC-RELANDCO-GTX-TO                                   
068900                   PROC-RELANDCO-OCF-FROM                                 
069000                   PROC-RELANDCO-OCF-TO                                   
069100     MOVE TODAYS-DATE            TO PROC-TIUPPDAT                         
069200     MOVE MSGI-IDUSER            TO PROC-IDUSER                           
069300     MOVE '1'                    TO PROC-KDSEGKEY                         
069400     .                                                                    
069500     EJECT                                                                
069600 HB-MOVE-FIELDS-ISRT SECTION.                                             
069700                                                                          
069800     IF MID-REDIRLON-IN NOT = ALL '+'                                     
069900       COMPUTE WS-PERCENT = MID-REDIRLON-IN  / 100                        
070000       MOVE WS-PERCENT           TO PROC-REDIRLON                         
070100     END-IF                                                               
070200                                                                          
070300     IF MID-REDMTRL-IN NOT = ALL '+'                                      
070400       COMPUTE WS-PERCENT = MID-REDMTRL-IN   / 100                        
070500       MOVE WS-PERCENT           TO PROC-REDMTRL                          
070600     END-IF                                                               
070700                                                                          
070800     IF MID-RELANDCO-FROM-IN NOT = ALL '+'                                
070900       COMPUTE WS-RELANDCO-FROM-PERC = WS-RELANDCO-FROM / 100             
071000       MOVE WS-RELANDCO-FROM-PERC TO PROC-RELANDCO-FROM                   
071100                                     PROC-RELANDCO-TO                     
071200     END-IF                                                               
071300                                                                          
071400     IF MID-RELANDCO-EXP-IN NOT = ALL '+'                                 
071500       COMPUTE WS-RELANDCO-PERC = 1 + (WS-RELANDCO-EXP / 100)             
071600       MOVE WS-RELANDCO-PERC     TO PROC-RELANDCO-EXP                     
071700     END-IF                                                               
071800                                                                          
071900     IF MID-TILANDCO-IN NOT = ALL '+'                                     
072000       MOVE MID-TILANDCO-IN        TO PROC-TILANDCO                       
072100     END-IF                                                               
072200                                                                          
072300     MOVE TODAYS-DATE            TO PROC-TIUPPDAT                         
072400     MOVE MSGI-IDUSER            TO PROC-IDUSER                           
072500     .                                                                    
072600     EJECT                                                                
072700 HB-MOVE-FIELDS SECTION.                                                  
072800                                                                          
072900     IF MID-RELANDCO-EXP-IN NOT = ALL '+'                                 
073000       COMPUTE WS-RELANDCO-PERC = 1 + (WS-RELANDCO-EXP / 100)             
073100       MOVE WS-RELANDCO-PERC     TO PROC-RELANDCO-EXP                     
073200     END-IF                                                               
073300                                                                          
073400     IF MID-REDIRLON-IN NOT = ALL '+'                                     
073500       COMPUTE WS-PERCENT = MID-REDIRLON-IN  / 100                        
073600       MOVE WS-PERCENT           TO PROC-REDIRLON                         
073700     END-IF                                                               
073800                                                                          
073900     IF MID-REDMTRL-IN NOT = ALL '+'                                      
074000       COMPUTE WS-PERCENT = MID-REDMTRL-IN   / 100                        
074100       MOVE WS-PERCENT           TO PROC-REDMTRL                          
074200     END-IF                                                               
074300                                                                          
074400     IF MID-RELANDCO-FROM-IN NOT = ALL '+'                                
074500       COMPUTE WS-RELANDCO-FROM-PERC = WS-RELANDCO-FROM / 100             
074600       MOVE PROC-RELANDCO-FROM    TO PROC-RELANDCO-TO                     
074700       MOVE WS-RELANDCO-FROM-PERC TO PROC-RELANDCO-FROM                   
074800     END-IF                                                               
074900                                                                          
075000     IF MID-TILANDCO-IN NOT = ALL '+'                                     
075100       MOVE MID-TILANDCO-IN       TO PROC-TILANDCO                        
075200     END-IF                                                               
075300                                                                          
075400     MOVE TODAYS-DATE            TO PROC-TIUPPDAT                         
075500     MOVE MSGI-IDUSER            TO PROC-IDUSER                           
075600     .                                                                    
075700     EJECT                                                                
075800 MFS-ERASE-FIELD-OUT SECTION.                                             
075900                                                                          
076000*    --- ALLA UTDATA-FÄLT                                                 
076100     MOVE MFS-ERASE-FIELD TO MOD-RELANDCO-EXP                             
076200                             MOD-RELANDCO-TO                              
076300                             MOD-TILANDCO                                 
076400                             MOD-RELANDCO-FROM                            
076500                             MOD-REDMTRL                                  
076600                             MOD-REDIRLON                                 
076700                             MOD-IDUSER                                   
076800                             MOD-TIUPPDAT                                 
076900     .                                                                    
077000     SKIP3                                                                
077100 MFS-ERASE-FIELD-IN SECTION.                                              
077200                                                                          
077300*    --- ALLA INDATA-FÄLT                                                 
077400     MOVE MFS-ERASE-FIELD TO MOD-RELANDCO-EXP-IN                          
077500                             MOD-TILANDCO-IN                              
077600                             MOD-RELANDCO-FROM-IN                         
077700                             MOD-REDMTRL-IN                               
077800                             MOD-REDIRLON-IN                              
077900     .                                                                    
078000     EJECT                                                                
078100 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
078200                                                                          
078300*    --- ALLA UTDATA-FÄLT                                                 
078400     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELANDCO-EXP                      
078500                                    MOD-RELANDCO-TO                       
078600                                    MOD-TILANDCO                          
078700                                    MOD-RELANDCO-FROM                     
078800                                    MOD-REDMTRL                           
078900                                    MOD-REDIRLON                          
079000                                    MOD-IDUSER                            
079100                                    MOD-TIUPPDAT                          
079200                                    MOD-IDDC                              
079300     .                                                                    
079400     SKIP3                                                                
079500 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
079600                                                                          
079700*    --- ALLA INDATA-FÄLT                                                 
079800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELANDCO-EXP-IN                   
079900                                    MOD-TILANDCO-IN                       
080000                                    MOD-RELANDCO-FROM-IN                  
080100                                    MOD-REDMTRL-IN                        
080200                                    MOD-REDIRLON-IN                       
080300                                    MOD-IDDC-IN                           
080400     .                                                                    
080500     EJECT                                                                
080600 MFS-FORM-ATTR SECTION.                                                   
080700                                                                          
080800*    --- ALL INDATA-FIELDS                                                
080900     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-RELANDCO-EXP-IN-ATTR             
081000                                     MOD-TILANDCO-IN-ATTR                 
081100                                     MOD-RELANDCO-FROM-IN-ATTR            
081200                                     MOD-REDMTRL-IN-ATTR                  
081300                                     MOD-REDIRLON-IN-ATTR                 
081400     .                                                                    
081500     SKIP2                                                                
081600 MFS-READ-IN-AGAIN SECTION.                                               
081700                                                                          
081800*    --- ALL INDATA-FIELDS                                                
081900     MOVE MFS-ADD-READ-FIELD TO MOD-RELANDCO-EXP-IN-ATTR                  
082000                                MOD-TILANDCO-IN-ATTR                      
082100                                MOD-RELANDCO-FROM-IN-ATTR                 
082200                                MOD-REDMTRL-IN-ATTR                       
082300                                MOD-REDIRLON-IN-ATTR                      
082400     .                                                                    
082500     EJECT                                                                
082600* --- IMS SECTIONS ---                                                    
082700     SKIP3                                                                
082800 IMS-GET-MSG SECTION.                                                     
082900                                                                          
083000     MOVE '  QC' TO GOOD-STATUSCODES                                      
083100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
083200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083300     PERFORM IMS-STATUSCHECK                                              
083400     .                                                                    
083500     SKIP3                                                                
083600 IMS-INSERT-MSG SECTION.                                                  
083700                                                                          
083800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
083900     MOVE SPACE TO GOOD-STATUSCODES                                       
084000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
084100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084200     PERFORM IMS-STATUSCHECK                                              
084300     .                                                                    
084400     EJECT                                                                
084500 IMS-GU-WDB601 SECTION.                                                   
084600                                                                          
084700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
084800          DELIMITED BY SIZE INTO SSA1                                     
084900     MOVE '  GE' TO GOOD-STATUSCODES                                      
085000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
085100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
085200     PERFORM IMS-STATUSCHECK                                              
085300     .                                                                    
085400     SKIP3                                                                
085500 IMS-GU-WDB617 SECTION.                                                   
085600                                                                          
085700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
085800          DELIMITED BY SIZE INTO SSA1                                     
085900     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
086000          DELIMITED BY SIZE INTO SSA2                                     
086100     MOVE '  GE' TO GOOD-STATUSCODES                                      
086200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB617 SSA1 SSA2               
086300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
086400     PERFORM IMS-STATUSCHECK                                              
086500     .                                                                    
086600     SKIP3                                                                
086700 IMS-ISRT-WDB617 SECTION.                                                 
086800                                                                          
086900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
087000          DELIMITED BY SIZE INTO SSA1                                     
087100     MOVE 'WDB617  ' TO SSA2                                              
087200     MOVE '  ' TO GOOD-STATUSCODES                                        
087300     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB617 SSA1 SSA2             
087400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSCHECK                                              
087600     .                                                                    
087700     SKIP3                                                                
087800 IMS-GHU-WDB617 SECTION.                                                  
087900                                                                          
088000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
088100          DELIMITED BY SIZE INTO SSA1                                     
088200     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
088300          DELIMITED BY SIZE INTO SSA2                                     
088400     MOVE '  GE' TO GOOD-STATUSCODES                                      
088500     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB617 SSA1 SSA2              
088600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
088700     PERFORM IMS-STATUSCHECK                                              
088800     .                                                                    
088900     SKIP3                                                                
089000 IMS-REPL-WDB617 SECTION.                                                 
089100                                                                          
089200     MOVE '  ' TO GOOD-STATUSCODES                                        
089300     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB617                       
089400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
089500     PERFORM IMS-STATUSCHECK                                              
089600     .                                                                    
089700     SKIP3                                                                
089800 IMS-STATUSCHECK SECTION.                                                 
089900                                                                          
090000     SET STATUS-IX TO 1                                                   
090100     SEARCH GOOD-STATUS                                                   
090200       AT END                                                             
090300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
090400         DELIMITED BY SIZE INTO ERROR-TEXT                                
090500         CALL FELLOG                                                      
090600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
090700         CONTINUE                                                         
090800     END-SEARCH                                                           
090900     .                                                                    
