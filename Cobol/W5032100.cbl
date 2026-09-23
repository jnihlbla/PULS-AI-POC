001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W5032100.                                                
001600 AUTHOR.         ARCHANA BHAT.                                            
001700 DATE-WRITTEN.   13/06/27.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        RESET INVENTORY ADJUSTMENTS                                      
002200*                                                                         
002310*        THE PROGRAM READS     WDB6                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W5T321                                              
002700*        MID:         W50321I1                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        MOD:         W50321O1                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W5032100'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004110 77  KDRC-DISPLAY                PIC Z(5).                                
004200                                                                          
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004410 77  WS-CDC                      PIC X(2)    VALUE '11'.                  
004500                                                                          
004700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005100                                                                          
005200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005300     88  KEYS-OK                             VALUE 'J'.                   
005400     88  KEYS-WRONG                          VALUE 'N'.                   
005500                                                                          
005510 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005520     88  INDATA-OK                           VALUE 'J'.                   
005530     88  INDATA-WRONG                        VALUE 'N'.                   
005540                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  OWN-MID                             VALUE '5321'.                
005800     88  GOOD-MID                            VALUE '5321'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007200     EJECT                                                                
007210                                                                          
007300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007301                                                                          
007302 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
007303 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)  COMP VALUE +16.               
007304 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  COMP VALUE +1000.             
007305 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
007306     SKIP3                                                                
007307                                                                          
007310*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007910                                                                          
008000 01  INF-ERR-MESSGS.                                                      
008010     03  WS-RESET-SCHED          PIC X(29)   VALUE                        
008020              'RESET IS SCHEDULED FOR SUNDAY'.                            
008030     03  WS-INVALID-DC-REQ       PIC X(25)   VALUE                        
008040              'REQUEST NOT VALID FOR CDC'.                                
008041     03  WS-PRESS-PF11           PIC X(23)   VALUE                        
008042              'PRESS PF11 TO START BMP'.                                  
008050     03  WS-ANN-CNT-DC           PIC X(40)   VALUE                        
008060              'REQUEST NOT VALID FOR ANNUAL COUNTING DC'.                 
008100     EJECT                                                                
008101                                                                          
008130 01  WS-IDDC                     PIC X(2)    VALUE SPACES.                
008140*                                                                         
008200*    --- WORK FIELDS FOR DATE AND TIME                                    
008201*                                                                         
008202 01  WS-YYMMDDHHMM.                                                       
008203     03  WS-YYMMDD               PIC 9(6).                                
008204     03  WS-TIME                 PIC 9(4).                                
008205                                                                          
008206 01  WS-HHMMSSTH                 PIC 9(8).                                
008207 01  FILLER REDEFINES WS-HHMMSSTH.                                        
008208     03  WS-HHMM                 PIC 9(4).                                
008209     03  WS-SSTH                 PIC 9(4).                                
008210     EJECT                                                                
008220*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
009400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009700     SKIP3                                                                
009800*01  MID -COPY W50321I1                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
010100     SKIP3                                                                
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400     03  MOD REDEFINES MSG-AREA.                                          
010500*      05  -COPY W50321O1                                                 
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010800     SKIP3                                                                
010900*01  -COPY WMFSAREA                                                       
011000     EJECT                                                                
011010 01  W-PROG-TO-PROG-SW.                                                   
011020*  03    -COPY WMSGSOP                                                    
011030     EJECT                                                                
011041*                                                                         
011042*    --- AREAS FOR COMMUNICATION                                          
011043*                                                                         
011050 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
011060 01  -COPY WZ01SEND                                                       
011070     EJECT                                                                
011080                                                                          
011095 01  HDR-AREA.                                                            
011096*    03  -COPY WZ01REQU                                                   
011097*    03  -COPY WZ04HDR                                                    
011098     EJECT                                                                
011099                                                                          
011100 01  DAP-LINE-AREA-1.                                                     
011101     03  WS-LINE1-TEXT           PIC X(25)   VALUE SPACES.                
011102     03  WS-SEND-IDDC            PIC X(02)   VALUE SPACES.                
011103                                                                          
011104 01  DAP-LINE-AREA-2.                                                     
011105     03  WS-LINE2-TEXT           PIC X(25)   VALUE SPACES.                
011106     03  WS-SEND-IDUSER          PIC X(08)   VALUE SPACES.                
011107     03  WS-SEND-BEANST          PIC X(25)   VALUE SPACES.                
011108     EJECT                                                                
011110*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  KEYS-FOR-DLI.                                                        
011601     03  W-IDDC-X.                                                        
011610         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011700     SKIP2                                                                
011800*    --- STATUS CODES FROM IMS                                            
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FOUND                       VALUE '  '.                  
012100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GOOD-STATUSCODES.                                                    
012500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNCTION CODES                                               
013100*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
013602 01  DLI-IO-WDB601.                                                       
013610*    03  -COPY WDB601                                                     
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014101     EJECT                                                                
014110*01  -COPY W0009   -PRE ALT-                                              
014120     EJECT                                                                
014130 01  DISTRDOC-PCB                PIC X.                                   
014140     EJECT                                                                
014200*01  -COPY W0008   -PRE WDP7-                                             
014300     05  FILLER                  PIC X.                                   
014401                                                                          
014402*01  -COPY W0008  -PRE WDB6-                                              
014410     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014601 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB DISTRDOC-PCB                   
014602                           WDP7-PCB WDB6-PCB.                             
014603 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB DISTRDOC-PCB                   
014620                           WDP7-PCB WDB6-PCB.                             
014700                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FOUND                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-CHECK-KEYS                                               
015210                                                                          
015300       IF KEYS-OK AND INDATA-OK                                           
015400         IF MFS-UPDATE                                                    
015500            PERFORM G-CHECK-INPUT                                         
015600            IF INDATA-OK                                                  
015700               PERFORM H-START-BMP                                        
015701               MOVE WS-RESET-SCHED  TO MOD-TEMFSINF                       
015710            END-IF                                                        
015720         ELSE                                                             
015730            IF MFS-FIRST                                                  
015740               MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                        
015750            ELSE                                                          
015760               PERFORM E-SAME-PAGE                                        
015770            END-IF                                                        
015780         END-IF                                                           
015900       END-IF                                                             
016000                                                                          
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W50321O1 + 4                      
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     IF MSG-DOUBLE-TRANSACTIONS                                           
017400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W50321I1                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W50321I1                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018600                                                                          
018700     MOVE LOW-VALUE TO MSG-AREA                                           
018800     MOVE 'W5O321N1' TO MFS-IDMOD                                         
018900     MOVE '5321' TO MOD-IDTRANS                                           
019000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF OWN-MID OR HELP-MID                                               
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
019800                                                                          
019900     ACCEPT WS-YYMMDD     FROM DATE                                       
019910     ACCEPT WS-HHMMSSTH   FROM TIME                                       
019920     MOVE   WS-HHMM       TO   WS-TIME                                    
020000     .                                                                    
020100     EJECT                                                                
020200 B-CHECK-KEYS SECTION.                                                    
020300                                                                          
020400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020500     MOVE '001'             TO MSGI-KDCALL                                
020600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020800     MOVE '5321'            TO MSGI-IDTRANS                               
020900     IF GOOD-MID                                                          
021010         MOVE MID-IDDC-IN   TO MSGI-IDDC                                  
021100     END-IF                                                               
021200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021400                                                                          
021500*    - LANGUAGE TO BE USED BY MEDKONV                                     
021600     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
021700                                                                          
021800     MOVE YES               TO KEYS-SW                                    
021900                                                                          
022001                                                                          
022002*    -- CHECK OF IDDC                                                     
022003     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                                
022004                                                                          
022005     IF MID-IDDC-IN NOT = ALL '+'                                         
022009        MOVE MID-IDDC-IN    TO W-IDDC                                     
022011        PERFORM IMS-GU-WDB601                                             
022012        IF SEGMENT-FOUND                                                  
022013           CONTINUE                                                       
022014        ELSE                                                              
022015           MOVE NOO         TO KEYS-SW                                    
022016        END-IF                                                            
022017     ELSE                                                                 
022018       MOVE NOO             TO INDATA-SW                                  
022020     END-IF                                                               
022021                                                                          
022102     IF KEYS-OK AND INDATA-OK                                             
022103       MOVE MID-IDDC-IN     TO MOD-IDDC-UT                                
022104                               W-IDDC                                     
022105                               WS-IDDC                                    
022106     ELSE                                                                 
022107       MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                
022110     END-IF                                                               
022200                                                                          
022300     IF KEYS-WRONG                                                        
022400       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
022500       CALL WMEDKONV     USING MED-WMEDAREA                               
022600       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
022700       MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                
022800                               MOD-IDDC-UT                                
022900     END-IF                                                               
023000     .                                                                    
023200     EJECT                                                                
023300 E-SAME-PAGE SECTION.                                                     
023310                                                                          
023330     IF MID-IDDC-IN = ALL '+'                                             
023340       MOVE MFS-ERASE-FIELD     TO MOD-IDDC-IN                            
023350     ELSE                                                                 
023360       IF OWN-MID OR HELP-MID                                             
023391         MOVE WS-PRESS-PF11     TO MOD-TEMFSINF                           
023392         PERFORM EA-MID-INDATA-TILL-MOD                                   
023393       ELSE                                                               
023394         MOVE MFS-ERASE-FIELD   TO MOD-IDDC-IN                            
023395       END-IF                                                             
023396     END-IF                                                               
023397     .                                                                    
023398 EA-MID-INDATA-TILL-MOD SECTION.                                          
023399                                                                          
023400     IF MID-IDDC-IN = ALL '+'                                             
023401        MOVE MFS-ERASE-FIELD    TO MOD-IDDC-IN                            
023403     ELSE                                                                 
023404        MOVE MFS-ADD-READ-FIELD TO MOD-IDDC-IN-ATTR                       
023405        MOVE MFS-DO-NOT-TOUCH-FIELD                                       
023406                                TO MOD-IDDC-IN                            
023407     END-IF                                                               
023408     .                                                                    
024800 G-CHECK-INPUT SECTION.                                                   
024810                                                                          
024811     MOVE YES                   TO INDATA-SW                              
024812     IF W-IDDC   = WS-CDC                                                 
024813        MOVE WS-INVALID-DC-REQ  TO MOD-TEMFSFEL                           
024814        MOVE NOO                TO INDATA-SW                              
024815     END-IF                                                               
024816                                                                          
024817     IF INDATA-OK                                                         
024831        IF DCS-FLINVACS = YES                                             
024832           MOVE WS-ANN-CNT-DC TO MOD-TEMFSFEL                             
024833           MOVE NOO           TO INDATA-SW                                
024834        END-IF                                                            
024850     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 H-START-BMP SECTION.                                                     
025200*                                                                         
025300*  ORDER JOB W513J091 OF BMP W513B2 AND SEND TO DAP                       
025400*                                                                         
027000     MOVE '5321'              TO MSGSOP-IDTRANS                           
027100     MOVE MFS-KDMFSFOR        TO MSGSOP-KDMFSFOR                          
027200     MOVE 'W513J091'          TO MSGSOP-IDPROCESS                         
027300     MOVE 'O'                 TO MSGSOP-KDSOPFUNK                         
027400                                                                          
027500     STRING 'IDDC(' WS-IDDC ')'                                           
028300          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
028400     PERFORM IMS-INSERT-ALTMSG                                            
028410                                                                          
028420     PERFORM HA-SEND-TO-DAP                                               
028500     .                                                                    
028600     EJECT                                                                
028610 HA-SEND-TO-DAP SECTION.                                                  
028620*                                                                         
028630*  SEND TO DISTRIBUTION AND PRINT                                         
028640*                                                                         
028650     PERFORM S90-OPEN-DAP-SEND                                            
028660                                                                          
028700     MOVE 001                         TO REQU-IDMSGVER                    
028800     MOVE 'R'                         TO REQU-KDPGMACT                    
028900     MOVE IDPGM                       TO REQU-IDUSER                      
029000                                                                          
029100     MOVE 'W50321-001'                TO HDR-IDOUTTYPE                    
029200     MOVE 'W50321'                    TO HDR-IDOUTREC                     
029300                                                                          
029500     MOVE WS-YYMMDDHHMM               TO HDR-IDLIST                       
029520     PERFORM S90-PUT-DAP-HEADER                                           
029530                                                                          
029540     MOVE 'RESET REQUESTED FOR DC: '  TO WS-LINE1-TEXT                    
029550     MOVE WS-IDDC                     TO WS-SEND-IDDC                     
029560     PERFORM S90-PUT-DAP-LINE-1                                           
029570                                                                          
029580     MOVE 'THE REQUEST WAS DONE BY: ' TO WS-LINE2-TEXT                    
029590     MOVE MSGI-IDUSER                 TO WS-SEND-IDUSER                   
029591     MOVE MSGI-BEANST                 TO WS-SEND-BEANST                   
029592     PERFORM S90-PUT-DAP-LINE-2                                           
029593                                                                          
029594     PERFORM S90-CLOSE-DAP-SEND                                           
029600     .                                                                    
029700     EJECT                                                                
029800 S90-OPEN-DAP-SEND SECTION.                                               
030000                                                                          
030100     MOVE 'CARPARTS.DAP.DISTRDOC'     TO SEND-ADDISPABS                   
030200     MOVE 'OPEN'                      TO SEND-KDFUNC                      
030300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030310                         SEND-OPEN-AREA                                   
030320     IF SEND-KDRC > ZERO                                                  
030330       MOVE SEND-KDRC                 TO KDRC-DISPLAY                     
030340       STRING 'WZ01SEND OPEN ERROR RC='  KDRC-DISPLAY                     
030350       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030360       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030370     END-IF                                                               
030380     .                                                                    
030391 S90-CLOSE-DAP-SEND SECTION.                                              
030393                                                                          
030394     MOVE 'CLOSE'                     TO SEND-KDFUNC                      
030395     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030396                                                                          
030397     IF SEND-KDRC > 0                                                     
030398       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
030399       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
030400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030401       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030402     END-IF                                                               
030403     .                                                                    
030405 S90-PUT-DAP-HEADER SECTION.                                              
030407                                                                          
030408     MOVE 'PUT'                       TO SEND-KDFUNC                      
030409     MOVE LENGTH OF HDR-AREA          TO SEND-KVDLEN                      
030410     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030411                         SEND-KVDLEN                                      
030412                         HDR-AREA                                         
030413     IF SEND-KDRC > ZERO                                                  
030414       MOVE SEND-KDRC                 TO KDRC-DISPLAY                     
030415       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
030416       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030417       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030418     END-IF                                                               
030419     .                                                                    
030420 S90-PUT-DAP-LINE-1 SECTION.                                              
030422                                                                          
030423     MOVE 'PUT'                       TO SEND-KDFUNC                      
030424     MOVE LENGTH OF DAP-LINE-AREA-1   TO SEND-KVDLEN                      
030425     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030426                         SEND-KVDLEN                                      
030427                         DAP-LINE-AREA-1                                  
030428     IF SEND-KDRC > ZERO                                                  
030429        MOVE SEND-KDRC                TO KDRC-DISPLAY                     
030430        STRING 'WZ01SEND PUT ERROR RC='  KDRC-DISPLAY                     
030431        DELIMITED BY SIZE INTO ERROR-TEXT                                 
030432        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
030433     END-IF                                                               
030434     .                                                                    
030435 S90-PUT-DAP-LINE-2 SECTION.                                              
030436                                                                          
030437     MOVE 'PUT'                       TO SEND-KDFUNC                      
030438     MOVE LENGTH OF DAP-LINE-AREA-2   TO SEND-KVDLEN                      
030439     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030440                         SEND-KVDLEN                                      
030441                         DAP-LINE-AREA-2                                  
030442     IF SEND-KDRC > ZERO                                                  
030443        MOVE SEND-KDRC                TO KDRC-DISPLAY                     
030444        STRING 'WZ01SEND PUT ERROR RC='  KDRC-DISPLAY                     
030445        DELIMITED BY SIZE INTO ERROR-TEXT                                 
030446        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
030447     END-IF                                                               
030448     .                                                                    
030449     EJECT                                                                
030450* --- IMS SECTIONS ---                                                    
030500     SKIP3                                                                
030600 IMS-GET-MSG SECTION.                                                     
030700                                                                          
030800     MOVE '  QC' TO GOOD-STATUSCODES                                      
030900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSCHECK                                              
031200     .                                                                    
031300     SKIP3                                                                
031400 IMS-INSERT-MSG SECTION.                                                  
031500                                                                          
031900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032000     MOVE SPACE TO GOOD-STATUSCODES                                       
032100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032300     PERFORM IMS-STATUSCHECK                                              
032400     .                                                                    
032501     EJECT                                                                
032502 IMS-INSERT-ALTMSG SECTION.                                               
032503     MOVE '  ' TO GOOD-STATUSCODES                                        
032504     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
032505     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
032506     PERFORM IMS-STATUSCHECK                                              
032507     .                                                                    
032508     EJECT                                                                
032509 IMS-GU-WDB601 SECTION.                                                   
032510                                                                          
032511     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
032512          DELIMITED BY SIZE INTO SSA1                                     
032513     MOVE '  GE' TO GOOD-STATUSCODES                                      
032514     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
032515     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
032516     PERFORM IMS-STATUSCHECK                                              
032520     .                                                                    
032600     EJECT                                                                
032700 IMS-STATUSCHECK SECTION.                                                 
032800                                                                          
032900     SET STATUS-IX TO 1                                                   
033000     SEARCH GOOD-STATUS                                                   
033100       AT END                                                             
033200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033300         DELIMITED BY SIZE INTO ERROR-TEXT                                
033400         CALL FELLOG                                                      
033500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033600         CONTINUE                                                         
033700     END-SEARCH                                                           
033800     .                                                                    
