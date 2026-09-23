000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5029100.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   11/09/30.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.PULS.ACSDOWNLOAD                                
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS PROGRAM IS CALLED FROM THE WEB SCREEN                       
001100*                                                                         
001200*        THE PROGRAM UPDATES   WDR2                                       
001300*                      READS   WDB6                                       
001400*    INDATA.                                                              
001500*        TRANSACTION: W50291T                                             
001600*        REQUEST:     W50291I1                                            
001700*                                                                         
001800*    OUTDATA.                                                             
001900*        RESPONSE:    W50291O1                                            
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
003300 77  IDPGM                       PIC X(08)   VALUE 'W5029100'.            
003310 77  WS-ADDRESS                  PIC X(50)                                
003400          VALUE 'CARPARTS.PULS.ACSDOWNLOAD'.                              
003413 77  WS-CNTR                     PIC 9(10) VALUE 0.                       
003414 77  WS-CNTR1                    PIC 9(10) VALUE 0.                       
003415 77  WS-IDFRIDATA                PIC X(25).                               
003425 77  WS-REQTYDEV-1               PIC S9V9(2) VALUE 0.                     
003426 77  WS-REQTYDEV-2               PIC S9V9(2) VALUE 0.                     
003427 77  WS-REQTYDEV-1-DISP          PIC 9(3) VALUE 0.                        
003428 77  WS-REQTYDEV-2-DISP          PIC 9(3) VALUE 0.                        
003430 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACES.                
003431 01  WS-SAVE-WDR2-AREA.                                                   
003440     03 WS-5104-PRAVCOST         PIC S9(7)V9(2)      COMP-3.              
003470     03 WS-5104-PRAVCOST-DEV1    PIC S9(7)V9(2)      COMP-3.              
003491     03 WS-5104-PRAVCOST-DEV2    PIC S9(7)V9(2)      COMP-3.              
003494     03 WS-5104-REQTYDEV-1       PIC S9V9(2)         COMP-3.              
003497     03 WS-5104-REQTYDEV-2       PIC S9V9(2)         COMP-3.              
003500     03 WS-5104-SUARTAVG         PIC S9(7)V9(2)      COMP-3.              
003510*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700 77  KDRC-DISPLAY                PIC Z(5).                                
003800                                                                          
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100                                                                          
004200                                                                          
004300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004400     88  KEYS-OK                             VALUE 'J'.                   
004500     88  KEYS-WRONG                          VALUE 'N'.                   
004510 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004520     88  INDATA-OK                           VALUE 'J'.                   
004530     88  INDATA-NOT-OK                       VALUE 'N'.                   
004600     EJECT                                                                
004700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004800 01  GENERAL-SUBPROGRAMS.                                                 
004900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005010     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005300*COMPOPT DB2BIND=YES            -- REMOVE IF PGM USES DB2 DIRECTLY        
005400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
005500     SKIP3                                                                
005600*    --- PARAMETERS TO ABEND                                              
005700                                                                          
005800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006100     SKIP3                                                                
006200 01  MESSAGE-CODES.                                                       
006331     03  ERROR-CODES.                                                     
006332         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
006333         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
006335         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
006336         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
006337         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
006338         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
006339         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
006340         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
006341         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
006342         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
006343         05  ERR-INRE-ALREADY-EXISTS PIC X(3)    VALUE '104'.             
006345         05  ERR-INV-INFO-NOT-FOUND  PIC X(3)    VALUE '221'.             
006346         05  ERR-INV-NOT-COMPLETED   PIC X(3)    VALUE '222'.             
006347         05  ERR-ACS-NOT-ALLOWED     PIC X(3)    VALUE '331'.             
006348         05  ERR-INV-NOT-STARTED     PIC X(3)    VALUE '332'.             
006349     03  INFO-CODES.                                                      
006350         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
006351         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
006352         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
006353         05  INF-PROCESS-STARTED     PIC X(3)    VALUE '015'.             
006360         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
006410     EJECT                                                                
006500*01  -COPY WDECAREA                                                       
006600     EJECT                                                                
006700*                                                                         
006800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006900     SKIP3                                                                
007000*01  -COPY WZ01SUB                                                        
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007300     SKIP3                                                                
007400 01  REQU-AREA.                                                           
007500*    03  -COPY WZ01REQU                                                   
007600*    03  -COPY W50291I1                                                   
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007900     SKIP3                                                                
008000 01  RESP-AREA.                                                           
008100*    03  -COPY WZ01RESP                                                   
008200*    03  -COPY W50291O1                                                   
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
008500     SKIP3                                                                
008600*01  -COPY WZ01SEND                                                       
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
008900     SKIP3                                                                
009000 01  SEND-AREA.                                                           
009100*    03  -COPY WZ01SOP  -PRE SOP-                                         
009300     EJECT                                                                
009400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  KEYS-FOR-DLI.                                                        
009900     03  W-IDDC-B6-X.                                                     
010000         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
010010     03  W-WDGXKEY-X.                                                     
010020          05 W-IDHTYP            PIC X(4)    VALUE '5103'.                
010030          05 W-LOWVALUE          PIC X(26)   VALUE LOW-VALUE.             
010040     03  W-IDDC-5104-X.                                                   
010050         05  W-IDDC-5104         PIC X(2)    VALUE SPACE.                 
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FOUND                       VALUE '  '.                  
010500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010700     SKIP2                                                                
010800 01  GOOD-STATUSCODES.                                                    
010900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200 01  SSA2                        PIC X(64).                               
011300     EJECT                                                                
011400*    --- IMS FUNCTION CODES                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700                                                                          
011800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
011900 01  DLI-IO-WDGX5104.                                                     
012000*    03  -COPY WDGX5104                                                   
012001                                                                          
012010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
012020 01  DLI-IO-WDB601.                                                       
012030*    03  -COPY WDB601                                                     
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300*01  -COPY W0009   -PRE MSG-                                              
012301                                                                          
012310*01  -COPY W0009   -PRE ALT0606-                                          
012400                                                                          
012500*01  -COPY W0008  -PRE 5104-                                              
012600     05  FILLER                  PIC X.                                   
012601                                                                          
012610*01  -COPY W0008  -PRE WDB6-                                              
012620     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING MSG-PCB ALT0606-PCB 5104-PCB WDB6-PCB.         
012900 MAIN SECTION.                                                            
013000     ENTRY 'DLITCBL' USING MSG-PCB ALT0606-PCB 5104-PCB WDB6-PCB.         
013100                                                                          
014500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
014600     IF SUB-KDRC = 0                                                      
014700       PERFORM A-INIT                                                     
014800       PERFORM B-CHECK-KEYS                                               
014900       IF KEYS-OK                                                         
014901         PERFORM F-READ-SHOW-INFO                                         
015100       END-IF                                                             
015200       PERFORM S02-RETURN-RESPONSE                                        
015600     END-IF                                                               
015700                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016310                                                                          
016400 A-INIT SECTION.                                                          
016500     MOVE ALL '+'                     TO RESP-AREA                        
016510     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
016520                                         RESP-IDMSG-INFO                  
016530                                         RESP-IDELMT-ERROR                
016550     MOVE '001'                       TO RESP-IDMSGVER                    
016551     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
016610     .                                                                    
016700     EJECT                                                                
016710*-----------------------------------------------------------------        
016720* DO AN INITIAL VALIDATION OF THE KEY VALUES RECEIVED FROM THE WEB        
016730*-----------------------------------------------------------------        
016800 B-CHECK-KEYS SECTION.                                                    
016900                                                                          
017000     MOVE YES                   TO KEYS-SW                                
017100                                                                          
017110     IF REQU-KDPGMACT = 'S' OR 'U' OR 'E'                                 
017120        CONTINUE                                                          
017121     ELSE                                                                 
017122        MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                       
017123        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
017124        MOVE NOO                TO KEYS-SW                                
017130     END-IF                                                               
017140                                                                          
017141     IF REQU-IDDC = ALL '+'                                               
017142        MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
017143        MOVE 'IDDC'              TO RESP-IDELMT-ERROR                     
017144        MOVE NOO                 TO KEYS-SW                               
017145     END-IF                                                               
017146                                                                          
017147     IF KEYS-OK                                                           
017150        MOVE REQU-IDDC          TO W-IDDC-B6                              
017160        PERFORM IMS-GET-WDB601                                            
017170        IF SEGMENT-FOUND                                                  
017180           IF DCS-FLINVACS = YES                                          
017190              CONTINUE                                                    
017191           ELSE                                                           
017192              MOVE ERR-ACS-NOT-ALLOWED                                    
017193                                TO RESP-IDMSG-ERROR                       
017195              MOVE NOO          TO KEYS-SW                                
017196           END-IF                                                         
017197        ELSE                                                              
017198           MOVE NOT-FOUND       TO RESP-IDMSG-ERROR                       
017199           MOVE 'IDDC'          TO RESP-IDELMT-ERROR                      
017200           MOVE NOO             TO KEYS-SW                                
017210        END-IF                                                            
017300                                                                          
017600     .                                                                    
017700     EJECT                                                                
017810 F-READ-SHOW-INFO SECTION.                                                
017900                                                                          
017901     MOVE REQU-IDDC             TO W-IDDC-5104                            
017902     PERFORM FA-READ-BASICDATA                                            
018700     .                                                                    
018800     EJECT                                                                
018810                                                                          
018900 FA-READ-BASICDATA SECTION.                                               
018910                                                                          
019000     EVALUATE TRUE                                                        
019100        WHEN REQU-KDPGMACT = 'S'                                          
019200          PERFORM FAA-GET-DATA                                            
019300                                                                          
019400        WHEN REQU-KDPGMACT = 'U'                                          
019500          PERFORM FAB-CHECK-INPUT                                         
019600          IF INDATA-OK                                                    
019604             PERFORM FAC-UPDATE                                           
019615          END-IF                                                          
019616        WHEN REQU-KDPGMACT = 'E'                                          
019617          PERFORM FAB-CHECK-INPUT                                         
019618          IF INDATA-OK                                                    
019619             PERFORM FAC-UPDATE                                           
019620          END-IF                                                          
019621          IF INDATA-OK                                                    
019622            PERFORM FAD-ORDER-ROUTINE                                     
019623          END-IF                                                          
019628     END-EVALUATE                                                         
019629     .                                                                    
019630*-----------------------------------------------------------------        
019631* FETCH DATA FROM WDGX5104                                                
019632*-----------------------------------------------------------------        
019633 FAA-GET-DATA SECTION.                                                    
019634     PERFORM IMS-GET-WDGX5104                                             
019635     IF SEGMENT-FOUND                                                     
019636        PERFORM FAAA-MOVE-DATA-TO-SCREEN                                  
019637     ELSE                                                                 
019638        MOVE ERR-INV-INFO-NOT-FOUND TO RESP-IDMSG-ERROR                   
019640     END-IF                                                               
019641     .                                                                    
019642     EJECT                                                                
019643                                                                          
019644*-----------------------------------------------------------------        
019645* POPULATE DATA FROM DB INTO THE MOD COPYTEXT                             
019646*-----------------------------------------------------------------        
019647 FAAA-MOVE-DATA-TO-SCREEN SECTION.                                        
019648     MOVE 5104-PRAVCOST      TO RESP-PRAVCOST                             
019649                                                                          
019650     MOVE 5104-SUARTAVG      TO RESP-SUARTAVG                             
019651     MOVE 5104-FLNOHAND      TO RESP-FLNOHAND                             
019652     MOVE 5104-FLBLINDCO     TO RESP-FLBLINDCO                            
019653     MOVE SPACES             TO RESP-KDACS                                
019654     MOVE 5104-PRAVCOST-DEV1 TO RESP-PRAVCOST-DEV1                        
019655     COMPUTE WS-REQTYDEV-1-DISP = 5104-REQTYDEV-1 * 100                   
019656     MOVE WS-REQTYDEV-1-DISP      TO RESP-REQTYDEV-1                      
019657     MOVE 5104-PRAVCOST-DEV2 TO RESP-PRAVCOST-DEV2                        
019658     COMPUTE WS-REQTYDEV-2-DISP = 5104-REQTYDEV-2 * 100                   
019659     MOVE WS-REQTYDEV-2-DISP      TO RESP-REQTYDEV-2                      
019660     .                                                                    
019661     EJECT                                                                
019662*-----------------------------------------------------------------        
019663* VALIDATE THE INPUT RECEIVED FROM THE SCREEN BEFORE UPDATING IN T        
019670*-----------------------------------------------------------------        
019700 FAB-CHECK-INPUT SECTION.                                                 
019701                                                                          
019702     MOVE YES                     TO INDATA-SW                            
019703                                                                          
019704     IF REQU-FLNOHAND NOT = YES AND NOO                                   
019707        MOVE ERR-INVALID-FIELD    TO RESP-IDMSG-ERROR                     
019708        MOVE 'FLNOHAND'           TO RESP-IDELMT-ERROR                    
019709        MOVE NOO                  TO INDATA-SW                            
019710     END-IF                                                               
019712                                                                          
019713     IF INDATA-OK                                                         
019714        IF REQU-FLBLINDCO NOT = YES AND NOO                               
019715           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
019716           MOVE 'FLBLINDCO'       TO RESP-IDELMT-ERROR                    
019717           MOVE NOO               TO INDATA-SW                            
019718        END-IF                                                            
019719     END-IF                                                               
019721                                                                          
019722     IF INDATA-OK                                                         
019723        IF REQU-KDACS NOT = 'A' AND 'D' AND ' ' AND '+'                   
019724           MOVE SYSTEM-ERROR      TO RESP-IDMSG-ERROR                     
019725           MOVE 'KDACS'           TO RESP-IDELMT-ERROR                    
019726           MOVE NOO               TO INDATA-SW                            
019727        END-IF                                                            
019728     END-IF                                                               
019729                                                                          
019730     IF INDATA-OK                                                         
019758        MOVE REQU-PRAVCOST        TO WS-IDFRIDATA                         
019760        PERFORM FABA-TRANSFORM-NUMERIC-DATA                               
019761        IF DEC-KDSVAR-OK                                                  
019762           MOVE DEC-IDEDITDATA    TO WS-5104-PRAVCOST                     
019763        ELSE                                                              
019764           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
019765           MOVE 'PRAVCOST'        TO RESP-IDELMT-ERROR                    
019766           MOVE NOO               TO INDATA-SW                            
019767        END-IF                                                            
019773     END-IF                                                               
019774                                                                          
019776     IF INDATA-OK                                                         
019784       MOVE REQU-SUARTAVG        TO WS-IDFRIDATA                          
019785       PERFORM FABA-TRANSFORM-NUMERIC-DATA                                
019786       IF DEC-KDSVAR-OK                                                   
019787          MOVE DEC-IDEDITDATA    TO WS-5104-SUARTAVG                      
019788       ELSE                                                               
019789          MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                      
019790          MOVE 'SUARTAVG'        TO RESP-IDELMT-ERROR                     
019791          MOVE NOO               TO INDATA-SW                             
019792       END-IF                                                             
019794     END-IF                                                               
019795                                                                          
019796     IF INDATA-OK                                                         
019804        MOVE REQU-PRAVCOST-DEV1    TO WS-IDFRIDATA                        
019805        MOVE 7                     TO DEC-KVHELTAL                        
019806        PERFORM FABA-TRANSFORM-NUMERIC-DATA                               
019807        IF DEC-KDSVAR-OK                                                  
019808           MOVE DEC-IDEDITDATA     TO WS-5104-PRAVCOST-DEV1               
019809        ELSE                                                              
019810           MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                    
019811           MOVE 'PRAVCOST-DEV1'    TO RESP-IDELMT-ERROR                   
019812           MOVE NOO                TO INDATA-SW                           
019813        END-IF                                                            
019815     END-IF                                                               
019816                                                                          
019817     IF INDATA-OK                                                         
019818        IF REQU-REQTYDEV-1 NOT NUMERIC                                    
019819           MOVE ERR-MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR                  
019820           MOVE 'REQTYDEV-1'         TO RESP-IDELMT-ERROR                 
019821           MOVE NOO                  TO INDATA-SW                         
019822        ELSE                                                              
019823           COMPUTE WS-REQTYDEV-1 = REQU-REQTYDEV-1 / 100                  
019824           MOVE WS-REQTYDEV-1        TO WS-5104-REQTYDEV-1                
019825        END-IF                                                            
019826     END-IF                                                               
019827                                                                          
019828     IF INDATA-OK                                                         
019836        MOVE REQU-PRAVCOST-DEV2    TO WS-IDFRIDATA                        
019837        PERFORM FABA-TRANSFORM-NUMERIC-DATA                               
019838        IF DEC-KDSVAR-OK                                                  
019839           MOVE DEC-IDEDITDATA     TO WS-5104-PRAVCOST-DEV2               
019840        ELSE                                                              
019841           MOVE ERR-INVALID-FIELD  TO RESP-IDMSG-ERROR                    
019842           MOVE 'PRAVCOST-DEV2'    TO RESP-IDELMT-ERROR                   
019843           MOVE NOO                TO INDATA-SW                           
019844        END-IF                                                            
019846     END-IF                                                               
019847                                                                          
019848     IF INDATA-OK                                                         
019849        IF REQU-REQTYDEV-2   NOT NUMERIC                                  
019850           MOVE ERR-MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR                  
019851           MOVE 'REQTYDEV-2'         TO RESP-IDELMT-ERROR                 
019852           MOVE NOO                  TO INDATA-SW                         
019853        ELSE                                                              
019854           COMPUTE WS-REQTYDEV-2 = REQU-REQTYDEV-2 / 100                  
019855           MOVE WS-REQTYDEV-2        TO WS-5104-REQTYDEV-2                
019856        END-IF                                                            
019857     END-IF                                                               
019858                                                                          
019859     .                                                                    
019860*-----------------------------------------------------------------        
019861* TRANSFORM THE NUMERIC VALUES RECEIVED FROM THE WEB                      
019862*-----------------------------------------------------------------        
019863 FABA-TRANSFORM-NUMERIC-DATA SECTION.                                     
019864     MOVE WS-IDFRIDATA  TO DEC-IDFRIDATA                                  
019865     MOVE 7             TO DEC-KVHELTAL                                   
019866     MOVE 2             TO DEC-KVDECIMAL                                  
019867                                                                          
019868     CALL WDECEDIT USING DEC-WDECAREA                                     
019869     .                                                                    
019870                                                                          
019871*-----------------------------------------------------------------        
019872* -UPDATE DATABASE WITH DATA FROM WEB SCREEN                              
019873* CHECK IF THE RECORD IS ALREADY PRESENT IN THE DB. IF NO, INSERT         
019874* RECORD INTO THE DB ELSE UPDATE THE RECORD.                              
019875*-----------------------------------------------------------------        
019876 FAC-UPDATE SECTION.                                                      
019877     MOVE REQU-IDDC          TO W-IDDC-5104                               
019878     PERFORM IMS-GET-HOLD-WDGX5104                                        
019879     IF SEGMENT-FOUND                                                     
019880        IF (5104-DASTADAT < 5104-DASTODAT                                 
019881        AND WS-CURRENT-DATE > 5104-DASTODAT)                              
019882        OR (5104-DASTADAT = 5104-DASTODAT                                 
019883        AND WS-CURRENT-DATE > 5104-DASTODAT)                              
019884        OR 5104-DASTADAT = ZERO                                           
019885           PERFORM FACA-MOVE-FIELDS                                       
019890           MOVE WS-CURRENT-DATE TO 5104-DAUPPDAT                          
019892           PERFORM IMS-REPL-WDGX5104                                      
019893           MOVE INF-UPDATE-OK   TO RESP-IDMSG-INFO                        
019894        ELSE                                                              
019895           MOVE ERR-UPDATE-NOT-ALLOWED TO RESP-IDMSG-ERROR                
019896        END-IF                                                            
019897                                                                          
019898     ELSE                                                                 
019899        PERFORM FACA-MOVE-FIELDS                                          
019900        MOVE WS-CURRENT-DATE TO 5104-DAREGDAT                             
019901        MOVE ZERO            TO 5104-DAUPPDAT                             
019902                                5104-DASTADAT                             
019903                                5104-DASTODAT                             
019904        PERFORM IMS-ISRT-WDGX5104                                         
019905        MOVE INF-UPDATE-OK   TO RESP-IDMSG-INFO                           
019906     END-IF                                                               
019907     .                                                                    
019908 FACA-MOVE-FIELDS SECTION.                                                
019909                                                                          
019910     MOVE REQU-IDDC             TO 5104-IDDC                              
019911     MOVE REQU-FLNOHAND         TO 5104-FLNOHAND                          
019912     MOVE REQU-FLBLINDCO        TO 5104-FLBLINDCO                         
019913     MOVE SPACES                TO 5104-KDACS                             
019914     MOVE REQU-IDUSER           TO 5104-IDUSER                            
019915     MOVE WS-5104-PRAVCOST      TO 5104-PRAVCOST                          
019916     MOVE WS-5104-PRAVCOST-DEV1 TO 5104-PRAVCOST-DEV1                     
019917     MOVE WS-5104-PRAVCOST-DEV2 TO 5104-PRAVCOST-DEV2                     
019918     MOVE WS-5104-REQTYDEV-1    TO 5104-REQTYDEV-1                        
019919     MOVE WS-5104-REQTYDEV-2    TO 5104-REQTYDEV-2                        
019920     MOVE WS-5104-SUARTAVG      TO 5104-SUARTAVG                          
019921     .                                                                    
019922*-----------------------------------------------------------------        
019923* CHECK IF KDACS SELECTED ON THE SCREEN FOR THE DC IS THE SAME AS         
019924* THE DB. IF NO, UPDATE IN THE DB AND ORDER W571B1 ROUTINE. IF SAM        
019925* THEN JUST ORDER THE ROUTINE                                             
019926*-----------------------------------------------------------------        
019927 FAD-ORDER-ROUTINE SECTION.                                               
019928                                                                          
019929     MOVE REQU-IDDC              TO W-IDDC-5104                           
019930                                    5104-IDDC                             
019931     PERFORM IMS-GET-HOLD-WDGX5104                                        
019932     IF SEGMENT-FOUND                                                     
019933        IF (5104-DASTADAT < 5104-DASTODAT                                 
019934        AND WS-CURRENT-DATE > 5104-DASTODAT)                              
019935        OR (5104-DASTADAT = 5104-DASTODAT                                 
019936        AND WS-CURRENT-DATE > 5104-DASTODAT)                              
019937        OR 5104-DASTADAT = ZERO                                           
019938           IF 5104-KDACS NOT = REQU-KDACS                                 
019939              MOVE REQU-KDACS       TO 5104-KDACS                         
019940              PERFORM IMS-REPL-WDGX5104                                   
019941           END-IF                                                         
019942*  -- TRIGGER ACTIVATION OF RTN W571B1 TO SOP                             
019943           PERFORM FADA-SETUP-SOP-DATA                                    
019944           PERFORM S04-SEND-OPEN                                          
019945           PERFORM S04-SEND-MESSAGE                                       
019946           PERFORM S04-SEND-CLOSE                                         
019947           MOVE INF-PROCESS-STARTED TO RESP-IDMSG-INFO                    
019948        ELSE                                                              
019949           MOVE ERR-UPDATE-NOT-ALLOWED TO RESP-IDMSG-ERROR                
019950        END-IF                                                            
019951     ELSE                                                                 
019952        PERFORM FACA-MOVE-FIELDS                                          
019953        MOVE REQU-KDACS      TO 5104-KDACS                                
019954        MOVE WS-CURRENT-DATE TO 5104-DAREGDAT                             
019955        MOVE ZERO            TO 5104-DAUPPDAT                             
019956                                5104-DASTADAT                             
019957                                5104-DASTODAT                             
019958        PERFORM IMS-ISRT-WDGX5104                                         
019959*  -- TRIGGER ACTIVATION OF RTN W571B1 TO SOP                             
019960        PERFORM FADA-SETUP-SOP-DATA                                       
019961        PERFORM S04-SEND-OPEN                                             
019962        PERFORM S04-SEND-MESSAGE                                          
019963        PERFORM S04-SEND-CLOSE                                            
019964        MOVE INF-PROCESS-STARTED TO RESP-IDMSG-INFO                       
019965     END-IF                                                               
019966     .                                                                    
019967     EJECT                                                                
019968*-----------------------------------------------------------------        
019969* SETUP THE SOP DATA FOR ORDERING W571B1 ROUTINE                          
019970*-----------------------------------------------------------------        
019971 FADA-SETUP-SOP-DATA SECTION.                                             
019972                                                                          
019973     MOVE 'W571B1'  TO SOP-REQU-IDPROCESS                                 
019974     MOVE 'A'       TO SOP-REQU-KDSOPFUNK                                 
019975     MOVE ZERO      TO SOP-REQU-TIORDDAT                                  
019976     MOVE SPACES    TO SOP-REQU-TESYMBV                                   
019977     STRING 'IDDC(' 5104-IDDC ')' DELIMITED BY SIZE                       
019978                  INTO SOP-REQU-TESYMBV                                   
019979     .                                                                    
019980     EJECT                                                                
020000                                                                          
020100*    --- DISPATCHER SECTIONS                                              
020200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
020300                                                                          
020400     MOVE 'GETARG'               TO SUB-KDFUNC                            
020500     MOVE WS-ADDRESS             TO SUB-ADDISPABS                         
020600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
020700                                                                          
020800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
020900                                                                          
021000     IF SUB-KDRC > 0                                                      
021100       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
021200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
021300       DELIMITED BY SIZE       INTO ERROR-TEXT                            
021400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021500     END-IF                                                               
021600     .                                                                    
021700     SKIP3                                                                
021800 S02-RETURN-RESPONSE SECTION.                                             
021900                                                                          
022000     MOVE 'RETURN'                TO SUB-KDFUNC                           
022100     MOVE LENGTH OF RESP-AREA     TO SUB-KVDLEN                           
022200                                                                          
022300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
022400                                                                          
022500     IF SUB-KDRC > 0                                                      
022600       MOVE SUB-KDRC              TO KDRC-DISPLAY                         
022700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
022800       DELIMITED BY SIZE        INTO ERROR-TEXT                           
022900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 S04-SEND-OPEN SECTION.                                                   
023400                                                                          
023500     MOVE 'OPEN'                 TO SEND-KDFUNC                           
023600     MOVE 'CARPARTS.PULS.SOP'    TO SEND-ADDISPABS                        
023700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
023800                                                                          
023900     IF SEND-KDRC > 0                                                     
024000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
024100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
024200       DELIMITED BY SIZE       INTO ERROR-TEXT                            
024300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
024400     END-IF                                                               
024500     .                                                                    
024600     SKIP3                                                                
024700 S04-SEND-MESSAGE SECTION.                                                
024800                                                                          
024900     MOVE 'PUT'                      TO SEND-KDFUNC                       
025000     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
025100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
025200                                                                          
025300     IF SEND-KDRC > 0                                                     
025400       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
025500       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
025600       DELIMITED BY SIZE           INTO ERROR-TEXT                        
025700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025800     END-IF                                                               
025900     .                                                                    
026000     SKIP3                                                                
026100 S04-SEND-CLOSE SECTION.                                                  
026200                                                                          
026300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
026400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
026500                                                                          
026600     IF SEND-KDRC > 0                                                     
026700       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
026800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
026900       DELIMITED BY SIZE           INTO ERROR-TEXT                        
027000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027500 IMS-GET-WDGX5104 SECTION.                                                
027600                                                                          
027640     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
027650          DELIMITED BY SIZE INTO SSA1                                     
027660     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
027670          DELIMITED BY SIZE INTO SSA2                                     
027900     MOVE '  GE' TO GOOD-STATUSCODES                                      
028000     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
028100     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
028200     PERFORM IMS-STATUSCHECK                                              
028300     .                                                                    
028400     SKIP3                                                                
028410 IMS-GET-HOLD-WDGX5104 SECTION.                                           
028420                                                                          
028421     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
028422          DELIMITED BY SIZE INTO SSA1                                     
028423     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
028424          DELIMITED BY SIZE INTO SSA2                                     
028450     MOVE '  GE' TO GOOD-STATUSCODES                                      
028460     CALL CBLTDLI USING GHU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2            
028470     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
028480     PERFORM IMS-STATUSCHECK                                              
028490     .                                                                    
029300 IMS-GET-WDB601 SECTION.                                                  
029400                                                                          
029410     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
029420          DELIMITED BY SIZE INTO SSA1                                     
029500     MOVE '  GE'              TO GOOD-STATUSCODES                         
029600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
029700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
029800     PERFORM IMS-STATUSCHECK                                              
029900     .                                                                    
030000     EJECT                                                                
030001 IMS-ISRT-WDGX5104 SECTION.                                               
030002                                                                          
030003     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
030004          DELIMITED BY SIZE INTO SSA1                                     
030005     MOVE   'WDGX5104'        TO SSA2                                     
030008     MOVE '  '                TO GOOD-STATUSCODES                         
030009     CALL CBLTDLI USING ISRT 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2           
030010     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
030011     PERFORM IMS-STATUSCHECK                                              
030012     .                                                                    
030013 IMS-REPL-WDGX5104 SECTION.                                               
030020                                                                          
030030     MOVE '  '                TO GOOD-STATUSCODES                         
030040     CALL CBLTDLI USING REPL 5104-PCB DLI-IO-WDGX5104                     
030050     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
030060     PERFORM IMS-STATUSCHECK                                              
030070     .                                                                    
030080     EJECT                                                                
030100 IMS-STATUSCHECK SECTION.                                                 
030200                                                                          
030300     SET STATUS-IX TO 1                                                   
030400     SEARCH GOOD-STATUS                                                   
030500       AT END                                                             
030600         STRING ' INVALID STATUS FROM IMS: ' STATUS-WS                    
030700           DELIMITED BY SIZE INTO ERROR-TEXT                              
030800         DISPLAY ERROR-TEXT                                               
030900         CALL FELLOG                                                      
031000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
031100         CONTINUE                                                         
031200     END-SEARCH                                                           
031300     .                                                                    
