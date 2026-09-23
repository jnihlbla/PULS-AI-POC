000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1115200.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   16/07/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE WDK6, WDM1 WITH PCOO DATA FROM MIC.                       
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDK6                                       
001200*        THE PROGRAM UPDATES   WDM1                                       
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- INPUT FROM MIC                                             
002300     SELECT W11153                     ASSIGN TO W11152D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W11153                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W11151      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W1115200'.            
003800 01  CHKP-VAR.                                                            
003900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004400     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700 77  WS-PREFERENTIAL             PIC 9(2)    VALUE 01.                    
004800 77  WS-IDARTNR                  PIC X(9).                                
004900 77  WS-IDLEVNR                  PIC X(5).                                
005000 77  WS-KDARTURS-PCOO            PIC X(2).                                
005100 77  WS-TIGILTIG-TOM             PIC S9(7)   COMP-3.                      
005200 77  WS-KDPCOO                   PIC X       VALUE 'A'.                   
005300 77  WS-COUNT-ISRT-WDM101        PIC 9(9)    VALUE 0.                     
005400 77  WS-COUNT-ISRT-WDM111        PIC 9(9)    VALUE 0.                     
005500 77  WS-COUNT-ISRT-WDM121        PIC 9(9)    VALUE 0.                     
005600 77  WS-COUNT-REPL-WDM121        PIC 9(9)    VALUE 0.                     
005610 77  DADATTID                    PIC 9(14).                               
005620 77  TODAYS-DATE                 PIC S9(8)   VALUE ZERO.                  
005700     SKIP2                                                                
005800 01  ERROR-TEXT.                                                          
005900     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006100                                                                          
006200 77  W11153-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W11153                       VALUE 'Y'.                   
006400     EJECT                                                                
006500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006600     EJECT                                                                
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  IN-AREA-START               PIC X(24)   VALUE                        
007800                                             'IN-AREA-START'.             
007900     SKIP2                                                                
008000                                                                          
008100*01  AREA -COPY W11151     -PRE IN-                                       
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  KEYS-TILL-DLI.                                                       
008700     03  W-IDARTNR-X.                                                     
008800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008900     03  W-KDSEGKEY-X.                                                    
009000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
009100     03  W-WDM101KY-X.                                                    
009200         05  W-IDARTNR-WDM1      PIC S9(9)   VALUE ZERO COMP-3.           
009300         05  W-IDLEVNR-WDM1      PIC X(5)    VALUE SPACE.                 
009400     03  W-BEAVTAL-X.                                                     
009500         05  W-BEAVTAL           PIC X(10)   VALUE SPACE.                 
009600     03  W-TIREGDA9-X.                                                    
009700         05  W-TIREGDA9          PIC S9(7)   VALUE ZERO COMP-3.           
009800     SKIP2                                                                
009900*    --- STATUS-KOD FRÅN IMS                                              
010000 01  STATUS-WS                   PIC XX.                                  
010100     88  SEGMENT-FOUND                       VALUE '  '.                  
010200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010400     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010500     88  IMS-NOT-OK                          VALUE 'XD'.                  
010600     SKIP2                                                                
010700 01  GOOD-STATUSCODES.                                                    
010800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(64).                               
011100 01  SSA2                        PIC X(64).                               
011200 01  SSA3                        PIC X(64).                               
011300     EJECT                                                                
011400*    --- IMS FUNCTION CODES                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011800                                                                          
011900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012000 01  DLI-IO-WDK601.                                                       
012100*    03  -COPY WDK601                                                     
012200     EJECT                                                                
012300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012400 01  DLI-IO-WDK611.                                                       
012500*    03  -COPY WDK611                                                     
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM101'.                      
012700 01  DLI-IO-WDM101.                                                       
012800*    03  -COPY WDM101                                                     
012900     EJECT                                                                
013000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM111'.                      
013100 01  DLI-IO-WDM111.                                                       
013200*    03  -COPY WDM111                                                     
013300     EJECT                                                                
013400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM121'.                      
013500 01  DLI-IO-WDM121.                                                       
013600*    03  -COPY WDM121                                                     
013700                                                                          
013710 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDT501'.            
013720 01  DLI-IO-WDT501.                                                       
013730*    03  -COPY WDT501   -PRE WDT501-                                      
013740     EJECT                                                                
013750 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT511'.                      
013760 01  DLI-IO-WDT511.                                                       
013770*    03  -COPY WDT511                                                     
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000                                                                          
014100*01  -COPY W0009   -PRE MSG-                                              
014200                                                                          
014300*01  -COPY W0008  -PRE WDK6-                                              
014400     05  FILLER                  PIC X.                                   
014500                                                                          
014600*01  -COPY W0008  -PRE WDM1-                                              
014700     05  FILLER                  PIC X.                                   
014710*01  -COPY W0008  -PRE WDT5-                                              
014720      05 FILLER                  PIC X.                                   
014800     EJECT                                                                
014900 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDM1-PCB WDT5-PCB.            
015000 MAIN SECTION.                                                            
015100     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDM1-PCB WDT5-PCB.            
015200                                                                          
015300     SKIP2                                                                
015400     PERFORM A-INIT                                                       
015500     PERFORM S01-READ-W11153                                              
015600     IF NOT END-OF-W11153                                                 
015700       MOVE IN-TIGILTIG-TOM      TO WS-TIGILTIG-TOM                       
015800       MOVE IN-KDARTURS-PCOO     TO WS-KDARTURS-PCOO                      
015900       PERFORM                                                            
016000         UNTIL END-OF-W11153                                              
016100                                                                          
016200         PERFORM B-PROCESS                                                
016300                                                                          
016400         IF CHKP-ANT > CHKP-MAX                                           
016500           PERFORM X-TAKE-CHECKPOINT                                      
016600         END-IF                                                           
016700                                                                          
016800         PERFORM S01-READ-W11153                                          
016900         IF END-OF-W11153 OR                                              
017000            (IN-IDARTNR NOT = WS-IDARTNR)                                 
017100           PERFORM IMS-GU-WDK601                                          
017200           IF WS-IDLEVNR = ART-IDLEVNR                                    
017300             PERFORM IMS-GHNP-WDK611                                      
017400             MOVE WS-KDARTURS-PCOO                                        
017500                                 TO CLAG-KDARTURS                         
018110             MOVE CLAG-KDARTURS  TO GLO-KDARTURS                          
018200             MOVE WS-TIGILTIG-TOM                                         
018300                                 TO CLAG-TIGILTIG-PCOO                    
018400             MOVE WS-KDPCOO      TO CLAG-KDPCOO                           
018500             PERFORM IMS-REPL-WDK611                                      
018501             IF GLO-KDARTURS > ' '                                        
018510               PERFORM S02-UPDATE-WDT5                                    
018520             END-IF                                                       
018600             ADD +1              TO CHKP-ANT                              
018700           END-IF                                                         
018800           MOVE 'A'              TO WS-KDPCOO                             
018900           MOVE IN-TIGILTIG-TOM  TO WS-TIGILTIG-TOM                       
019000           MOVE IN-KDARTURS-PCOO TO WS-KDARTURS-PCOO                      
019100         END-IF                                                           
019200       END-PERFORM                                                        
019300     END-IF                                                               
019400                                                                          
019500                                                                          
019600     PERFORM Z-FINIT                                                      
019700                                                                          
019800     MOVE ZERO                   TO RETURN-CODE                           
019900     GOBACK                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 A-INIT SECTION.                                                          
020300     SKIP2                                                                
020400                                                                          
020500     PERFORM IMS-RESTART                                                  
020600                                                                          
020700     OPEN INPUT W11153                                                    
020800                                                                          
020900     MOVE FUNCTION CURRENT-DATE (3:6)                                     
021000                                 TO TODAYS-DATE                           
021100                                                                          
021200     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
021300     .                                                                    
021400     EJECT                                                                
021500 B-PROCESS SECTION.                                                       
021600                                                                          
021700     MOVE IN-IDARTNR             TO W-IDARTNR                             
021800                                    WS-IDARTNR                            
021900                                    W-IDARTNR-WDM1                        
022000     MOVE IN-IDLEVNR             TO WS-IDLEVNR                            
022100                                    W-IDLEVNR-WDM1                        
022200     MOVE IN-BEAVTAL             TO W-BEAVTAL                             
022300                                                                          
022400     COMPUTE W-TIREGDA9 = 9999999 - TODAYS-DATE                           
022500                                                                          
022600     IF IN-IDSTAMIC = WS-PREFERENTIAL                                     
022700       CONTINUE                                                           
022800     ELSE                                                                 
022900       MOVE 'P'                  TO WS-KDPCOO                             
023000     END-IF                                                               
023100                                                                          
023200     PERFORM IMS-GU-WDM101                                                
023300     IF SEGMENT-MISSING                                                   
023400       MOVE IN-IDARTNR           TO ARTU-IDARTNR                          
023500       MOVE IN-IDLEVNR           TO ARTU-IDLEVNR                          
023510       MOVE ZERO                 TO ARTU-KVLS-PCOO                        
023600       PERFORM IMS-ISRT-WDM101                                            
023700       ADD +1                    TO WS-COUNT-ISRT-WDM101                  
023800       ADD +1                    TO CHKP-ANT                              
023900                                                                          
024000       MOVE IN-BEAVTAL           TO AVT-BEAVTAL                           
024100       PERFORM IMS-ISRT-WDM111                                            
024200       ADD +1                    TO WS-COUNT-ISRT-WDM111                  
024300       ADD +1                    TO CHKP-ANT                              
024400                                                                          
024500       PERFORM BA-INSERT-WDM121                                           
024600                                                                          
024700     ELSE                                                                 
024800       PERFORM IMS-GU-WDM111                                              
024900       IF SEGMENT-MISSING                                                 
025000         MOVE IN-BEAVTAL         TO AVT-BEAVTAL                           
025100         PERFORM IMS-ISRT-WDM111                                          
025200         ADD +1                  TO WS-COUNT-ISRT-WDM111                  
025300         ADD +1                  TO CHKP-ANT                              
025400                                                                          
025500         PERFORM BA-INSERT-WDM121                                         
025600       ELSE                                                               
025700         PERFORM IMS-GHNP-WDM121                                          
025800         IF SEGMENT-FOUND AND                                             
025900            IN-TIGILTIG-FOM  = STAV-TIGILTIG-FOM  AND                     
026000            IN-TIGILTIG-TOM  = STAV-TIGILTIG-TOM  AND                     
026100            IN-KDARTURS-PCOO = STAV-KDARTURS-PCOO AND                     
026200            IN-IDSTAMIC      = STAV-IDSTAMIC                              
026300           CONTINUE                                                       
026400         ELSE                                                             
026410           IF SEGMENT-FOUND AND                                           
026500              W-TIREGDA9 = STAV-TIREGDAT-9KOMPL                           
026600             MOVE IN-TIGILTIG-FOM                                         
026700                                 TO STAV-TIGILTIG-FOM                     
026800             MOVE IN-TIGILTIG-TOM                                         
026900                                 TO STAV-TIGILTIG-TOM                     
027000             MOVE IN-KDARTURS-PCOO                                        
027100                                 TO STAV-KDARTURS-PCOO                    
027200             MOVE IN-IDSTAMIC    TO STAV-IDSTAMIC                         
027300             PERFORM IMS-REPL-WDM121                                      
027400             ADD +1              TO WS-COUNT-REPL-WDM121                  
027500             ADD +1              TO CHKP-ANT                              
027600           ELSE                                                           
027700             PERFORM BA-INSERT-WDM121                                     
027800           END-IF                                                         
027900         END-IF                                                           
028000       END-IF                                                             
028100     END-IF                                                               
028200                                                                          
028300     .                                                                    
028400     EJECT                                                                
028500 BA-INSERT-WDM121 SECTION.                                                
028600                                                                          
028700       COMPUTE STAV-TIREGDAT-9KOMPL = 9999999 - TODAYS-DATE               
028800       MOVE IN-TIGILTIG-FOM      TO STAV-TIGILTIG-FOM                     
028900       MOVE IN-TIGILTIG-TOM      TO STAV-TIGILTIG-TOM                     
029000       MOVE IN-KDARTURS-PCOO     TO STAV-KDARTURS-PCOO                    
029100       MOVE IN-IDSTAMIC          TO STAV-IDSTAMIC                         
029200       MOVE 'W11152'             TO STAV-IDUSER                           
029300       MOVE TODAYS-DATE          TO STAV-TIREGDAT                         
029400       PERFORM IMS-ISRT-WDM121                                            
029500       ADD +1                    TO WS-COUNT-ISRT-WDM121                  
029600       ADD +1                    TO CHKP-ANT                              
029700                                                                          
029800     .                                                                    
029900     EJECT                                                                
030000 Z-FINIT SECTION.                                                         
030100                                                                          
030200     CLOSE W11153                                                         
030300                                                                          
030400     DISPLAY 'WDM101 INSERTS = ' WS-COUNT-ISRT-WDM101                     
030500     DISPLAY 'WDM111 INSERTS = ' WS-COUNT-ISRT-WDM111                     
030600     DISPLAY 'WDM121 INSERTS = ' WS-COUNT-ISRT-WDM121                     
030700     DISPLAY 'WDM121 REPLACE = ' WS-COUNT-REPL-WDM121                     
030800                                                                          
030900     MOVE 'S'                    TO POSTSUM-OPKOD                         
031000     CALL POSTSUM             USING POSTSUM-PARM                          
031100     .                                                                    
031200     EJECT                                                                
031300 S01-READ-W11153  SECTION.                                                
031400     SKIP2                                                                
031500     READ W11153               INTO IN-AREA                               
031600       AT END                                                             
031700         SET END-OF-W11153       TO TRUE                                  
031800                                                                          
031900     NOT AT END                                                           
032000       MOVE 'W11153'             TO POSTSUM-FDNAMN                        
032100       MOVE 'W11152D1'           TO POSTSUM-DDNAMN2                       
032200       MOVE SPACE                TO POSTSUM-TRANSTYP                      
032300       CALL POSTSUM           USING POSTSUM-PARM                          
032400                                                                          
032500     END-READ                                                             
032600     .                                                                    
032700     EJECT                                                                
032701                                                                          
032710 S02-UPDATE-WDT5 SECTION.                                                 
032720                                                                          
032730     PERFORM IMS-GU-WDT501                                                
032731     IF SEGMENT-FOUND                                                     
032732      PERFORM IMS-GHNP-WDT511                                             
032733      MOVE ART-IDLEVNR TO GLO-IDLEVNR                                     
032734      PERFORM IMS-REPL-WDT511                                             
032735     END-IF                                                               
032740     IF SEGMENT-MISSING                                                   
032750        MOVE W-IDARTNR TO WDT501-ARTU-IDARTNR                             
032760        PERFORM IMS-ISRT-WDT501                                           
032770     END-IF                                                               
032780                                                                          
032791     MOVE 'MIC'                 TO GLO-IDUSER                             
032792     MOVE SPACES                TO GLO-IDLEVNR                            
032793     MOVE FUNCTION CURRENT-DATE(1:14) TO DADATTID                         
032794     MOVE FUNCTION CURRENT-DATE(1:8)  TO TODAYS-DATE                      
032795     COMPUTE GLO-DADATTID-9KOMPL =                                        
032796             99999999999999 - DADATTID                                    
032797                                                                          
032798     PERFORM IMS-ISRT-WDT511                                              
032799     .                                                                    
032800     EJECT                                                                
032801                                                                          
032810 X-TAKE-CHECKPOINT   SECTION.                                             
032900                                                                          
033000     PERFORM IMS-CHECKPOINT                                               
033100     MOVE ZERO                   TO CHKP-ANT                              
033200     .                                                                    
033300     EJECT                                                                
033400* --- IMS SECTIONS  ---                                                   
033500                                                                          
033600     EJECT                                                                
033700 IMS-GU-WDK601 SECTION.                                                   
033800                                                                          
033900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
034000          DELIMITED BY SIZE INTO SSA1                                     
034100     MOVE SPACES                 TO GOOD-STATUSCODES                      
034200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
034300     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
034400     PERFORM IMS-STATUSCHECK                                              
034500     .                                                                    
034600     EJECT                                                                
034700 IMS-GHNP-WDK611 SECTION.                                                 
034800                                                                          
034900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
035000          DELIMITED BY SIZE INTO SSA1                                     
035100     MOVE SPACES                 TO GOOD-STATUSCODES                      
035200     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
035300     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
035400     PERFORM IMS-STATUSCHECK                                              
035500     .                                                                    
035600     SKIP3                                                                
035700 IMS-REPL-WDK611 SECTION.                                                 
035800                                                                          
035900     MOVE SPACES                 TO GOOD-STATUSCODES                      
036000     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
036100     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
036200     PERFORM IMS-STATUSCHECK                                              
036300     .                                                                    
036400     EJECT                                                                
036500 IMS-GU-WDM101 SECTION.                                                   
036600                                                                          
036700     STRING 'WDM101  (WDM101KY =' W-WDM101KY-X ')'                        
036800          DELIMITED BY SIZE INTO SSA1                                     
036900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
037000     CALL CBLTDLI USING GU WDM1-PCB DLI-IO-WDM101 SSA1                    
037100     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
037200     PERFORM IMS-STATUSCHECK                                              
037300     .                                                                    
037400     SKIP3                                                                
037500 IMS-ISRT-WDM101 SECTION.                                                 
037600                                                                          
037700     MOVE 'WDM101 '              TO SSA1                                  
037800     MOVE SPACES                 TO GOOD-STATUSCODES                      
037900     CALL CBLTDLI USING ISRT WDM1-PCB DLI-IO-WDM101 SSA1                  
038000     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
038100     PERFORM IMS-STATUSCHECK                                              
038200     .                                                                    
038300     SKIP3                                                                
038400 IMS-GU-WDM111 SECTION.                                                   
038500                                                                          
038600     STRING 'WDM101  (WDM101KY =' W-WDM101KY-X ')'                        
038700          DELIMITED BY SIZE INTO SSA1                                     
038800     STRING 'WDM111  (BEAVTAL  =' W-BEAVTAL-X ')'                         
038900          DELIMITED BY SIZE INTO SSA2                                     
039000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
039100     CALL CBLTDLI USING GU WDM1-PCB DLI-IO-WDM111 SSA1 SSA2               
039200     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
039300     PERFORM IMS-STATUSCHECK                                              
039400     .                                                                    
039500     SKIP3                                                                
039600 IMS-ISRT-WDM111 SECTION.                                                 
039700                                                                          
039800     STRING 'WDM101  (WDM101KY =' W-WDM101KY-X ')'                        
039900          DELIMITED BY SIZE INTO SSA1                                     
040000     MOVE 'WDM111 '              TO SSA2                                  
040100     MOVE SPACES                 TO GOOD-STATUSCODES                      
040200     CALL CBLTDLI USING ISRT WDM1-PCB DLI-IO-WDM111 SSA1 SSA2             
040300     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
040400     PERFORM IMS-STATUSCHECK                                              
040500     .                                                                    
040600     SKIP3                                                                
040700 IMS-GHNP-WDM121 SECTION.                                                 
040800                                                                          
040900     MOVE 'WDM121 '              TO SSA1                                  
041000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
041100     CALL CBLTDLI USING GHNP WDM1-PCB DLI-IO-WDM121 SSA1                  
041200     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
041300     PERFORM IMS-STATUSCHECK                                              
041400     .                                                                    
041500     SKIP3                                                                
041600 IMS-ISRT-WDM121 SECTION.                                                 
041700                                                                          
041800     STRING 'WDM101  (WDM101KY =' W-WDM101KY-X ')'                        
041900          DELIMITED BY SIZE INTO SSA1                                     
042000     STRING 'WDM111  (BEAVTAL  =' W-BEAVTAL-X ')'                         
042100          DELIMITED BY SIZE INTO SSA2                                     
042200     MOVE 'WDM121 '              TO SSA3                                  
042300     MOVE SPACES                 TO GOOD-STATUSCODES                      
042400     CALL CBLTDLI USING ISRT WDM1-PCB DLI-IO-WDM121 SSA1 SSA2 SSA3        
042500     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
042600     PERFORM IMS-STATUSCHECK                                              
042700     .                                                                    
042800     SKIP3                                                                
042900 IMS-REPL-WDM121 SECTION.                                                 
043000                                                                          
043100     MOVE '  '                   TO GOOD-STATUSCODES                      
043200     CALL CBLTDLI USING REPL WDM1-PCB DLI-IO-WDM121                       
043300     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
043400     PERFORM IMS-STATUSCHECK                                              
043500     .                                                                    
043600     EJECT                                                                
043610 IMS-GU-WDT501 SECTION.                                                   
043620     STRING 'WDT501  (IDARTNR = ' W-IDARTNR-X ')'                         
043630          DELIMITED BY SIZE INTO SSA1                                     
043640     MOVE '  GE' TO GOOD-STATUSCODES                                      
043650     CALL CBLTDLI USING GU  WDT5-PCB DLI-IO-WDT501 SSA1                   
043660     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
043670     PERFORM IMS-STATUSCHECK                                              
043680     .                                                                    
043690     EJECT                                                                
043691 IMS-ISRT-WDT501 SECTION.                                                 
043692     MOVE 'WDT501 ' TO SSA1                                               
043693     MOVE '  II' TO GOOD-STATUSCODES                                      
043694     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT501 SSA1                  
043695     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
043696     PERFORM IMS-STATUSCHECK                                              
043697     .                                                                    
043698 IMS-GHNP-WDT511 SECTION.                                                 
043699     MOVE   'WDT511  *F' TO SSA1                                          
043700     MOVE '  ' TO GOOD-STATUSCODES                                        
043701     CALL CBLTDLI USING GHNP  WDT5-PCB DLI-IO-WDT511 SSA1                 
043702     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
043703     PERFORM IMS-STATUSCHECK                                              
043704     .                                                                    
043705     EJECT                                                                
043706 IMS-REPL-WDT511 SECTION.                                                 
043707     MOVE '  ' TO GOOD-STATUSCODES                                        
043708     CALL CBLTDLI USING REPL  WDT5-PCB DLI-IO-WDT511                      
043709     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
043710     PERFORM IMS-STATUSCHECK                                              
043711     .                                                                    
043712     EJECT                                                                
043713 IMS-ISRT-WDT511 SECTION.                                                 
043714     STRING 'WDT501  (IDARTNR  =' W-IDARTNR-X ')'                         
043715            DELIMITED BY SIZE INTO SSA1                                   
043716     MOVE 'WDT511 ' TO SSA2                                               
043717     MOVE '  ' TO GOOD-STATUSCODES                                        
043718     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT511 SSA1 SSA2             
043719     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
043720     PERFORM IMS-STATUSCHECK                                              
043721     .                                                                    
043730 IMS-RESTART SECTION.                                                     
043800     SKIP2                                                                
043900     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
044000     MOVE '  '                   TO GOOD-STATUSCODES                      
044100     CALL CBLTDLI USING XRST MSG-PCB                                      
044200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
044300                        CHKP-AREA-LENGTH CHKP-AREA                        
044400     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
044500     PERFORM IMS-STATUSCHECK                                              
044600     .                                                                    
044700     SKIP3                                                                
044800 IMS-CHECKPOINT SECTION.                                                  
044900     SKIP2                                                                
045000     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
045100     MOVE '  XD'                 TO GOOD-STATUSCODES                      
045200     CALL CBLTDLI USING CHKP MSG-PCB                                      
045300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
045400                        CHKP-AREA-LENGTH CHKP-AREA                        
045500     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
045600     PERFORM IMS-STATUSCHECK                                              
045700                                                                          
045800     IF IMS-NOT-OK                                                        
045900       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
046000                                 TO ERROR-TEXT-STR                        
046100       DISPLAY ERROR-TEXT                                                 
046200       CALL FELLOG                                                        
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 IMS-STATUSCHECK SECTION.                                                 
046700     SKIP2                                                                
046800     SET STATUS-IX               TO 1                                     
046900     SEARCH GOOD-STATUS                                                   
047000       AT END                                                             
047100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047200           DELIMITED BY SIZE INTO ERROR-TEXT                              
047300         DISPLAY ERROR-TEXT                                               
047400         CALL FELLOG                                                      
047500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
047600         CONTINUE                                                         
047700     END-SEARCH                                                           
047800     .                                                                    
