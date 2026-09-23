000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5101700.                                                
000300 AUTHOR.         BHAT ARCHANA.                                            
000400 DATE-WRITTEN.   22/08/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM READS VAT CODES FILE AND INSERTS/UPDATES            
001000*        THE VAT CODES IN THE DATABASE.                                   
001100*                                                                         
001200*        THE PROGRAM UPDATES   WDG2                                       
001300*        THE PROGRAM READS     WDB6                                       
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- VAT CODES                                                  
002400     SELECT W51017                     ASSIGN TO W51017D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W51017                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY WF10M17      -L.                                               
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W5101700'.            
003900 01  CHKP-VAR.                                                            
004000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800 77  W-IDLEGSEL                  PIC X(4)    VALUE SPACES.                
004900 77  WS-SAVE-IDLEGSEL            PIC X(4)    VALUE SPACES.                
005000     SKIP2                                                                
005100 01  ERROR-TEXT.                                                          
005200     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500 77  W51017-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W51017                       VALUE 'Y'.                   
005700     EJECT                                                                
005710 77  WS-LEGSEL-FND-SW            PIC X       VALUE 'N'.                   
005720     88  WS-LEGSEL-FND                       VALUE 'J'.                   
005730     EJECT                                                                
005800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES TODAYS-DATE.                                        
006000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006200     03  TODAYS-DATE-DAY         PIC 9(2).                                
006300     EJECT                                                                
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500*                                                                         
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     EJECT                                                                
007000*    --- PARAMETRAR TILL POSTSUM                                          
007100*                                                                         
007200*01  -COPY W0005   -PRE  POSTSUM-                                         
007300     EJECT                                                                
007400 01  IN-AREA-START               PIC X(24)   VALUE                        
007500                                             'IN-AREA-START'.             
007600     SKIP2                                                                
007700                                                                          
007800*01  AREA -COPY WF10M17     -PRE IN-                                      
007900*                                                                         
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  KEYS-TILL-DLI.                                                       
008400     03  W-WDGXKEY-X.                                                     
008500         05  W-IDHTYP            PIC X(4)    VALUE '9309'.                
008600         05  W-IDFTG             PIC X(2)    VALUE SPACE.                 
008700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
008800     03  W-KY9310-X.                                                      
008900         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
009000         05  W-KDVAT             PIC X(2)    VALUE SPACE.                 
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FOUND                       VALUE '  '.                  
009500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009800     88  IMS-NOT-OK                          VALUE 'XD'.                  
009900     SKIP2                                                                
010000 01  GOOD-STATUSCODES.                                                    
010100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(64).                               
010500     EJECT                                                                
010600*    --- IMS FUNCTION CODES                                               
010700*01  -COPY W0003                                                          
010800     EJECT                                                                
010900*    ---  DLI INPUT-OUTPUT AREA                                           
011000                                                                          
011100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9309'.                    
011200 01  DLI-IO-WDGX9309.                                                     
011300*    03  -COPY WDGX9309                                                   
011400     EJECT                                                                
011500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9310'.                    
011600 01  DLI-IO-WDGX9310.                                                     
011700*    03  -COPY WDGX9310                                                   
011800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
011900 01  DLI-IO-WDB601.                                                       
012000*    03  -COPY WDB601                                                     
012100                                                                          
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500*01  -COPY W0009   -PRE MSG-                                              
012600                                                                          
012700*01  -COPY W0008  -PRE 9309-                                              
012810     05 KFBA-IDHTYP          PIC X(4).                                    
012820     05 KFBA-IDFTG           PIC 9(2).                                    
012840     05 FILLER               PIC X(24).                                   
012850     05 KFBA-IDLANDX2        PIC X(2).                                    
012860     05 KFBA-KDVAT           PIC X(2).                                    
012900                                                                          
013000*01  -COPY W0008  -PRE WDB6-                                              
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013300 PROCEDURE DIVISION  USING MSG-PCB 9309-PCB WDB6-PCB.                     
013400 MAIN SECTION.                                                            
013500     ENTRY 'DLITCBL' USING MSG-PCB 9309-PCB WDB6-PCB.                     
013600                                                                          
013700     SKIP2                                                                
013800     PERFORM A-INIT                                                       
013900     PERFORM S01-READ-W51017                                              
014000     PERFORM UNTIL END-OF-W51017                                          
014100       IF CHKP-ANT > CHKP-MAX                                             
014200         PERFORM X-TAKE-CHECKPOINT                                        
014300       END-IF                                                             
014400       PERFORM B-PROCESS-VAT                                              
014500       PERFORM S01-READ-W51017                                            
014600     END-PERFORM                                                          
014610     DISPLAY 'CHKP-CNT:' CHKP-ANT                                         
014700                                                                          
014800     PERFORM Z-FINIT                                                      
014900                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500     SKIP2                                                                
015600                                                                          
015700     PERFORM IMS-RESTART                                                  
015800                                                                          
015900     OPEN INPUT W51017                                                    
016000                                                                          
016100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016200     .                                                                    
016300     EJECT                                                                
016400 B-PROCESS-VAT SECTION.                                                   
016500                                                                          
016510     IF IN-IDLEGSEL = WS-SAVE-IDLEGSEL                                    
016520       PERFORM BB-UPDATE-VAT                                              
016530     ELSE                                                                 
016600       PERFORM BA-CHECK-IDFTG                                             
016700       PERFORM IMS-GU-WDGX9309                                            
016800       IF SEGMENT-FOUND                                                   
016900         PERFORM BB-UPDATE-VAT                                            
017000       ELSE                                                               
017610         MOVE W-IDFTG         TO 9309-IDFTG                               
017620         MOVE W-IDHTYP        TO 9309-IDHTYP                              
017700         PERFORM IMS-ISRT-WDGX9309                                        
017800         ADD +1             TO CHKP-ANT                                   
017810                                                                          
017900         PERFORM BC-INSERT-VAT                                            
018000       END-IF                                                             
018100     END-IF                                                               
018400     .                                                                    
018500     EJECT                                                                
018600 BA-CHECK-IDFTG SECTION.                                                  
018601                                                                          
018602     MOVE IN-IDLEGSEL       TO W-IDLEGSEL                                 
018603                               WS-SAVE-IDLEGSEL                           
018604     PERFORM IMS-GU-WDB601-LSEL                                           
018605     IF SEGMENT-FOUND                                                     
018606       MOVE YES             TO WS-LEGSEL-FND-SW                           
018607       MOVE DCS-IDFTG       TO W-IDFTG                                    
018608     ELSE                                                                 
018609       MOVE NOO             TO WS-LEGSEL-FND-SW                           
018610     END-IF                                                               
018611     .                                                                    
018612     EJECT                                                                
018620 BB-UPDATE-VAT SECTION.                                                   
018700                                                                          
018710     IF WS-LEGSEL-FND                                                     
018800       MOVE IN-IDLAND         TO W-IDLANDX2                               
018900       MOVE IN-KDVAT          TO W-KDVAT                                  
019000       PERFORM IMS-GHU-WDGX9310                                           
019100       IF SEGMENT-FOUND                                                   
019200          MOVE IN-REVAT        TO 9310-REVAT                              
019300          MOVE IN-BEVAT        TO 9310-BEVAT                              
019310          MOVE IN-DAUPPDAT     TO 9310-DAUPPDAT                           
019320          MOVE IN-DADELDAT     TO 9310-DADELDAT                           
019400          MOVE 'W5101700'      TO 9310-IDUSER                             
019500          PERFORM IMS-REPL-WDGX9310                                       
019510          ADD +1               TO CHKP-ANT                                
019600       ELSE                                                               
019700         PERFORM BC-INSERT-VAT                                            
019800       END-IF                                                             
019910     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 BC-INSERT-VAT SECTION.                                                   
020300                                                                          
020400     MOVE IN-IDLAND         TO 9310-IDLANDX2                              
020500     MOVE IN-KDVAT          TO 9310-KDVAT                                 
020600     MOVE IN-REVAT          TO 9310-REVAT                                 
020700     MOVE IN-BEVAT          TO 9310-BEVAT                                 
020710     MOVE IN-DAREGDAT       TO 9310-DAREGDAT                              
020720     MOVE IN-DAUPPDAT       TO 9310-DAUPPDAT                              
020730     MOVE IN-DADELDAT       TO 9310-DADELDAT                              
020800     MOVE 'W5101700'        TO 9310-IDUSER                                
020900     PERFORM IMS-ISRT-WDGX9310                                            
021000     ADD +1                 TO CHKP-ANT                                   
021100     .                                                                    
021200     EJECT                                                                
021300 Z-FINIT SECTION.                                                         
021400                                                                          
021500                                                                          
021600     CLOSE W51017                                                         
021700     SKIP2                                                                
021800     MOVE 'S' TO POSTSUM-OPKOD                                            
021900     CALL POSTSUM USING POSTSUM-PARM                                      
022000     .                                                                    
022100     EJECT                                                                
022200 S01-READ-W51017  SECTION.                                                
022300     SKIP2                                                                
022400     READ W51017 INTO IN-AREA                                             
022500     AT END                                                               
022600        SET END-OF-W51017 TO TRUE                                         
022700                                                                          
022800     NOT AT END                                                           
022900        MOVE 'W51017' TO POSTSUM-FDNAMN                                   
023000        MOVE 'W51017D1' TO POSTSUM-DDNAMN2                                
023100        MOVE SPACES    TO POSTSUM-TRANSTYP                                
023200        CALL POSTSUM USING POSTSUM-PARM                                   
023300     END-READ                                                             
023400     .                                                                    
023500     EJECT                                                                
023600 X-TAKE-CHECKPOINT   SECTION.                                             
023700                                                                          
023800     PERFORM IMS-CHECKPOINT                                               
023900     MOVE ZERO TO CHKP-ANT                                                
024000     .                                                                    
024100     EJECT                                                                
024200* --- IMS SECTIONS  ---                                                   
024300                                                                          
024400 IMS-GU-WDGX9309 SECTION.                                                 
024500                                                                          
024600     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
024700          DELIMITED BY SIZE INTO SSA1                                     
024800     MOVE '  GE' TO GOOD-STATUSCODES                                      
024900     CALL CBLTDLI USING GU 9309-PCB DLI-IO-WDGX9309 SSA1                  
025000     MOVE 9309-STATUS-CODE TO STATUS-WS                                   
025100     PERFORM IMS-STATUSCHECK                                              
025200     .                                                                    
025300     SKIP3                                                                
025400 IMS-ISRT-WDGX9309 SECTION.                                               
025500                                                                          
025600     MOVE 'WDG201  ' TO SSA1                                              
025800     MOVE '  II' TO GOOD-STATUSCODES                                      
025900     CALL CBLTDLI USING ISRT 9309-PCB DLI-IO-WDGX9309 SSA1                
026000     MOVE 9309-STATUS-CODE TO STATUS-WS                                   
026100     PERFORM IMS-STATUSCHECK                                              
026200     .                                                                    
026300     SKIP3                                                                
026400 IMS-ISRT-WDGX9310 SECTION.                                               
026500                                                                          
026600     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
026700          DELIMITED BY SIZE INTO SSA1                                     
026800     MOVE 'WDGX9310' TO SSA2                                              
026900     MOVE '  II' TO GOOD-STATUSCODES                                      
027000     CALL CBLTDLI USING ISRT 9309-PCB DLI-IO-WDGX9310 SSA1 SSA2           
027100     MOVE 9309-STATUS-CODE TO STATUS-WS                                   
027200     PERFORM IMS-STATUSCHECK                                              
027300     .                                                                    
027400     SKIP3                                                                
027500 IMS-GHU-WDGX9310 SECTION.                                                
027600                                                                          
027700     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
027800          DELIMITED BY SIZE INTO SSA1                                     
027900     STRING 'WDGX9310(KY9310   =' W-KY9310-X ')'                          
028000          DELIMITED BY SIZE INTO SSA2                                     
028100     MOVE '  GE' TO GOOD-STATUSCODES                                      
028200     CALL CBLTDLI USING GHU 9309-PCB DLI-IO-WDGX9310 SSA1 SSA2            
028300     MOVE 9309-STATUS-CODE TO STATUS-WS                                   
028400     PERFORM IMS-STATUSCHECK                                              
028500     .                                                                    
028600     SKIP3                                                                
028700 IMS-REPL-WDGX9310 SECTION.                                               
028800                                                                          
028900     MOVE '  ' TO GOOD-STATUSCODES                                        
029000     CALL CBLTDLI USING REPL 9309-PCB DLI-IO-WDGX9310                     
029100     MOVE 9309-STATUS-CODE TO STATUS-WS                                   
029200     PERFORM IMS-STATUSCHECK                                              
029300     .                                                                    
029400     EJECT                                                                
029500 IMS-GU-WDB601-LSEL SECTION.                                              
029600     STRING 'WDB601  (IDLEGSEL =' W-IDLEGSEL ')'                          
029700            DELIMITED BY SIZE INTO SSA1                                   
029800     MOVE '  GBGE'              TO GOOD-STATUSCODES                       
029900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
030000     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
030100     PERFORM IMS-STATUSCHECK                                              
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-RESTART SECTION.                                                     
030500     SKIP2                                                                
030600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030700     MOVE '  ' TO GOOD-STATUSCODES                                        
030800     CALL CBLTDLI USING XRST MSG-PCB                                      
030900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031000                        CHKP-AREA-LENGTH CHKP-AREA                        
031100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031200     PERFORM IMS-STATUSCHECK                                              
031300     .                                                                    
031400     SKIP3                                                                
031500 IMS-CHECKPOINT SECTION.                                                  
031600     SKIP2                                                                
031700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031800     MOVE '  XD' TO GOOD-STATUSCODES                                      
031900     CALL CBLTDLI USING CHKP MSG-PCB                                      
032000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
032100                        CHKP-AREA-LENGTH CHKP-AREA                        
032200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032300     PERFORM IMS-STATUSCHECK                                              
032400                                                                          
032500     IF IMS-NOT-OK                                                        
032600       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
032700                                    TO ERROR-TEXT-STR                     
032800       DISPLAY ERROR-TEXT                                                 
032900       CALL FELLOG                                                        
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 IMS-STATUSCHECK SECTION.                                                 
033400     SKIP2                                                                
033500     SET STATUS-IX TO 1                                                   
033600     SEARCH GOOD-STATUS                                                   
033700       AT END                                                             
033800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033900           DELIMITED BY SIZE INTO ERROR-TEXT-STR                          
034000         DISPLAY ERROR-TEXT                                               
034100         CALL FELLOG                                                      
034200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
034300         CONTINUE                                                         
034400     END-SEARCH                                                           
034500     .                                                                    
