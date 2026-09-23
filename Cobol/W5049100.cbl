000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W5049100.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   14/05/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME                                                                 
000900*        CARPARTS.PULS.RECEIVEI                                           
001000*    FUNCTION:                                                            
001100*        STARTS BY COMMUNICATION REGISTER VIA WZ01SEND.                   
001200*        READS ALL ROWS IN SENT DATA VIA WZ01RECV.                        
001300*        BUILD UP ROWS IN T01IVW.                                         
001400*                                                                         
001500*        THE PROGRAM UPDATES TABLE T01IVW                                 
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W50491X                                             
001900*        REQUEST:     WF2102I1 INTRASTAT                                  
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)  VALUE 'W5049100'.             
002700                                                                          
002800*    --- WORKFIELD FOR ERROR MESSAGES WHEN CALLING ABEND.                 
002900 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
003000 77  KDRC-DISPLAY                PIC Z(5).                                
003100                                                                          
003200 77  YES                         PIC X      VALUE 'J'.                    
003300 77  NOO                         PIC X      VALUE 'N'.                    
003400                                                                          
003500 77  WS-IX                       PIC S9(9)  VALUE +0    COMP SYNC.        
003600 77  RADER-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003700 77  LOPNR-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003800 77  MAX-RADER                   PIC S9(9)  VALUE +50   COMP SYNC.        
003900 77  MAX-LINES                  PIC S9(9)  VALUE +80000 COMP SYNC.        
004000 77  W-ANT                       PIC S9(3)  VALUE ZERO COMP-3.            
004100                                                                          
004200 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004300     88  KEYS-OK                            VALUE 'J'.                    
004400     88  KEYS-ERROR                         VALUE 'N'.                    
004500                                                                          
004600 77  DUPLICATE-SW                PIC X.                                   
004700     88  DUPLICATE-ERROR                    VALUE 'J'.                    
004800     88  DUPLICATE-OK                       VALUE 'N'.                    
004900                                                                          
005000     EJECT                                                                
005100*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
005200 01  GENERAL-SUBPROGRAMS.                                                 
005300     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
005400     03  WZ01RECV                PIC X(8)   VALUE 'WZ01RECV'.             
005500     03  WZ01SEND                PIC X(8)   VALUE 'WZ01SEND'.             
005600     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
005700     SKIP3                                                                
005800                                                                          
005900*    --- PARAMETERS TO ABEND                                              
006000 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
006100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
006200 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
006300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
006400     SKIP3                                                                
006500 01  MESSAGE-CODES.                                                       
006600     03  ERR-DUPLICATE-LINES     PIC X(3)   VALUE '029'.                  
006700     EJECT                                                                
006800                                                                          
006900*    --- AREAS FOR WORK FIELDS                                            
007000 01  FILLER                      PIC X(16)  VALUE 'WDAT-CONTROL'.         
007100     SKIP3                                                                
007200 01  -COPY WDATAREA                                                       
007300     EJECT                                                                
007400 01  FILLER                      PIC X(16)  VALUE 'WDAT-AREA'.            
007500     SKIP3                                                                
007600                                                                          
007700 01  FILLER                      PIC X(16)  VALUE 'INT-CONTROL'.          
007800     SKIP3                                                                
007900 01  -COPY W522INT      -PRE INT-                                         
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)  VALUE 'INT-AREA'.             
008200     SKIP3                                                                
008300                                                                          
008400*    --- AREAS FOR COMMUNICATION                                          
008500 01  FILLER                      PIC X(16)  VALUE 'RECV-CONTROL'.         
008600     SKIP3                                                                
008700 01  -COPY WZ01RECV                                                       
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)  VALUE 'RECV-AREA'.            
009000     SKIP3                                                                
009100                                                                          
009200 01  RECV-AREA.                                                           
009300*    03  -COPY WZ01REQU -PRE IN-                                          
009400*    03  -COPY WF2102I1 -PRE MID-WF2102I1-                                
009500     EJECT                                                                
009600                                                                          
009700 01  -COPY WZ01SEND                                                       
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)  VALUE 'SEND-AREA'.            
010000     SKIP3                                                                
010100                                                                          
010200 01  SEND-AREA.                                                           
010300*    03  -COPY WZ01RESP                                                   
010400     EJECT                                                                
010500                                                                          
010600                                                                          
010700 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
010800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010900                                                                          
011000 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
011100 01  DB2-WS.                                                              
011200     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
011300         88  CURSOR-OK                      VALUE 000.                    
011400         88  LINES-FOUND                    VALUE 000.                    
011500         88  LINES-MISSING                  VALUE 100.                    
011600         88  DUPLICATE-LINES                VALUE 811.                    
011700         88  RESOURCE-WRONG                 VALUE 904.                    
011800                                                                          
011900     03  GOOD-SQLCODECODES.                                               
012000         05  GOOD-SQLCODE OCCURS 5                                        
012100             INDEXED BY SQLCODE-IX PIC 9(3).                              
012200     EJECT                                                                
012300                                                                          
012400 01  FILLER                      PIC X(16)  VALUE 'WS-AREA'.              
012500 01  WS-AREA.                                                             
012600     03 WS-DATUM                 PIC X(8)   VALUE SPACE.                  
012700     03 WS-KLOCKAN               PIC 9(7)   VALUE ZERO.                   
012800     03 WS-IDDISTR-1             PIC 9(5)   VALUE ZERO.                   
012900     03 WS-IDKUNDNR-1            PIC 9(7)   VALUE ZERO.                   
013000     03 WS-IDARTNR-1             PIC 9(9)   VALUE ZERO.                   
013100                                                                          
013200     03 WS-DAREGDAT              PIC X(8)   VALUE SPACE.                  
013300     03 WS-DAEXDAT               PIC 9(8)   VALUE ZERO.                   
013400     03 WS-TIREGTID              PIC S9(6)  VALUE ZERO COMP-3.            
013500     03 WS-TIEXTID               PIC 9(6)   VALUE ZERO.                   
013600     03 WS-IDLOPNR               PIC S9(5)  VALUE ZERO COMP-3.            
013700     03 WS-IDPTYP                PIC X(3)   VALUE SPACE.                  
013800     03 WS-TIRP-1                PIC S9(2)  VALUE ZERO COMP-3.            
013900     03 WS-TIAA-1                PIC S9(2)  VALUE ZERO COMP-3.            
014000     03 WS-TIMM-1                PIC S9(2)  VALUE ZERO COMP-3.            
014100     03 WS-FLKLAR                PIC X(1)   VALUE 'N'.                    
014200                                                                          
014300     03 WS-INTRASTAT-DATA        PIC X(200) VALUE SPACE.                  
014400     03 WS-INTRASTAT-DATA-2      PIC X(200) VALUE SPACE.                  
014500                                                                          
014600 01  FILLER                      PIC X(16)  VALUE 'T01IVW-AREA'.          
014700*01  -COPY T01IVW -PRE T01IVW-                                            
014800     EJECT                                                                
014900                                                                          
015000     EXEC SQL INCLUDE T01IVW END-EXEC.                                    
015100     EJECT                                                                
015200                                                                          
015300 LINKAGE SECTION.                                                         
015400 PROCEDURE DIVISION.                                                      
015500 MAIN SECTION.                                                            
015600                                                                          
015700     PERFORM S01-READ-OPEN                                                
015800     PERFORM S02-READ-MESSAGE                                             
015900     PERFORM A-INIT                                                       
016000     IF RECV-KDRC = ZERO                                                  
016100       PERFORM UNTIL RECV-KDRC > ZERO OR WS-IX > MAX-LINES                
016200         ADD 1 TO LOPNR-IX                                                
016300         PERFORM B-PERFORM-LINES                                          
016400         PERFORM C-UPDATE-T01IVW                                          
016500         PERFORM S02-READ-MESSAGE                                         
016600       END-PERFORM                                                        
016700     END-IF                                                               
016800                                                                          
016900     PERFORM S03-READ-CLOSE                                               
017000* TO MANY LINES INCREASE MAX-LINES OR MAKE A RESTART FUNCTION             
017100     IF WS-IX > MAX-LINES                                                 
017200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
017300     END-IF                                                               
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900                                                                          
018000 A-INIT SECTION.                                                          
018100     MOVE YES                          TO KEYS-SW                         
018200     MOVE NOO                          TO DUPLICATE-SW                    
018300     MOVE ZERO                         TO RADER-IX                        
018400     MOVE ZERO                         TO LOPNR-IX                        
018500     MOVE ZERO                         TO WS-IX                           
018600                                                                          
018700     INITIALIZE GOOD-SQLCODECODES                                         
018800     .                                                                    
018900     EJECT                                                                
019000                                                                          
019100 B-PERFORM-LINES SECTION.                                                 
019200     IF MID-WF2102I1-IDPTYP = 'INT'                                       
019300       PERFORM BA-PERFORM-LINES-INTRASTAT                                 
019400     END-IF                                                               
019500     .                                                                    
019600     EJECT                                                                
019700                                                                          
019800 BA-PERFORM-LINES-INTRASTAT SECTION.                                      
019900     MOVE 'AAMMDD'      TO DAT-KDDATFORM                                  
020000     MOVE MID-WF2102I1-DAFINDOC(3:6) TO DAT-I-TIDATUM                     
020100     CALL WDATKONV USING                                                  
020200          DAT-KDDATFORM                                                   
020300          DAT-I-TIDATUM                                                   
020400          DAT-O-TIDATUM                                                   
020500          DAT-KDSVAR                                                      
020600     MOVE DAT-TIMM                     TO WS-TIRP-1                       
020700                                                                          
020800     MOVE MID-WF2102I1-DAEXDAT(3:2)    TO WS-TIAA-1                       
020900                                                                          
021000     MOVE +0  TO W-ANT                                                    
021100     INSPECT MID-WF2102I1-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS        
021200             BEFORE INITIAL ' '                                           
021300     IF W-ANT = 0                                                         
021400       MOVE +1  TO W-ANT                                                  
021500     END-IF                                                               
021600     MOVE MID-WF2102I1-IDEXCUST-1(1:W-ANT) TO WS-IDDISTR-1                
021700                                                                          
021800     MOVE +0  TO W-ANT                                                    
021900     INSPECT MID-WF2102I1-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS        
022000             BEFORE INITIAL ' '                                           
022100     IF W-ANT = 0                                                         
022200       MOVE +1  TO W-ANT                                                  
022300     END-IF                                                               
022400     MOVE MID-WF2102I1-IDEXCUST-2(1:W-ANT) TO WS-IDKUNDNR-1               
022500                                                                          
022600     MOVE +0  TO W-ANT                                                    
022700     INSPECT MID-WF2102I1-IDARTNR-FINANCE                                 
022800             TALLYING W-ANT FOR CHARACTERS                                
022900             BEFORE INITIAL ' '                                           
023000     IF W-ANT = 0                                                         
023100       MOVE +1  TO W-ANT                                                  
023200     END-IF                                                               
023300     MOVE MID-WF2102I1-IDARTNR-FINANCE(1:W-ANT) TO WS-IDARTNR-1           
023400                                                                          
023500     MOVE WS-TIAA-1                    TO INT-TIAA                        
023600     MOVE WS-TIRP-1                    TO INT-TIRP                        
023700     MOVE MID-WF2102I1-DAEXDAT         TO WS-DAREGDAT                     
023800     MOVE MID-WF2102I1-TIEXTID         TO WS-TIREGTID                     
023900     MOVE MID-WF2102I1-IDPTYP          TO WS-IDPTYP                       
024000     MOVE LOPNR-IX                     TO WS-IDLOPNR                      
024100     MOVE MID-WF2102I1-IDLANDX3-BET    TO INT-IDLANDX3-BET                
024200     MOVE MID-WF2102I1-IDLANDX3-SEND   TO INT-IDLANDX3-SEND               
024300     MOVE MID-WF2102I1-IDLANDX3-REC    TO INT-IDLANDX3-REC                
024400     MOVE MID-WF2102I1-KDVALISO        TO INT-KDVALISO                    
024500     MOVE MID-WF2102I1-PRKURS          TO INT-PRKURS                      
024600     MOVE MID-WF2102I1-IDPARTNR        TO INT-IDPARTNR                    
024700     MOVE MID-WF2102I1-KDFINDOC        TO INT-KDFINDOC                    
024800     MOVE MID-WF2102I1-DAFINDOC        TO INT-DAFINDOC                    
024900     MOVE MID-WF2102I1-IDFINDOC        TO INT-IDFINDOC                    
025000     MOVE WS-IDDISTR-1                 TO INT-IDDISTR                     
025100     MOVE WS-IDKUNDNR-1                TO INT-IDKUNDNR                    
025200     MOVE WS-IDARTNR-1                 TO INT-IDARTNR                     
025300     MOVE MID-WF2102I1-BEART           TO INT-BEART                       
025400     MOVE MID-WF2102I1-IDSTATNR        TO INT-IDSTATNR                    
025500     MOVE MID-WF2102I1-VKORDNTO        TO INT-VKORDNTO                    
025700     MOVE MID-WF2102I1-VKORDNTO-3DEC   TO INT-VKORDNTO-3DEC               
025900     MOVE MID-WF2102I1-KVLEVART        TO INT-KVLEVART                    
026000     MOVE MID-WF2102I1-KDARTURS        TO INT-KDARTURS                    
026100     MOVE MID-WF2102I1-KDFRAKT         TO INT-KDFRAKT                     
026200     MOVE MID-WF2102I1-BELEVVIL        TO INT-BELEVVIL                    
026300     MOVE MID-WF2102I1-SUNTO           TO INT-SUNTO                       
026400     MOVE MID-WF2102I1-KDVALISO-SEND   TO INT-KDVALISO-SEND               
026500     MOVE MID-WF2102I1-PRKURS-SEND     TO INT-PRKURS-SEND                 
026600     MOVE MID-WF2102I1-IDVAT-LEG       TO INT-IDVAT-LEG                   
026700     MOVE MID-WF2102I1-IDVAT-BET       TO INT-IDVAT-BET                   
026800     MOVE MID-WF2102I1-IDVAT-RESP      TO INT-IDVAT-RESP                  
026900     MOVE MID-WF2102I1-IDVAT-AGENT     TO INT-IDVAT-AGENT                 
027000                                                                          
027100     MOVE SPACE                        TO WS-INTRASTAT-DATA               
027200                                          WS-INTRASTAT-DATA-2             
027300     MOVE INT-W522INT-001              TO WS-INTRASTAT-DATA               
027400     MOVE INT-W522INT-002              TO WS-INTRASTAT-DATA-2             
027500                                                                          
027600     ADD 1                             TO WS-IX                           
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
028000 C-UPDATE-T01IVW SECTION.                                                 
028100     IF MID-WF2102I1-IDPTYP = 'INT'                                       
028200       PERFORM DB2-INSERT-T01IVW-INTRASTAT                                
028300     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600                                                                          
028700*    --- DISPATCHER-SECTIONS                                              
028800 S01-READ-OPEN SECTION.                                                   
028900     MOVE 'OPEN'                      TO RECV-KDFUNC                      
029000     MOVE 'CARPARTS.PULS.RECEIVEI'    TO RECV-ADDISPABS                   
029100     CALL WZ01RECV USING RECV-CONTROL-AREA                                
029200                         RECV-OPEN-AREA                                   
029300     IF RECV-KDRC > 0                                                     
029400       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
029500       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
029600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
029700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029800     END-IF                                                               
029900     .                                                                    
030000     SKIP3                                                                
030100                                                                          
030200 S02-READ-MESSAGE SECTION.                                                
030300     MOVE 'GET'                           TO RECV-KDFUNC                  
030400     MOVE LENGTH OF RECV-AREA             TO RECV-KVDLEN                  
030500     CALL WZ01RECV USING RECV-CONTROL-AREA                                
030600                         RECV-KVDLEN                                      
030700                         RECV-AREA                                        
030800     IF RECV-KDRC > 1                                                     
030900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
031000       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
031100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031300     END-IF                                                               
031400     .                                                                    
031500     SKIP3                                                                
031600                                                                          
031700 S03-READ-CLOSE SECTION.                                                  
031800     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
031900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
032000                                                                          
032100     IF RECV-KDRC > 0                                                     
032200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
032300       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
032400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
032500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900                                                                          
033000 DB2-INSERT-T01IVW-INTRASTAT SECTION.                                     
033100     SKIP2                                                                
033200     MOVE 000811 TO GOOD-SQLCODECODES                                     
033300     EXEC SQL                                                             
033400         INSERT INTO T01IVW                                               
033500         (DAREGDAT,                                                       
033600          TIREGTID,                                                       
033700          IDLOPNR,                                                        
033800          IDPTYP,                                                         
033900          TIRP,                                                           
034000          FLKLAR,                                                         
034100          IV_DATA,                                                        
034200          IV_DATA2)                                                       
034300         VALUES(:WS-DAREGDAT,                                             
034400                :WS-TIREGTID,                                             
034500                :WS-IDLOPNR,                                              
034600                :WS-IDPTYP,                                               
034700                :WS-TIRP-1,                                               
034800                'N',                                                      
034900                :WS-INTRASTAT-DATA,                                       
035000                :WS-INTRASTAT-DATA-2)                                     
035100     END-EXEC                                                             
035200                                                                          
035300     MOVE SQLCODE TO SQLCODE-WS                                           
035400     PERFORM DB2-STATUS-CHECK                                             
035500     .                                                                    
035600     EJECT                                                                
035700                                                                          
035800 DB2-STATUS-CHECK     SECTION.                                            
035900     SET SQLCODE-IX TO 1                                                  
036000     SEARCH GOOD-SQLCODE                                                  
036100       AT END                                                             
036200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
036300          DELIMITED BY SIZE INTO ERROR-TEXT                               
036400          CALL ABEND USING RKOD-ABEND-DB2                                 
036500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
036600     END-SEARCH                                                           
036700     .                                                                    
