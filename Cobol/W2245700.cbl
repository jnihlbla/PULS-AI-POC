000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2245700.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   02/04/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE WDG301/302 - WDGX2213/14                                  
001000*                                                                         
001100                                                                          
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*          --- INPUT FILE TO UPDATE WDGX2213/14                           
002000     SELECT W2245701                   ASSIGN TO W22457D1.                
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP3                                                                
002400 FILE SECTION.                                                            
002500     SKIP3                                                                
002600 FD  W2245701                                                             
002700     RECORDING       F                                                    
002800     BLOCK CONTAINS  0.                                                   
002900                                                                          
003000*01  -COPY W2245801      -L.                                              
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W2245700'.            
003500 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
003600 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
003700 01  CHKP-VAR.                                                            
003800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004300     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004400 01  W-KVPOST-IN                 PIC S9(9)  VALUE ZERO COMP SYNC.         
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700     SKIP2                                                                
004800*    --- PARAMETERS TO ABEND                                              
004900                                                                          
005000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005300 01  ERROR-TEXT.                                                          
005400     03  FILLER                  PIC X(08)   VALUE 'ERR-TEXT'.            
005500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 77  W2245701-EOF-SW             PIC X       VALUE 'N'.                   
005800     88  END-OF-W2245701                     VALUE 'Y'.                   
005900     EJECT                                                                
006000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES TODAYS-DATE.                                        
006200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006400     03  TODAYS-DATE-DAY         PIC 9(2).                                
006500     EJECT                                                                
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
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
008100*01  AREA -COPY W2245801     -PRE IN-                                     
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  KEYS-TILL-DLI.                                                       
008700                                                                          
008800     03  W-WDGX2213-X.                                                    
008900         05  W-IDHTYP-2213       PIC X(4)    VALUE '2213'.                
009000         05  W-IDDC-2213         PIC X(2)    VALUE SPACE.                 
009100         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
009200                                                                          
009300*    --- STATUS-KOD FRÅN IMS                                              
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FOUND                       VALUE '  '.                  
009600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009800     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009900     88  IMS-NOT-OK                          VALUE 'XD'.                  
010000     SKIP2                                                                
010100 01  GOOD-STATUSCODES.                                                    
010200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010300     SKIP3                                                                
010400 01  SSA1                        PIC X(64).                               
010500 01  SSA2                        PIC X(64).                               
010600     EJECT                                                                
010700*    --- IMS FUNCTION CODES                                               
010800*01  -COPY W0003                                                          
010900     EJECT                                                                
011000*    ---  DLI INPUT-OUTPUT AREA                                           
011100                                                                          
011110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4579'.                    
011200 01  W-WDGXKEY-X.                                                         
011210     05  W-IDHTYP            PIC X(4)    VALUE '4579'.                    
011220     05  W-IDPGM             PIC X(8)    VALUE 'W2245700'.                
011230     05  FILLER              PIC X(18)   VALUE LOW-VALUE.                 
011600     EJECT                                                                
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011710                                                                          
011800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2214'.                    
011900 01  DLI-IO-WDGX2214.                                                     
012000*    03  -COPY WDGX2214                                                   
012100     EJECT                                                                
012110*-ÅTERSTARTSREGISTER WDR4                                                 
012120 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
012130 01  DLI-IO-WDGX4580.                                                     
012140*    03  -COPY WDGX4580                                                   
012150     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400*01  -COPY W0009   -PRE MSG-                                              
012500     EJECT                                                                
012600*01  -COPY W0008  -PRE 2214-                                              
012700     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012900*01  -COPY W0008  -PRE 4579-                                              
013000     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING MSG-PCB 2214-PCB 4579-PCB.                     
013300 MAIN SECTION.                                                            
013400     ENTRY 'DLITCBL' USING MSG-PCB 2214-PCB 4579-PCB.                     
013500                                                                          
013600     PERFORM A-INIT                                                       
013700                                                                          
013800     PERFORM IMS-LAS-ATERSTART                                            
013900     IF 4580-KVPOST > +0                                                  
014000        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
014100     ELSE                                                                 
014200       PERFORM S01-READ-W2245701                                          
014300     END-IF                                                               
014400                                                                          
014500     PERFORM UNTIL END-OF-W2245701                                        
014600       IF CHKP-ANT > CHKP-MAX                                             
014700         PERFORM X-TAKE-CHECKPOINT                                        
014800       END-IF                                                             
014900                                                                          
014910       MOVE IN-IDDC           TO W-IDDC-2213                              
015000       MOVE IN-IDARTNR        TO 2214-IDARTNR                             
015100       PERFORM IMS-ISRT-WDGX2214                                          
015110                                                                          
015200       ADD  +1                TO CHKP-ANT                                 
015300                                                                          
015400       PERFORM S01-READ-W2245701                                          
015500     END-PERFORM                                                          
015600                                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500     MOVE 'A-INIT                   ' TO CURRENT-SECTION                  
016600                                                                          
016700     PERFORM IMS-RESTART                                                  
016800                                                                          
016900     OPEN INPUT W2245701                                                  
017000                                                                          
017100     MOVE +0                 TO W-KVPOST-IN                               
017200                                CHKP-ANT                                  
017300                                                                          
017400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017500     .                                                                    
017600     EJECT                                                                
017700 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
017800     MOVE 'B-LAES-FRAM-TILL-CHKPOINT' TO CURRENT-SECTION                  
017900                                                                          
018000     PERFORM S01-READ-W2245701                                            
018100     PERFORM UNTIL END-OF-W2245701 OR                                     
018200                   W-KVPOST-IN = 4580-KVPOST                              
018300        PERFORM S01-READ-W2245701                                         
018400     END-PERFORM                                                          
018500                                                                          
018600     IF END-OF-W2245701                                                   
018700        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
018800                      TO ERROR-TEXT                                       
018900        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
019000     END-IF                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 Z-FINIT SECTION.                                                         
019400     MOVE 'Z-FINIT                  ' TO CURRENT-SECTION                  
019500                                                                          
019600     CLOSE W2245701                                                       
019700                                                                          
019800*    NOLLA ÅTERSTARTINFORMATIONEN                                         
019900     PERFORM IMS-LAS-ATERSTART                                            
020000     MOVE +0                TO 4580-KVPOST                                
020100     ACCEPT 4580-TIUPPDAT FROM DATE                                       
020200     ACCEPT 4580-TIUPPTID FROM TIME                                       
020300                                                                          
020400     PERFORM IMS-REPL-ATERSTART                                           
020500                                                                          
020600     MOVE 'S'               TO POSTSUM-OPKOD                              
020700     CALL POSTSUM        USING POSTSUM-PARM                               
020800     .                                                                    
020900     EJECT                                                                
021000 S01-READ-W2245701  SECTION.                                              
021100     MOVE 'S01-READ-W2245701        ' TO CURRENT-SECTION                  
021200     SKIP2                                                                
021300     READ W2245701        INTO IN-AREA                                    
021400     AT END                                                               
021500        SET END-OF-W2245701 TO TRUE                                       
021600                                                                          
021700     NOT AT END                                                           
021800        MOVE 'W2245701'     TO POSTSUM-FDNAMN                             
021900        MOVE 'W22457D1'     TO POSTSUM-DDNAMN2                            
022000        MOVE 'IN'           TO POSTSUM-TRANSTYP                           
022100        CALL POSTSUM     USING POSTSUM-PARM                               
022200                                                                          
022300        ADD +1              TO W-KVPOST-IN                                
022400     END-READ                                                             
022500     .                                                                    
022600     EJECT                                                                
022700 X-TAKE-CHECKPOINT   SECTION.                                             
022800     MOVE 'X-TAKE-CHECKPOINT        ' TO CURRENT-SECTION                  
022900                                                                          
023000*    UPPDATERA ÅTERSTARTREGISTRET                                         
023100     PERFORM IMS-LAS-ATERSTART                                            
023200                                                                          
023300     MOVE W-KVPOST-IN       TO 4580-KVPOST                                
023400     ACCEPT 4580-TIUPPDAT FROM DATE                                       
023500     ACCEPT 4580-TIUPPTID FROM TIME                                       
023600                                                                          
023700     PERFORM IMS-REPL-ATERSTART                                           
023800                                                                          
023900*    TAG CHECKPOINT                                                       
024000     PERFORM IMS-CHECKPOINT                                               
024100                                                                          
024200     MOVE +0                TO CHKP-ANT                                   
024300     .                                                                    
024400     EJECT                                                                
024500* --- IMS SECTIONS  ---                                                   
024600                                                                          
024700 IMS-ISRT-WDGX2214 SECTION.                                               
024800                                                                          
024900     STRING 'WDG301  (WDG3KEY  =' W-WDGX2213-X ')'                        
025000            DELIMITED BY SIZE INTO SSA1                                   
025100     MOVE 'WDG302   '           TO SSA2                                   
025200     MOVE '  II'                TO GOOD-STATUSCODES                       
025300     CALL CBLTDLI USING ISRT 2214-PCB DLI-IO-WDGX2214 SSA1 SSA2           
025400     MOVE 2214-STATUS-CODE      TO STATUS-WS                              
025500     PERFORM IMS-STATUSCHECK                                              
025600     .                                                                    
025700     EJECT                                                                
025800 IMS-RESTART SECTION.                                                     
025900     MOVE 'IMS-RESTART          ' TO DBS-SECTION                          
026000                                                                          
026100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026200     MOVE '  ' TO GOOD-STATUSCODES                                        
026300     CALL CBLTDLI USING XRST MSG-PCB                                      
026400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026500                        CHKP-AREA-LENGTH CHKP-AREA                        
026600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026700     PERFORM IMS-STATUSCHECK                                              
026800     .                                                                    
026900     SKIP3                                                                
027000 IMS-CHECKPOINT SECTION.                                                  
027100     MOVE 'IMS-CHECKPOINT       ' TO DBS-SECTION                          
027200                                                                          
027300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027400     MOVE '  XD' TO GOOD-STATUSCODES                                      
027500     CALL CBLTDLI USING CHKP MSG-PCB                                      
027600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027700                        CHKP-AREA-LENGTH CHKP-AREA                        
027800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027900     PERFORM IMS-STATUSCHECK                                              
028000                                                                          
028100     IF IMS-NOT-OK                                                        
028200       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
028300                                    TO ERROR-TEXT-STR                     
028400       DISPLAY ERROR-TEXT                                                 
028500       CALL FELLOG                                                        
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 IMS-LAS-ATERSTART SECTION.                                               
029000     MOVE 'IMS-LAS-ATERSTART    ' TO DBS-SECTION                          
029100                                                                          
029200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
029300                    DELIMITED BY SIZE INTO SSA1                           
029400     MOVE 'WDR470 '        TO SSA2                                        
029500     MOVE '  '             TO GOOD-STATUSCODES                            
029600     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
029700     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
029800     PERFORM IMS-STATUSCHECK                                              
029900     .                                                                    
030000                                                                          
030100 IMS-REPL-ATERSTART SECTION.                                              
030200     MOVE 'IMS-REPL-ATERSTART   ' TO DBS-SECTION                          
030300                                                                          
030400     MOVE '  '             TO GOOD-STATUSCODES                            
030500     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
030600     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
030700     PERFORM IMS-STATUSCHECK                                              
030800     .                                                                    
030900                                                                          
031000     EJECT                                                                
031100 IMS-STATUSCHECK SECTION.                                                 
031200     SKIP2                                                                
031300     SET STATUS-IX TO 1                                                   
031400     SEARCH GOOD-STATUS                                                   
031500       AT END                                                             
031600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031700           DELIMITED BY SIZE INTO ERROR-TEXT                              
031800         DISPLAY ERROR-TEXT                                               
031900         CALL FELLOG                                                      
032000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
032100         CONTINUE                                                         
032200     END-SEARCH                                                           
032300     .                                                                    
