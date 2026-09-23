000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF104100.                                                
000400 AUTHOR.         BHAT ARCHANA.                                            
000500 DATE-WRITTEN.   19/09/06.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS PROGRAM CREATES 2 FILES - ONE TO BE USED BY BILLIT          
001100*        AND OTHER FOR THE SAP MANUAL UPDATES(A20 AND A21).               
001200*        IT ALSO CHECKS IF THE TRADING PARTNER SENT BY SAP IS             
001300*        VALID                                                            
001400*                                                                         
001500*                                                                         
001600*    ABENDCODES:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- MASTER DATA FROM SAP                                       
002900     SELECT WF1051                     ASSIGN TO WF1041D1.                
003000     SKIP2                                                                
003100*          --- TO SAP                                                     
003200     SELECT WF1052                     ASSIGN TO WF1041D2.                
003300     SKIP2                                                                
003400*          --- TO BILLIT                                                  
003500     SELECT WF1053                     ASSIGN TO WF1041D3.                
003501     SKIP2                                                                
003510*          --- ERROR FILE                                                 
003520     SELECT WF1054                     ASSIGN TO WF1041D4.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  WF1051                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400 01  IN-POST        PIC X(1054).                                          
004500     SKIP3                                                                
004600 FD  WF1052                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900 01  UT-POST1       PIC X(430).                                           
005000     SKIP3                                                                
005100 FD  WF1053                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400 01  UT-POST2       PIC X(1054).                                          
005500     EJECT                                                                
005510 FD  WF1054                                                               
005520     RECORDING  V                                                         
005530     BLOCK CONTAINS 0.                                                    
005540 01  UT-ERR-RECORD  PIC X(1054).                                          
005550     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'WF104100'.            
005900 77  YES                         PIC X       VALUE 'J'.                   
006000 77  NOO                         PIC X       VALUE 'N'.                   
006010 77  WS-KDTRADP                  PIC X(4)    VALUE SPACES.                
006011 77  WS-SAVE-KDTRADP             PIC X(4)    VALUE SPACES.                
006020 77  WS-VALID-KDTRADP            PIC X(1)    VALUE SPACES.                
006040 77  WS-HDR                      PIC X(1)    VALUE SPACES.                
006100                                                                          
006200 77  WF1051-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-WF1051                       VALUE 'J'.                   
006400     EJECT                                                                
006500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES TODAYS-DATE.                                        
006700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006900     03  TODAYS-DATE-DAY         PIC 9(2).                                
007000     EJECT                                                                
007100 01  GENERAL-SUBPROGRAMS.                                                 
007200*                                                                         
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     SKIP2                                                                
007600*    --- PARAMETERS TO ABEND                                              
007700                                                                          
007800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008010 77  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
008100     SKIP2                                                                
008200 01  ERROR-TEXT.                                                          
008300     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
008400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL POSTSUM                                          
008700*                                                                         
008800*01  -COPY W0005   -PRE  POSTSUM-                                         
008900     EJECT                                                                
009000 01  IN-AREA-START               PIC X(24)   VALUE                        
009100                                 'IN-AREA-START  '.                       
009200 01  IN-AREA.                                                             
009300     03  IN-KDTRADP              PIC X(4).                                
009400     03  IN-RECTYP               PIC X(3).                                
009500     03  IN-KONTO                PIC X(1047).                             
009600     SKIP2                                                                
009700     EJECT                                                                
009800 01  UT1-AREA-START              PIC X(24)   VALUE                        
009900                                 'UT2-AREA-START  '.                      
010000 01  UT-AREA1                    PIC X(430).                              
010100     SKIP2                                                                
010200     EJECT                                                                
010300 01  UT-AREA-START               PIC X(24)   VALUE                        
010400                                 'UT-AREA-START  '.                       
010500 01  UT-AREA2                    PIC X(1054).                             
010600     SKIP2                                                                
010700     EJECT                                                                
010710 01  WF1054-HEAD.                                                         
010720     03  FILLER               PIC X(13) VALUE 'TRADING PRTNR'.            
010730     03  FILLER               PIC X(1)  VALUE ';'.                        
010760                                                                          
010770 01  WF1054-DATA.                                                         
010781     03  WF1054-ERR-LINE         PIC X(1054).                             
010793     EJECT                                                                
010794 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
010795       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010796                                                                          
010797 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
010798 01  DB2-WS.                                                              
010799     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
010800         88  CURSOR-OK                      VALUE 000.                    
010801         88  LINES-FOUND                    VALUE 000.                    
010802         88  LINES-MISSING                  VALUE 100.                    
010803         88  RESOURCE-WRONG                 VALUE 904.                    
010804     03  GOOD-SQLCODES.                                                   
010805         05  GOOD-SQLCODE OCCURS 5                                        
010806             INDEXED BY SQLCODE-IX PIC 9(3).                              
010807     EJECT                                                                
010808*    --------------- DB2 INPUT-OUTPUT AREA ---------------                
010809                                                                          
010810 01  FILLER                      PIC X(16)  VALUE 'T01LSEL-AREA'.         
010811                                                                          
010812*01  -COPY T01LSEL -PRE LSEL-                                             
010813     EJECT                                                                
010814     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
010820 PROCEDURE DIVISION.                                                      
010900 MAIN SECTION.                                                            
011000     SKIP2                                                                
011100                                                                          
011200     PERFORM A-INIT                                                       
011300     PERFORM S01-READ-WF1051                                              
011400     PERFORM UNTIL END-OF-WF1051                                          
011500       IF IN-KDTRADP = WS-SAVE-KDTRADP                                    
011600         PERFORM B-PROCESS                                                
011700       ELSE                                                               
011800         MOVE IN-KDTRADP  TO WS-SAVE-KDTRADP                              
011900         PERFORM BA-CHECK-KDTRADP                                         
012000         PERFORM B-PROCESS                                                
012100       END-IF                                                             
012200       PERFORM S01-READ-WF1051                                            
012300     END-PERFORM                                                          
012400                                                                          
012500     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 A-INIT SECTION.                                                          
013200                                                                          
013300     OPEN INPUT  WF1051                                                   
013400                                                                          
013500     OPEN OUTPUT WF1052                                                   
013600                 WF1053                                                   
013610                 WF1054                                                   
013700     SKIP2                                                                
013800     ACCEPT TODAYS-DATE  FROM DATE                                        
013900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014200 B-PROCESS SECTION.                                                       
014300                                                                          
014400     IF WS-VALID-KDTRADP = YES                                            
014500       EVALUATE IN-RECTYP                                                 
014600         WHEN 'A20'                                                       
014700           PERFORM BB-PROCESS-SAP                                         
014800         WHEN 'A21'                                                       
014900           PERFORM BB-PROCESS-SAP                                         
015000         WHEN OTHER                                                       
015100           PERFORM BC-PROCESS-BILLIT                                      
015200       END-EVALUATE                                                       
015210     ELSE                                                                 
015220       PERFORM S13-CREATE-WF1054                                          
015300     END-IF                                                               
015400     .                                                                    
015500     EJECT                                                                
015510 BA-CHECK-KDTRADP SECTION.                                                
015520                                                                          
015521     MOVE FUNCTION UPPER-CASE(IN-KDTRADP) TO WS-KDTRADP                   
015530     PERFORM DB2-SEARCH-T01LSEL-TAB                                       
015540     IF LINES-FOUND                                                       
015550       MOVE YES                TO WS-VALID-KDTRADP                        
015560     ELSE                                                                 
015570       MOVE NOO                TO WS-VALID-KDTRADP                        
015590     END-IF                                                               
015591     .                                                                    
015592     EJECT                                                                
015600 BB-PROCESS-SAP SECTION.                                                  
015700     MOVE IN-AREA TO UT-AREA1                                             
015800     PERFORM S11-WRITE-WF1052                                             
015900     .                                                                    
016000     EJECT                                                                
016100 BC-PROCESS-BILLIT SECTION.                                               
016200     MOVE IN-AREA TO UT-AREA2                                             
016300     PERFORM S12-WRITE-WF1053                                             
016400     .                                                                    
016500     EJECT                                                                
016600                                                                          
016700 Z-FINIT SECTION.                                                         
016800     CLOSE WF1051                                                         
016900           WF1052                                                         
017000           WF1053                                                         
017010           WF1054                                                         
017100     SKIP2                                                                
017200     MOVE 'S' TO POSTSUM-OPKOD                                            
017300     CALL POSTSUM USING POSTSUM-PARM                                      
017400     .                                                                    
017500     EJECT                                                                
017600 S01-READ-WF1051  SECTION.                                                
017700     READ WF1051 INTO IN-AREA                                             
017800     AT END                                                               
017900        MOVE HIGH-VALUE TO IN-AREA                                        
018000        SET END-OF-WF1051 TO TRUE                                         
018100                                                                          
018200     NOT AT END                                                           
018300        MOVE 'WF1051' TO POSTSUM-FDNAMN                                   
018400        MOVE 'WF1041D1' TO POSTSUM-DDNAMN2                                
018500        MOVE 'IN-POST' TO POSTSUM-TRANSTYP                                
018600        CALL POSTSUM USING POSTSUM-PARM                                   
018700     END-READ                                                             
018800     .                                                                    
018900     EJECT                                                                
019000 S11-WRITE-WF1052 SECTION.                                                
019100                                                                          
019200     WRITE UT-POST1 FROM UT-AREA1                                         
019300                                                                          
019400     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
019500     MOVE 'WF1052' TO POSTSUM-FDNAMN                                      
019600     MOVE 'WF1041D2' TO POSTSUM-DDNAMN2                                   
019700     CALL POSTSUM USING POSTSUM-PARM                                      
019800     .                                                                    
019900     EJECT                                                                
020000 S12-WRITE-WF1053 SECTION.                                                
020100                                                                          
020200     WRITE UT-POST2 FROM UT-AREA2                                         
020300                                                                          
020400     MOVE SPACES     TO POSTSUM-TRANSTYP                                  
020500     MOVE 'WF1053' TO POSTSUM-FDNAMN                                      
020600     MOVE 'WF1041D3' TO POSTSUM-DDNAMN2                                   
020700     CALL POSTSUM USING POSTSUM-PARM                                      
020800     .                                                                    
020900     EJECT                                                                
021000 S13-CREATE-WF1054 SECTION.                                               
021010                                                                          
021071*    MOVE IN-KDTRADP         TO WF1054-KDTRADP                            
021074*    IF WS-HDR = YES                                                      
021075*      CONTINUE                                                           
021076*    ELSE                                                                 
021077*      WRITE UT-ERR-RECORD   FROM WF1054-HEAD                             
021078*      MOVE YES              TO WS-HDR                                    
021079*    END-IF                                                               
021080                                                                          
021081     MOVE IN-AREA            TO WF1054-ERR-LINE                           
021082     WRITE UT-ERR-RECORD   FROM WF1054-ERR-LINE                           
021083     .                                                                    
021090     EJECT                                                                
021095 DB2-SEARCH-T01LSEL-TAB SECTION.                                          
021096                                                                          
021097     MOVE 000100305      TO GOOD-SQLCODES                                 
021098                                                                          
021099     EXEC SQL                                                             
021100      SELECT  IDLEGSEL                                                    
021102                                                                          
021104      INTO    :LSEL-IDLEGSEL                                              
021105                                                                          
021106      FROM    T01LSEL                                                     
021107                                                                          
021108      WHERE   KDTRADP = :WS-KDTRADP                                       
021109     END-EXEC                                                             
021110                                                                          
021111     MOVE SQLCODE        TO SQLCODE-WS                                    
021112     PERFORM DB2-STATUS-CHECK                                             
021113     .                                                                    
021114 DB2-STATUS-CHECK  SECTION.                                               
021115     SET SQLCODE-IX TO 1                                                  
021116     SEARCH GOOD-SQLCODE                                                  
021117       AT END                                                             
021118          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
021119          DELIMITED BY SIZE INTO ERROR-TEXT                               
021120          CALL ABEND USING RKOD-ABEND-DB2                                 
021130       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
021140          CONTINUE                                                        
021150     END-SEARCH                                                           
021160     .                                                                    
