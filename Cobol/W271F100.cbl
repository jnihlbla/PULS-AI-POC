000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W271F100.                                                
000300 AUTHOR.         DATTA ARUP.                                              
000400 DATE-WRITTEN.   15/02/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        UPDATE DATE, TIDATUM-CROSS, IN WDK7                              
001000*        FOR DELAY IN REF ORDER                                           
001100*                                                                         
001200*        THE PROGRAM UPDATES   WDK7                                       
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- W271F1 INPUT FILE.                                         
002300     SELECT W271F1                     ASSIGN TO W271F1D1.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W271F1                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W271F1      -L.                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W271F100'.            
003800 77  YES                         PIC X       VALUE 'J'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
004000     SKIP2                                                                
004100 01  ERROR-TEXT.                                                          
004200     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
004300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
004400                                                                          
004500 77  W271F1-EOF-SW               PIC X       VALUE 'N'.                   
004600     88  END-OF-W271F1                       VALUE 'Y'.                   
004700     EJECT                                                                
004800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES TODAYS-DATE.                                        
005000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005200     03  TODAYS-DATE-DAY         PIC 9(2).                                
005300     EJECT                                                                
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500*                                                                         
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     EJECT                                                                
006000*    --- PARAMETRAR TILL POSTSUM                                          
006100*                                                                         
006200*01  -COPY W0005   -PRE  POSTSUM-                                         
006300     EJECT                                                                
006400 01  IN-AREA-START               PIC X(24)   VALUE                        
006500                                             'IN-AREA-START'.             
006600     SKIP2                                                                
006700                                                                          
006800*01  AREA -COPY W271F1     -PRE IN-                                       
006900*                                                                         
007000     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007200     SKIP3                                                                
007300 01  KEYS-TILL-DLI.                                                       
007400     03  W-IDARTNR-X.                                                     
007500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007600     03  W-IDDC-X.                                                        
007700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
007800     SKIP2                                                                
007900*    --- STATUS-KOD FRÅN IMS                                              
008000 01  STATUS-WS                   PIC XX.                                  
008100     88  SEGMENT-FOUND                       VALUE '  '.                  
008200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008400     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008500     88  IMS-NOT-OK                          VALUE 'XD'.                  
008600     SKIP2                                                                
008700 01  GOOD-STATUSCODES.                                                    
008800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     SKIP3                                                                
009000 01  SSA1                        PIC X(64).                               
009100 01  SSA2                        PIC X(64).                               
009200     EJECT                                                                
009300*    --- IMS FUNCTION CODES                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009600*    ---  DLI INPUT-OUTPUT AREA                                           
009700                                                                          
009800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
009900 01  DLI-IO-WDK701.                                                       
010000*    03  -COPY WDK701                                                     
010100     EJECT                                                                
010200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
010300 01  DLI-IO-WDK711.                                                       
010400*    03  -COPY WDK711                                                     
010500     EJECT                                                                
010600 LINKAGE SECTION.                                                         
010700                                                                          
010800*01  -COPY W0009  -PRE MSG-                                               
010900                                                                          
011000*01  -COPY W0008  -PRE WDK7-                                              
011100     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011300                                                                          
011400 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB.                              
011500 MAIN SECTION.                                                            
011600     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB.                              
011700                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
012000     PERFORM S01-READ-W271F1                                              
012100     PERFORM UNTIL END-OF-W271F1                                          
012200                                                                          
012300       MOVE IN-IDARTNR                TO W-IDARTNR                        
012400       MOVE IN-IDDC                   TO W-IDDC                           
012500                                                                          
012600       PERFORM IMS-GHU-WDK711                                             
012700       IF SEGMENT-FOUND                                                   
012710          IF IN-TIDATUM-CROSS         =  SLAG-TIDATUM-CROSS               
012720             CONTINUE                                                     
012730          ELSE                                                            
012800             MOVE IN-TIDATUM-CROSS                                        
012900                                      TO SLAG-TIDATUM-CROSS               
013100             PERFORM IMS-REPL-WDK711                                      
013200          END-IF                                                          
013201       ELSE                                                               
013202          DISPLAY 'PART NOT IN DC :'                                      
013203                  IN-IDARTNR '/' IN-IDDC                                  
013210       END-IF                                                             
013300                                                                          
013400       PERFORM S01-READ-W271F1                                            
013500     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014400     SKIP2                                                                
014500     OPEN INPUT W271F1                                                    
014600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014700     .                                                                    
014800     EJECT                                                                
014900 Z-FINIT SECTION.                                                         
015000     CLOSE W271F1                                                         
015100     SKIP2                                                                
015200     MOVE 'S' TO POSTSUM-OPKOD                                            
015300     CALL POSTSUM USING POSTSUM-PARM                                      
015400     .                                                                    
015500     EJECT                                                                
015600 S01-READ-W271F1  SECTION.                                                
015700     SKIP2                                                                
015800     READ W271F1 INTO IN-AREA                                             
015900     AT END                                                               
016100        SET END-OF-W271F1 TO TRUE                                         
016200     NOT AT END                                                           
016300        MOVE 'W271F1' TO POSTSUM-FDNAMN                                   
016400        MOVE 'W271F1D1' TO POSTSUM-DDNAMN2                                
016500        MOVE SPACE     TO POSTSUM-TRANSTYP                                
016600        CALL POSTSUM USING POSTSUM-PARM                                   
016700     END-READ                                                             
016800     .                                                                    
016900     EJECT                                                                
017000* --- IMS SECTIONS  ---                                                   
017100                                                                          
017200     EJECT                                                                
017300 IMS-GHU-WDK711 SECTION.                                                  
017400                                                                          
017500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
017600          DELIMITED BY SIZE INTO SSA1                                     
017700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
017800          DELIMITED BY SIZE INTO SSA2                                     
017900     MOVE '  GE' TO GOOD-STATUSCODES                                      
018000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
018100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
018200     PERFORM IMS-STATUSCHECK                                              
018300     .                                                                    
018400     SKIP3                                                                
018500 IMS-REPL-WDK711 SECTION.                                                 
018600                                                                          
018700     MOVE '  ' TO GOOD-STATUSCODES                                        
018800     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
018900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
019000     PERFORM IMS-STATUSCHECK                                              
019100     .                                                                    
019200     EJECT                                                                
019300 IMS-STATUSCHECK SECTION.                                                 
019400     SKIP2                                                                
019500     SET STATUS-IX TO 1                                                   
019600     SEARCH GOOD-STATUS                                                   
019700       AT END                                                             
019800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
019900           DELIMITED BY SIZE INTO ERROR-TEXT                              
020000         DISPLAY ERROR-TEXT                                               
020100         CALL FELLOG                                                      
020200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
020300         CONTINUE                                                         
020400     END-SEARCH                                                           
020500     .                                                                    
