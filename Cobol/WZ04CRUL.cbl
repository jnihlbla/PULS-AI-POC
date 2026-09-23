000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ04CRUL.                                                
000400 AUTHOR.         ANDRE KJELL.                                             
000500 DATE-WRITTEN.   10/09/24.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        CHECK IF A MATCHING RULE IN D&P EXISTS                           
001100*        THE PROGRAM READS   TABLE TZ4DIRU                                
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100 77  IDPGM                       PIC X(8)    VALUE 'WZ04CRUL'.            
003200 77  YES                         PIC X       VALUE 'J'.                   
003300 77  NOO                         PIC X       VALUE 'N'.                   
003500     EJECT                                                                
004200 01  GENERAL-SUBPROGRAMS.                                                 
004300*                                                                         
004400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004700                                                                          
004800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
004900                                                                          
005000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005210 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
005300                                                                          
005400 01  ERROR-TEXT.                                                          
005500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005700     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
008200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
008300                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
008500 01  DB2-WS.                                                              
008600     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
008700         88  CURSOR-OK                       VALUE 000.                   
008800         88  LINES-FOUND                     VALUE 000.                   
008900         88  LINES-MISSING                   VALUE 100.                   
009000         88  MULTIPLE-LINES                  VALUE 811.                   
009100     03  GOOD-SQLCODECODES.                                               
009200         05  GOOD-SQLCODE OCCURS 5                                        
009300             INDEXED BY SQLCODE-IX PIC 9(3).                              
009400*    ---  DLI INPUT-OUTPUT AREA                                           
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)  VALUE 'TZ4DIRU-AREA'.         
009700                                                                          
009800*01  -COPY TZ4DIRU -PRE DIRU-                                             
009810                                                                          
009900     EJECT                                                                
010000     EXEC SQL INCLUDE TZ4DIRU END-EXEC.                                   
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010210                                                                          
010220*01  -COPY WZ04CRUL                                                       
010300                                                                          
010400     EJECT                                                                
010500 PROCEDURE DIVISION  USING CRUL-WZ04CRUL.                                 
010600 MAIN SECTION.                                                            
010900                                                                          
011010     INITIALIZE GOOD-SQLCODECODES                                         
011100                                                                          
011200     PERFORM DB2-SELECT-TZ4DIRU-TAB                                       
011300     IF LINES-FOUND                                                       
011400       MOVE ZERO  TO CRUL-KDRC                                            
012100     ELSE                                                                 
012200       MOVE 4     TO CRUL-KDRC                                            
012210     END-IF                                                               
012300                                                                          
012400     MOVE ZERO TO RETURN-CODE                                             
012500     GOBACK                                                               
012600     .                                                                    
012700     EJECT                                                                
016000 DB2-SELECT-TZ4DIRU-TAB  SECTION.                                         
016100                                                                          
016200     MOVE '000100811'  TO GOOD-SQLCODECODES                               
016300     EXEC SQL                                                             
016410         SELECT  IDOUTTYPE                                                
016420               , IDOUTREC_FROM                                            
016430               , IDOUTREC_TO                                              
017110         INTO    :DIRU-IDOUTTYPE                                          
017120               , :DIRU-IDOUTREC-FROM                                      
017130               , :DIRU-IDOUTREC-TO                                        
017800         FROM    TZ4DIRU                                                  
017900         WHERE   IDOUTTYPE  = :CRUL-IDOUTTYPE                             
017950         AND     IDOUTREC_FROM <= :CRUL-IDOUTREC                          
017960         AND     IDOUTREC_TO   >= :CRUL-IDOUTREC                          
018000     END-EXEC                                                             
018100                                                                          
018200     MOVE SQLCODE TO SQLCODE-WS                                           
018300     PERFORM DB2-STATUS-CHECK                                             
018400     .                                                                    
018500     EJECT                                                                
018600 DB2-STATUS-CHECK  SECTION.                                               
018700                                                                          
018800     SET SQLCODE-IX TO 1                                                  
018900     SEARCH GOOD-SQLCODE                                                  
019000       AT END                                                             
019100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
019200          DELIMITED BY SIZE INTO ERROR-TEXT                               
019300          CALL ABEND USING RKOD-ABEND-DB2                                 
019400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
019500     END-SEARCH                                                           
019600     .                                                                    
