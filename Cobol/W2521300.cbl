000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000400 PROGRAM-ID.             W2521300.                                        
000500 AUTHOR.                 INGER STENING.                                   
000600 DATE-WRITTEN.           19/05/09.                                        
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PART INFORMATION - PURCHASE PLANNING                             
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SELECT  W25212                   ASSIGN  UT-S-W25213D1.              
001800     SELECT  W25213                   ASSIGN  UT-S-W25213D2.              
001900 DATA DIVISION.                                                           
002000                                                                          
002100 FILE SECTION.                                                            
002200                                                                          
002300 FD  W25212                                                               
002400     RECORDING      F                                                     
002500     BLOCK CONTAINS 0.                                                    
002600*01  -COPY W25209      -L.                                                
002700                                                                          
002800 FD  W25213                                                               
002900     RECORDING   F                                                        
003000     BLOCK CONTAINS 0.                                                    
003100*01  RECORD -COPY W25209 -PRE  OUT-  -L.                                  
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W2521300'.            
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  W25212-EOF-SW               PIC X       VALUE 'N'.                   
003900     88  END-OF-W25212                       VALUE 'J'.                   
004000 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
004100                                                                          
004200 01  DYNAMISKA-SUBPROGRAM.                                                
004300   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
004400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
004500                                                                          
004600 01  RETURKODER.                                                          
004700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
004800   03  RKOD                      PIC S9(4)   VALUE +0   COMP SYNC.        
004900     SKIP2                                                                
005000                                                                          
005100*01  -COPY W0005       -PRE POSTSUM-.                                     
005200     EJECT                                                                
005300 01  IN-AREA-START               PIC X(24)   VALUE                        
005400                                 'IN-AREA-START  '.                       
005500*01  AREA   -COPY W25209      -PRE IN-                                    
005600     EJECT                                                                
005700 01  OUT-AREA-START              PIC X(24)   VALUE                        
005800                                 'OUT-AREA-START  '.                      
005900*01  AREA   -COPY W25209      -PRE OUT-                                   
006000     EJECT                                                                
006100******************************************************************        
006200*                                                                         
006300*        ARBETS-AREOR TILL DB2-SEKTIONERNA                                
006400******************************************************************        
006500 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
006600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
006700                                                                          
006800 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
006900 01  DB2-WS.                                                              
007000     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
007100         88  CURSOR-OK                      VALUE 000.                    
007200         88  LINES-FOUND                    VALUE 000.                    
007300         88  LINES-MISSING                  VALUE 100.                    
007400         88  RESOURCE-WRONG                 VALUE 904.                    
007500     03  GOOD-SQLCODECODES.                                               
007600         05  GOOD-SQLCODE OCCURS 5                                        
007700             INDEXED BY SQLCODE-IX PIC 9(3).                              
007800     EJECT                                                                
007900*    --------------- DB2 INPUT-OUTPUT AREA ---------------                
008000                                                                          
008100 01  FILLER                      PIC X(16)  VALUE 'TB1ACCE-AREA'.         
008200                                                                          
008300*01  -COPY TB1ACCE -PRE TB1ACCE-                                          
008400     EJECT                                                                
008500     EXEC SQL INCLUDE TB1ACCE END-EXEC.                                   
008600     EJECT                                                                
008700                                                                          
008800 LINKAGE SECTION.                                                         
008900                                                                          
009000 PROCEDURE DIVISION.                                                      
009100                                                                          
009200     PERFORM A-INIT                                                       
009300     PERFORM S01-LAS-W25212                                               
009400                                                                          
009500     PERFORM UNTIL END-OF-W25212                                          
009600       PERFORM C-INFO-FROM-TB1ACCE-TAB                                    
009700       PERFORM S11-WRITE-W25213                                           
009800       PERFORM S01-LAS-W25212                                             
009900     END-PERFORM                                                          
010000                                                                          
010100     PERFORM Z-FINIT                                                      
010200     MOVE ZERO TO RETURN-CODE                                             
010300     GOBACK                                                               
010400     .                                                                    
010500     EJECT                                                                
010600 A-INIT SECTION.                                                          
010700                                                                          
010800     OPEN INPUT  W25212                                                   
010900          OUTPUT W25213                                                   
011000                                                                          
011100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011110     .                                                                    
011300     EJECT                                                                
011400 C-INFO-FROM-TB1ACCE-TAB SECTION.                                         
011500                                                                          
011600     PERFORM DB2-SELECT-TB1ACCE-TAB                                       
011700                                                                          
011710     MOVE IN-AREA              TO OUT-AREA                                
011800     IF LINES-FOUND                                                       
011910       MOVE TB1ACCE-IDUPPDSU   TO OUT-IDUPPDSU                            
011920       MOVE TB1ACCE-IDUPPDKU   TO OUT-IDUPPDKU                            
012000       MOVE TB1ACCE-BEUPPDSU   TO OUT-BEUPPDSU                            
012100     ELSE                                                                 
012200       MOVE SPACES             TO OUT-IDUPPDSU                            
012210                                  OUT-IDUPPDKU                            
012300                                  OUT-BEUPPDSU                            
012400     END-IF                                                               
012410     .                                                                    
012600     EJECT                                                                
012700 S01-LAS-W25212 SECTION.                                                  
012800                                                                          
012900     READ W25212 INTO IN-AREA                                             
013000     AT END                                                               
013100        MOVE HIGH-VALUE TO IN-AREA                                        
013200        SET END-OF-W25212 TO TRUE                                         
013300                                                                          
013400     NOT AT END                                                           
013500        MOVE 'W25212'   TO POSTSUM-FDNAMN                                 
013600        MOVE 'W25212D1' TO POSTSUM-DDNAMN2                                
013700*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
013800        MOVE 'IN '      TO POSTSUM-TRANSTYP                               
013900        CALL POSTSUM USING POSTSUM-PARM                                   
014000     END-READ                                                             
014010     .                                                                    
014200     EJECT                                                                
014300 S11-WRITE-W25213 SECTION.                                                
014400                                                                          
014500     WRITE OUT-RECORD FROM OUT-AREA                                       
014600                                                                          
014700     MOVE 'IN'       TO POSTSUM-TRANSTYP                                  
014800     MOVE 'W25213'   TO POSTSUM-FDNAMN                                    
014900     MOVE 'W25213D2' TO POSTSUM-DDNAMN2                                   
015000     CALL POSTSUM USING POSTSUM-PARM                                      
015010     .                                                                    
015200     EJECT                                                                
015300 S99-ABEND SECTION.                                                       
015400     SKIP3                                                                
015500     MOVE RKOD-ABEND-UTAN-DUMP  TO RKOD                                   
015600     CALL ABEND USING RKOD                                                
015610     .                                                                    
015800     EJECT                                                                
015900 Z-FINIT   SECTION.                                                       
016000                                                                          
016100     CLOSE  W25212                                                        
016200            W25213                                                        
016300                                                                          
016400     MOVE 'S' TO POSTSUM-OPKOD                                            
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016510     .                                                                    
016700     EJECT                                                                
016800 DB2-SELECT-TB1ACCE-TAB  SECTION.                                         
016900                                                                          
017000     MOVE 000100  TO GOOD-SQLCODECODES                                    
017100     EXEC SQL                                                             
017200       SELECT                                                             
017310          IDUPPDSU                                                        
017320         ,IDUPPDKU                                                        
017400         ,BEUPPDSU                                                        
017500       INTO                                                               
017600          :TB1ACCE-IDUPPDSU                                               
017610         ,:TB1ACCE-IDUPPDKU                                               
017700         ,:TB1ACCE-BEUPPDSU                                               
017800                                                                          
017900        FROM   TB1ACCE                                                    
018000                                                                          
018100        WHERE   IDARTNR = :IN-IDARTNR                                     
018200        ORDER BY TIAOINF DESC                                             
018300        FETCH FIRST 1 ROW ONLY                                            
018400                                                                          
018500     END-EXEC                                                             
018600                                                                          
018700     MOVE SQLCODE TO SQLCODE-WS                                           
018800     PERFORM DB2-STATUS-CHECK                                             
018900     .                                                                    
019000     EJECT                                                                
019100 DB2-STATUS-CHECK  SECTION.                                               
019200                                                                          
019300     SET SQLCODE-IX TO 1                                                  
019400     SEARCH GOOD-SQLCODE                                                  
019500       AT END                                                             
019600         MOVE RKOD-ABEND-UTAN-DUMP TO RKOD                                
019700         PERFORM S99-ABEND                                                
019800       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
019900     END-SEARCH                                                           
020000     .                                                                    
020100     EJECT                                                                
