000010 PROCESS DYNAM                                                            
000020* --NEEDED FOR LINKING BATCH-DB2 PROGRAM                                  
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3719C00.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   16/11/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800     FUNCTION:                                                            
000900         THIS PROGRAM READS ALL ROWS FROM THE DB2 TABLE BYLACK            
001000                                                                          
001100                                                                          
001200     ABENDCODES:                                                          
001300         U0016 -  . . . .                                                 
001400         U1000 -  . . . .                                                 
001500                                                                          
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- ALL ROWS FROM BYLACK                                       
002500     SELECT W3719C                     ASSIGN TO W3719CD1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W3719C                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  POST -COPY W3719C -PRE  UT-  -L.                                     
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W3719C00'.            
004000 77  YES                         PIC X       VALUE 'J'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200     EJECT                                                                
004300                                                                          
004400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES TODAYS-DATE.                                        
004600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004800     03  TODAYS-DATE-DAY         PIC 9(2).                                
004900     EJECT                                                                
005000 01  GENERAL-SUBPROGRAMS.                                                 
005100                                                                          
005200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005310     03  FELLOG                  PIC X(8)    VALUE 'FELLOG '.             
005400     SKIP2                                                                
005500*    --- PARAMETERS TO ABEND                                              
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006000     SKIP2                                                                
006100 01  ERROR-TEXT.                                                          
006200     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006400     EJECT                                                                
006500                                                                          
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700                                                                          
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000 01  UT-AREA-START               PIC X(24)   VALUE                        
007100                                 'UT-AREA-START  '.                       
007200     SKIP2                                                                
007300                                                                          
007400*01  AREA -COPY W3719C     -PRE UT-                                       
007500     EJECT                                                                
007600                                                                          
007700*        WORK-AREAS FOR DB2-SECTIONS                                      
007800                                                                          
007900 01  FILLER                       PIC X(16)   VALUE 'DB2-WS     '.        
008000*01  -COPY BYLACK     -PRE BYLACK-                                        
008100     EJECT                                                                
008200                                                                          
008300 01  FILLER                       PIC X(16)   VALUE 'BYLACK-AREA'.        
008400       EXEC SQL INCLUDE BYLACK END-EXEC.                                  
008500                                                                          
008600 01  FILLER                       PIC X(16)   VALUE 'SQLCA-AREA'.         
008700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
008800*                        **** STATUS-CODE FROM DB2                        
008900                                                                          
009000 01  FILLER                       PIC X(16)   VALUE 'SQLCODE-WS'.         
009100 01  DB2-WS.                                                              
009200   03  SQLCODE-WS                 PIC S9(3)   VALUE ZERO.                 
009300     88  ROW-FOUND                            VALUE +000.                 
009400     88  ROW-MISSING                          VALUE +100.                 
009500   03  GOOD-SQLCODES.                                                     
009600     05  GOOD-SQLCODE OCCURS 5                                            
009700         INDEXED BY SQLCODE-IX    PIC 999.                                
009800     EJECT                                                                
009900 PROCEDURE DIVISION.                                                      
010000 MAIN SECTION.                                                            
010100     SKIP2                                                                
010200                                                                          
010300     PERFORM A-INIT                                                       
010400                                                                          
010500     PERFORM B-EXECUTE                                                    
010600                                                                          
010700     PERFORM Z-FINIT                                                      
010800                                                                          
010900     MOVE ZERO TO RETURN-CODE                                             
011000     GOBACK                                                               
011100     .                                                                    
011200     EJECT                                                                
011300 A-INIT SECTION.                                                          
011400                                                                          
011500     OPEN OUTPUT W3719C                                                   
011600     SKIP2                                                                
011700     ACCEPT TODAYS-DATE  FROM DATE                                        
011800                                                                          
011900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012000     .                                                                    
012100     EJECT                                                                
012200 B-EXECUTE SECTION.                                                       
012300     PERFORM DB2-OPEN-CRS-BYLACK                                          
012400                                                                          
012500     PERFORM DB2-FETCH-CRS-BYLACK                                         
012600     PERFORM UNTIL ROW-MISSING                                            
012700       MOVE BYLACK-DAAAVV        TO UT-DAAAVV                             
012800       MOVE BYLACK-IDARTNR       TO UT-IDARTNR                            
012900       MOVE BYLACK-IDPTYP        TO UT-IDPTYP                             
013000       MOVE BYLACK-KVANTAL       TO UT-KVANTAL                            
013100       PERFORM S11-WRITE-W3719C                                           
013200                                                                          
013300       PERFORM DB2-FETCH-CRS-BYLACK                                       
013400     END-PERFORM                                                          
013500                                                                          
013600     PERFORM DB2-CLOSE-CRS-BYLACK                                         
013700     .                                                                    
013800     EJECT                                                                
013900                                                                          
014000 Z-FINIT SECTION.                                                         
014100     CLOSE W3719C                                                         
014200     SKIP2                                                                
014300     MOVE 'S' TO POSTSUM-OPKOD                                            
014400     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014600     EJECT                                                                
014700 S11-WRITE-W3719C SECTION.                                                
014800                                                                          
014900     WRITE UT-POST FROM UT-AREA                                           
015000                                                                          
015100     MOVE 'W3719C' TO POSTSUM-FDNAMN                                      
015200     MOVE 'W3719CD1' TO POSTSUM-DDNAMN2                                   
015300     CALL POSTSUM USING POSTSUM-PARM                                      
015400     .                                                                    
015500     EJECT                                                                
015600* --- DB2 SECTIONS  ---                                                   
015700                                                                          
015800 DB2-OPEN-CRS-BYLACK SECTION.                                             
015900                                                                          
016000     EXEC SQL DECLARE BYLACK-CRS CURSOR FOR                               
016100     SELECT   IDARTNR,                                                    
016200              DAAAVV,                                                     
016300              IDPTYP,                                                     
016400              KVANTAL                                                     
016500                                                                          
016600     FROM     BYLACK                                                      
016700                                                                          
017100     ORDER BY IDARTNR,                                                    
017200              DAAAVV,                                                     
017300              IDPTYP                                                      
017400                                                                          
017500     FOR FETCH ONLY                                                       
017600     END-EXEC                                                             
017700                                                                          
017800     MOVE 000            TO GOOD-SQLCODES                                 
017900     EXEC SQL OPEN BYLACK-CRS END-EXEC                                    
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018100     PERFORM DB2-STATUS-CHECK                                             
018200     .                                                                    
018300     EJECT                                                                
018400                                                                          
018500 DB2-FETCH-CRS-BYLACK SECTION.                                            
018600                                                                          
018700     EXEC SQL FETCH BYLACK-CRS INTO                                       
018800            :BYLACK-IDARTNR,                                              
018900            :BYLACK-DAAAVV,                                               
019000            :BYLACK-IDPTYP,                                               
019100            :BYLACK-KVANTAL                                               
019200     END-EXEC                                                             
019300                                                                          
019400     MOVE 000100         TO GOOD-SQLCODES                                 
019500     MOVE SQLCODE        TO SQLCODE-WS                                    
019600     PERFORM DB2-STATUS-CHECK                                             
019700     .                                                                    
019800     EJECT                                                                
019900                                                                          
020000 DB2-CLOSE-CRS-BYLACK SECTION.                                            
020100                                                                          
020200     EXEC SQL CLOSE BYLACK-CRS                                            
020300     END-EXEC                                                             
020400     .                                                                    
020500     EJECT                                                                
020600                                                                          
020700 DB2-STATUS-CHECK SECTION.                                                
020800     SET SQLCODE-IX         TO 1                                          
020900     SEARCH GOOD-SQLCODE AT END CALL FELLOG                               
021000        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
021100           CONTINUE                                                       
021200     END-SEARCH                                                           
021300     .                                                                    
