000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5171900.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   21/04/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION: CREATE FILE TO AZURE DATALAKE IN DISPLAY FORMAT            
000900*                                                                         
001000*    ABENDCODES:                                                          
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- INPUT FILE FROM W51716X1                                   
002100     SELECT W51716X1                   ASSIGN TO W51719D1.                
002200*          --- OUTPUT FILE TO AZURE DATALAKE                              
002300     SELECT W51716X                    ASSIGN TO W51719D2.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W51716X1                                                             
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W51716X1        -PRE  IN-   -L.                                
003400     SKIP3                                                                
003500 FD  W51716X                                                              
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  RECORD -COPY W51716X  -PRE  OUTX- -L.                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W5171900'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  IX1                         PIC S9(9)   VALUE +0 COMP SYNC.          
004700                                                                          
004800 77  W51716X1-EOF-SW             PIC X       VALUE 'N'.                   
004900     88  END-OF-W51716X1                     VALUE 'J'.                   
005000     EJECT                                                                
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005600     SKIP2                                                                
005700*    --- PARAMETERS TO ABEND                                              
005800                                                                          
005900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006200     SKIP2                                                                
006300 01  ERROR-TEXT.                                                          
006400     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                 'IN-AREA-START  '.                       
007300                                                                          
007400*01  AREA -COPY W51716X1    -PRE IN-                                      
007500     EJECT                                                                
007600 01  OUT1-AREA-START             PIC X(24)   VALUE                        
007700                                 'OUT1-AREA-START  '.                     
007800                                                                          
007900*01  AREA -COPY W51716X     -PRE OUTX-                                    
008000     EJECT                                                                
008100 PROCEDURE DIVISION.                                                      
008200 MAIN SECTION.                                                            
008300     SKIP2                                                                
008400                                                                          
008500     PERFORM A-INIT                                                       
008600     PERFORM S01-READ-W51716X1                                            
008700     PERFORM UNTIL END-OF-W51716X1                                        
008800       PERFORM S11-WRITE-W51716X                                          
008900                                                                          
009000       PERFORM S01-READ-W51716X1                                          
009100     END-PERFORM                                                          
009200                                                                          
009300     PERFORM Z-FINIT                                                      
009400                                                                          
009500     MOVE ZERO TO RETURN-CODE                                             
009600     GOBACK                                                               
009700     .                                                                    
009800     EJECT                                                                
009900 A-INIT SECTION.                                                          
010000                                                                          
010100     OPEN INPUT  W51716X1                                                 
010200                                                                          
010300          OUTPUT W51716X                                                  
010400                                                                          
010500     .                                                                    
010600     EJECT                                                                
010700 Z-FINIT SECTION.                                                         
010800     CLOSE W51716X1                                                       
010900           W51716X                                                        
011000     SKIP2                                                                
011100     MOVE 'S' TO POSTSUM-OPKOD                                            
011200     CALL POSTSUM USING POSTSUM-PARM                                      
011300     .                                                                    
011400     EJECT                                                                
011500 S01-READ-W51716X1 SECTION.                                               
011600                                                                          
011700     READ W51716X1    INTO IN-AREA                                        
011800     AT END                                                               
011900        MOVE HIGH-VALUE TO IN-AREA                                        
012000        SET END-OF-W51716X1 TO TRUE                                       
012100                                                                          
012200     NOT AT END                                                           
012300        MOVE 'W51716X1' TO POSTSUM-FDNAMN                                 
012400        MOVE 'W51719D1' TO POSTSUM-DDNAMN2                                
012500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
012600        CALL POSTSUM USING POSTSUM-PARM                                   
012700     END-READ                                                             
012800     .                                                                    
012900     EJECT                                                                
013000 S11-WRITE-W51716X SECTION.                                               
013100                                                                          
013300     MOVE IN-IDDC                TO OUTX-IDDC                             
013400     MOVE IN-KDPRODSL            TO OUTX-KDPRODSL                         
013500     MOVE IN-DAVVREG             TO OUTX-DAVVREG                          
013600     MOVE IN-SUAKSV              TO OUTX-SUAKSV                           
013700     MOVE IN-SULSV               TO OUTX-SULSV                            
013800     MOVE IN-SUEFRV              TO OUTX-SUEFRV                           
013900     MOVE IN-SURESSV             TO OUTX-SURESSV                          
014000     MOVE IN-SUOKSV              TO OUTX-SUOKSV                           
026200                                                                          
026300     PERFORM S11A-WRITE-W51716X                                           
026400     .                                                                    
026500     EJECT                                                                
026600 S11A-WRITE-W51716X SECTION.                                              
026700                                                                          
026800     WRITE OUTX-RECORD FROM OUTX-AREA                                     
026900                                                                          
027000     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
027100     MOVE 'W51716X'  TO POSTSUM-FDNAMN                                    
027200     MOVE 'W51719D2' TO POSTSUM-DDNAMN2                                   
027300     CALL POSTSUM USING POSTSUM-PARM                                      
027400     .                                                                    
027500     EJECT                                                                
027600 S99-ABEND SECTION.                                                       
027700                                                                          
027800     SKIP2                                                                
027900     MOVE 'S' TO POSTSUM-OPKOD                                            
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     CALL ABEND USING RKOD-ABEND                                          
028200     .                                                                    
