000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0152200.                                                
000400 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   94/03/17.                                                
000510 DATE-COMPILED.                                                           
000520                                                                          
000530*    FUNKTION:                                                            
000540*        TAR IN EN FIL MED STATISTIK OM SEKUNDÄRA INDEX                   
000550*        OCH SKAPAR EN PRINTFIL MED INTRESSANT INFORMATION                
000560*                                                                         
000570*                                                                         
000580                                                                          
000590     EJECT                                                                
000600 ENVIRONMENT DIVISION.                                                    
000700                                                                          
000800 CONFIGURATION SECTION.                                                   
000900                                                                          
001000 INPUT-OUTPUT SECTION.                                                    
001100                                                                          
001200 FILE-CONTROL.                                                            
001300                                                                          
001400*              STATISTIK-DATA FRÅN VSAM-INDEX                             
001500     SELECT STATFIL                    ASSIGN TO W01522D1.                
001600                                                                          
001700*              LIST-DATA                                                  
001800     SELECT LISTFIL                    ASSIGN TO W01522D2.                
001900                                                                          
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200                                                                          
002300 FILE SECTION.                                                            
002400                                                                          
002500 FD  STATFIL                                                              
002600     RECORDING       V                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900 01  FILLER                      PIC X(133).                              
003000                                                                          
003100                                                                          
003200 FD  LISTFIL                                                              
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600 01  LIST-POST                   PIC X(80).                               
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W0152200'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  CLUSTER                     PIC X(10) VALUE 'CLUSTER --'.            
004400 77  CL-INDEX                    PIC X(10) VALUE '   INDEX -'.            
004500 77  REC-TOTAL                   PIC X(14) VALUE 'REC-TOTAL-----'.        
004600 77  REC-DELETED                 PIC X(14) VALUE 'REC-DELETED---'.        
004700 77  REC-INSERTED                PIC X(14) VALUE 'REC-INSERTED--'.        
004800 77  REC-UPDATED                 PIC X(14) VALUE 'REC-UPDATED---'.        
004900 77  REC-RETRIEVED               PIC X(14) VALUE 'REC-RETRIEVED-'.        
005000 77  SPACE-TYPE                  PIC X(14) VALUE 'SPACE-TYPE----'.        
005100 77  SPACE-PRI                   PIC X(14) VALUE 'SPACE-PRI-----'.        
005200 77  SPACE-SEC                   PIC X(14) VALUE 'SPACE-SEC-----'.        
005300                                                                          
005400 77  STATFIL-EOF-SW              PIC X       VALUE 'N'.                   
005500     88  END-OF-STATFIL                      VALUE 'J'.                   
005600                                                                          
005700     EJECT                                                                
005800 01  STAT-AREA-START             PIC X(24)   VALUE                        
005900                                 'STAT-AREA-START'.                       
006000                                                                          
006100 01  STAT-AREA.                                                           
006200     03 FILLER                   PIC X.                                   
006300     03 STAT-CLUSTER             PIC X(10).                               
006400     03 FILLER                   PIC X(6).                                
006500     03 STAT-CLUSTER-NAME        PIC X(40).                               
006600     03 FILLER                   PIC X(76).                               
006700                                                                          
006800                                                                          
006900 01  STAT-AREA2 REDEFINES STAT-AREA.                                      
007000     03 FILLER                   PIC X(8).                                
007100     03 STAT-KOL1                PIC X(14).                               
007200     03 STAT-VAR1                PIC X(10).                               
007300     03 FILLER                   PIC X(5).                                
007400     03 STAT-KOL2                PIC X(14).                               
007500     03 STAT-VAR2                PIC X(10).                               
007600     03 FILLER                   PIC X(5).                                
007700     03 STAT-KOL3                PIC X(14).                               
007800     03 STAT-VAR3                PIC X(10).                               
007900     03 FILLER                   PIC X(5).                                
008000     03 STAT-KOL4                PIC X(14).                               
008100     03 STAT-VAR4                PIC X(10).                               
008200                                                                          
008300     EJECT                                                                
008400 01  LIST-AREA-START             PIC X(24)   VALUE                        
008500                                 'LIST-AREA-START  '.                     
008600                                                                          
008700 01  LIST-BLANK.                                                          
008800     03 FILLER                   PIC X(8)    VALUE SPACE.                 
008900                                                                          
009000 01  LIST-AREA.                                                           
009100     03 FILLER                   PIC X.                                   
009200     03 LIST-CLUSTER             PIC X(10).                               
009300     03 FILLER                   PIC X(6).                                
009400     03 LIST-CLUSTER-NAME        PIC X(40).                               
009500                                                                          
009600                                                                          
009700 01  LIST-AREA2.                                                          
009800     03 FILLER                   PIC X(1).                                
009900     03 LIST-KOL1                PIC X(14).                               
010000     03 LIST-VAR1                PIC X(10).                               
010100     03 FILLER                   PIC X(3).                                
010200     03 LIST-KOL2                PIC X(14).                               
010300     03 LIST-VAR2                PIC X(10).                               
010400     03 FILLER                   PIC X(3).                                
010500     03 LIST-KOL3                PIC X(14).                               
010600     03 LIST-VAR3                PIC X(10).                               
010700                                                                          
010800     EJECT                                                                
010900 PROCEDURE DIVISION.                                                      
011000                                                                          
011100 STYR SECTION.                                                            
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011500     PERFORM S01-LAES-STATFIL                                             
011600     PERFORM UNTIL END-OF-STATFIL                                         
011700       EVALUATE TRUE                                                      
011800                                                                          
011900         WHEN STAT-CLUSTER = CLUSTER                                      
012000           WRITE LIST-POST FROM LIST-BLANK                                
012100           WRITE LIST-POST FROM LIST-BLANK                                
012200           MOVE STAT-AREA TO LIST-AREA                                    
012300           WRITE LIST-POST FROM LIST-AREA                                 
012400                                                                          
012500         WHEN STAT-CLUSTER = CL-INDEX                                     
012600           WRITE LIST-POST FROM LIST-BLANK                                
012700           MOVE STAT-AREA TO LIST-AREA                                    
012800           WRITE LIST-POST FROM LIST-AREA                                 
012900                                                                          
013000         WHEN STAT-KOL1 = REC-TOTAL                                       
013100           PERFORM B-WRITE                                                
013200                                                                          
013300         WHEN STAT-KOL1 = REC-DELETED                                     
013400           PERFORM B-WRITE                                                
013500                                                                          
013600         WHEN STAT-KOL1 = REC-INSERTED                                    
013700           PERFORM B-WRITE                                                
013800                                                                          
013900         WHEN STAT-KOL1 = REC-UPDATED                                     
014000           PERFORM B-WRITE                                                
014100                                                                          
014200         WHEN STAT-KOL1 = REC-RETRIEVED                                   
014300           PERFORM B-WRITE                                                
014400                                                                          
014500         WHEN STAT-KOL1 = SPACE-TYPE                                      
014600           PERFORM B-WRITE                                                
014700                                                                          
014800         WHEN STAT-KOL1 = SPACE-PRI                                       
014900           PERFORM B-WRITE                                                
015000                                                                          
015100         WHEN STAT-KOL1 = SPACE-SEC                                       
015200           PERFORM B-WRITE                                                
015300                                                                          
015400         WHEN OTHER                                                       
015500           CONTINUE                                                       
015600       END-EVALUATE                                                       
015700                                                                          
015800       PERFORM S01-LAES-STATFIL                                           
015900     END-PERFORM                                                          
016000                                                                          
016100     PERFORM Z-FINIT                                                      
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT  SECTION.                                                         
016800                                                                          
016900     OPEN INPUT  STATFIL                                                  
017000     OPEN OUTPUT LISTFIL                                                  
017100                                                                          
017200     MOVE SPACE TO LIST-AREA                                              
017300                   LIST-AREA2                                             
017400     .                                                                    
017500     EJECT                                                                
017600 B-WRITE  SECTION.                                                        
017700                                                                          
017800     MOVE STAT-KOL1 TO LIST-KOL1                                          
017900     MOVE STAT-VAR1 TO LIST-VAR1                                          
018000     MOVE STAT-KOL2 TO LIST-KOL2                                          
018100     MOVE STAT-VAR2 TO LIST-VAR2                                          
018200     MOVE STAT-KOL3 TO LIST-KOL3                                          
018300     MOVE STAT-VAR3 TO LIST-VAR3                                          
018400     WRITE LIST-POST FROM LIST-AREA2                                      
018500     .                                                                    
018600     EJECT                                                                
018700 S01-LAES-STATFIL SECTION.                                                
018800                                                                          
018900     READ STATFIL INTO STAT-AREA                                          
019000       AT END                                                             
019100          SET END-OF-STATFIL TO TRUE                                      
019200       END-READ                                                           
019300     .                                                                    
019400     SKIP3                                                                
019500 Z-FINIT SECTION.                                                         
019600                                                                          
019700     CLOSE STATFIL LISTFIL                                                
019800     .                                                                    
