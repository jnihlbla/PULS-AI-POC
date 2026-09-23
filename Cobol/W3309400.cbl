000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3309400.                                                 
000400 AUTHOR.        KARIN                                                     
000500     DATE-WRITTEN.  MARS 1990.                                            
000600     REMARKS.                                                             
000700*              PLOCKAR UR EN KONKATINERAD STATISTIK-URVALS-LISTA          
000800*              UT URVAL GJORDA AV EN ANVÄNDARE.                           
000900*                                                                         
001000     SKIP2                                                                
001100 ENVIRONMENT DIVISION.                                                    
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400     SELECT INFIL1                       ASSIGN TO W33094D1.              
001500     SKIP2                                                                
001600     SELECT INFIL2                       ASSIGN TO W33094D2.              
001700     SKIP2                                                                
001800     SELECT UTFIL                        ASSIGN TO W33094D3.              
001900     SKIP2                                                                
002000 DATA DIVISION.                                                           
002100 FILE SECTION.                                                            
002200     SKIP3                                                                
002300 FD  INFIL1                                                               
002400     LABEL RECORD   STANDARD                                              
002500     RECORDING      F                                                     
002600     BLOCK CONTAINS 0.                                                    
002700 01  FILLER         PIC X(133).                                           
002800     SKIP3                                                                
002900 FD  INFIL2                                                               
003000     LABEL RECORD   STANDARD                                              
003100     RECORDING      F                                                     
003200     BLOCK CONTAINS 0.                                                    
003300 01  FILLER         PIC X(122).                                           
003400     SKIP3                                                                
003500 FD  UTFIL                                                                
003600     LABEL RECORD   STANDARD                                              
003700     RECORDING      V                                                     
003800     BLOCK CONTAINS 0.                                                    
003900 01  UTPOST         PIC X(133).                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200*                                                                         
004300 01  PROGRAM-NAMN                PIC X(6)   VALUE 'W33094'.               
004400*                                                                         
004500     SKIP2                                                                
004600 01  GENERELLA-KONSTANTER.                                                
004700     03  JA                      PIC X(1)    VALUE 'J'.                   
004800     03  NEJ                     PIC X(1)    VALUE 'N'.                   
004900     SKIP2                                                                
005000 01  END-OF-FILE-SWITCHAR.                                                
005100     03  INFIL1-EOF             PIC X(1)    VALUE 'N'.                    
005200     03  INFIL2-EOF             PIC X(1)    VALUE 'N'.                    
005300     SKIP2                                                                
005400 01  INDATA1.                                                             
005500     03  FILLER                 PIC X(80).                                
005600     03  USERIDSTR1             PIC X(6).                                 
005700     03  FILLER                 PIC X(2).                                 
005800     03  USERID1                PIC X(7).                                 
005900     03  FILLER                 PIC X(38).                                
006000     SKIP2                                                                
006100 01  INDATA2.                                                             
006200     03  FILLER                 PIC X(80).                                
006300     03  USERIDSTR2             PIC X(6).                                 
006400     03  FILLER                 PIC X(2).                                 
006500     03  USERID2                PIC X(7).                                 
006600     03  FILLER                 PIC X(38).                                
006700     SKIP2                                                                
006800 01  UTDATA.                                                              
006900     03  FILLER                 PIC X(133).                               
007000     SKIP2                                                                
007100 01  OLD-USER                   PIC X(7).                                 
007200     SKIP2                                                                
007300     EJECT                                                                
007400 LINKAGE SECTION.                                                         
007500 01  PARM.                                                                
007600     03  LENGD                  PIC S9(4) COMP SYNC.                      
007700     03  USER                   PIC X(7).                                 
007800     SKIP3                                                                
007900 PROCEDURE DIVISION USING PARM.                                           
008000     SKIP2                                                                
008100     MOVE '0' TO OLD-USER                                                 
008200     PERFORM A-INIT                                                       
008300     PERFORM B-LAES-INFIL1                                                
008400     PERFORM UNTIL INFIL1-EOF = JA                                        
008500       IF USERIDSTR1 = 'USERID'                                           
008600         MOVE USERID1 TO OLD-USER                                         
008700       END-IF                                                             
008800       IF OLD-USER = USER                                                 
008900         MOVE INDATA1 TO UTDATA                                           
009000         PERFORM D-SKRIV-UTFIL                                            
009100       END-IF                                                             
009200       PERFORM B-LAES-INFIL1                                              
009300     END-PERFORM                                                          
009400*                                                                         
009500     MOVE '0' TO OLD-USER                                                 
009600     PERFORM C-LAES-INFIL2                                                
009700     PERFORM UNTIL INFIL2-EOF = JA                                        
009800       IF USERIDSTR2 = 'USERID'                                           
009900         MOVE USERID2 TO OLD-USER                                         
010000       END-IF                                                             
010100       IF OLD-USER = USER                                                 
010200         MOVE INDATA2 TO UTDATA                                           
010300         PERFORM D-SKRIV-UTFIL                                            
010400       END-IF                                                             
010500       PERFORM C-LAES-INFIL2                                              
010600     END-PERFORM                                                          
010700     PERFORM Z-FINIT                                                      
010800     GOBACK.                                                              
010900     EJECT                                                                
011000 A-INIT   SECTION.                                                        
011100     SKIP2                                                                
011200     OPEN INPUT INFIL1                                                    
011300                INFIL2                                                    
011400     OPEN OUTPUT UTFIL                                                    
011500     .                                                                    
011600     EJECT                                                                
011700 B-LAES-INFIL1  SECTION.                                                  
011800     SKIP2                                                                
011900     READ INFIL1 INTO INDATA1                                             
012000         AT END MOVE JA TO INFIL1-EOF                                     
012100     END-READ                                                             
012200     .                                                                    
012300     EJECT                                                                
012400 C-LAES-INFIL2  SECTION.                                                  
012500     SKIP2                                                                
012600     READ INFIL2 INTO INDATA2                                             
012700         AT END MOVE JA TO INFIL2-EOF                                     
012800     END-READ                                                             
012900     .                                                                    
013000     EJECT                                                                
013100 D-SKRIV-UTFIL  SECTION.                                                  
013200     SKIP2                                                                
013300     WRITE UTPOST FROM UTDATA                                             
013400     .                                                                    
013500     EJECT                                                                
013600 Z-FINIT  SECTION.                                                        
013700     SKIP2                                                                
013800     CLOSE INFIL1                                                         
013900           INFIL2                                                         
014000           UTFIL                                                          
014100     .                                                                    
