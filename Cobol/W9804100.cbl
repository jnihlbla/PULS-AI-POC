000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9804100.                                                
000400 AUTHOR.         KARIN OLSSON.                                            
000500 DATE-WRITTEN.   90/11/29.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR EN FIL MED ALLA RUTINER SOM FINNS DEFINIERADE I           
001100*        DATAMANGER. FILEN ANVÄNDES AV VOLVO DATA FÖR DERAS               
001200*        ABENDSTATISTIK.                                                  
001300*                                                                         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FIL FRÅN DATAMANGER MED ALLA RUTINER                       
002800     SELECT DMRFIL                     ASSIGN TO W98041D1.                
002900     SKIP2                                                                
003000*          --- FIL TILL VOLVO DATA MED ALLA W-RUTINER                     
003100     SELECT W98041                     ASSIGN TO W98041D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  DMRFIL                                                               
003800     LABEL RECORD    STANDARD                                             
003900     RECORDING       V                                                    
004000     BLOCK CONTAINS  0.                                                   
004100     SKIP2                                                                
004200*01  -COPY W98041DM        -L.                                            
004400                                                                          
004500     SKIP3                                                                
004600 FD  W98041                                                               
004700     LABEL RECORD    STANDARD                                             
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000     SKIP2                                                                
005100*01  POST -COPY W98041     -PRE  UT-  -L.                                 
005300                                                                          
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005601                                                                          
005610*    -- CHECKED BY WY2000                                                 
005700 77  IDPGM                       PIC X(8)    VALUE 'W9804100'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
005910 77  SPARAT-RUTIN-NAMN           PIC X(6)    VALUE SPACE.                 
006000                                                                          
006100 77  DMRFIL-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-DMRFIL                       VALUE 'J'.                   
006300     EJECT                                                                
006400 01  ANSV-AVD                    PIC X(5)    VALUE '57050'.               
006500     EJECT                                                                
006600 01  IN-AREA-START               PIC X(24)   VALUE                        
006700                                 'IN-AREA-START  '.                       
006800     SKIP2                                                                
006900                                                                          
007000*01  AREA -COPY W98041DM       -PRE IN-                                   
007200     EJECT                                                                
007300 01  UT-AREA-START               PIC X(24)   VALUE                        
007400                                 'UT-AREA-START  '.                       
007500     SKIP2                                                                
007600                                                                          
007700*01  AREA -COPY W98041         -PRE UT-                                   
007900     EJECT                                                                
008000 PROCEDURE DIVISION.                                                      
008100     SKIP2                                                                
008200                                                                          
008300     PERFORM A-INIT                                                       
008400     PERFORM S01-LAES-DMRFIL                                              
008500     PERFORM UNTIL END-OF-DMRFIL                                          
008600       IF IN-TKN-KOL-37-43 = 'ROUTINE'                                    
008610         IF IN-RUTIN-NAMN NOT = SPARAT-RUTIN-NAMN                         
008620           MOVE IN-RUTIN-NAMN TO SPARAT-RUTIN-NAMN                        
008700           PERFORM B-SKAPA-UT-POST                                        
008800           PERFORM S11-SKRIV-W98041                                       
008810         END-IF                                                           
008900       END-IF                                                             
009000       PERFORM S01-LAES-DMRFIL                                            
009100     END-PERFORM                                                          
009200                                                                          
009300                                                                          
009400     PERFORM Z-FINIT                                                      
009500                                                                          
009600     MOVE ZERO TO RETURN-CODE                                             
009700     GOBACK                                                               
009800     .                                                                    
009900     EJECT                                                                
010000 A-INIT SECTION.                                                          
010100                                                                          
010200     OPEN INPUT  DMRFIL                                                   
010300                                                                          
010400     OPEN OUTPUT W98041                                                   
010500     .                                                                    
010600     EJECT                                                                
010700 B-SKAPA-UT-POST   SECTION.                                               
010800                                                                          
010900     MOVE IN-RUTIN-NAMN TO UT-RUTIN-NAMN                                  
011000     MOVE ANSV-AVD      TO UT-IDAVD                                       
011100     .                                                                    
011200     EJECT                                                                
011300 Z-FINIT SECTION.                                                         
011400     CLOSE DMRFIL                                                         
011500           W98041                                                         
011600     .                                                                    
011700     EJECT                                                                
011800 S01-LAES-DMRFIL  SECTION.                                                
011900     SKIP2                                                                
012000     READ DMRFIL INTO IN-AREA                                             
012100     AT END                                                               
012200        SET END-OF-DMRFIL TO TRUE                                         
012300     END-READ                                                             
012400     .                                                                    
012500     EJECT                                                                
012600 S11-SKRIV-W98041 SECTION.                                                
012700     SKIP2                                                                
012800     WRITE UT-POST FROM UT-AREA                                           
012900     .                                                                    
013000     EJECT                                                                
