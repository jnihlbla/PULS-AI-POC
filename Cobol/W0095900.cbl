000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W0091000.                                                 
000400 AUTHOR.        KJELL                                                     
000500     DATE-WRITTEN.  JUL 1989.                                             
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        GENERERA EN FIL UTGÅENDE FRÅN EN MALL OCH                        
001000*        EN FIL MED POSTER SOM INNEHÅLLER ETT NAMN I POS 1-8              
001100*        MALLEN INNEHÅLLER "&NAME" I DEN POSITION DÄR                     
001200*        RESPEKTIVE NAMN SKA STOPPAS IN.                                  
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*    --- INFILER:                                                         
002100*           --- FIL MED NAMN:                                             
002200     SELECT NAMNFIL                      ASSIGN TO W00959D1.              
002300*           --- GENERERAD FIL   :                                         
002400     SELECT UTFIL                        ASSIGN TO W00959D2.              
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  NAMNFIL                                                              
003100     LABEL RECORD   STANDARD                                              
003200     RECORDING      F                                                     
003300     BLOCK CONTAINS 0.                                                    
003400     SKIP2                                                                
003500 01  NAMNPOST.                                                            
003600     03  NAMN       PIC X(10).                                            
003700     03  FILLER     PIC X(70).                                            
003800     SKIP3                                                                
003900     SKIP3                                                                
004000 FD  UTFIL                                                                
004100     LABEL RECORD   STANDARD                                              
004200     RECORDING      F                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP2                                                                
004500 01  UTPOST         PIC X(80).                                            
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004801                                                                          
004810*    -- CHECKED BY WY2000                                                 
004900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W0095900'.            
005000*    --- FLAGGOR                                                          
005100 77  NAMNFIL-EOF-SW              PIC X(1)    VALUE 'N'.                   
005200     88  END-OF-NAMNFIL                      VALUE 'J'.                   
005300     SKIP3                                                                
005400 77  DEL-1                       PIC X(70).                               
005500 77  DEL-2                       PIC X(70).                               
005600 77  DEL-3                       PIC X(70).                               
005610     SKIP3                                                                
005620 77  DLM-1                       PIC X(6).                                
005630 77  DLM-2                       PIC X(6).                                
005640 77  DLM-3                       PIC X(6).                                
005700     SKIP3                                                                
005800 77  P                           PIC S9(4)   COMP.                        
005900 77  LAENGD-1                    PIC S9(4)   COMP.                        
006000 77  LAENGD-2                    PIC S9(4)   COMP.                        
006100 77  LAENGD-3                    PIC S9(4)   COMP.                        
006200     EJECT                                                                
006300 LINKAGE SECTION.                                                         
006400     SKIP2                                                                
006500 01  EXEC-PARM.                                                           
006600     03  PARM-LAENGD             PIC S9(4)   COMP.                        
006700     03  PARM-VAERDE             PIC X(80).                               
006800     EJECT                                                                
006900 PROCEDURE DIVISION USING EXEC-PARM.                                      
007000     SKIP2                                                                
007100     MOVE 1 TO P                                                          
007200     UNSTRING PARM-VAERDE                                                 
007300              DELIMITED BY '&NAME.' OR '¤¤'                               
007400              INTO DEL-1 DELIMITER IN DLM-1 COUNT IN LAENGD-1             
007500                   DEL-2 DELIMITER IN DLM-2 COUNT IN LAENGD-2             
007600                   DEL-3 DELIMITER IN DLM-3 COUNT IN LAENGD-3             
007700     IF DLM-1 = '¤¤'                                                      
007800       MOVE SPACE TO DEL-2 DEL-3                                          
007900       MOVE ZERO  TO LAENGD-2 LAENGD-3                                    
008000     END-IF                                                               
008100     IF DLM-2 = '¤¤'                                                      
008200       MOVE SPACE TO DEL-3                                                
008300       MOVE ZERO  TO LAENGD-3                                             
008400     END-IF                                                               
008500     DISPLAY DEL-1  LAENGD-1                                              
008600     DISPLAY DEL-2 LAENGD-2                                               
008700     DISPLAY DEL-3 LAENGD-3                                               
008800                                                                          
008900     OPEN INPUT NAMNFIL OUTPUT UTFIL                                      
009000                                                                          
009100     READ NAMNFIL                                                         
009200     AT END SET END-OF-NAMNFIL TO TRUE END-READ                           
009300                                                                          
009400     PERFORM UNTIL END-OF-NAMNFIL                                         
009500         MOVE SPACE TO UTPOST                                             
009600         STRING DEL-1 DELIMITED BY '   '                                  
009700                INTO UTPOST                                               
009800         IF LAENGD-2 > 0                                                  
009900           COMPUTE P = LAENGD-1 + 1                                       
010000           STRING NAMN  DELIMITED BY SPACE                                
010100                  INTO UTPOST                                             
010200                  WITH POINTER P                                          
010300           STRING DEL-2 DELIMITED BY '   '                                
010400                  INTO UTPOST                                             
010500                  WITH POINTER P                                          
010600         END-IF                                                           
010700         IF LAENGD-3 > 0                                                  
010800           STRING NAMN  DELIMITED BY SPACE                                
010900                  INTO UTPOST                                             
011000                  WITH POINTER P                                          
011100           STRING DEL-3 DELIMITED BY '   '                                
011200                  INTO UTPOST                                             
011300                  WITH POINTER P                                          
011400         END-IF                                                           
011500         WRITE UTPOST                                                     
011600         READ NAMNFIL                                                     
011700         AT END SET END-OF-NAMNFIL TO TRUE END-READ                       
011800     END-PERFORM                                                          
011900                                                                          
012000     CLOSE NAMNFIL UTFIL                                                  
012100     MOVE ZERO TO RETURN-CODE                                             
012200     GOBACK                                                               
012300     .                                                                    
