000100*COMPOPT VRTEREUS=YES                                                     
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.   W010WDL8.                                                  
000400 AUTHOR.         P-A FORSBERG.                                            
000500 DATE-WRITTEN.   06-03-07.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*   RENSNING AV WDL8                                                      
000900*                                                                         
001000 DATA DIVISION.                                                           
001100                                                                          
001200 WORKING-STORAGE SECTION.                                                 
001300                                                                          
001400 77 IDPGM     PIC X(8) VALUE 'W010WDL8'.                                  
001500 77 IX        PIC S9(9) VALUE ZERO COMP SYNC.                             
001600 01 SW-FIRST          PIC X VALUE 'J'.                                    
001700 01 W-DATE.                                                               
001800    03 W-YEAR         PIC XX.                                             
001900    03 FILLER         PIC X(4).                                           
002000 01 W-DATE4.                                                              
002100    03 W-DATEC.                                                           
002200    05 W-CENTURY      PIC XX.                                             
002300    05 W-YEAR4        PIC XX.                                             
002400    03 W-DATEZ    REDEFINES W-DATEC PIC 9999.                             
002500                                                                          
002600 LINKAGE SECTION.                                                         
002700 01  SEGNAMN          PIC X(8).                                           
003100 01 OLD.                                                                  
003200    03 -COPY  WDL811                                                      
003300    03 -COPY    WDL801 -RED AAR-WDL811                                    
003400     EJECT                                                                
003500 PROCEDURE DIVISION USING SEGNAMN OLD.                                    
003600                                                                          
003700     MOVE ZERO TO RETURN-CODE                                             
003800     IF SW-FIRST = 'J'                                                    
003900       ACCEPT W-DATE FROM DATE                                            
004000       MOVE '20' TO W-CENTURY                                             
004100       IF W-YEAR > '90'                                                   
004200         MOVE '19' TO W-CENTURY                                           
004300       END-IF                                                             
004400       MOVE W-YEAR TO W-YEAR4                                             
004500       MOVE 'N' TO SW-FIRST                                               
004600       SUBTRACT 6 FROM W-DATEZ                                            
004700     END-IF                                                               
004800     IF SEGNAMN =  'WDL811'                                               
004900       IF  AAR-TIAAAA IN OLD = W-DATEZ OR                                 
005000           AAR-TIAAAA IN OLD < W-DATEZ                                    
005200         MOVE +8 TO RETURN-CODE                                           
005300       END-IF                                                             
005400     END-IF                                                               
005500     GOBACK                                                               
005600      .                                                                   
