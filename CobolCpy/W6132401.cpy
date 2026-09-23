000100 01  2401-W6132401.                                                       
000200*                                 FOR SELECTION OF PARTS WHICH            
000300*                                 ARE NOT IN APPROVED STATUS              
000400     03 2401-IDARTNR         PIC S9(9)           COMP-3.                  
000500*                                 PART NUMBER                             
000600     03 2401-KVBEST          PIC S9(7)           COMP-3.                  
000700*                                 ORDERD QUANTITY                         
000800     03 2401-TIORDTIME       PIC 9(10).                                   
000900*                                 ORDER DATE AND TIME                     
001000*                                 (YYMMDDHHMM[SS])                        
001100     03 2401-KDSTAPF         PIC X.                                       
001200*                                 STATUS FOR FILLING                      
001300     03 2401-ADART-FOM.                                                   
001400*                                 PARTS-ADRESS WHERE TO MOVE FROM         
001500        05 2401-ADLAGOMR-FOM PIC S9(3)           COMP-3.                  
001600*                                 AREA ADDRESS FROM                       
001700        05 2401-ADGANG-FOM   PIC S9(3)           COMP-3.                  
001800*                                 AISLE ADDRESS FROM                      
001900        05 2401-ADPLATS-FOM  PIC S9(5)           COMP-3.                  
002000*                                 LOCATION ADDRESS FROM                   
002100     03 2401-ADART-TOM.                                                   
002200*                                 PARTS-ADRESS WHERE TO MOVE IT           
002300        05 2401-ADLAGOMR-TOM PIC S9(3)           COMP-3.                  
002400*                                 AREA ADDRESS TO                         
002500        05 2401-ADGANG-TOM   PIC S9(3)           COMP-3.                  
002600*                                 AISLE ADDRESS TO                        
002700        05 2401-ADPLATS-TOM  PIC S9(5)           COMP-3.                  
002800*                                 LOCATION ADDRESS TO                     
002900*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
