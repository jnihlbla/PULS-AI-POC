000100 01  2301-W6132301.                                                       
000200*                                 APPROVED PARTS                          
000300     03 2301-IDARTNR         PIC S9(9)           COMP-3.                  
000400*                                 PART NUMBER                             
000500     03 2301-KVBEST          PIC S9(7)           COMP-3.                  
000600*                                 ORDERD QUANTITY                         
000700     03 2301-TIORDTIME       PIC 9(10).                                   
000800*                                 ORDER DATE AND TIME                     
000900*                                 (YYMMDDHHMM[SS])                        
001000     03 2301-TIHOTIME        PIC 9(10).                                   
001100*                                 HAND OVER DATE AND TIME                 
001200*                                 (YYMMDDHHMM[SS])                        
001300     03 2301-KVBEST-ANDR     PIC S9(7)           COMP-3.                  
001400*                                 CHANGED QTY FROM THE ORDERED            
001500     03 2301-TIAVSL          PIC 9(10).                                   
001600*                                 FINISHING DATE AND TIME                 
001700*                                 (YYMMDDHHMM[SS])                        
001800     03 2301-ADART-FOM.                                                   
001900*                                 PARTS-ADRESS WHERE TO MOVE FROM         
002000        05 2301-ADLAGOMR-FOM PIC S9(3)           COMP-3.                  
002100*                                 AREA ADDRESS FROM                       
002200        05 2301-ADGANG-FOM   PIC S9(3)           COMP-3.                  
002300*                                 AISLE ADDRESS FROM                      
002400        05 2301-ADPLATS-FOM  PIC S9(5)           COMP-3.                  
002500*                                 LOCATION ADDRESS FROM                   
002600     03 2301-ADART-TOM.                                                   
002700*                                 PARTS-ADRESS WHERE TO MOVE IT           
002800        05 2301-ADLAGOMR-TOM PIC S9(3)           COMP-3.                  
002900*                                 AREA ADDRESS TO                         
003000        05 2301-ADGANG-TOM   PIC S9(3)           COMP-3.                  
003100*                                 AISLE ADDRESS TO                        
003200        05 2301-ADPLATS-TOM  PIC S9(5)           COMP-3.                  
003300*                                 LOCATION ADDRESS TO                     
003400*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
