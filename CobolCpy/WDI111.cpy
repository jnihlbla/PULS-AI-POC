000100 01  PIET-WDI111.                                                         
000200*                                 PIE-TRANSAR HISTORIK                    
000300*                                 INKOMNA TRANSAR FR≈N PIE                
000400*                                 FYSISK NYCKEL: IDPIERAD                 
000500*                                                                         
000600     03 PIET-IDPIERAD        PIC X(20).                                   
000700*                                 PIE ORDERAD NR/REFERENS                 
000800*                                 PIE ORDERLINE NO/REFERENS               
000900     03 PIET-KDFEL           PIC 9.                                       
001000*                                 FELKOD                                  
001100     03 PIET-KDSOFT          PIC S9              COMP-3.                  
001200*                                 0 NORMAL ORDER                          
001300*                                 1 VCEM SOFTWARE ORDER                   
001400*                                 2 VADIS SOFTWARE ORDER                  
001500*                                 3 OTHER SOFTWARE ORDER                  
001600     03 PIET-KVANTEX         PIC S9              COMP-3.                  
001700*                                 ANTAL EXEMPLAR                          
001800     03 PIET-TIUPPDAT        PIC S9(7)           COMP-3.                  
001900*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
002000*                                 UPDATING DATE     (YYMMDD)              
002100*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
