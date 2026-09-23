000100 01  6344-WDGX6344.                                                       
000200*                                 ÖVERBELAGDA LAGERPLATSER                
000300*                                 LAGERPLATSER                            
000400*                                 FYSISK NYCKEL: KY6344                   
000500*                                 (ADLAGOMR + ADGANG + ADPLDEL)           
000600     03 6344-ADLAGOMR        PIC S9(3)           COMP-3.                  
000700*                                 LAGEROMRÅDE                             
000800*                                 AREA                                    
000900     03 6344-ADGANG          PIC S9(3)           COMP-3.                  
001000*                                 GÅNG                                    
001100*                                 AISLE                                   
001200     03 6344-ADPLDEL         PIC 9(2).                                    
001300*                                 DEL AV ADPLATS (1:A 2 SIFFROR)          
001400*                                 BAY - FIRST 2 DIGIT OF ADPLATS          
001500     03 6344-KVPLATS         PIC S9(3)           COMP-3.                  
001600*                                 NO OF ADDRESSES IN A BAY                
001700*                                 ANTAL LAGERPLATSER I ETT STÄLL          
001800     03 6344-KVANTART        PIC S9(5)           COMP-3.                  
001900*                                 ANTAL-ARTIKLAR                          
002000*                                 QUANTITY PARTS                          
002100*** END OF VILMAII-COPY LENGTH= 11 BYTES                                  
