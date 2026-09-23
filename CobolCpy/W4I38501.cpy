000100 01  MID-W4I38501.                                                        
000200*                                 COPYTEXT FOR MID W4I38501               
000300     03 MID-PFI-IDTRP.                                                    
000400*                                 TRANSPORTIDENTITET                      
000500        05 MID-IDTRPLOS      PIC X(3).                                    
000600*                                 TRANSPORTLÖSNING                        
000700        05 MID-IDTRPVAR      PIC X(2).                                    
000800*                                 TRANSPORTLÖSNINGSGRUPP                  
000900     03 MID-PF7-IDTRP.                                                    
001000*                                 TRANSPORTIDENTITET                      
001100        05 MID-IDTRPLOS      PIC X(3).                                    
001200*                                 TRANSPORTLÖSNING                        
001300        05 MID-IDTRPVAR      PIC X(2).                                    
001400*                                 TRANSPORTLÖSNINGSGRUPP                  
001500     03 MID-PFI-IDDC         PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-PF7-IDDC         PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-PFE-IDTRP.                                                    
002000*                                 TRANSPORTIDENTITET                      
002100        05 MID-IDTRPLOS      PIC X(3).                                    
002200*                                 TRANSPORTLÖSNING                        
002300        05 MID-IDTRPVAR      PIC X(2).                                    
002400*                                 TRANSPORTLÖSNINGSGRUPP                  
002500     03 MID-PFE-IDORDER      PIC 9(7).                                    
002600*                                 VOLVO PARTS ORDERNUMMER                 
002700     03 MID-PF8-IDTRP.                                                    
002800*                                 TRANSPORTIDENTITET                      
002900        05 MID-IDTRPLOS      PIC X(3).                                    
003000*                                 TRANSPORTLÖSNING                        
003100        05 MID-IDTRPVAR      PIC X(2).                                    
003200*                                 TRANSPORTLÖSNINGSGRUPP                  
003300     03 MID-PF8-IDORDER      PIC 9(7).                                    
003400*                                 VOLVO PARTS ORDERNUMMER                 
003500     03 MID-TOT-VLORDNTO     PIC X(8).                                    
003600*                                 ORDERVOLYM NETTO (M3)                   
003700     03 MID-TOT-VKORDNTO     PIC X(8).                                    
003800*                                 ORDERVIKT NETTO (KG)                    
