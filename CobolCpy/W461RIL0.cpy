000100 01  RIL-W461RIL0.                                                        
000200*                                 INVOICE HEAD -FOLLOWING AFTER           
000300*                                 RIK   TO IMPORTER                       
000400*                                 RECORD TYPE RIL                         
000500     03 RIL-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIL-PRAVDRAG         PIC 9(7)V9(2).                               
000800*                                 DEDUCTION (SEK)                         
000900     03 RIL-PREMBHNT         PIC 9(7)V9(2).                               
001000*                                 PACKING & HANDL COSTS (SEK)             
001100*                                 INCLUDES SURCHARGE FOR DAILY            
001200*                                 ORDERS (SUFKTTILL)                      
001300     03 RIL-PRFOERS          PIC 9(7)V9(2).                               
001400*                                 INSURANCE SEK                           
001500     03 RIL-PRFRAKT          PIC 9(7)V9(2).                               
001600*                                 FREIGHT COST SEK                        
001700     03 RIL-PRLEGKST         PIC 9(7)V9(2).                               
001800*                                 LEGALIZATION FEE (SEK)                  
001900     03 RIL-PRMOMS           PIC 9(7)V9(2).                               
002000*                                 VAT                                     
002100     03 RIL-SUFKTTILL        PIC 9(7)V9(2).                               
002200*                                 ADDITIONAL COSTS (SEK)                  
002300     03 RIL-KDFRAKT          PIC 9(2).                                    
002400*                                 FREIGHT CODE                            
002500     03 FILLER               PIC X(12).                                   
002600*** END COPY W461RIL0C0  LENGTH=80                                        
