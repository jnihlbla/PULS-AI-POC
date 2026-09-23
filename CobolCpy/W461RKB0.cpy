000100 01  RKB-W461RKB0.                                                        
000200*                                 CREDITTRANSACTION-HEAD                  
000300*                                 RECORD TYP RKB                          
000400     03 RKB-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RKB-KDCLAGER         PIC 9.                                       
000700*                                 CENTRAL WAREHOUSE CODE                  
000800     03 RKB-IDDISTR          PIC 9(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 RKB-IDKUNDNR         PIC 9(6).                                    
001100*                                 CUSTOMER NO                             
001200     03 RKB-IDKNOTNR         PIC 9(7).                                    
001300*                                 CREDIT NOTE NUMBER                      
001400     03 RKB-TIM-KN           PIC 9(6).                                    
001500     03 RKB-IDLEVANM         PIC X(7).                                    
001600*                                 DISCREPANCY REPORT NO                   
001700     03 RKB-PREMBHNT         PIC 9(7)V9(2).                               
001800*                                 PACKING & HANDL COSTS (SEK)             
001900*                                 INCLUDES SURCHARGE FOR DAILY            
002000*                                 ORDERS (SUFKTTILL)                      
002100     03 RKB-PRFRAKT          PIC 9(7)V9(2).                               
002200*                                 FREIGHT COST SEK                        
002300     03 RKB-PRLEGKST         PIC 9(7)V9(2).                               
002400*                                 LEGALIZATION FEE (SEK)                  
002500     03 RKB-PRFOERS          PIC 9(7)V9(2).                               
002600*                                 INSURANCE SEK                           
002700     03 RKB-PRMOMS           PIC 9(7)V9(2).                               
002800*                                 VAT                                     
002900     03 FILLER               PIC X.                                       
003000*** END COPY W461RKB0C0  LENGTH=80                                        
