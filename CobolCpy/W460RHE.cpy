000100 01  KRED-W460RHE.                                                        
000200*                                 CRED TRANS. FROM VIPS                   
000300*                                 TO NOAC RECORD TYPE RHE                 
000400     03 KRED-IDPTYP          PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 KRED-IDDISTR         PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 KRED-IDKUNDNR        PIC 9(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 KRED-KDCLAGER        PIC 9.                                       
001100*                                 CENTRAL WAREHOUSE CODE                  
001200     03 KRED-IDLEVANM        PIC X(7).                                    
001300*                                 DISCREPANCY REPORT NO                   
001400     03 KRED-KDFRAKT         PIC 9(2).                                    
001500*                                 FREIGHT CODE                            
001600     03 KRED-RELANDCO        PIC 9(3).                                    
001700*                                 LANDING COST PERCENT                    
001800     03 KRED-PREMBHNT        PIC 9(7)V9(2).                               
001900*                                 PACKING & HANDL COSTS (SEK)             
002000*                                 INCLUDES SURCHARGE FOR DAILY            
002100*                                 ORDERS (SUFKTTILL)                      
002200     03 KRED-PRFRAKT         PIC 9(7)V9(2).                               
002300*                                 FREIGHT COST SEK                        
002400     03 KRED-PRLEGKST        PIC 9(7)V9(2).                               
002500*                                 LEGALIZATION FEE (SEK)                  
002600     03 KRED-PRFOERS         PIC 9(7)V9(2).                               
002700*                                 INSURANCE (SEK)                         
002800     03 KRED-PREXPKST        PIC 9(7)V9(2).                               
002900*                                 SERVICE CHARGE (SEK)                    
003000     03 KRED-PRMOMS          PIC 9(7)V9(2).                               
003100*                                 VAT                                     
003200*** END COPY W460RHECC0  LENGTH=80                                        
