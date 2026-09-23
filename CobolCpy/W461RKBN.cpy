000100 01  RKB-W461RKBN-CTX.                                                    
000200*                                 CREDITTRANSACTION-HEAD                  
000300*                                 RECORD TYP RKB                          
000400     03 RKB-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RKB-IDDC             PIC X(2).                                    
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 RKB-IDDISTR          PIC 9(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 RKB-IDKUNDNR         PIC 9(6).                                    
001100*                                 CUSTOMER NO                             
001200     03 RKB-IDKNOTNR         PIC 9(7).                                    
001300*                                 CREDIT NOTE NUMBER                      
001400     03 RKB-TIM-KN           PIC 9(6).                                    
001500*                                 DATE CREDIT NOTE ISSUE                  
001600     03 RKB-IDRAPPNR         PIC 9(7).                                    
001700*                                 DISCREPANCY REPORT NUMBER               
001800     03 RKB-PREMBHNT         PIC 9(7)V9(2).                               
001900*                                 PACKING O HANDL COSTS                   
002000     03 RKB-PRFRAKT          PIC 9(7)V9(2).                               
002100*                                 FREIGHT COST                            
002200     03 RKB-PRLEGKST         PIC 9(7)V9(2).                               
002300*                                 LEGALIZATION FEE                        
002400     03 RKB-PRFOERS          PIC 9(7)V9(2).                               
002500*                                 INSURANCE FEE                           
002600     03 RKB-PRMOMS           PIC 9(7)V9(2).                               
002700*                                 VAT                                     
002800     03 RKB-KDVALISO         PIC X(3).                                    
002900*                                 CURRENCY CODE BY ISO-STANDARD.          
003000     03 RKB-SUKRENTO         PIC 9(11)V9(2).                              
003100*                                 CREDITED GOODSVALUE NET                 
003200     03 RKB-SUKRETOT         PIC 9(11)V9(2).                              
003300*                                 TOTAL CREDITED VALUE                    
003400*** END OF VILMAII-COPY LENGTH= 109 BYTES                                 
