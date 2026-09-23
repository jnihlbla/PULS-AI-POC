000100 01  6328-WDGX6328.                                                       
000200*                                 SKROTFÖRSLAG GODKÄNNARE                 
000300*                                 PERSONUPPGIFTER                         
000400*                                 FYSISK NYCKEL: KY6328                   
000500*                                 (SUBEL + IDUSER-GODK)                   
000600     03 6328-SUBEL           PIC 9(7).                                    
000700*                                 SUMMABELOPP                             
000800*                                 SUM AMOUNT                              
000900     03 6328-IDUSER-GODK     PIC X(8).                                    
001000*                                 ANVÄNDAR-ID GODKÄNNARE                  
001100*                                 USER ID APPROVER                        
001200     03 6328-BEANST-GODK     PIC X(25).                                   
001300*                                 GODKÄNNARES NAMN                        
001400*                                 NAME OF APPROVER                        
001500     03 6328-IDMAIL          PIC X(60).                                   
001600*                                 MAIL ADRESS                             
001700*                                 MAIL ADDRESS                            
001800     03 6328-IDUSER-OREG     PIC X(8).                                    
001900*                                 ANSVARIGT USERID ORDERREG.              
002000*                                 RESPONSIBLE USERID ORDERREG.            
002100     03 6328-TIUPPDAT        PIC S9(7)           COMP-3.                  
002200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002300*                                 UPDATING DATE     (YYMMDD)              
002400     03 6328-IDUSER-PRI      PIC X(8).                                    
002500*                                 ANVÄNDAR-ID PRIMÄRKONTROLL              
002600*                                 USER ID PRIMARY INSPECTION              
002700     03 6328-FILLER          PIC X(10).                                   
002800*** END OF VILMAII-COPY LENGTH= 130 BYTES                                 
