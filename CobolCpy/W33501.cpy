000100 01  W33501.                                                              
000200*                                 PARTS INFORMATION TRANS FROM            
000300*                                 MARKET COMPANY                          
000400     03 IDVTYP               PIC X.                                       
000500*                                 RECORD TYPE VERSION                     
000600     03 IDMARKBO             PIC X.                                       
000700*                                 MARKET COMPANY CODE                     
000800*                                 A = VCS                                 
000900*                                 B = VCEM                                
001000*                                 C = NORDIC WITHOUT SWEDEN               
001100*                                 D = VCUK                                
001200*                                 E = VCNA                                
001300*                                 F = VCI                                 
001400*                                 G = VCAS                                
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 PART NUMBER                             
001700     03 KDRABATT             PIC 9(3).                                    
001800*                                 PURCHASE DISCOUNT CODE                  
001900     03 PRARTBTO-MARK        PIC S9(7)V9(2)      COMP-3.                  
002000*                                 SUGGESTED RETAIL PER MARKET.            
002100     03 KDARTKAM             PIC 9(5).                                    
002200*                                 TRANSFER CONDITION CODE                 
002300*** END COPY W33501      LENGTH=20                                        
