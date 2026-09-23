000100 01  W33510.                                                              
000200*                                 PAYER. INFORM.                          
000300*                                                                         
000400     03 IDVTYP               PIC X.                                       
000500*                                 RECORD TYPE VERSION                     
000600     03 IDPROMR.                                                          
000700*                                 PRICE AREA                              
000800        05 IDMARKBO          PIC X.                                       
000900*                                 MARKET COMPANY CODE                     
001000*                                 A = VCS                                 
001100*                                 B = VCEM                                
001200*                                 C = NORDIC WITHOUT SWEDEN               
001300*                                 D = VCUK                                
001400*                                 E = VCNA                                
001500*                                 F = VCI                                 
001600*                                 G = VCAS                                
001700        05 IDPROMRN          PIC X(2).                                    
001800*                                 PRICE AREA SERIALNUMBER                 
001900     03 TISTADAT             PIC S9(7)           COMP-3.                  
002000*                                 GENERAL START DATE                      
002100     03 001-GRUPP            OCCURS 99 TIMES.                             
002200        05 KDRABATT          PIC 9(3).                                    
002300*                                 PURCHASE DISCOUNT CODE                  
002400        05 REARTRAB-DO       PIC S9(2)V9(2)      COMP-3.                  
002500*                                 PARTS DISCOUNT DAYORDER                 
002600        05 REARTRAB-BULK     PIC S9(2)V9(2)      COMP-3.                  
002700*                                 PARTS DISCOUNT STOCKORDER               
002800*** END COPY W335101A    LENGTH=899                                       
