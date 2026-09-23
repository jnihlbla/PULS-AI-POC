000100 01  W33520.                                                              
000200*                                 PAYER. INFORM.  TRANS FROM              
000300*                                 MARKET COMPANY  TRANSFERCODE            
000400     03 IDVTYP               PIC X.                                       
000500*                                 RECORD TYPE VERSION                     
000600     03 IDPROMR.                                                          
000700*                                 PRICE AREA (DISCOUNT STRUCTURE)         
000800        05 IDMARKBO          PIC X.                                       
000900*                                 MARKET COMPANY CODE                     
001000*                                 A = SWEDEN                              
001100*                                 B = VCEM                                
001200*                                 C = NORDIC WITHOUT SWEDEN               
001300*                                 D = UK                                  
001400*                                 E = VCNA                                
001500*                                 F = VCI                                 
001600*                                 G = VCAS                                
001700        05 IDPROMRN          PIC X(2).                                    
001800*                                 PRICE AREA SERIALNUMBER                 
001900     03 TISTADAT             PIC S9(7)           COMP-3.                  
002000*                                 GENERAL START DATE                      
002100     03 TISTODAT             PIC S9(7)           COMP-3.                  
002200*                                 GENERAL STOP DATE YYMMDD                
002300     03 KDARTKAM             PIC 9(5).                                    
002400*                                 TRANSFER DISCOUNT CODE                  
002500     03 REARTRAB-DO          PIC S9(2)V9(2)      COMP-3.                  
002600*                                 PARTS DISCOUNT DAYORDER                 
002700     03 REARTRAB-BULK        PIC S9(2)V9(2)      COMP-3.                  
002800*                                 PARTS DISCOUNT STOCKORDER               
002900*** END COPY W33520      LENGTH=23                                        
