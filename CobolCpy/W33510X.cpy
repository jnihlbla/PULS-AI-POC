000100 01  W33510X.                                                             
000200*                                 PAYER. INFORM.  TRANS FROM              
000300*                                 MARKET COMPANY                          
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
001900     03 TISTADAT-XBMS        PIC X(6).                                    
002000     03 001-GRUPP            OCCURS 99 TIMES.                             
002100        05 KDRABATT          PIC 9(3).                                    
002200*                                 PURCHASE DISCOUNT CODE                  
002300        05 REARTRAB-DO-XBMS  PIC S9(3)V9(2)      COMP-3.                  
002400*                                 STOCK BALANCE/PERIOD REQ F KIT          
002500        05 REARTRAB-MO-XBMS  PIC S9(3)V9(2)      COMP-3.                  
002600*                                 STOCK BALANCE/PERIOD REQ F KIT          
002700     03 FILLER               PIC X(11).                                   
002800*** END COPY W33510X     LENGTH=912                                       
