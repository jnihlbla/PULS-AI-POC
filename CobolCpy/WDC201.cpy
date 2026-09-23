000100 01  PRO-WDC201.                                                          
000200*                                 PRIS-RABATT REGISTER                    
000300*                                 PRISOMRÅDESINFORMATION                  
000400*                                 FYSISK NYCKEL: IDPROMR                  
000500     03 PRO-IDPROMR.                                                      
000600*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
000700*                                 PRICE AREA                              
000800        05 PRO-IDMARKBO      PIC X.                                       
000900*                                 MARKNADSBOLAGSKOD                       
001000*                                 MARKET COMPANY CODE                     
001100*                                 A = VCS                                 
001200*                                 B = VCEM                                
001300*                                 C = NORDIC WITHOUT SWEDEN               
001400*                                 D = VCUK                                
001500*                                 E = VCNA                                
001600*                                 F = VCI                                 
001700*                                 G = VCAS                                
001800        05 PRO-IDPROMRN      PIC X(2).                                    
001900*                                 PRISOMRÅDE LÖPNUMMER                    
002000*                                 PRICE AREA SERIALNUMBER                 
002100     03 PRO-KDORDKL-DOG      PIC S9              COMP-3.                  
002200*                                 DAGORDERKLASSGRÄNS FÖR BETALARE         
002300*                                 DAY ORDER LIMIT FOR CUSTOMER            
002400*** END OF VILMAII-COPY LENGTH= 4 BYTES                                   
