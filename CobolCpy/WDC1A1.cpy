000100 01  SEQA-WDC1A1.                                                         
000200*                                 ARTIKEL PRIS-INFO                       
000300*                                 SEKUNDÄRT INDEX TILL WDC101             
000400*                                 FYSISK NYCKEL: WDC1A1KY                 
000500*                                 (KDARTKAM + IDARTNR + IDMARKBO)         
000600*                                 SEKUNDÄR NYCKEL: WDC1ASEQ               
000700*                                 (KDARTKAM)                              
000800     03 SEQA-KDARTKAM        PIC 9(5).                                    
000900*                                 TRANSFER KOD                            
001000*                                 TRANSFER CONDITION CODE                 
001100     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 SEQA-IDMARKBO        PIC X.                                       
001500*                                 MARKNADSBOLAGSKOD                       
001600*                                 MARKET COMPANY CODE                     
001700*                                 A = VCS                                 
001800*                                 B = VCEM                                
001900*                                 C = NORDIC WITHOUT SWEDEN               
002000*                                 D = VCUK                                
002100*                                 E = VCNA                                
002200*                                 F = VCI                                 
002300*                                 G = VCAS                                
002400*** END OF VILMAII-COPY LENGTH= 11 BYTES                                  
