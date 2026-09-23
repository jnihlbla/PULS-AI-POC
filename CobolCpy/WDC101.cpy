000100 01  ART-WDC101.                                                          
000200*                                 ARTIKEL PRIS-INFO                       
000300*                                 FYSISK NYCKEL: WDC101KY                 
000400*                                  (IDARTNR IDMARKBO)                     
000500     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 ART-IDMARKBO         PIC X.                                       
000900*                                 MARKNADSBOLAGSKOD                       
001000*                                 MARKET COMPANY CODE                     
001100     03 ART-KDARTKAM         PIC 9(5).                                    
001200*                                 TRANSFER KOD                            
001300*                                 TRANSFER CONDITION CODE                 
001400     03 ART-KDARTRAB         PIC 9(2).                                    
001500*                                 RABATTKOD (ARTIKELPRIS)                 
001600*                                 PURCHASE DISCOUNT CODE                  
001700     03 ART-PRARTBTO-MARK    PIC S9(7)V9(2)      COMP-3.                  
001800*                                 BRUTTOPRIS PER MARKNAD (FOB)            
001900*                                 SUGGESTED RETAIL PER MARKET.            
002000     03 ART-TIUPPDAT         PIC S9(7)           COMP-3.                  
002100*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
002200*                                 UPDATING DATE     (YYMMDD)              
002300     03 ART-KDARTRAB-ALT     PIC 9(3).                                    
002400*                                 ALTERNATIV RABATTKOD ART.PRIS           
002500*                                 ALTERNATE PURCHASE DISC. CODE           
002600*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
