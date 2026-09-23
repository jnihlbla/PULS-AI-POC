000100 01  COST-W335COST.                                                       
000200*                                 LÄNKAREA TILL W335COST - RÄKNA          
000300*                                 OM SJÄLVKOST MED                        
000400*                                  MÅNADSKURS                             
000500     03 COST-IDARTNR         PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 COST-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 COST-PRARTBES-MON    PIC S9(7)V9(2)      COMP-3.                  
001000*                                 BESTÄLLNINGSPRIS I SEK TILL MÅN         
001100*                                 ADSKURS                                 
001200     03 COST-PRARTBES-MONLOC PIC S9(7)V9(2)      COMP-3.                  
001300*                                 BESTÄLLNINGSPRIS I SEK TILL MÅN         
001400*                                 ADSKURS                                 
001500     03 COST-PRARTSJK-MON    PIC S9(7)V9(2)      COMP-3.                  
001600*                                 ARTIKELNS SJÄLVKOSTNAD TILL MÅN         
001700*                                 ADSKURS                                 
001800     03 COST-PRARTSJK-MONLOC PIC S9(7)V9(2)      COMP-3.                  
001900*                                 ARTIKELNS SJÄLVKOSTNAD TILL MÅN         
002000*                                 ADSKURS                                 
002100     03 COST-IDLEVNR         PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 COST-TIMM            PIC 9(2).                                    
002400*                                 MÅNAD (MM)                              
002500*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
