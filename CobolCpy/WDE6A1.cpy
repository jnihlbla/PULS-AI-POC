000100 01  SEQA-WDE6A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE601             
000300*                                 FYSISK NYCKEL: WDE6A1KY                 
000400*                                 (IDDISTR, IDDC, KDFRAKT,                
000500*                                  KDORDSTA, IDKUNDNR, IDPRODNR)          
000600*                                 SECONDARY KEY: WDE6ASEQ                 
000700*                                 (IDDISTR, IDDC, KDFRAKT,                
000800*                                  KDORDSTA, IDKUNDNR)                    
000900     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200     03 SEQA-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 SEQA-KDFRAKT         PIC S9(3)           COMP-3.                  
001600*                                 FRAKTSÄTT DC TILL KUND                  
001700*                                 FREIGHT CODE                            
001800     03 SEQA-KDORDSTA        PIC S9              COMP-3.                  
001900*                                 VOLVOORDERSTATUS                        
002000*                                 VOLVO ORDER STATUS                      
002100     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400     03 SEQA-IDPRODNR        PIC S9(7)           COMP-3.                  
002500*                                 PRODUKTIONSNUMMER                       
002600*                                 PRODUCTION NUMBER                       
002700*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
