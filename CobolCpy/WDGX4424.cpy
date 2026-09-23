000100 01  TCENT-WDGX4424.                                                      
000200*                                 TRANSPORTCENTRALENS INFORMATION         
000300*                                 NYCKEL: WDGXKEY                         
000400*                                         (BEROUTE, IDTRANSP)             
000500*                                 SÖKBEGREPP: BEROUTE, IDTRANSP           
000600     03 TCENT-BEROUTE        PIC X(25).                                   
000700*                                 FÄRDVÄG, DESTINATION                    
000800     03 TCENT-IDTRANSP-NAMN  PIC X(15).                                   
000900*                                 TRANSPORTMEDEL NAMN                     
001000     03 TCENT-KVKOLLI-TRPT   PIC S9(5)           COMP-3.                  
001100*                                 ANTAL KOLLI TRANSPORT                   
001200     03 TCENT-VKORDBTO-TRPT  PIC S9(6)V9(1)      COMP-3.                  
001300*                                 BRUTTOVIKT TRANSPORT                    
001400     03 TCENT-VLORDBTO-TRPT  PIC S9(4)V9(3)      COMP-3.                  
001500*                                 BRUTTOVOLYM TRANSPORT                   
001600     03 TCENT-ADCLGEO.                                                    
001700*                                 IDDC + GEO ADRESS                       
001800        05 TCENT-IDDC        PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000        05 TCENT-ADFLGEO     PIC X(3).                                    
002100*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002200     03 TCENT-ADFLOMR        PIC S9(3)           COMP-3.                  
002300*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002400     03 TCENT-ADRUTNIV       PIC S9(3)           COMP-3.                  
002500*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002600*** END COPY WDGX4424    LENGTH=60                                        
