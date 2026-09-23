000100 01  TRPTRUT-WDGX4408.                                                    
000200*                                 4408 HTR TRANSPORTREGISTER              
000300*                                 RUT-SEGMENT                             
000400*                                 NYCKEL: WDGXKEY                         
000500*                                         (ADCLGEO, ADFLOMR,              
000600*                                          ADRUTNIV, LOWVALUE)            
000700     03 TRPTRUT-ADCLGEO.                                                  
000800*                                 IDDC + GEO ADRESS                       
000900        05 TRPTRUT-IDDC      PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 TRPTRUT-ADFLGEO   PIC X(3).                                    
001200*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
001300     03 TRPTRUT-ADFLOMR      PIC S9(3)           COMP-3.                  
001400*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
001500     03 TRPTRUT-ADRUTNIV     PIC S9(3)           COMP-3.                  
001600*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
001700     03 TRPTRUT-LOWVALUE     PIC X.                                       
001800     03 TRPTRUT-FLRUTFUL     PIC X.                                       
001900*                                 J = RUTAN ÄR FULL, LEDIG FINNS          
002000*                                 N = PLATS FINNS I RUTAN                 
002100*** END OF VILMAII-COPY LENGTH= 11 BYTES                                  
