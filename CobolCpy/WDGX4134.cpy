000100 01  4134-WDGX4134.                                                       
000200*                                 POSTNUMMER KROSS                        
000300*                                 ARTIKLAR                                
000400*                                 NYCKEL KY4134:                          
000500*                                 (IDLAND + ADPOSTNR-FOM,                 
000600*                                           ADPOSTNR-TOM                  
000700     03 4134-IDLANDX2        PIC X(2).                                    
000800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000900*                                 2-LETTER CODE FOR COUNTRY               
001000     03 4134-ADPOSTNR-FOM    PIC X(10).                                   
001100*                                 LÄGSTA POSTNUMMER I INTERVALL           
001200*                                 LOWEST POSTAL NUMBER                    
001300     03 4134-ADPOSTNR-TOM    PIC X(10).                                   
001400*                                 HÖGSTA POSTNUMMER I INTERVALL           
001500*                                 HIGHEST POSTAL NUMBER                   
001600     03 4134-IDDISTR         PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900     03 4134-IDKUNDNR        PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
