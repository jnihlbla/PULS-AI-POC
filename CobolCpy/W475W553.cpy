000010*** EDIT ALLOWED                                                          
000001*      LEDTEXTER I 8 MÖJLIGA SPRÅK:                                       
000002*                                                                         
000010*        1 - SVENSKA                                                      
000020*        2 - ENGELSKA                                                     
000030*        3 - FRANSKA                                                      
000040*        4 - SPANSKA                                                      
000050*        5 - TYSKA                                                        
000070*        7 - ITALIENSKA                                                   
000071*        6 - FLAMLÄNDSKA                                                  
000080*        8 - LEDIGT                                                       
000090*                                                                         
000100 01  W475W553.                                                            
000200*             ***  PRODUKTGRUPPSUNDERLAG                                  
000300*                                                                         
000400*                                   - LEDTEXTER                           
000700                                                                          
000800*                                                                         
000900    04  PRODGRP-SUMMA-LEDTEXTER.                                          
001000       05  FILLER  PIC X(12) VALUE '       SUMMA'.                        
001100       05  FILLER  PIC X(12) VALUE '       TOTAL'.                        
001200       05  FILLER  PIC X(12) VALUE '       TOTAL'.                        
001300       05  FILLER  PIC X(12) VALUE '       TOTAL'.                        
001400       05  FILLER  PIC X(12) VALUE '       TOTAL'.                        
001500       05  FILLER  PIC X(12) VALUE '      TOTALE'.                        
001510       05  FILLER  PIC X(12) VALUE '       TOTAL'.                        
001520       05  FILLER  PIC X(12) VALUE '       TOTAL'.                        
001600    04  FILLER  REDEFINES  PRODGRP-SUMMA-LEDTEXTER.                       
001700       05  PRODGRP-SUMMA-LEDTEXT    PIC X(12) OCCURS 8 TIMES.             
001710*                                                                         
001800*** END COPY W475W553    LENGTH=      OLD LENGTH=                         
