000100 01  4473-WDGX4473.                                                       
000200*                                 LISTBESTÄLLNINGAR                       
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDHTYP + IDDC +                        
000500*                                  KDPRCGRP + LOW-VALUE)                  
000600     03 4473-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4473-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4473-KDPRCGRP        PIC X(5).                                    
001200*                                 PRODUKTIONSKANALSGRUPP                  
001300*                                 GROUP OF PRODUCTION CHANNELS            
001400     03 4473-LOW-VALUE       PIC X(19).                                   
001500*** END COPY WDGX4473    LENGTH=30                                        
