000100 01  4308-WDGX4308.                                                       
000200*                                 LÅSNINGSREGISTER                        
000300*                                 ANNULLERINGSSEGMENTET                   
000400*                                 NYCKEL: WDGXKEY                         
000500*                                         (IDRADNR-ORD-TOM,               
000600*                                          LOWVALUE)                      
000700     03 4308-IDRADNR-ORD-TOM PIC S9(5)           COMP-3.                  
000800*                                 RADNUMMER PÅ VOLVOORDER TOM             
000900     03 4308-LOWVALUE        PIC X(7).                                    
001000     03 4308-IDRADNR-ORD-FROM                                             
001100                             PIC S9(5)           COMP-3.                  
001200*                                 RADNUMMER PÅ VOLVOORDER FROM            
001300     03 4308-KVANNANT        PIC S9(7)           COMP-3.                  
001400*                                 ANNULLERAT ANTAL ARTIKLAR               
001500*** END COPY WDGX4308C0  LENGTH=17                                        
