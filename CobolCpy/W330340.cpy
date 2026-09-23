000100 01  W330340.                                                             
000200*                                 ARTIKELSTATISTIK                        
000300*                                 PRIMÄWREGISTER                          
000400*                                 FÖRSÄLJNING MED                         
000500*                                 MANUELLT SATT PRIS                      
000600*                                  ( PRIS I RAD )                         
000700     03 IDPTYP               PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 SULEVANT-MAN         PIC S9(9)           COMP-3.                  
001000*                                 ANTAL LEV. ART TILL                     
001100*                                 MANUELLT PRIS                           
001200     03 SUARTFSG-MAN         PIC S9(9)V9(2)      COMP-3.                  
001300*                                 SUMMA FSG/ARTIKEL TILL                  
001400*                                 PRIS I RAD                              
001500*** END COPY W330340CC0  LENGTH=14                                        
