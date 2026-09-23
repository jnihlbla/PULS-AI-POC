000100 01  W330320.                                                             
000200*                                 ARTIKELSTATISTIK                        
000300*                                 PRIMÄWREGISTER                          
000400*                                 FÖRSÄLJNING MED                         
000500*                                 RABATT/FUNKTIONSGRUPP                   
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 SULEVANT-RAB         PIC S9(9)           COMP-3.                  
000900*                                 ANTAL LEV. ART TILL RABATT              
001000*                                 PER FKNGRP                              
001100     03 SUARTFSG-RAB         PIC S9(9)V9(2)      COMP-3.                  
001200*                                 SUMMA FSG/ART TILL RABATT               
001300*                                 PER FKNGRP                              
001400*** END COPY W330320CC0  LENGTH=14                                        
