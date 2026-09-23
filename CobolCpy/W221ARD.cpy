000010*** EDIT ALLOWED                                                          
000100 01  W221ARD.                                                             
000200*                                 ODETTE-SEGMENT ARD                      
000300*                                 ARTICLE DETAILS                         
000400*                                                                         
000500*                                 FORMAT : HELD-AS                        
000600     03 IDPT                 PIC X(3).                                    
000700*                                 POSTTYP                                 
000710     03 IDPTYP-LTH           PIC X(3) VALUE '055'.                        
000720*                                 LÄNGD PÅ FÄLT                           
000900     03 IDART                PIC X(24).                                   
001000*                                 ARTIKELNUMMER        TAG 7304           
001300     03 BESTNR               PIC X(17).                                   
001400*                                 BESTÄLLNINGSNUMMER   TAG 1022           
001410     03 BELEVART14           PIC X(14) VALUE SPACE.                       
001420*                                                      TAG 7860           
001430     03 FILLER               PIC X(35) VALUE SPACE.                       
001440*                                                      TAG 7194           
001500*** END COPY W221ARD     LENGTH=96    OLD LENGTH=61                       
