000010 01  WXTRPACK.                                                            
000020*                                 FILEN VISAR DET HÖGSTA PACK-            
000030*                                 DATUMET SOM FÖREKOM UNDER               
000040*                                 FÖRRA KÖRNINGEN. ALLT SOM               
000050*                                 PACKATS DÄREFTER, DVS MED ETT           
000060*                                 HÖGRE PACKDATUM ÄN DET SOM              
000070*                                 LIGGER I FILEN, KOMMER ATT              
000080*                                 SKRIVAS UT PÅ DEN NYA PACK-             
000090*                                 FILEN.                                  
000100     03 TIPACKN-FOREG        PIC S9(7)           COMP-3.                  
000110*                                 PACKNINGSDATUM         (ÅÅMMDD)         
000120     03 FILLER               PIC X(76).                                   
000130*                                                                         
      *** END COPY WXTRPACK    LENGTH=80                                        
