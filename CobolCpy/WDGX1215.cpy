000100 01  1215-WDGX1215.                                                       
000200*                                 KATALOG VADIS                           
000300*                                 BILMODELL                               
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP + IDMODELL +                    
000600*                                  LOWVALUE)                              
000700     03 1215-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 1215-IDMODELL        PIC X(3).                                    
001000*                                 BILENS MODELBETECKNING ("XX0")          
001100*                                 MODEL ID FOR A VECHICLE ("XX0")         
001200     03 1215-LOWVALUE        PIC X(23).                                   
001300*** END COPY WDGX1215    LENGTH=30                                        
