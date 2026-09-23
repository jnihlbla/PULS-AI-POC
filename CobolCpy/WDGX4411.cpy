000100 01  4411-WDGX4411.                                                       
000200*                                 ROT FÖR LAGRING AV DEFINITION           
000300*                                 LASTNINGSOMRÅDE/RUTA                    
000400*                                 NYCKEL: WDGXKEY                         
000500*                                         (IDHTYP, IDDC,                  
000600*                                          ADFLGEO, LOWVALUE)             
000700*                                 SÖKBEGREPP: IDHTYP                      
000800     03 4411-IDHTYP          PIC X(4).                                    
000900*                                 HÄNDELSETYP                             
001000     03 4411-ADCLGEO.                                                     
001100*                                 IDDC + GEO ADRESS                       
001200        05 4411-IDDC         PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400        05 4411-ADFLGEO      PIC X(3).                                    
001500*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
001600     03 4411-LOWVALUE        PIC X(21).                                   
001700*** END COPY WDGX4411    LENGTH=30                                        
