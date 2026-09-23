000100 01  4417-WDGX4417.                                                       
000200*                                 ROT FÖR LAGRING                         
000300*                                 AV DEFINITION AV STÄLLAGE/NIVÅ          
000400*                                 NYCKEL: WDGXKEY                         
000500*                                         (IDHTYP, IDDC,                  
000600*                                          ADFLGEO, LOWVALUE)             
000700*                                 SÖKBEGREPP: IDHTYP                      
000800     03 4417-IDHTYP          PIC X(4).                                    
000900*                                 HÄNDELSETYP                             
001000     03 4417-ADCLGEO.                                                     
001100*                                 IDDC + GEO ADRESS                       
001200        05 4417-IDDC         PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400        05 4417-ADFLGEO      PIC X(3).                                    
001500*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
001600     03 4417-LOWVALUE        PIC X(21).                                   
001700*** END COPY WDGX4417    LENGTH=30                                        
