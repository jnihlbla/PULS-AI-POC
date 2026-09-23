000100 01  4305-WDGX4305.                                                       
000200*                                 4305 LÅSNINGSREGISTER                   
000300*                                 NYCKEL: WDGXKEY                         
000400*                                         (IDHTYP, IDDC,                  
000500*                                          LOWVALUE)                      
000600*                                 SÖKBEGREPP: IDHTYP                      
000700     03 4305-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 4305-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 4305-LOWVALUE        PIC X(24).                                   
001200*** END COPY WDGX4305    LENGTH=30                                        
