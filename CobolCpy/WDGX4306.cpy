000100 01  4306-WDGX4306.                                                       
000200*                                 LÅSNINGSREGISTER                        
000300*                                 NYCKEL:  WDGXKEY                        
000400*                                          (IDPRODNR, LOWVALUE)           
000500     03 4306-IDPRODNR        PIC S9(7)           COMP-3.                  
000600*                                 PRODUKTIONSNUMMER                       
000700     03 4306-LOWVALUE        PIC X(6).                                    
000800     03 4306-FLANNULL        PIC X.                                       
000900*                                 ANNULLATION ?                           
001000     03 4306-KDPACLAS        PIC S9              COMP-3.                  
001100*                                 LÅSNING AV PACKNINGEN                   
001200*                                 0 = OLÅST                               
001300*                                 1 = ORDEVIS VALD                        
001400*                                 2 = PÅBÖRJAD AV ORDERVIS                
001500*                                 3 = PACKAD AV ORDERVIS                  
001600*                                 4 =                                     
001700*                                 5 = KOLLIVIS VALD                       
001800*** END COPY WDGX4306C0  LENGTH=12                                        
