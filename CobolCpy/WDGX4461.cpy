000100 01  4461-WDGX4461.                                                       
000200*                                 LÖPNUMMER PLOCKSATSER                   
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                   (IDHTYP + IDDC +                      
000500*                                    LOW-VALUE)                           
000600     03 4461-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4461-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4461-LOW-VALUE       PIC X(24).                                   
001200*** END COPY WDGX4461    LENGTH=30                                        
