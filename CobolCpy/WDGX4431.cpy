000100 01  4431-WDGX4431.                                                       
000200*                                 TRANSPORT-TABELL ROT                    
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                   (IDHTYP + IDDC+                       
000500*                                    LOW-VALUE)                           
000600     03 4431-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4431-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4431-LOW-VALUE       PIC X(24).                                   
001200*** END COPY WDGX4431    LENGTH=30                                        
