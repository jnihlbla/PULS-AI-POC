000100 01  4451-WDGX4451.                                                       
000200*                                 PRODUKTIONSTIDS TABELL                  
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                   (IDHTYP + IDDC +                      
000500*                                    IDPTIDTAB + LOW-VALUE)               
000600     03 4451-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4451-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4451-IDPTIDTAB       PIC 9(2).                                    
001200*                                 PRODUKTIONSTIDTABELLSIDENTITET          
001300*                                 PRODUCTION TIME TABLE IDENT.            
001400     03 4451-LOW-VALUE       PIC X(22).                                   
001500*** END COPY WDGX4451    LENGTH=30                                        
