000100 01  4537-WDGX4537-CTX.                                                   
000200*                                 PLANERAD PRODUKTION/TRP                 
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDHTYP + IDDC +                        
000500*                                  IDTRPLOS + LOW-VALUE)                  
000600     03 4537-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4537-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4537-IDTRPLOS        PIC X(3).                                    
001200*                                 TRANSPORTLÖSNING                        
001300*                                 TRANSPORTSOLUTION                       
001400     03 4537-LOW-VALUE       PIC X(21).                                   
001500*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
