000100 01  WDGX01DC-CTX.                                                        
000200*                                 ROTSEGMENT HÄNDELSEDATABASER:           
000300*                                 (WDG2/3/6, WDR1/2/4/5, W6G1/2)          
000400*                                 NYCKEL WDGXKEY:                         
000500*                                 (IDHTYP + IDDC + 24 LOW-VALUE)          
000600     03 IDHTYP               PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 NYCKEL-VALFRI        PIC X(24).                                   
001100*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
