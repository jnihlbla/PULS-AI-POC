000100 01  4445-WDGX4445.                                                       
000200*                                 PRODUKTIONSKANALSSTYRTABEL              
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                   (IDHTYP + IDDC +                      
000500*                                   IDPRCTAB + LOW-VALUE)                 
000600     03 4445-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4445-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4445-IDPRCTAB        PIC 9(2).                                    
001200*                                 PRCTABELLIDENTITET                      
001300*                                 PRC TABLE IDENTITY                      
001400     03 4445-LOW-VALUE       PIC X(22).                                   
001500*** END COPY WDGX4445    LENGTH=30                                        
