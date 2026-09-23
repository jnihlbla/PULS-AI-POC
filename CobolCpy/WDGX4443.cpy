000100 01  4443-WDGX4443.                                                       
000200*                                 PRODUKTIONSKLASSTABELL                  
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                   (IDHTYP + IDDC +                      
000500*                                   IDPKLTAB + LOW-VALUE)                 
000600     03 4443-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4443-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4443-IDPKLTAB        PIC X(2).                                    
001200*                                 PRODUKTIONSKLASSTABELLSID               
001300*                                 PRODUCTION CLASS TABLE ID               
001400     03 4443-LOW-VALUE       PIC X(22).                                   
001500*** END COPY WDGX4443    LENGTH=30                                        
