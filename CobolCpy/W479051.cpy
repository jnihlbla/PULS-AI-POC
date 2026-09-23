000100 01  W479051.                                                             
000200*                                 ORDERREGISTRERINGSDATUM F÷R             
000300*                                 ALLA KUNDORDER                          
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 IDKUNDRF             PIC X(10).                                   
001200*                                 KUNDENS REFERENS                        
001300     03 TIORDREG             PIC S9(7)           COMP-3.                  
001400*                                 ORDERREGISTRERINGSDATUM  ≈≈MMDD         
001500*** END COPY W479051CC0  LENGTH=24                                        
