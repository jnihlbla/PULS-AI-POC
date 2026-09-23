000100 01  RENS-W461051.                                                        
000200*                                 RENSNINGS POST TILL NOAC PT-051         
000300     03 RENS-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 RENS-KDCLAGER        PIC S9              COMP-3.                  
000600*                                 CENTRALLAGERKOD                         
000700     03 RENS-IDDISTR         PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 RENS-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 RENS-IDORDNR         PIC S9(7)           COMP-3.                  
001200*                                 ORDERNR             IDORDNR-002         
001300     03 RENS-BEVOLREF        PIC X(10).                                   
001400*                                 VOLVO REFERENS                          
001500     03 RENS-TIORDREG        PIC S9(7)           COMP-3.                  
001600*                                 ORDERREGISTRERINGSDATUM  ≈≈MMDD         
001700     03 FILLER               PIC X(6).                                    
001800*** END COPY W461051CC0  LENGTH=35                                        
