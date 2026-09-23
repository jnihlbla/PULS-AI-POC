000100 01  BENAEMNING-W12103.                                                   
000200*                                 BENÄMNINGAR MED FELFLAGGADE             
000300*                                 ARTIKLAR OCH REGISTRERADE               
000400*                                 BENÄMNINGAR SOM "VÄNTAR PÅ              
000500*                                 NAMNLEX" .                              
000600     03 BENAEMNING-IDARTNR   PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 BENAEMNING-BEART     PIC X(25).                                   
000900*                                 ARTIKELBENÄMNING                        
001000     03 BENAEMNING-KDFEL     PIC S9(3)           COMP-3.                  
001100*                                 FELKOD                                  
001200     03 BENAEMNING-TIUPPDAT-STOP                                          
001300                             PIC S9(7)           COMP-3.                  
001400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001500*** END COPY W12103CCC0  LENGTH=36                                        
