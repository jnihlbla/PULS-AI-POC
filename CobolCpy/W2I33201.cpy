000100 01  MID-W2I33201.                                                        
000200*                                 MID-COPYTEXT FÖR W20332                 
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-CMD              PIC X.                                       
000600     03 MID-IDDIRGRP-E       PIC X(10).                                   
000700*                                 DIREKTLEVERANSGRUPP                     
000800     03 MID-TISTADAT-E       PIC 9(6).                                    
000900*                                 GENERELLT STARTDATUM                    
001000     03 MID-BELEV            PIC X(35).                                   
001100*                                 LEVERANTÖRSNAMN                         
001200     03 MID-TABELLRAD        OCCURS 36 TIMES.                             
001300*                                 GRUPP MED TABELL RADER                  
001400        05 MID-IDDIRGRP      PIC X(10).                                   
001500*                                 DIREKTLEVERANSGRUPP                     
001600        05 MID-TISTADAT      PIC 9(6).                                    
001700*                                 GENERELLT STARTDATUM                    
001800*** END OF VILMAII-COPY LENGTH= 633 BYTES                                 
