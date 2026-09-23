000100 01  MID-W4I40501.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-INPUT.                                                        
000600        05 MID-FLARTDC-UPD   PIC X.                                       
000700*                                 INDIKERAR OM ART SKA VARA PÅ DC         
000800        05 MID-KDBEHX        PIC X.                                       
000900*                                 BEHANDLINGSKOD-X                        
001000        05 MID-KDANMORS-UPD  PIC X(2).                                    
001100*                                 ORSAK TILL LEVERANSANMÄRKNING           
001200        05 MID-IDFKNGRP-UPD  PIC 9(5).                                    
001300*                                 FUNKTIONSGRUPP                          
001400        05 MID-IDARTNR-UPD   PIC 9(9).                                    
001500*                                 ARTIKELNUMMER                           
001600        05 MID-IDDC-EXCP-UPD PIC X(2).                                    
001700*                                 DC FÖR RETUR EJ TILLÅTET                
001800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
