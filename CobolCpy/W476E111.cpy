000100 01  KND-W476E111.                                                        
000200*                                 TRANSPORTRELEASEREG.POST                
000300*                                 GODSMOTTAGARE                           
000400     03 KND-IDPTYP           PIC X(4).                                    
000500*                                 POSTTYP              IDPTYP-004         
000600     03 KND-IDSHIPM          PIC 9(7).                                    
000700*                                 SKEPPNINGSNUMMER                        
000800     03 KND-IDDISTR          PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 KND-IDKUNDNR         PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 KND-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 KND-BEGMT.                                                        
001500*                                 GODSMOTTAGARNAMN                        
001600        05 KND-BEGMT-RAD1    PIC X(35).                                   
001700*                                 GODSMOTTAGARNAMN RAD 1                  
001800        05 KND-BEGMT-RAD2    PIC X(35).                                   
001900*                                 GODSMOTTAGARNAMN RAD 2                  
002000     03 KND-ADGMT-GATA       PIC X(35).                                   
002100*                                 GODSMOTTAGARADRESS GATA                 
002200     03 KND-ADGMT-PADR       PIC X(35).                                   
002300*                                 GODSMOTTAGARADRESS POSTADRESS           
002400     03 KND-ADGMT-LAND       PIC X(35).                                   
002500*                                 GODSMOTTAGARADRESS LAND                 
002600*** END OF VILMAII-COPY LENGTH= 195 BYTES                                 
