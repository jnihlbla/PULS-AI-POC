000100 01  W476BL1.                                                             
000200*                                 COPYTEXT TILL BOD WEB (HUVUD)           
000300     03 IDAFPRCD             PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 IDKUNDNR             PIC Z(7).                                    
000600*                                 KUNDNUMMER                              
000700     03 KDORDKL              PIC Z.                                       
000800*                                 ORDERKLASS                              
000900     03 IDORDNR              PIC Z(5).                                    
001000*                                 ORDERNUMMER UTGÅR PD90                  
001100     03 DATUM                PIC 9(6).                                    
001200     03 IDLBBET              PIC X(12).                                   
001300*                                 LASTBÄRARBETECKNING                     
001400     03 BEGMT.                                                            
001500*                                 GODSMOTTAGARNAMN                        
001600        05 BEGMT-RAD1        PIC X(35).                                   
001700*                                 GODSMOTTAGARNAMN RAD 1                  
001800        05 BEGMT-RAD2        PIC X(35).                                   
001900*                                 GODSMOTTAGARNAMN RAD 2                  
002000     03 ADGMT-GATA           PIC X(35).                                   
002100*                                 GODSMOTTAGARADRESS GATA                 
002200     03 ADGMT-PADR           PIC X(35).                                   
002300*                                 GODSMOTTAGARADRESS POSTADRESS           
002400*** END OF VILMAII-COPY LENGTH= 181 BYTES                                 
