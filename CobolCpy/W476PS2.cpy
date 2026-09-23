000100 01  SUBHDR-W476PS2.                                                      
000200*                                 COPYTEXT FOR PACKING SPEC SUBHE         
000300*                                 ADER WEB-LDC                            
000400     03 SUBHDR-IDAFPRCD      PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 SUBHDR-RAD1-IDKUNDNR PIC Z(7).                                    
000700*                                 KUNDNUMMER                              
000800     03 SUBHDR-RAD1-BEGMT-RAD1                                            
000900                             PIC X(35).                                   
001000*                                 GODSMOTTAGARNAMN RAD 1                  
001100     03 SUBHDR-RAD1-ADGMT-GATA                                            
001200                             PIC X(35).                                   
001300*                                 GODSMOTTAGARADRESS GATA                 
001400     03 SUBHDR-RAD2-BEGMT-RAD2                                            
001500                             PIC X(35).                                   
001600*                                 GODSMOTTAGARNAMN RAD 2                  
001700     03 SUBHDR-RAD2-ADGMT-PADR                                            
001800                             PIC X(35).                                   
001900*                                 GODSMOTTAGARADRESS POSTADRESS           
002000     03 SUBHDR-RAD3-ADGMT-LAND                                            
002100                             PIC X(35).                                   
002200*                                 GODSMOTTAGARADRESS LAND                 
002300*** END OF VILMAII-COPY LENGTH= 192 BYTES                                 
