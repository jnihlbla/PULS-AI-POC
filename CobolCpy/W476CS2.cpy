000100 01  SUBHDR-W476CS2.                                                      
000200*                                 COPYTEXT FOR CARGO SPEC SUBHEAD         
000300*                                 ER WEB-LDC                              
000400     03 SUBHDR-IDAFPRCD      PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 SUBHDR-RAD2-IDKUNDNR PIC Z(5)9.                                   
000700*                                 KUNDNUMMER                              
000800     03 SUBHDR-RAD2-BEGMT1   PIC X(35).                                   
000900*                                 GODSMOTTAGARNAMN RAD 1                  
001000     03 SUBHDR-RAD2-ADGMT-GATA                                            
001100                             PIC X(35).                                   
001200*                                 GODSMOTTAGARADRESS GATA                 
001300     03 SUBHDR-RAD3-BEGMT2   PIC X(35).                                   
001400*                                 GODSMOTTAGARNAMN RAD 2                  
001500     03 SUBHDR-RAD3-ADGMT-PADR                                            
001600                             PIC X(35).                                   
001700*                                 GODSMOTTAGARADRESS POSTADRESS           
001800     03 SUBHDR-RAD4-ADGMT-LAND                                            
001900                             PIC X(35).                                   
002000*                                 GODSMOTTAGARADRESS LAND                 
002100     03 SUBHDR-RAD4B-IDTFN   PIC X(20).                                   
002200*                                 TELEFONNUMMER EXTERNT                   
002300*** END OF VILMAII-COPY LENGTH= 211 BYTES                                 
