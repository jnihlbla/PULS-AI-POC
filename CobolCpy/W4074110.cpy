000100 01  W407-W4074110.                                                       
000200*                                 COPYTEXT FÖR SUBPGM W4074110            
000300     03 W407-IDPRTLST        PIC X(8).                                    
000400*                                 LOGISK PRINTER+LISTA IDENTITET          
000500*                                 LOGICAL PRINTER+LIST IDENTITY           
000600     03 W407-IDRETSND.                                                    
000700*                                 RETURSÄNDNING                           
000800*                                 RETURN TRANSFER                         
000900        05 W407-IDRT         PIC X(3).                                    
001000*                                 RETURTERMINAL                           
001100*                                 RETURN TERMINAL                         
001200        05 W407-IDRTLOP      PIC 9(3).                                    
001300*                                 RETUR TERMINAL LÖPNUMMER                
001400*                                 RETURN TERMINAL SEQUENCE NUMBER         
001500*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
