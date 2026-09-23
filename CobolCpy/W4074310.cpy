000100 01  W407-W4074310.                                                       
000200*                                 COPYTEXT FÖR SUBPGM W4074310            
000300     03 W407-IDPRTLST        PIC X(8).                                    
000400*                                 LOGISK PRINTER+LISTA IDENTITET          
000500*                                 LOGICAL PRINTER+LIST IDENTITY           
000600     03 W407-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 W407-IDRETSND.                                                    
001000*                                 RETURSÄNDNING                           
001100*                                 RETURN TRANSFER                         
001200        05 W407-IDRT         PIC X(3).                                    
001300*                                 RETURTERMINAL                           
001400*                                 RETURN TERMINAL                         
001500        05 W407-IDRTLOP      PIC 9(3).                                    
001600*                                 RETUR TERMINAL LÖPNUMMER                
001700*                                 RETURN TERMINAL SEQUENCE NUMBER         
001800*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
