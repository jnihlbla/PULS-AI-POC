000100 01  VTH-W510VTH.                                                         
000200*                                 LÄNKAREA FÖR VTH-BESTÄMNING             
000300*                                                                         
000400*                                 SKICKA MED IDDC, IDARTNR, IDLEV         
000500*                                 NR, IDFKNGRP, BEFT                      
000600*                                 TILLBAKA FÅR MAN KDVTH 1 - 9            
000700*                                                                         
000800     03 VTH-IDDC             PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 VTH-IDARTNR          PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 VTH-IDLEVNR          PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 VTH-IDFKNGRP         PIC S9(5)           COMP-3.                  
001500*                                 FUNKTIONSGRUPP                          
001600     03 VTH-BEFT             PIC S9(3)           COMP-3.                  
001700*                                 FÖRPACKNINGSTYP                         
001800     03 VTH-KDVTH            PIC S9              COMP-3.                  
001900*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
002000*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
