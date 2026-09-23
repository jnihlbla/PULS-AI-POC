000100 01  SEQA-WDF6A1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDF601             
000300*                                 PU-DIREKTLEVERANTÖRER                   
000400*                                 FYSISK NYCKEL:  WDF6A1KY                
000500*                                 (IDLEVNR + IDDISTR + KDORDKL +          
000600*                                  IDKUNDNR + IDPRODNR)                   
000700*                                 SEKUNDÄR NYCKEL:  WDF6ASEQ              
000800*                                 (IDLEVNR + IDDISTR + KDORDKL +          
000900*                                  IDKUNDNR)                              
001000     03 SEQA-IDLEVNR         PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 SEQA-KDORDKL         PIC S9              COMP-3.                  
001500*                                 ORDERKLASS                              
001600     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
001700*                                 KUNDNUMMER                              
001800     03 SEQA-IDPRODNR        PIC S9(7)           COMP-3.                  
001900*                                 PRODUKTIONSNUMMER                       
002000*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
