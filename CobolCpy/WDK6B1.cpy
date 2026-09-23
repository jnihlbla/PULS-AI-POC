000100 01  SEQB-WDK6B1.                                                         
000200*                                 ARTIKELREGISTER                         
000300*                                 SEKUNDÄRT INDEX TILL WDK601             
000400*                                 FUNKTIONSGRUPP + PRODUKTSLAG            
000500*                                 FYSISK NYCKEL: WDK6B1KY                 
000600*                                  (IDFKNGRP,KDPRODSL,IDLEVNR,            
000700*                                   IDARTNR)                              
000800*                                 SECONDARY NYCKEL: WDK6BSEQ              
000900*                                  (IDFKNGRP,KDPRODSL)                    
001000     03 SEQB-IDFKNGRP        PIC S9(5)           COMP-3.                  
001100*                                 FUNKTIONSGRUPP                          
001200*                                 FUNCTION GROUP                          
001300     03 SEQB-KDPRODSL        PIC S9(3)           COMP-3.                  
001400*                                 PRODUKTSLAG                             
001500*                                 PRODUCT GROUP                           
001600     03 SEQB-IDLEVNR         PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
