000100 01  SEQA-WDB7A1.                                                         
000200*                                 KUNDREGISTER DEALER INFO                
000300*                                 SEKUNDÄRT INDEX TILL WDB701             
000400*                                 FYSISK NYCKEL: WDB7A1KY                 
000500*                                 (IDDEALER-VIPS + IDLANDX2 +             
000600*                                  IDDISTR + IDKUNDNR)                    
000700*                                 SEKUNDÄR NYCKEL: WDB7ASEQ               
000800*                                 (IDDEALER-VIPS + IDLANDX2)              
000900     03 SEQA-IDDEALER-VIPS   PIC X(6).                                    
001000*                                 VIPS ÅTERFÖRSÄLJARE                     
001100*                                 VIPS DEALER                             
001200     03 SEQA-IDLANDX2        PIC X(2).                                    
001300*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001400*                                 2-LETTER CODE FOR COUNTRY               
001500     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000*                                 CUSTOMER NO                             
002100*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
