000100 01  SEQA-WDB2A1.                                                         
000200*                                 KUNDREGISTER GODSMOTTAGARE              
000300*                                 SEKUNDÄRT INDEX TILL WDB201             
000400*                                 FYSISK NYCKEL: WDB2A1KY                 
000500*                                 (IDPARTNR + IDFTG +                     
000600*                                  IDDISTR + IDKUNDNR)                    
000700*                                 SEKUNDÄR NYCKEL: WDB2ASEQ               
000800*                                 (IDPARTNR + IDFTG +                     
000900*                                  IDDISTR + IDKUNDNR)                    
001000     03 SEQA-IDPARTNR        PIC X(9).                                    
001100*                                 PARTNERNUMMER                           
001200*                                 PARTNER NO                              
001300     03 SEQA-IDFTG           PIC 9(2).                                    
001400*                                 FÖRETAGSID EKONOM REDOVISNING           
001500*                                 COMPANY IDENTITY ACCOUNTING             
001600     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
