000100 01  SEQB-WDB2B1.                                                         
000200*                                 KUNDREGISTER GODSMOTTAGARE              
000300*                                 SEKUNDÄRT INDEX TILL WDB201             
000400*                                 FYSISK NYCKEL: WDB2B1KY                 
000500*                                 (IDLEVNR + IDDISTR + IDKUNDNR)          
000600*                                 SEKUNDÄR NYCKEL: WDB2BSEQ               
000700*                                 (IDLEVNR)                               
000800     03 SEQB-IDLEVNR         PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001100     03 SEQB-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQB-IDKUNDNR        PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
