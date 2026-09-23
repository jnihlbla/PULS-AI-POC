000100 01  SEQC-WDB2C1.                                                         
000200*                                 KUNDREGISTER GODSMOTTAGARE              
000300*                                 SEKUNDÄRT INDEX TILL WDB201             
000400*                                 FYSISK NYCKEL: WDB2C1KY                 
000500*                                 (IDDC-BULK+IDDISTR+IDKUNDNR)            
000600*                                 SEKUNDÄR NYCKEL: WDB2CSEQ               
000700*                                 (IDDC-BULK)                             
000800     03 SEQC-IDDC-BULK       PIC X(2).                                    
000900*                                 IDENTIFIERARE BULKORDERLAGER            
001000*                                 WAREHOUSE IDENTIFIER BULK               
001100*                                 ORDERS                                  
001200     03 SEQC-IDDISTR         PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 SEQC-IDKUNDNR        PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800*** END OF VILMAII-COPY LENGTH= 9 BYTES                                   
