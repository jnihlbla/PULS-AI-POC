000100 01  SEQB-WDF5B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDF501             
000300*                                 CROSS-INDEX                             
000400*                                 FYSISK NYCKEL: WDF5B1KY                 
000500*                                 (IDLEVART + IDARTNR +                   
000600*                                  IDLEVNR + IDBENR)                      
000700*                                 SEKUNDÄR NYCKEL: WDF5BSEQ               
000800*                                 (IDLEVART)                              
000900     03 SEQB-IDLEVART        PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTNR                     
001100*                                 SUPPLIER PARTNO                         
001200     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 SEQB-IDLEVNR         PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800     03 SEQB-IDBENR          PIC S9              COMP-3.                  
001900*                                 BENÄMNINGSNUMMER                        
002000*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
