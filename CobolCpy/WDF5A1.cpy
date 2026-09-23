000100 01  SEQA-WDF5A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDF501             
000300*                                 CROSS-INDEX                             
000400*                                 FYSISK NYCKEL:  WDF5A1KY                
000500*                                 (IDLEVNR + IDLEVART +                   
000600*                                  IDARTNR + IDBENR)                      
000700     03 SEQA-IDLEVNR         PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001000     03 SEQA-IDLEVART        PIC X(30).                                   
001100*                                 LEVERANTÖRENS ARTNR                     
001200*                                 SUPPLIER PARTNO                         
001300     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 SEQA-IDBENR          PIC S9              COMP-3.                  
001700*                                 BENÄMNINGSNUMMER                        
001800*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
