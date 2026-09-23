000100 01  W2132222.                                                            
000200*                                 POSTER FÖR UPPD AV HÄNDELSEBAS          
000300*                                                                         
000400     03 IDHTYP               PIC X(4).                                    
000500*                                 HÄNDELSETYP                             
000600     03 WDGX2222.                                                         
000700*                                 BYTE HUVUDLEVERANTÖR                    
000800*                                 (TRANSAKTION R01)                       
000900        05 IDARTNR           PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100        05 IDLEVNR           PIC S9(5)           COMP-3.                  
001200*                                 LEVERANTÖRNUMMER                        
001300        05 IDSYSTEM          PIC X(4).                                    
001400*                                 VOLVO VCAS SYSTEMNUMMER                 
001500        05 FILLER            PIC X(13).                                   
001600*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
