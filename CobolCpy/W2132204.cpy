000100 01  W2132204.                                                            
000200*                                 POSTER FÖR UPPD AV HÄNDELSEBAS          
000300*                                                                         
000400     03 IDHTYP               PIC X(4).                                    
000500*                                 HÄNDELSETYP                             
000600     03 WDGX2204.                                                         
000700*                                 LOG. REG. 2204 LEVERANSPLAN             
000800*                                 KONTROLL ORSAK > 50                     
000900*                                 OMSPEC ORSAK < 50                       
001000        05 IDARTNR           PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200        05 KDLPORS           PIC S9(3)           COMP-3.                  
001300*                                 LEVERANSPLANEORSAK                      
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 FILLER               PIC X.                                       
001700*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
