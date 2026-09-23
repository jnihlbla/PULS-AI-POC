000100 01  MID-R22-W0IR2201.                                                    
000200*                                 COPYTEXT FÖR MID W0IR2201               
000300*                                                                         
000400     03 MID-R22-IDPTYP       PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 MID-R22-IDARTNR      PIC X(8).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-R22-IDBEST-1     PIC 9(3).                                    
000900     03 MID-R22-IDBEST-2     PIC 9(6).                                    
001000     03 MID-R22-IDBEST-3     PIC 9(3).                                    
001100     03 MID-R22-IDLEVNR-BEST PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MID-R22-TIBEST       PIC X(6).                                    
001400*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
001500     03 MID-R22-KVBEST       PIC X(7).                                    
001600*                                 BESTÄLLT ANTAL                          
001700     03 MID-R22-KDBEH-BEST   PIC X.                                       
001800*                                 BEHANDLINGSKOD BESTÄLLNING              
001900*** END COPY W0IR2201    LENGTH=42                                        
