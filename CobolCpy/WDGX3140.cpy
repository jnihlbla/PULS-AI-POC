000010 01  3140-WDGX3140.                                                       
000020*                                 BYTES                                   
000030*                                 KVITTNINGSTABELL                        
000040*                                 FYSISK NYCKEL: WDGXKEY                  
000050*                                 (IDARTNR-BYT + IDDISTR +                
000060*                                  IDTABNR)                               
000070*                                 S÷KBEGREPP: IDARTNR,                    
000080*                                 IDDISTR, IDTABNR)                       
000090     03 3140-IDARTNR-BYT     PIC S9(9)           COMP-3.                  
000100*                                 ARTIKELNUMMER F÷R BYTES                 
000110*                                 EXCHANGE PART NUMBER                    
000120     03 3140-IDDISTR         PIC S9(5)           COMP-3.                  
000130*                                 DISTRIKTNUMMER                          
000140*                                 DISTRICT NUMBER                         
000150     03 3140-IDTABNR         PIC S9(3)           COMP-3.                  
000160*                                 TABELLNUMMER                            
000170*                                 TABELNUMBER                             
000180     03 3140-TIREGDAT        PIC S9(7)           COMP-3.                  
000190*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000200*                                 REGISTRATION DATE (YYMMDD)              
000210     03 FILLER               PIC X(46).                                   
      *** END COPY WDGX3140    LENGTH=60                                        
