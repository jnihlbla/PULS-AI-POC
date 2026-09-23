000100 01  RTN-WDP112.                                                          
000200*                                 RUTIN-INFO I SUBMIT-DATABAS             
000300*                                 FYSISK NYCKEL IDRUTIN                   
000400     03 RTN-IDRUTIN          PIC X(8).                                    
000500*                                 RUTINNAMN (GRUPP AV JOBB)               
000600     03 RTN-TIREGDAT         PIC S9(7)           COMP-3.                  
000700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000800     03 RTN-TIUPPDAT         PIC S9(7)           COMP-3.                  
000900*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
001000     03 RTN-BERUTIN          PIC X(25).                                   
001100*                                 RUTIN BESKRIVNING                       
001200     03 RTN-FILLER           PIC X(43).                                   
001300*** END COPY WDP112CCC0  LENGTH=84                                        
