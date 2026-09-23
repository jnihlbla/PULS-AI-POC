000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***  - OUTPUT UR FAKTURERINGEN               
000400*                            *************************************        
001100 01  DIST73-IDDISTR          PIC 9(5)    COMP-3.                          
001200*                                                                         
001300*    TOTALLISTE FÖR TYSKLAND OCH FAKTUROR FÖR STIC                        
001500*                                                                         
005030       88  DIST73-TYSKL-TOT  VALUE    2278.                               
005100*                                                                         
005110       88  DIST73-STIC-PV    VALUE    1470 1478.                          
005120*                                                                         
005200*** END COPY WWDIST73    LENGTH=0                                         
