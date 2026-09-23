000100* GENERATION OF COBOL HOST STRUCTURE FROM T01PATE-TAB                     
000200  01 T01PATE.                                                             
000300*              T01PATE                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 IDSPRAK                           PIC X(2).                         
000700*              2-STÄLLIG ISO SPRÅKKOD                                     
000800   03 KDBETALV                          PIC X(4).                         
000900*              BETALNINGSVILLKOR KUNDRESKONTRA                            
001000   03 BEBETVIL                          PIC X(30).                        
001100*              BETALNINGSVILLKORSTEXT                                     
001200   03 DAREGDAT                          PIC X(8).                         
001300*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
001400   03 DAUPPDAT                          PIC X(8).                         
001500*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
001600   03 IDUSER                            PIC X(8).                         
001700*              ANVÄNDARENS SÄKERHETS ID                                   
001800*                                                                         
001900*** END OF VILMAII-COPY LENGTH= 64 OLD LENGTH=                            
