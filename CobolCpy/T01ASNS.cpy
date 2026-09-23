000100* GENERATION OF COBOL HOST STRUCTURE FROM T01ASNS-TAB                     
000200  01 T01ASNS.                                                             
000300*              T01ASNS                                                    
000400   03 IDLEGSEL   PIC X(4).                                                
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 KDFINDOC   PIC X(4).                                                
000700*              TYP FINANSIELLT DOKUMENT                                   
000800   03 KDPARTTY   PIC X(3).                                                
000900*              TYP AV BETALARE                                            
001000   03 KDPARTGR   PIC X(15).                                               
001100*              GRUPP AV BETALARE                                          
001200   03 IDLANDX3   PIC X(3).                                                
001300*              3-STÄLLIG LANDSBETECKNINGSKOD                              
001400   03 IDLOPNR    PIC S9(3) COMP-3.                                        
001500*              LÖPNUMMER                                                  
001600   03 DAREGDAT   PIC X(8).                                                
001700*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
001800   03 IDUSER     PIC X(8).                                                
001900*              ANVÄNDARENS SÄKERHETS ID                                   
002000*                                                                         
002100*** END OF VILMAII-COPY LENGTH= 47 OLD LENGTH=                            
