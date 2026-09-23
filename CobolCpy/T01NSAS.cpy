000100* GENERATION OF COBOL HOST STRUCTURE FROM T01NSAS-TAB                     
000200  01 T01NSAS.                                                             
000300*              T01NSAS                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 KDFINDOC                          PIC X(4).                         
000700*              TYP FINANSIELLT DOKUMENT                                   
000800   03 KDPARTTY                          PIC X(3).                         
000900*              TYP AV BETALARE                                            
001000   03 KDPARTGR                          PIC X(15).                        
001100*              GRUPP AV BETALARE                                          
001200   03 IDLANDX3                          PIC X(3).                         
001300*              3-STÄLLIG LANDSBETECKNINGSKOD                              
001400   03 TIAA                              PIC S9(3) COMP-3.                 
001500*              ÅR    (ÅÅ)                                                 
001600   03 IDLOPNR                           PIC S9(3) COMP-3.                 
001700*              LÖPNUMMER                                                  
001800   03 DAREGDAT                          PIC X(8).                         
001900*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
002000   03 IDUSER                            PIC X(8).                         
002100*              ANVÄNDARENS SÄKERHETS ID                                   
002200*                                                                         
002300*** END OF VILMAII-COPY LENGTH= 49 OLD LENGTH=                            
