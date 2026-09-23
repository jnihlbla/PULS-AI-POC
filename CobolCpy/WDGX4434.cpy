000100 01  4434-WDGX4434.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 TRANSPORTAVGÅNGAR                       
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 (IDTRP + TITRPAVG + LOW-VALUE)          
000600     03 4434-IDTRP.                                                       
000700*                                 TRANSPORTIDENTITET                      
000800*                                 TRANSPORTIDENTITY                       
000900        05 4434-IDTRPLOS     PIC X(3).                                    
001000*                                 TRANSPORTLÖSNING                        
001100*                                 TRANSPORTSOLUTION                       
001200        05 4434-IDTRPVAR     PIC X(2).                                    
001300*                                 TRANSPORTLÖSNINGSGRUPP                  
001400*                                 TRANSPORTSOLUTIONGROUP                  
001500     03 4434-TITRPAVG        PIC S9(7)           COMP-3.                  
001600*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
001700*                                 TRANSPORTDEPARTURETIME, VVDTTMM         
001800     03 4434-LOW-VALUE       PIC X.                                       
001900     03 4434-BETRPFIR        PIC X(15).                                   
002000*                                 TRANSPORTFIRMANS NAMN                   
002100*                                 NAME OF THE TRANSPORTCOMPANY            
002200     03 4434-KDFARLIG        PIC S9              COMP-3.                  
002300*                                 KOD FÖR FARLIGT GODS                    
002400*                                 DANGEROUS GOODS CODE                    
002500     03 4434-VLTRPMIN        PIC S9(3)           COMP-3.                  
002600*                                 MINSTA TILLÅTNA VOLYM I M3              
002700*                                 MINIMUM VOLUME PERMITTED IN M3          
002800     03 4434-FILLER          PIC X(2).                                    
002900*** END COPY WDGX4434C0  LENGTH=30                                        
