000100* GENERATION OF COBOL HOST STRUCTURE FROM PCCPYP-TAB                      
000200  01 PCCPYP.                                                              
000300*              PRICE CORRECTIONS                                          
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STƒLLIG LANDSBETECKNINGSKOD                              
000600   03 KDTARGTY                          PIC X(1).                         
000700*              TARGET TYPE                                                
000800   03 KDPSLTYP                          PIC X(1).                         
000900*              PRODUCT GROUP TYPE                                         
001000   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001100*              PRODUKTSLAG                                                
001200   03 DAAARP                            PIC S9(7) COMP-3.                 
001300*              ≈R - REDOVISNINGSPERIOD (≈≈≈≈RP)                           
001400*              12 PER ≈R                                                  
001500   03 TIAAAA                            PIC S9(5) COMP-3.                 
001600*              ≈RTAL (≈≈≈≈)                                               
001700   03 REPRISCH                          PIC S9(3)V9(2) COMP-3.            
001800*              PRISƒNDRING I PROCENT VID NY                               
001900*              PRISLISTA                                                  
002000*                                                                         
002100*** END OF VILMAII-COPY LENGTH= 16 OLD LENGTH=                            
