000100* GENERATION OF COBOL HOST STRUCTURE FROM CPNCCPP-TAB                     
000200  01 CPNCCPP.                                                             
000300*              CAR PARK / NEW CAR REPORT                                  
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 KDTARGTY                          PIC X(1).                         
000700*              TARGET TYPE                                                
000800   03 KDPRODSL                          PIC S9(3) COMP-3.                 
000900*              PRODUKTSLAG                                                
001000   03 DAAARP                            PIC S9(7) COMP-3.                 
001100*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
001200*              12 PER ÅR                                                  
001300   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
001400*              VÄRDE TILL DEALER NET                                      
001500   03 KVCARPAR-UNWE                     PIC S9(9) COMP-3.                 
001600*              CAR PARK ANTAL EJ VIKTAT                                   
001700   03 KVCARPAR-WE                       PIC S9(9) COMP-3.                 
001800*              CAR PARK ANTAL DUBBELVIKTAT                                
001900   03 REPRISCH                          PIC S9(3)V9(2) COMP-3.            
002000*              PRISÄNDRING I PROCENT VID NY                               
002100*              PRISLISTA                                                  
002200*                                                                         
002300*** END OF VILMAII-COPY LENGTH= 30 OLD LENGTH=                            
