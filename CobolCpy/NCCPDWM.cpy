000100* GENERATION OF COBOL HOST STRUCTURE FROM NCCPDWM-TAB                     
000200  01 NCCPDWM.                                                             
000300*              NEW CAR SALES AND CAR PARK HEAD                            
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 KDTARGTY                          PIC X(1).                         
000700*              TARGET TYPE                                                
000800   03 IDDEALER                          PIC X(6).                         
000900*              DEALER KUNDNUMMER                                          
001000   03 DAFSGVV                           PIC S9(7) COMP-3.                 
001100*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
001200   03 BECARMOD                          PIC X(10).                        
001300*              BILMODELL                                                  
001400   03 KDFUEL                            PIC X(1).                         
001500*              BRÄNSLETYP                                                 
001600   03 KDTURBO                           PIC X(1).                         
001700*              TURBO ?                                                    
001800   03 IDPID-MAIN                        PIC X(3).                         
001900*              PID MAIN TYPE                                              
002000   03 IDPID-ENG                         PIC X(2).                         
002100*              PID ENGINE                                                 
002200   03 IDPID-SV                          PIC X(2).                         
002300*              PID SALES VERSION                                          
002400   03 KVCARPAR-UNWE                     PIC S9(9) COMP-3.                 
002500*              CAR PARK ANTAL EJ VIKTAT                                   
002600   03 KVCARPAR-WE                       PIC S9(9) COMP-3.                 
002700*              CAR PARK ANTAL DUBBELVIKTAT                                
002800*                                                                         
002900*** END OF VILMAII-COPY LENGTH= 42 OLD LENGTH=                            
