000100 01  W225P232.                                                            
000200*                                 POSTTYP 232 INFO OM AVBOKADE            
000300*                                 RADER MM                                
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KDCLAGER             PIC S9              COMP-3.                  
001000*                                 CENTRALLAGERKOD                         
001100     03 IDDISTR              PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 KDORDKL              PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500     03 REINKORD             PIC S9              COMP-3.                  
001600*                                 INKOMMEN ORDERRAD                       
001700     03 RERORAD              PIC S9V9(2)         COMP-3.                  
001800*                                 RESTNOTERAD MÄNGD (DEL AV RAD)          
001900     03 REFYSAVV             PIC S9V9(2)         COMP-3.                  
002000*                                 FYSISK AVVIKELSE                        
002100     03 REAVBRAD             PIC S9V9(2)         COMP-3.                  
002200*                                 AVBOKAD MÄNGD (DEL AV RAD)              
002300*** END COPY W225P232C0  LENGTH=20                                        
