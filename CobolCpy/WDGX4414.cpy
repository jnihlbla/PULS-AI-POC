000100 01  RUTA-WDGX4414.                                                       
000200*                                 4414 HTR DEFINITION AV                  
000300*                                 LASTNINGSOMRÅDE/RUTA                    
000400*                                 RUTSEGMENT                              
000500*                                 NYCKEL: WDGXKEY                         
000600*                                         (ADRUTNIV, LOWVALUE)            
000700*                                 SÖKBEGREPP: TESPAERR                    
000800     03 RUTA-ADRUTNIV        PIC S9(3)           COMP-3.                  
000900*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
001000     03 RUTA-LOWVALUE        PIC X(8).                                    
001100     03 RUTA-VLRUTNIV        PIC S9(3)           COMP-3.                  
001200*                                 VOLYM PÅ RUTA-NIVÅ (M3)                 
001300     03 RUTA-TESPAERR        PIC X(20).                                   
001400*                                 SPÄRRTEXT                               
001500*** END COPY WDGX4414C0  LENGTH=32                                        
