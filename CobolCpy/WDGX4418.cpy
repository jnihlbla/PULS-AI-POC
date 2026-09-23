000100 01  STAELL-WDGX4418.                                                     
000200*                                 DEFINITION AV STÄLLAGE/NIVÅ             
000300*                                 NYCKEL: WDGXKEY                         
000400*                                         (ADFLOMR, ADRUTNIV,             
000500*                                          DIHMODUL, DIDMODUL,            
000600*                                          LOWVALUE)                      
000700     03 STAELL-ADFLOMR       PIC S9(3)           COMP-3.                  
000800*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
000900     03 STAELL-ADRUTNIV      PIC S9(3)           COMP-3.                  
001000*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
001100     03 STAELL-DIHMODUL      PIC S9(3)           COMP-3.                  
001200*                                 MODUL-HÖJD                              
001300     03 STAELL-DIDMODUL      PIC S9(3)           COMP-3.                  
001400*                                 MODUL-DJUP                              
001500     03 STAELL-LOWVALUE      PIC X(2).                                    
001600     03 STAELL-TBSPAERR      PIC X(50).                                   
001700*                                 ANGER I BIT-MAPSFORM OM EN              
001800*                                 MODUL ÄR LEDIG ELLER UPPTAGEN.          
001900*                                 EN BIT PER MODUL I NIVÅN PÅ             
002000*                                 STÄLLAGE. VARJE NIVÅ HAR                
002100*                                 400 MODULER.                            
002200*                                 BITEN = 1 = UPPTAGEN                    
002300*                                 BITEN = 0 = LEDIG                       
002400     03 STAELL-VLRUTNIV      PIC S9(3)           COMP-3.                  
002500*                                 VOLYM PÅ RUTA-NIVÅ (M3)                 
002600*** END COPY WDGX4418C0  LENGTH=62                                        
