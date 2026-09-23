000100 01  4512-WDGX4512.                                                       
000200*                                 ORDERRADSREGISTER                       
000300*                                 PRIORITETSTYRNINGSREGLER                
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (KDRAPRIO + KDTPOTYP +                  
000600*                                  KDORDKL  + IDDISTR  +                  
000700*                                  IDDISTR  + LOWVALUE)                   
000800*                                 SÖKFÄLT:    KDRAPRIO,                   
000900*                                  KDTPOTYP,  KDORDKL,                    
001000*                                  IDDISTRF,  IDDISTRT                    
001100     03 4512-KDRAPRIO        PIC S9(3)           COMP-3.                  
001200*                                 PRIORITETSKOD PÅ RADEN                  
001300*                                 PRIORITY CODE ON THE LINE               
001400     03 4512-KDTPOTYP        PIC S9              COMP-3.                  
001500*                                 TYP AV TIDPLANERAD ORDER                
001600*                                 TYPE OF TIME PLANNED ORDER              
001700     03 4512-KDORDKL         PIC S9              COMP-3.                  
001800*                                 ORDERKLASS                              
001900*                                 ORDER CLASS                             
002000     03 4512-IDDISTR-FOM     PIC S9(5)           COMP-3.                  
002100*                                 LÄGSTA DISTRIKTNR I INTERVALL           
002200*                                 LOWEST DISTRICT NUMBER                  
002300     03 4512-IDDISTR-TOM     PIC S9(5)           COMP-3.                  
002400*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002500*                                 HIGHEST DISTRICT NUMBER                 
002600*** END COPY WDGX4512C0  LENGTH=10                                        
