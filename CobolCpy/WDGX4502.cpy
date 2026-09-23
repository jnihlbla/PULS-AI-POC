000100 01  4502-WDGX4502.                                                       
000200*                                 STYRPARAMETRAR                          
000300*                                 ORDERRADSREGISTER                       
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (KDRAPRIO + LOWVALUE)                   
000600     03 4502-KDRAPRIO        PIC S9(3)           COMP-3.                  
000700*                                 PRIORITETSKOD PÅ RADEN                  
000800*                                 PRIORITY CODE ON THE LINE               
000900     03 4502-LOWVALUE        PIC X(3).                                    
001000     03 4502-BERAPRIO        PIC X(10).                                   
001100*                                 PRIORITETSBENÄMNING                     
001200*                                 PRIORITY DESCRIPTION                    
001300     03 4502-FLPRIO          PIC X.                                       
001400*                                 PRIORITERAD                             
001500*                                 HAS PRIORITY                            
001600     03 4502-KVVECKOR-TECK   PIC S9(3)           COMP-3.                  
001700*                                 ANTAL VECKOR FÖR HEL ROTÄCKNING         
001800*                                 TIME FOR TOTAL RESERVATION              
001900     03 4502-RELEVFOR        PIC S9V9(2)         COMP-3.                  
002000*                                 RELATIONSKOEFFICIENT RESTORDER          
002100*                                 BACK ORDER COEFFICIENT                  
002200     03 4502-REROFORD        PIC S9(3)           COMP-3.                  
002300*                                 RESTORDERFÖRDELNINGSFAKTOR              
002400*                                 BACK ORDER DIVISION FACTOR              
002500     03 FILLER               PIC X(3).                                    
002600*** END COPY WDGX4502C0  LENGTH=25                                        
