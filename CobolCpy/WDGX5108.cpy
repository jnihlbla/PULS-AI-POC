000100 01  5108-WDGX5108-CTX.                                                   
000200*                                 STYRPARAMETRAR STDPRISPROCESS           
000300*                                 FYSISK NYCKEL: IDFTG                    
000400     03 5108-IDFTG           PIC 9(2).                                    
000500*                                 FÖRETAGSID EKONOM REDOVISNING           
000600*                                 COMPANY IDENTITY ACCOUNTING             
000700     03 5108-IDUSER          PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900*                                 USER SECURITY-IDENTITY                  
001000     03 5108-REEMBINF        PIC S9(3)V9(2)      COMP-3.                  
001100*                                 INFLATIONSFAKTOR EMBALAGE               
001200*                                 INFLATION FACTOR FOR EMBALAGE           
001300     03 5108-SULSNIV-MIN     PIC S9(11).                                  
001400*                                 LÄGSTA GRÄNS FÖR LS ÄNDRING             
001500*                                 LOWEST LIMIT FOR LS CHANGES             
001600     03 5108-SULSNIV-MAX     PIC S9(11).                                  
001700*                                 HÖGSTA GRÄNS FÖR LS ÄNDRING             
001800*                                 HIGHEST LIMIT FOR LS CHANGES            
001900     03 5108-TIUPPDAT        PIC S9(7)           COMP-3.                  
002000*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002100*                                 UPDATING DATE     (YYMMDD)              
002200*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
