000100 01  1116-WDGX1116.                                                       
000200*                                 ERSÄTTNINGSBEVAKNING                    
000300*                                 NYA ARTIKLAT FRÅN PV/LV                 
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                  (IDARTNR)                              
000600     03 1116-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 1116-KDERS-OLD       PIC S9(3)           COMP-3.                  
001000*                                 ERSÄTTNINGSKOD                          
001100*                                 SUPERSESSION CODE                       
001200     03 1116-KDERS-NEW       PIC S9(3)           COMP-3.                  
001300*                                 ERSÄTTNINGSKOD                          
001400*                                 SUPERSESSION CODE                       
001500     03 1116-TIREGDAT        PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700*                                 REGISTRATION DATE (YYMMDD)              
001800     03 FILLER               PIC X(12).                                   
001900*** END COPY WDGX1116C0  LENGTH=25                                        
