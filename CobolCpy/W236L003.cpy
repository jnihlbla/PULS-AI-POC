000100 01  W236L003.                                                            
000200*                                 COPYTEXT FÖR LÄSNING AV                 
000300*                                 LEVERANS-PLAN.                          
000400     03 KDCALL               PIC S9(3)           COMP-3.                  
000500      88 LAS-LEV-BESK        VALUE +105.                                  
000600     03 KDSVAR               PIC X.                                       
000700      88 LEV-BESK-FINNS      VALUE ' '.                                   
000800*                                 SVAR FRÅN SUBPROGRAM                    
000900     03 IO-AREA.                                                          
001000*                                 WDD908                                  
001100        05 TILEVBSK-AVS      PIC S9(5)           COMP-3.                  
001200*                                 AVSÄNDNINGSVECKA     (ÅÅVV)             
001300*                                 ENL LEVERANSBESKED                      
001400        05 KVAVIS-BSKKVAR    PIC S9(7)           COMP-3.                  
001500*                                 LEVERANSBESKEDANTAL EFTER               
001600*                                 AVBOKNING                               
001700*** END COPY W236L003C0  LENGTH=10                                        
