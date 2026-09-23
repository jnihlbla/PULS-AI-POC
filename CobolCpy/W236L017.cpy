000100 01  W236L017.                                                            
000200*                                 COPYTEXT FÖR LÄSNING AV                 
000300*                                 LEVERANS-PLAN.  KOMM. MELLAN            
000400*                                 W2362000 OCH W2362010.                  
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-LEV-BESK        VALUE +105.                                  
000700     03 KDSVAR               PIC X.                                       
000800      88 LEV-BESK-FINNS      VALUE ' '.                                   
000900*                                 SVAR FRÅN SUBPROGRAM                    
001000     03 IO-AREA.                                                          
001100*                                 WDD908                                  
001200        05 TILEVBSK-AVS      PIC S9(5)           COMP-3.                  
001300*                                 AVSÄNDNINGSVECKA     (ÅÅVV)             
001400*                                 ENL LEVERANSBESKED                      
001500        05 KVAVIS-BSKKVAR    PIC S9(7)           COMP-3.                  
001600*                                 LEVERANSBESKEDANTAL EFTER               
001700*                                 AVBOKNING                               
001800*** END COPY W236L017C0  LENGTH=10                                        
