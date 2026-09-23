000100 01  W231L007.                                                            
000200*                                 LÄNKAREA FÖR LÄSNING AV AVROP           
000300     03 KDCALL               PIC S9(3)           COMP-3.                  
000400      88 LAES-AVROP          VALUE +7.                                    
000500     03 FLJANEJ-AVROP        PIC X.                                       
000600*                                 JA/NEJ-FLAGGA                           
000700     03 IO-AREA.                                                          
000800        05 KDAVROP           PIC S9              COMP-3.                  
000900*                                 AVROPSKOD                               
001000        05 TIAVROP-INL       PIC S9(5)           COMP-3.                  
001100*                                 INLEVERANSDATUM (PLANERAD)              
001200*                                 (ÅÅVV)                                  
001300        05 TIAVROP-DISP      PIC S9(5)           COMP-3.                  
001400*                                 DISPONIBELVECKA  (PLANERAD)             
001500*                                 (ÅÅVV)                                  
001600        05 KVAVROP           PIC S9(7)           COMP-3.                  
001700*                                 AVROPSKVANTITET                         
001800*** END COPY W231L007C0  LENGTH=14                                        
