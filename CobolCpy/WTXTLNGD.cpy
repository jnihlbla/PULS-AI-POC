000100 01  TXT-WTXTLNGD.                                                        
000200*                                 PARAMETERAREA TILL WTXTLNGD.            
000300*                                 ANTAL TECKEN I GIVET DATA               
000400*                                 RÄKNAS TILL FÖRSTA DUBBELSPACE          
000500*                                 -------- INPARAMETER ---------          
000600*                                 FILLERX100 ÄR DEN TEXT SOM RÄKN         
000700*                                 AS                                      
000800*                                 -------- UTPARAMETER ----------         
000900*                                 DIFAELT ANTAL TECKEN I TEXTEN           
001000*                                 ----- ANROP  ------------------         
001100*                                 CALL WTXTLNGD USING                     
001200*                                               TXT-WTXTLNGD              
001300     03 TXT-TETEXT           PIC X(100).                                  
001400     03 TXT-DIFAELT          PIC S9(3)           COMP-3.                  
001500*                                 FÄLTLÄNGD                               
001600*** END OF VILMAII-COPY LENGTH= 102 BYTES                                 
