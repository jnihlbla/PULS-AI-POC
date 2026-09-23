000100 01  REV-WREVAREA.                                                        
000200*                                 PARAMETERAREA TILL WREVERSE.            
000300*                                 GIVET DATA VÄNDS OM SÅ ATT I            
000400*                                 GIVEN TEXTSTRÄNG, FÖRSTA TECKEN         
000500*                                 HAMNAR SIST OSV.                        
000600*                                 -------- INPARAMETER ---------          
000700*                                 TETEXT ÄR DEN TEXT SOM SKALL            
000800*                                 VÄNDAS.                                 
000900*                                 -------- UTPARAMETER ----------         
001000*                                 TETEXT INNEHÅLLER DEN TEXT SOM          
001100*                                 ÄR OMVÄND.                              
001200*                                 ----- ANROP  ------------------         
001300*                                 CALL WREVERSE USING REV-TETEXT          
001400     03 REV-TETEXT           PIC X(100).                                  
001500*                                 TEXT-STRÄNG ATT VÄNDA                   
001600*                                 TEXT STRING TO REVERSE                  
001700*** END COPY WREVAREAC0  LENGTH=100                                       
