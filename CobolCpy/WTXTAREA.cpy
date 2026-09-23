000100 01  RTXT-WTXTAREA.                                                       
000200*                                 PARAMETERAREA TILL WTXTTR.              
000300*                                 GIVET DATA ÖVERSÄTTS ENLIGT             
000400*                                 KDTEXTTR. G = ALLA TECKEN UTOM          
000500*                                 FÖRSTA BLIR GEMENER. V = ALLA           
000600*                                 TECKEN BLIR VERSALER.                   
000700*                                 -------- INPARAMETRAR --------          
000800*                                 KDTEXTTR VAD SOM SKALL GÖRAS            
000900*                                 TETEXTTR TEXTEN SOM BEARBETAS           
001000*                                 -------- UTPARAMETER ----------         
001100*                                 TETEXTTR BEARBETAD TEXT                 
001200*                                 KDSVAR   SVARSKOD                       
001300*                                 ----- ANROP  ------------------         
001400*                                 CALL WTXTTR USING RTXT-WTXTAREA         
001500     03 RTXT-KDTEXTTR        PIC X.                                       
001600*                                 TEXTÖVERSÄTTNING                        
001700*                                 TEXT-TRANSLATION                        
001800     03 RTXT-TETEXTTR        PIC X(60).                                   
001900*                                 TEXT ATT ÖVERSÄTTA                      
002000*                                 TEXT TO TRANSLATE                       
002100     03 RTXT-KDSVAR          PIC X.                                       
002200      88 RTXT-ANROP-OK       VALUE ' '.                                   
002300      88 RTXT-ANROP-SAKNAS   VALUE 'S'.                                   
002400      88 RTXT-FEL            VALUE 'F'.                                   
002500*                                 SVARSKOD FRÅN SUBPROGRAM                
002600*                                 RETURN CODE FROM SUBPROGAM              
002700*** END COPY WTXTAREAC0  LENGTH=62                                        
