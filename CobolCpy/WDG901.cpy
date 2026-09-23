000100 01  POST-WDG901.                                                         
000200*                                 REGISTER FÖR INPUT TILL BATCH           
000300*                                 REGISTRERAD POST-SEGMENT                
000400*                                 FYSISK NYCKEL: WDG901KY                 
000500*                                 (TIREGDAT + TKLOCK)                     
000600     03 POST-TIREGDAT        PIC S9(7)           COMP-3.                  
000700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000800*                                 REGISTRATION DATE (YYMMDD)              
000900     03 POST-TIKLOCK         PIC S9(9)           COMP-3.                  
001000*                                 KLOCKSLAG (TTMMSSTH)                    
001100*                                 TIME OF DAY (HHMMSSTH)                  
001200     03 POST-IDUSER          PIC X(8).                                    
001300*                                 ANVÄNDARENS SÄKERHETS ID                
001400*                                 USER SECURITY-IDENTITY                  
001500     03 POST-IDLTERM         PIC X(8).                                    
001600*                                 LOGISKT TERMINALNAMN                    
001700*                                 IDENTITY OF LOGICAL TERMINAL            
001800     03 POST-TIBORT          PIC S9(7)           COMP-3.                  
001900*                                 BORTTAGSDATUM  (ÅÅMMDD)                 
002000*                                 DELETE DATE    (YYMMDD)                 
002100     03 POST-REGPOST         PIC X(90).                                   
002200*** END COPY WDG901CCC0  LENGTH=119                                       
