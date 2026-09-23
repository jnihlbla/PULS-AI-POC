000100 01  W00520I1.                                                            
000200*                                 GENERAL ENTRANCE FOR NEWWORK TR         
000300*                                 AFFIC TO BE LOGGED                      
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 TIREGDAT             PIC 9(6).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000900*                                 REGISTRATION DATE (YYMMDD)              
001000     03 TIREGTID             PIC 9(6).                                    
001100*                                 REGISTRERINGSTID                        
001200*                                 GENERAL REGISTRATION TIME               
001300     03 IDUSER               PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500*                                 USER SECURITY-IDENTITY                  
001600     03 KVMILSEC             PIC 9(6).                                    
001700*                                 ANTAL MILLISEK FÖR WEBSVAR              
001800*                                 NO OF MILLISEC FOR WEB RESPONDE         
001900     03 BEWEBSCR             PIC X(50).                                   
002000*                                 VALD BILD I PULS WEBBEN                 
002100*                                 CHOSEN SCREEN ON PULS WEB               
002200     03 BEWEBURL             PIC X(50).                                   
002300*                                 LÄNK PULS WEB SIDA                      
002400*                                 LINK PULS WEBSITE                       
002500*** END OF VILMAII-COPY LENGTH= 128 BYTES                                 
