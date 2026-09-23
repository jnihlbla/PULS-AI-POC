000100 01  USER-WDM401.                                                         
000200*                                 URVALSREGISTER                          
000300*                                 URVALSIDENTITET SEGMENT                 
000400*                                 FYSISK NYCKEL: WDM401KY                 
000500*                                 IDUSER + TIREGDAT + TIREGTID            
000600     03 USER-IDUSER          PIC X(8).                                    
000700*                                 ANVÄNDARENS SÄKERHETS ID                
000800*                                 USER SECURITY-IDENTITY                  
000900     03 USER-TIREGDAT        PIC S9(7)           COMP-3.                  
001000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001100*                                 REGISTRATION DATE (YYMMDD)              
001200     03 USER-TIREGTID        PIC S9(7)           COMP-3.                  
001300*                                 REGISTRERINGSTID                        
001400*                                 GENERAL REGISTRATION TIME               
001500     03 USER-IDURVAL         PIC X(8).                                    
001600*                                 URVALS IDENTITET                        
001700*                                 CHOICE IDENTITY                         
001800     03 USER-IDTRANS         PIC X(4).                                    
001900*                                 BILDNUMMER                              
002000*                                 SCREEN NUMBER                           
002100     03 USER-FLLISTA         PIC X.                                       
002200*                                 LISTUTTAGSFLAGGA                        
002300*                                 PRINTING FLAG                           
002400*** END COPY WDM401CCC0  LENGTH=29                                        
