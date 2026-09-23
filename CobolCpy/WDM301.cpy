000100 01  USER-WDM301-CTX.                                                     
000200*                                 URVALSREGISTER                          
000300*                                 ARTIKELSTATISTIK                        
000400*                                 URVALSIDENTITET SEGMENT                 
000500*                                 FYSISK NYCKEL: WDM301KY                 
000600*                                 IDUSER + DAREGDAT + TIREGTID            
000700     03 USER-IDUSER          PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900*                                 USER SECURITY-IDENTITY                  
001000     03 USER-DAREGDAT        PIC 9(8).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001200*                                 REGISTRATION DATE (YYYYMMDD)            
001300     03 USER-TIREGTID        PIC S9(7)           COMP-3.                  
001400*                                 REGISTRERINGSTID                        
001500*                                 GENERAL REGISTRATION TIME               
001600     03 USER-IDFSGURV        PIC X(8).                                    
001700*                                 URVALS IDENTITET                        
001800*                                 CHOICE IDENTITY                         
001900     03 USER-IDTRANS         PIC X(4).                                    
002000*                                 BILDNUMMER                              
002100*                                 SCREEN NUMBER                           
002200     03 USER-FLLISTA         PIC X.                                       
002300*                                 LISTUTTAGSFLAGGA                        
002400*                                 PRINTING FLAG                           
002500*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
