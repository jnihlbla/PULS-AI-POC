000100 01  SEQA-WDM3A1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDM301             
000300*                                 URVALSREGISTER                          
000400*                                 FYSISK NYCKEL: WDM3A1KY                 
000500*                                  (IDUSER + IDFSGURV + IDTRANS           
000600*                                   + DAREGDAT + TIREGTID)                
000700*                                 SEKUNDÄR NYCKEL: WDM3ASEQ               
000800*                                  (IDUSER + IDFSGURV)                    
000900     03 SEQA-IDUSER          PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100*                                 USER SECURITY-IDENTITY                  
001200     03 SEQA-IDFSGURV        PIC X(8).                                    
001300*                                 URVALS IDENTITET                        
001400*                                 CHOICE IDENTITY                         
001500     03 SEQA-IDTRANS         PIC X(4).                                    
001600*                                 BILDNUMMER                              
001700*                                 SCREEN NUMBER                           
001800     03 SEQA-DAREGDAT        PIC 9(8).                                    
001900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002000*                                 REGISTRATION DATE (YYYYMMDD)            
002100     03 SEQA-TIREGTID        PIC S9(7)           COMP-3.                  
002200*                                 REGISTRERINGSTID                        
002300*                                 GENERAL REGISTRATION TIME               
002400     03 SEQA-FLLISTA         PIC X.                                       
002500*                                 LISTUTTAGSFLAGGA                        
002600*                                 PRINTING FLAG                           
002700*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
