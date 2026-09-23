000100 01  RESP-WF0253O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0253         
000300*                                 SYSTEM MAINTENANCE                      
000400     03 RESP-IDSYSTEM-KEY    PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600*                                 VOLVO VCCS SYSTEM NUMBER                
000700     03 RESP-KVMINUT         PIC Z(4)9.                                   
000800*                                 ANTAL MINUTER                           
000900*                                 NO OF MINUTES                           
001000     03 RESP-KVDAGAR         PIC Z9.                                      
001100*                                 ANTAL DAGAR                             
001200     03 RESP-FLKLAR          PIC X.                                       
001300*                                 AVSLUTNINGSMARKERING                    
001400*                                 FINISHED FLAG                           
001500     03 RESP-DAREGDAT        PIC Z(8).                                    
001600*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001700*                                 REGISTRATION DATE (YYYYMMDD)            
001800     03 RESP-DAUPPDAT        PIC Z(8).                                    
001900*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
002000*                                                                         
002100*                                 UPDATING DATE     (YYYYMMDD)            
002200*                                                                         
002300     03 RESP-IDUSER          PIC X(8).                                    
002400*                                 ANVƒNDARENS SƒKERHETS ID                
002500*                                 USER SECURITY-IDENTITY                  
002600*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
