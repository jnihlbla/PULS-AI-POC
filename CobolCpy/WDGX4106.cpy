000100 01  4106-WDGX4106.                                                       
000200*                                 ATTEST AV KREDITNOTA                    
000300*                                 INFO ATTESTERARE                        
000400*                                 WDR5                                    
000500*                                 FYSISK NYCKEL: TIDATETIME               
000600     03 4106-TIDATETIME      PIC X(14).                                   
000700*                                 DATUM OCH TID YYYYMMDDHHMMSS            
000800*                                 DATE AND TIME YYYYMMDDHHMMSS            
000900     03 4106-IDUSER-GODK     PIC X(8).                                    
001000*                                 ANVÄNDAR-ID GODKÄNNARE                  
001100*                                 USER ID APPROVER                        
001200     03 4106-BEANST-GODK     PIC X(25).                                   
001300*                                 GODKÄNNARES NAMN                        
001400*                                 NAME OF APPROVER                        
001500     03 4106-FLKLAR          PIC X.                                       
001600*                                 AVSLUTNINGSMARKERING                    
001700*                                 FINISHED FLAG                           
001800     03 4106-TIUPPDAT        PIC S9(7)           COMP-3.                  
001900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002000*                                 UPDATING DATE     (YYMMDD)              
002100*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
