000100 01  W21912.                                                              
000200*                                 KAMPANJDATA FRÅN QW90                   
000300*                                 POSTTYP = Q99                           
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 KDSTATUS-KAMP        PIC X.                                       
000800*                                 STATUS AV MATCHNING                     
000900*                                 N = NYTILLKOMMEN POST                   
001000*                                 C = FÖRÄNDRAD POST                      
001100*                                 D = BORTTAGEN POST                      
001200     03 IDKAMP               PIC X(7).                                    
001300*                                 SERVICEKAMPANJ                          
001400     03 TISTADAT-KAMP        PIC 9(6).                                    
001500*                                 STARTDATUM FÖR KAMPANJ                  
001600     03 TISTODAT-KAMP        PIC 9(6).                                    
001700*                                 STOPPDATUM FÖR KAMPANJ                  
001800     03 KVKAMP-CARS          PIC 9(7).                                    
001900*                                 ANTAL BILAR I KAMPANJ                   
002000*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
