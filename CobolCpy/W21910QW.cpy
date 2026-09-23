000100 01  W21910QW.                                                            
000200*                                 KAMPANJDATA FRÅN QW90                   
000300*                                 POSTTYP = Q99                           
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 RESP-IMP-COUNTRY     PIC X(2).                                    
000800     03 RESP-IMP-ID          PIC X(6).                                    
000900     03 IDKAMP               PIC X(7).                                    
001000*                                 SERVICEKAMPANJ                          
001100     03 TICREATE             PIC X(6).                                    
001200     03 TISTADAT-KAMP        PIC X(6).                                    
001300*                                 STARTDATUM FÖR KAMPANJ                  
001400     03 TISTODAT-KAMP        PIC X(6).                                    
001500*                                 STOPPDATUM                              
001600     03 RESP-ADM             PIC X.                                       
001700     03 KVKAMP-CARS          PIC 9(7).                                    
001800*                                 ANTAL BILAR I KAMPANJ                   
001900     03 KVCARS-PRF           PIC 9(7).                                    
002000     03 KVFORGN-CARS         PIC 9(7).                                    
002100     03 KVOWN-CARS-PRF       PIC 9(7).                                    
002200     03 KVCARS-PRF-MNL       PIC 9(7).                                    
002300*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
