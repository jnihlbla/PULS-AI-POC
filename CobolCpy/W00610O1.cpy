000100 01  RESP-W00610O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM W00610         
000300*                                 START SOP RUTINE IN WEB                 
000400     03 RESP-IDTRANS         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 RESP-TEMFSFEL        PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 RESP-IDPROCESS-ATTR  PIC X(2).                                    
001100*                                 MFS ATTRIBUTFÄLT                        
001200     03 RESP-IDPROCESS       PIC X(10).                                   
001300*                                 PROCESSNAMN                             
001400*                                 PROCESS NAME                            
001500     03 RESP-KDSOPFUNK-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 RESP-KDSOPFUNK       PIC X(2).                                    
001800*                                 MFS BEHANDLING AV INPUTFÄLT             
001900*                                 MFS DISPOSITION OF INPUT FIELD          
002000     03 RESP-TESYMBV-ATTR    PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 RESP-TESYMBV         PIC X(500).                                  
002300     03 RESP-TEMFSINF        PIC X(55).                                   
002400*                                 INFORMATIONSMEDDELANDE                  
002500*                                 INFORMATION MESSAGE                     
002600*** END OF VILMAII-COPY LENGTH= 617 BYTES                                 
