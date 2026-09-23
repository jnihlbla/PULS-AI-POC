000100 01  RESP-W60207O1.                                                       
000200*                                 COPYTEXT FÖR RESP W60207O1              
000300*                                                                         
000400     03 RESP-FLAGGA-DEL-UPD-ATTR                                          
000500                             PIC X(2).                                    
000600*                                 MFS ATTRIBUTFÄLT                        
000700     03 RESP-FLAGGA-DEL-UPD  PIC X.                                       
000800*                                 ALLMÄN FLAGGA                           
000900*                                 GENERAL FLAG                            
001000     03 RESP-LINE            OCCURS 15 TIMES.                             
001100        05 RESP-TEKRFEL-LINE-ATTR                                         
001200                             PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400        05 RESP-TEKRFEL-LINE PIC X(66).                                   
001500*** END OF VILMAII-COPY LENGTH= 1023 BYTES                                
