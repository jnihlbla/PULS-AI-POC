000100 01  REQU-W60207I1.                                                       
000200*                                                                         
000300     03 REQU-IDKR-KEY        PIC 9(5).                                    
000400*                                 KONTROLLRAPPORT NUMMER                  
000500*                                 INSPECTION REPORT NUMBER                
000600     03 REQU-INPUT.                                                       
000700*                                                                         
000800        05 REQU-FLAGGA-DEL-UPD                                            
000900                             PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100*                                 GENERAL FLAG                            
001200        05 REQU-TEKRFEL-LINE OCCURS 15 TIMES                              
001300                             PIC X(66).                                   
001400*** END OF VILMAII-COPY LENGTH= 996 BYTES                                 
