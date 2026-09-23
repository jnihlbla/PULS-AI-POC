000100 01  MID-W6I20701.                                                        
000200*                                                                         
000300     03 MID-IDKR-IN          PIC 9(5).                                    
000400*                                 KONTROLLRAPPORT NUMMER                  
000500*                                 INSPECTION REPORT NUMBER                
000600     03 MID-IDKR-UT          PIC 9(5).                                    
000700*                                 KONTROLLRAPPORT NUMMER                  
000800*                                 INSPECTION REPORT NUMBER                
000900     03 MID-INPUT.                                                        
001000*                                                                         
001100        05 MID-FLAGGA-DEL    PIC X.                                       
001200*                                 ALLMÄN FLAGGA                           
001300*                                 GENERAL FLAG                            
001400        05 MID-TEKRFEL       OCCURS 15 TIMES                              
001500                             PIC X(66).                                   
001600*** END OF VILMAII-COPY LENGTH= 1001 BYTES                                
