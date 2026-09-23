000100 01  W221UNB.                                                             
000200     03 IDPT                 PIC X(3).                                    
000300*                                 POSTTYP                  TB314          
000500     03 IDPTYP-LTH           PIC X(3) VALUE '073'.                        
000600*                                 LÄNGD PÅ FÄLT                           
000800     03 IDFROM               PIC X(5).                                    
000900*                                 SÄNDANDE FÖRETAG                        
001100     03 IDLEVNR              PIC X(5).                                    
001200*                                 MOTTAGANDE FÖRETAG                      
001400     03 IDSNRF               PIC X(14).                                   
001500*                                 ÖVERFÖRINGSREFERENS                     
001700     03 FILLER               PIC X(49) VALUE SPACE.                       
001800*                                 FILLER                                  
002600*** END COPY W221UNB     LENGTH=79    OLD LENGTH=79                       
