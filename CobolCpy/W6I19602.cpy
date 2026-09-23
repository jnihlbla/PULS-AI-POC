000100 01  MID-W6I19602.                                                        
000200*                                 MID-COPYTEXT FÖR W60196                 
000300*                                 FRÅN BILD 6115                          
000400     03 MID-ADINLOMR-PRT     PIC X(4).                                    
000500*                                 PRINTERPLACERING                        
000600*                                 PLACE OF A PRINTER                      
000700     03 MID-KVPOST           PIC 9(7).                                    
000800*                                 RÄKNARE, ANTAL POSTER                   
000900*                                 RECORD COUNTER                          
001000     03 MID-IDDC             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 MID-IDLOPNRM         OCCURS 15 TIMES                              
001400                             PIC 9(9).                                    
001500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001600*                                 (0VVDLLLLK)                             
001700*                                 SERIAL NO RECEIVING REPORT              
001800*                                 (0WWDLLLLC)                             
