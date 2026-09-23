000100 01  MOD-W50321O1.                                                        
000200*                                 RESET INVENTORY ADJUSTMENTS             
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 MOD-TEMFSINF         PIC X(55).                                   
001800*                                 INFORMATIONSMEDDELANDE                  
001900*                                 INFORMATION MESSAGE                     
002000*** END OF VILMAII-COPY LENGTH= 105 BYTES                                 
