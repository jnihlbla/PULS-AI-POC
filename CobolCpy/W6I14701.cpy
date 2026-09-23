000100 01  MID-W6I14701.                                                        
000200*                                 MID FOR FINAL REPORTING                 
000300*                                 WITH DEVIATION                          
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 MID-IDDC-UT          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 MID-KVTRANS-UT       PIC 9(3).                                    
001100*                                 ANTAL TRANSAKTIONER                     
001200*                                 NUMBER OF TRANSACTIONS                  
001300     03 MID-KVTRANS-IN       PIC X(3).                                    
001400*                                 ANTAL TRANSAKTIONER                     
001500*                                 NUMBER OF TRANSACTIONS                  
001600     03 MID-IDLOPNRM         OCCURS 13 TIMES                              
001700                             PIC X(8).                                    
001800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001900*                                 (0VVDLLLLK)                             
002000*                                 SERIAL NO RECEIVING REPORT              
002100*                                 (0WWDLLLLC)                             
002200     03 MID-KVANTMOT         OCCURS 13 TIMES                              
002300                             PIC X(6).                                    
002400*                                 ANTAL MOTTAGET                          
002500*                                 QUANTITY RECEIVED                       
002600     03 MID-FLSVAR           OCCURS 13 TIMES                              
002700                             PIC X.                                       
002800*                                 ALLMÄN SVARSFLAGGA                      
002900*                                 GENERAL REPLY FLAG                      
003000*** END OF VILMAII-COPY LENGTH= 205 BYTES                                 
