000100 01  MID-W6I14601.                                                        
000200*                                 MID FOR FINAL REPORTING                 
000300*                                 WITHOUT SPLIT DEVIATION                 
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 MID-IDDC-UT          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 MID-INPUT.                                                        
001100        05 MID-KVTRANS-UT    PIC 9(3).                                    
001200*                                 ANTAL TRANSAKTIONER                     
001300*                                 NUMBER OF TRANSACTIONS                  
001400        05 MID-KVTRANS-IN    PIC X(3).                                    
001500*                                 ANTAL TRANSAKTIONER                     
001600*                                 NUMBER OF TRANSACTIONS                  
001700        05 MID-LINE-INPUT    OCCURS 13 TIMES.                             
001800           07 MID-IDLOPNRM   PIC X(8).                                    
001900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002000*                                 (0VVDLLLLK)                             
002100*                                 SERIAL NO RECEIVING REPORT              
002200*                                 (0WWDLLLLC)                             
002300           07 MID-FLSVAR     PIC X.                                       
002400*                                 ALLMÄN SVARSFLAGGA                      
002500*                                 GENERAL REPLY FLAG                      
002600*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
