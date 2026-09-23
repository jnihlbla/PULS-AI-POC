000100 01  PRNO-W335PRNO.                                                       
000200*                                 LÄNKAREA TILL W335PRNO - UTTAG          
000300*                                 AV PRISFRÅGENUMMER                      
000400*                                 KDCALL = 1: HÄMTA LEDIGT FRÅGEN         
000500*                                 UMMER                                   
000600*                                 KDCALL = 2: ADDERA +1 TILL FRÅG         
000700*                                 ENUMMER                                 
000800*                                 KDCALL = 3: UPPDATERA BASEN MED         
000900*                                  NÄSTA LEDIGA                           
001000*                                             FRÅGENUMMER                 
001100*                                                                         
001200     03 PRNO-KDCALL          PIC S9(3)           COMP-3.                  
001300*                                 ANROPSTYP                               
001400     03 PRNO-IDPRQUES-IN     PIC 9(7).                                    
001500*                                 PRISFRÅGA NR                            
001600     03 PRNO-IDPRQUES-UT     PIC 9(7).                                    
001700*                                 PRISFRÅGA NR                            
001800*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
