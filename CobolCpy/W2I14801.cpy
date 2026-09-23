000100 01  MID-W2I14801.                                                        
000200*                                 MID-COPYTEXT FÖR W2014800               
000300     03 MID-RAD              OCCURS 10 TIMES.                             
000400*                                 RADINFORMATION                          
000500        05 MID-RAD-CMD       PIC X.                                       
000600*                                 BEHANDLINGSKOD                          
000700        05 MID-RAD-KDPRODSL  PIC Z9.                                      
000800*                                 PRODUKTSLAG                             
000900     03 MID-KDPRODSL         PIC 9(2).                                    
001000*                                 PRODUKTSLAG                             
001100     03 MID-KVPERIOD-BERS    PIC 9(2).                                    
001200*                                 ANTAL PERIODER OI BEHOV ERSATT.         
001300     03 MID-KVPERIOD-BTILLK  PIC 9(2).                                    
001400*                                 ANTAL PERIODER OI BEHOV TILLK.          
001500     03 MID-KVPERIOD-VERS    PIC 9(2).                                    
001600*                                 ANTAL PERIODER OI VÄRDE ERSATT.         
001700     03 MID-KVPERIOD-HERS    PIC 9(2).                                    
001800*                                 ANTAL PERIODER OI HIST. ERSATT.         
001900*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
