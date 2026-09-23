000010 01  MID-W6I17101.                                                        
000020*                                 MID-COPYTEXT FÖR W6017100               
000030     03 MID-INPUT            OCCURS 14 TIMES.                             
000040*                                 INDATA FÖR UPPDATERING                  
000050        05 MID-KDCMDVAL      PIC X(3).                                    
000060*                                 GENERELL KOMMANDOKOD                    
000070        05 MID-IDARTNR       PIC X(8).                                    
000080*                                 ARTIKELNUMMER                           
000090        05 MID-BEART         PIC X(20).                                   
000100        05 MID-KVBUFF-F      PIC X(7).                                    
000110*                                 FÖRÄDLAT BUFFERSALDO                    
000120        05 MID-ADBUFPPL      PIC X(2).                                    
000130*                                 PALLPLATSNUMMER I BUFFERT               
000140        05 MID-ADBUFFOMR     PIC X(2).                                    
000150*                                 BUFFERTOMRÅDE                           
000160        05 MID-ADBUFFGANG    PIC X(2).                                    
000170*                                 BUFFERT GÅNG                            
000180        05 MID-ADBUFFPL      PIC X(5).                                    
000190*                                 BUFFERPLATSNUMMER                       
000200        05 MID-TEMFSMED      PIC X(20).                                   
000210*                                 INFO-MEDDELANDE FÖR FÄLT/RAD            
      *** END COPY W6I17101    LENGTH=966                                       
