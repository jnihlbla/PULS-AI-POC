000100 01  MID-W5I30801.                                                        
000200*                                 MID-COPYTEXT FÖR BILD  5308             
000300*                                 AUTOMATJUSTERING                        
000400     03 MID-IDLISTNR-IN      PIC 9(6).                                    
000500     03 MID-IDUSER-IN        PIC X(8).                                    
000600*                                 ANVÄNDARENS SÄKERHETS ID                
000700     03 MID-OK-IN            PIC X.                                       
000800     03 MID-INPUT            OCCURS 10 TIMES.                             
000900        05 MID-ANTAL-IN      PIC X(6).                                    
001000*                                 ANTAL                                   
001100        05 MID-TECKEN-IN     PIC X.                                       
001200        05 MID-DIFFERANS-IN  PIC X(6).                                    
001300*                                 ANTAL                                   
001400        05 MID-FLOMINV-IN    PIC X.                                       
001500*                                 ALLMÄN FLAGGA                           
001600        05 MID-FELRAD-IN     PIC X.                                       
001700*** END OF VILMAII-COPY LENGTH= 165 BYTES                                 
