000100 01  SLUT-W460011.                                                        
000200*                                 END CARD FROM VIPS TO NOAC-DO           
000300*                                 POSTTYP = RH9                           
000400     03 SLUT-SORT-IDDISTR    PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 SLUT-SORT-TIFILDAT   PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 SLUT-SORT-TIHHMMSS   PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 SLUT-SORT-IDLOPNR-FIL                                             
001100                             PIC 9(5).                                    
001200*                                 TRANSAKTIONS-LÖPNUMMER                  
001300     03 SLUT-SORT-IDLOPNR    PIC 9(5).                                    
001400*                                 TRANSAKTIONS-LÖPNUMMER                  
001500     03 SLUT-IDPTYP          PIC X(3).                                    
001600*                                 POSTTYP                                 
001700     03 SLUT-KVTRANS         PIC 9(7).                                    
001800*                                 ANTAL TRANSAKTIONER                     
001900     03 FILLER               PIC X(41).                                   
002000     03 SLUT-KDFEL           PIC 9(3).                                    
002100*                                 FELKOD                                  
002200*** END COPY W460011CC0  LENGTH=80                                        
