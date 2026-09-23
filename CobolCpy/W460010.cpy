000100 01  START-W460010.                                                       
000200*                                 1:ST CARD FROM VIPS TO NOAC-DO          
000300*                                 POSTTYP = RH0                           
000400     03 START-SORT-IDDISTR   PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 START-SORT-TIFILDAT  PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 START-SORT-TIHHMMSS  PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 START-SORT-IDLOPNR-FIL                                            
001100                             PIC 9(5).                                    
001200*                                 TRANSAKTIONS-LÖPNUMMER                  
001300     03 START-SORT-IDLOPNR   PIC 9(5).                                    
001400*                                 TRANSAKTIONS-LÖPNUMMER                  
001500     03 START-IDPTYP         PIC X(3).                                    
001600*                                 POSTTYP                                 
001700     03 START-IDDISTR        PIC 9(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 START-TIFILDAT       PIC 9(6).                                    
002000*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
002100     03 START-TIHHMMSS       PIC 9(6).                                    
002200*                                 TIM - MIN - SEK   (HHMMSS)              
002300     03 FILLER               PIC X(32).                                   
002400     03 START-KDFEL          PIC 9(3).                                    
002500*                                 FELKOD                                  
002600*** END COPY W460010CC0  LENGTH=80                                        
