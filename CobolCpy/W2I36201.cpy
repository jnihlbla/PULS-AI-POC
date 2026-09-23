000100 01  MID-W2I36201.                                                        
000200*                                 MID-COPYTEXT FÖR W2362                  
000300     03 MID-INPUT.                                                        
000400*                                 MID-INDATA 2362                         
000500        05 MID-SEND-IDDC     PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700        05 MID-RECV-IDDC     PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900        05 MID-FLFLYG        PIC X.                                       
001000        05 MID-IDKAMPRF      PIC X(7).                                    
001100*                                 KAMPANJREFERENS                         
001200        05 MID-BELAGINS-DEL  PIC X(60).                                   
001300*                                 DEL AV LAGERINSTRUKTION                 
001400        05 MID-ORDERRAD      OCCURS 12 TIMES.                             
001500*                                 INDATA UPPDATERINGSFÄLT                 
001600*                                 2362                                    
001700           07 MID-IDARTNR-RAD                                             
001800                             PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000           07 MID-KVBEART-RAD                                             
002100                             PIC X(6).                                    
002200*                                 BESTÄLLT ANTAL STYCKEN                  
002300           07 MID-BERADREF-RAD                                            
002400                             PIC X(10).                                   
002500*                                 KUNDENS RADREFERENS                     
002600        05 MID-FLSLUT        PIC X.                                       
002700*                                 AVSLUTNINGSFLAGGA                       
002800*** END OF VILMAII-COPY LENGTH= 373 BYTES                                 
