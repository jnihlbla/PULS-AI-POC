000100 01  W3O151V1.                                                            
000200*                                 COPYTEXT FÖR MOD W3O151V1               
000300*                                 TRANS 3151 (W30151T)                    
000400*                                 EXCH PART-NUM TO NEW PART-NUMS          
000500     03 IDTRANS              PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 IDMFSFEL             PIC X(3).                                    
000800*                                 MFS FELMEDDELANDE NUMMER                
000900     03 IDARTNR-BYT-INPUT    PIC 9(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDARTNR-NEXT         PIC 9(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 COUNTER              PIC 9(2).                                    
001400*                                 RÄKNARE, ANTAL POSTER                   
001500     03 IDARTNR              OCCURS 13 TIMES                              
001600                             PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800*** END OF VILMAII-COPY LENGTH= 144 BYTES                                 
