000100 01  W3O152V1.                                                            
000200*                                 COPYTEXT FÖR MOD W3O152V1               
000300*                                 TRANS 3152 (W30152T)                    
000400*                                 NEW PART-NUM TO EXCH PART-NUMS          
000500     03 IDTRANS              PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 IDMFSFEL             PIC X(3).                                    
000800*                                 MFS FELMEDDELANDE NUMMER                
000900     03 IDARTNR-INPUT        PIC 9(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 COUNTER              PIC 9(2).                                    
001200*                                 RÄKNARE, ANTAL POSTER                   
001300     03 IDARTNR-BYT          OCCURS 13 TIMES                              
001400                             PIC 9(9).                                    
001500*                                 ARTIKELNUMMER                           
001600*** END OF VILMAII-COPY LENGTH= 135 BYTES                                 
