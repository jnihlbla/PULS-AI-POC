000100 01  MID-W30180I1.                                                        
000200*                                 MID-COPYTEXT TILL PGM W30180            
000300*                                 RENOVATOR ORDER                         
000400     03 MID-IDDISTR          PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 MID-KVRADER          PIC 9(5).                                    
000800*                                 ANTAL RADER                             
000900*                                 NUMBER OF LINES                         
001000     03 MID-TABELLRAD        OCCURS 1000 TIMES.                           
001100        05 MID-IDARTNR-OBJ   PIC 9(8).                                    
001200*                                 OBJEKTNUMMER                            
001300        05 MID-KVBEART-UPD   PIC 9(6).                                    
001400*                                 BESTÄLLT ANTAL STYCKEN                  
001500*                                 ORDERED QUANTITY                        
001600*** END OF VILMAII-COPY LENGTH= 14009 BYTES                               
