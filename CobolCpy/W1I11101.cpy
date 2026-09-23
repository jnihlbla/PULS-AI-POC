000010 01  MID-W1I11101.                                                        
000020*                                 MID-COPYTEXT FÖR W1011100               
000030     03 MID-IDARTNR-IN       PIC X(9).                                    
000040*                                 ARTIKELNUMMER                           
000050     03 MID-IDARTNR-UT       PIC X(9).                                    
000060*                                 ARTIKELNUMMER                           
000070     03 MID-INPUT.                                                        
000080*                                 INRAPPORTERINGSDEL 1111                 
000090        05 MID-IDSTATNR      OCCURS 5 TIMES                               
000100                             PIC X(9).                                    
000110*                                 STATISTISKT NUMMER                      
000120*                                 1 = NORSKT                              
000130*                                 2 = ENGELSKT                            
000140*                                 3 = BELGISKT                            
000150*                                 4 = PERUANSKT                           
000160*                                 5 = SVENSKT                             
000170*                                 6 =                                     
      *** END COPY W1I11101    LENGTH=63                                        
