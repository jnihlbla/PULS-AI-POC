000100 01  W37181.                                                              
000200*                                 KVITTNINGSBARA    BYTESARTIKLAR         
000300*                                                                         
000400     03 SORTKEY.                                                          
000500*                                                                         
000600        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
000700*                                 FUNKTIONSGRUPP                          
000800        05 IDTABNR           PIC S9(3)           COMP-3.                  
000900*                                 TABELLNUMMER                            
001000        05 IDARTNR           PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 BEART-SVE            PIC X(25).                                   
001300*                                 SVENSK ARTIKELBENÄMNING                 
001400     03 BEART-ENG            PIC X(25).                                   
001500*                                 ENGELSK ARTIKELBENÄMNING                
001600     03 KDANDRING            PIC X.                                       
001700*                                 ÄNDRINGSKOD                             
001800*                                 TILLÄGG   = T                           
001900*                                 BORTTAG   = B                           
002000*                                 JUSTERING = J                           
002100*** END COPY W37181CCC0  LENGTH=61                                        
