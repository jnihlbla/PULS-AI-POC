000100 01  W21641.                                                              
000200*                                 COPYTEXT TILL FIL W21641                
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 REKSIFFR             PIC S9              COMP-3.                  
000600*                                 KONTROLLSIFFRA                          
000700     03 BEART-SVE            PIC X(25).                                   
000800*                                 SVENSK ARTIKELBENÄMNING                 
000900     03 BEART-ENG            PIC X(25).                                   
001000*                                 ENGELSK ARTIKELBENÄMNING                
001100     03 IDLEVNR              PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 KDARTURS             PIC X(2).                                    
001400*                                 ARTIKELURSPRUNGSKOD                     
001500     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001600*                                 FUNKTIONSGRUPP                          
001700     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
001800*                                 ARTIKELNS SJÄLVKOSTNAD                  
001900     03 PRARTBTO-MARK-A      PIC S9(7)V9(2)      COMP-3.                  
002000*                                 BRUTTOPRIS PER MARKNAD (FOB)            
002100     03 VKART                PIC S9(7)           COMP-3.                  
002200*                                 ARTIKELVIKT (G)                         
002300     03 TEARTNOT             OCCURS 2 TIMES                               
002400                             PIC X(40).                                   
002500*                                 ARTIKEL NOTERING                        
002600     03 BEEMBLEM             OCCURS 10 TIMES                              
002700                             PIC X(5).                                    
002800*                                 EMBLEM                                  
002900*** END OF VILMAII-COPY LENGTH= 210 BYTES                                 
