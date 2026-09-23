000100 01  MID-W90416I1.                                                        
000200*                                 MID-COPYTEXT FÖR W904160                
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-BELEVART-IN      PIC X(30).                                   
000800*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
000900     03 MID-BELEVART-UT      PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001100     03 MID-IDLEVNR-IN       PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MID-IDLEVNR-UT       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 MID-IDSKYLT-IN       PIC X(3).                                    
001600*                                 NATIONALITETSTECKEN                     
001700*                                 SPRÅKIDENTIFIKATION                     
001800     03 MID-IDSKYLT-UT       PIC X(3).                                    
001900*                                 NATIONALITETSTECKEN                     
002000*                                 SPRÅKIDENTIFIKATION                     
002100     03 MID-FILLER           PIC X.                                       
002200     03 MID-FILLER           PIC X.                                       
002300     03 MID-FILLER           PIC X(2).                                    
002400     03 MID-FILLER           PIC X(2).                                    
002500     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MID-KDSTRRAD-ENTER   PIC X.                                       
002800*                                 TYP AV STRUKTURRAD                      
002900     03 MID-IDRADNR-ENTER    PIC 9(4).                                    
003000*                                 RADNUMMER                               
003100     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MID-KDSTRRAD-NEXT    PIC X.                                       
003400*                                 TYP AV STRUKTURRAD                      
003500     03 MID-IDRADNR-NEXT     PIC 9(4).                                    
003600*                                 RADNUMMER                               
003700     03 MID-RADER            OCCURS 10 TIMES.                             
003800*                                                                         
003900        05 MID-FILLER        PIC X.                                       
004000        05 MID-FILLER        PIC X(4).                                    
004100        05 MID-FILLER        PIC X(9).                                    
004200     03 MID-FILLER           PIC X.                                       
004300*** END OF VILMAII-COPY LENGTH= 269 BYTES                                 
