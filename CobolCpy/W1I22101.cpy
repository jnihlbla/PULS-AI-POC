000100 01  MID-W1I22101.                                                        
000200*                                 MID-COPYTEXT FÖR W122100                
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
002100     03 MID-1002-SATS-IN     PIC X.                                       
002200*                                 ALLMÄN SVARSFLAGGA                      
002300     03 MID-1002-SATS-UT     PIC X.                                       
002400*                                 ALLMÄN SVARSFLAGGA                      
002500     03 MID-KDPRODSL-IN      PIC X(2).                                    
002600*                                 PRODUKTSLAG                             
002700     03 MID-KDPRODSL-UT      PIC X(2).                                    
002800*                                 PRODUKTSLAG                             
002900     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MID-KDSTRRAD-ENTER   PIC X.                                       
003200*                                 TYP AV STRUKTURRAD                      
003300     03 MID-IDRADNR-ENTER    PIC 9(4).                                    
003400*                                 RADNUMMER                               
003500     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MID-KDSTRRAD-NEXT    PIC X.                                       
003800*                                 TYP AV STRUKTURRAD                      
003900     03 MID-IDRADNR-NEXT     PIC 9(4).                                    
004000*                                 RADNUMMER                               
004100     03 MID-RADER            OCCURS 10 TIMES.                             
004200*                                                                         
004300        05 MID-SELECT        PIC X.                                       
004400        05 MID-IDRADNR       PIC X(4).                                    
004500*                                 RADNUMMER                               
004600        05 MID-IDARTNR       PIC X(9).                                    
004700*                                 ARTIKELNUMMER                           
004800     03 MID-KDPRTVAL         PIC X.                                       
004900*                                 PRINTER-VAL KOD                         
005000*** END OF VILMAII-COPY LENGTH= 269 BYTES                                 
