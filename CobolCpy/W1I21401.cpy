000100 01  MID-W1I21401.                                                        
000200*                                 MID-COPYTEXT F÷R W121400                
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDSKYLT-IN       PIC X(3).                                    
000800*                                 NATIONALITETSTECKEN                     
000900*                                 SPR≈KIDENTIFIKATION                     
001000     03 MID-IDSKYLT-UT       PIC X(3).                                    
001100*                                 NATIONALITETSTECKEN                     
001200*                                 SPR≈KIDENTIFIKATION                     
001300     03 MID-VECKA-FOM-IN     PIC X(4).                                    
001400*                                 ≈R - VECKA  (≈≈VV)                      
001500     03 MID-VECKA-FOM-UT     PIC X(4).                                    
001600*                                 ≈R - VECKA  (≈≈VV)                      
001700     03 MID-VECKA-TOM-IN     PIC X(4).                                    
001800*                                 ≈R - VECKA  (≈≈VV)                      
001900     03 MID-VECKA-TOM-UT     PIC X(4).                                    
002000*                                 ≈R - VECKA  (≈≈VV)                      
002100     03 MID-IDRADNR-DOLT     PIC X(4).                                    
002200*                                 RADNUMMER                               
002300     03 MID-IDRADNR-DOLT2    PIC X(4).                                    
002400*                                 RADNUMMER                               
002500     03 MID-IDSATSNR-DOLT    PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MID-INPUT            OCCURS 13 TIMES.                             
002800*                                                                         
002900        05 MID-SELECT        PIC X.                                       
003000        05 MID-IDLEVNR       PIC X(5).                                    
003100*                                 LEVERANT÷RNUMMER                        
003200        05 MID-ART-LEV       PIC X(30).                                   
003300*                                 LEVERANT÷RENS ARTIKELBENƒMNING          
003400        05 MID-STRTYP        PIC X.                                       
003500*                                 STRUKTURTYP                             
003600     03 MID-KDPRTVAL         PIC X.                                       
003700*                                 PRINTER-VAL KOD                         
003800*** END OF VILMAII-COPY LENGTH= 539 BYTES                                 
