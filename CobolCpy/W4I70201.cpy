000100 01  MID-W4I70201.                                                        
000200*                                 MID-COPYTEXT FÖR W4070200               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDFTG-IN         PIC X(2).                                    
000600*                                 FÖRETAGSID EKONOM REDOVISNING           
000700     03 MID-IDARTNR-UT       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MID-IDFTG-UT         PIC X(2).                                    
001000*                                 FÖRETAGSID EKONOM REDOVISNING           
001100     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-TIGILTIG-FOM-ENTER                                            
001400                             PIC 9(6).                                    
001500*                                 GILTIGHETSDATUM FOM                     
001600     03 MID-KDANMORS-ENTER   PIC X(2).                                    
001700*                                 ORSAK TILL LEVERANSANMÄRKNING           
001800     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MID-TIGILTIG-FOM-NEXT                                             
002100                             PIC 9(6).                                    
002200*                                 GILTIGHETSDATUM FOM                     
002300     03 MID-KDANMORS-NEXT    PIC X(2).                                    
002400*                                 ORSAK TILL LEVERANSANMÄRKNING           
002500     03 MID-INPUT.                                                        
002600        05 MID-IDARTNR-UPP   PIC 9(9).                                    
002700*                                 ARTIKELNUMMER                           
002800        05 MID-TIGILTIG-FOM-UPP                                           
002900                             PIC 9(6).                                    
003000*                                 GILTIGHETSDATUM FOM                     
003100        05 MID-TIGILTIG-TOM-UPP                                           
003200                             PIC 9(6).                                    
003300*                                 GILTIGHETSDATUM TOM                     
003400        05 MID-IDFTG-UPP     PIC 9(2).                                    
003500*                                 FÖRETAGSID EKONOM REDOVISNING           
003600        05 MID-KDANMORS-UPP  PIC X(2).                                    
003700*                                 ORSAK TILL LEVERANSANMÄRKNING           
003800        05 MID-IDANALYS-UPP  PIC X(12).                                   
003900*                                 ANALYSNUMMER                            
004000        05 MID-IDKONTO-UPP   PIC X(10).                                   
004100*                                 KONTO                                   
004200        05 MID-IDKST-UPP     PIC X(10).                                   
004300*                                 KOSTNADSSTÄLLE                          
004400        05 MID-IDUSER-UPP    PIC X(2).                                    
004500        05 MID-FLBORT-UPP    PIC X.                                       
004600*                                 BORTTAGNINGSFLAGGA                      
004700*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
