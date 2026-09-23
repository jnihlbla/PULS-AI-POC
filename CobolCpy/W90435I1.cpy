000100 01  MID-W90435I1.                                                        
000200*                                 MID-COPYTEXT FÖR W9043500               
000300*                                 SPIE2'S VER. AV W4070200                
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDFTG-IN         PIC X(2).                                    
000700*                                 FÖRETAGSID EKONOM REDOVISNING           
000800     03 MID-IDARTNR-UT       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MID-IDFTG-UT         PIC X(2).                                    
001100*                                 FÖRETAGSID EKONOM REDOVISNING           
001200     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MID-TIGILTIG-FOM-ENTER                                            
001500                             PIC 9(6).                                    
001600*                                 GILTIGHETSDATUM FOM                     
001700     03 MID-KDANMORS-ENTER   PIC X(2).                                    
001800*                                 ORSAK TILL LEVERANSANMÄRKNING           
001900     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-TIGILTIG-FOM-NEXT                                             
002200                             PIC 9(6).                                    
002300*                                 GILTIGHETSDATUM FOM                     
002400     03 MID-KDANMORS-NEXT    PIC X(2).                                    
002500*                                 ORSAK TILL LEVERANSANMÄRKNING           
002600     03 MID-INPUT.                                                        
002700        05 MID-IDARTNR-UPP   PIC 9(9).                                    
002800*                                 ARTIKELNUMMER                           
002900        05 MID-TIGILTIG-FOM-UPP                                           
003000                             PIC 9(6).                                    
003100*                                 GILTIGHETSDATUM FOM                     
003200        05 MID-TIGILTIG-TOM-UPP                                           
003300                             PIC 9(6).                                    
003400*                                 GILTIGHETSDATUM TOM                     
003500        05 MID-IDFTG-UPP     PIC 9(2).                                    
003600*                                 FÖRETAGSID EKONOM REDOVISNING           
003700        05 MID-KDANMORS-UPP  PIC X(2).                                    
003800*                                 ORSAK TILL LEVERANSANMÄRKNING           
003900        05 MID-IDANALYS-UPP  PIC X(12).                                   
004000*                                 ANALYSNUMMER                            
004100        05 MID-IDKONTO-UPP   PIC X(10).                                   
004200*                                 KONTO                                   
004300        05 MID-IDKST-UPP     PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500        05 MID-IDUSER-UPP    PIC X(2).                                    
004600        05 MID-FLBORT-UPP    PIC X.                                       
004700*                                 BORTTAGNINGSFLAGGA                      
004800*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
