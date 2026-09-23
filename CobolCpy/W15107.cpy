000100 01  W15107.                                                              
000200*                                 MELLANLAGRINGS-FIL I PROCEDUR           
000300*                                 W151P007                                
000400*                                 LISTFIL MED UTDRAG AV RUBRIK-           
000500*                                 ER PÅ RAD 1 I VISS KATALOG.             
000600     03 IDCATNR              PIC 9(5).                                    
000700*                                 KATALOG-ID                              
000800     03 BEEMBLEM             PIC X(5).                                    
000900*                                 EMBLEM                                  
001000     03 IDCATGRP-FRAN        PIC 9(2).                                    
001100*                                 KATALOG-GRUPP                           
001200     03 IDCATGRP-TILL        PIC 9(2).                                    
001300*                                 KATALOG-GRUPP                           
001400     03 IDSKYLT              PIC X(3).                                    
001500*                                 NATIONALITETSTECKEN                     
001600*                                 SPRÅKIDENTIFIKATION                     
001700     03 IDCATGRP-H           PIC 9(2).                                    
001800*                                 KATALOG-GRUPP                           
001900     03 IDCATGRP-U           PIC 9(2).                                    
002000*                                 KATALOG-GRUPP                           
002100     03 IDCATAVS             PIC 9(4).                                    
002200*                                 KATALOG-AVSNITT                         
002300     03 BERUBTXT             PIC X(30).                                   
002400*                                 RUBRIKTEXT                              
002500     03 RAD-FINNS            PIC X.                                       
002600     03 IDILLU               PIC S9(5)           COMP-3.                  
002700*                                 ILLUSTRATIONENS NR                      
002800     03 NYILLU               PIC X.                                       
002900     03 DIGILL               PIC X.                                       
003000     03 FLAVSTVAD            PIC X.                                       
003100*                                 AVSNITTET SKICKAS TILL VADIS?           
003200     03 FLAVSUST             PIC X.                                       
003300*                                 AVSNITTET ÄNDRAT?                       
003400     03 KDPRTVAL             PIC X.                                       
003500*                                 PRINTER-VAL KOD                         
003600     03 IDSKYLT-H            PIC X(3).                                    
003700*                                 NATIONALITETSTECKEN                     
003800*                                 SPRÅKIDENTIFIKATION                     
003900     03 IDLISTA              PIC X(3).                                    
004000*                                 LISTNUMMER                              
004100     03 KDCATPUB-R           PIC X(3).                                    
004200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004300     03 KDCATPUB-R-FOM       PIC X(3).                                    
004400*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004500     03 KDCATPUB-R-TOM       PIC X(3).                                    
004600*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004700*** END OF VILMAII-COPY LENGTH= 79 BYTES                                  
