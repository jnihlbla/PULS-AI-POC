000100 01  MID-W1I53101.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 KATALOGIDENTITET                        
000400     03 MID-IDCATNR-IN       PIC X(5).                                    
000500*                                 KATALOG-ID                              
000600     03 MID-IDCATNR-UT       PIC X(5).                                    
000700*                                 KATALOG-ID                              
000800     03 MID-BECAT.                                                        
000900        05 MID-BECAT-RAD1    PIC X(40).                                   
001000*                                 KATALOGBETECKNING                       
001100*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
001200        05 MID-BECAT-RAD2    PIC X(20).                                   
001300*                                 KATALOGBETECKNING                       
001400*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
001500     03 MID-BEEMBLEM         PIC X(5).                                    
001600*                                 EMBLEM                                  
001700     03 MID-BEMASTER         PIC X(12).                                   
001800*                                 MASTERNAMN FÖR FORDON                   
001900     03 MID-KDFORDON         PIC X(2).                                    
002000*                                 FORDONSSLAG                             
002100     03 MID-FLKOPIE          PIC X.                                       
002200*                                 TILLÅTEN ATT LÅNA HÄRIFRÅN              
002300     03 MID-TIOMBRYT-F       PIC 9(6).                                    
002400*                                 OMBRYTNINGSDATUM                        
002500     03 MID-IDILLU           PIC 9(5).                                    
002600*                                 ILLUSTRATIONENS NR                      
002700     03 MID-TIOMBRYT         OCCURS 17 TIMES                              
002800                             PIC 9(6).                                    
002900*                                 OMBRYTNINGSDATUM                        
003000     03 MID-TENOTE           PIC X(40).                                   
003100*                                 NOTERINGSFÄLT                           
003200*** END OF VILMAII-COPY LENGTH= 243 BYTES                                 
