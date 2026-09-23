000100 01  MOD-W1O56101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 ILLUSTRATIONSREGISTER                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDILLU-IN        PIC X(5).                                    
000900*                                 ILLUSTRATIONENS NR                      
001000     03 MOD-IDILLU-UT        PIC X(5).                                    
001100*                                 ILLUSTRATIONENS NR                      
001200     03 MOD-IDCATNR-NEXT     PIC X(5).                                    
001300*                                 KATALOG-ID                              
001400     03 MOD-IDCATGRP-NEXT    PIC X(2).                                    
001500*                                 KATALOG-GRUPP                           
001600     03 MOD-IDCATAVS-NEXT    PIC X(4).                                    
001700*                                 KATALOG-AVSNITT                         
001800     03 MOD-KDCATPUB-NEXT    PIC X(6).                                    
001900*                                 PUBLICERINGS TIDKOD, KATALOGRAD         
002000     03 MOD-IDCATNR-ATTR     PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-IDCATNR          PIC 9(5).                                    
002300*                                 KATALOG-ID                              
002400     03 MOD-IDILLU-1-ATTR    PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-IDILLU-1         PIC Z(5).                                    
002700*                                 ILLUSTRATIONENS NR                      
002800     03 MOD-IDILLU-2-ATTR    PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDILLU-2         PIC Z(5).                                    
003100*                                 ILLUSTRATIONENS NR                      
003200     03 MOD-IDILLU-3-ATTR    PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-IDILLU-3         PIC Z(5).                                    
003500*                                 ILLUSTRATIONENS NR                      
003600     03 MOD-IDSUBNR-ATTR     PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-IDSUBNR          PIC 9.                                       
003900*                                 SUBMAPPNUMBER                           
004000     03 MOD-KDFORDON         PIC X(2).                                    
004100*                                 FORDONSSLAG                             
004200     03 MOD-IDMAPP           PIC 9(3).                                    
004300*                                 MAPPNR                                  
004400     03 MOD-IDILLU           PIC Z(5).                                    
004500*                                 ILLUSTRATIONENS NR                      
004600     03 FILLER               OCCURS 5 TIMES.                              
004700        05 MOD-IDRUBNR-ATTR  PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-IDRUBNR       PIC Z(5).                                    
005000*                                 RUBRIKNUMMER                            
005100     03 MOD-BERUBTXT         OCCURS 3 TIMES                               
005200                             PIC X(30).                                   
005300*                                 RUBRIKTEXT                              
005400     03 MOD-TIREGDAT         PIC 9(6).                                    
005500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005600     03 MOD-TIREGDAG-D       PIC 9(6).                                    
005700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005800     03 MOD-BEF-GRP          OCCURS 4 TIMES.                              
005900        05 FILLER            OCCURS 8 TIMES.                              
006000           07 MOD-KAT-TILL-GRP                                            
006100                             PIC X(17).                                   
006200     03 MOD-TENOTE-ATTR      PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-TENOTE           PIC X(40).                                   
006500*                                 NOTERINGSFÄLT                           
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 890 BYTES                                 
