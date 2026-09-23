000100 01  RESP-W1W213O1.                                                       
000200*                                 RESP-COPYTEXT FÖR W1W21300              
000300*                                                                         
000400     03 RESP-IDRADNR-START   PIC 9(4).                                    
000500*                                 RADNUMMER                               
000600     03 RESP-IDRADNR-NEXT    PIC 9(4).                                    
000700*                                 RADNUMMER                               
000800     03 RESP-KVRADER         PIC 9(5).                                    
000900*                                 ANTAL RADER                             
001000     03 RESP-BEART           PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 RESP-KDPRODSL        PIC Z9.                                      
001300*                                 PRODUKTSLAG                             
001400     03 RESP-IDFKNGRP        PIC Z(3)9.                                   
001500*                                 FUNKTIONSGRUPP                          
001600     03 RESP-IDSTRTYP        PIC X.                                       
001700*                                 STRUKTURTYP                             
001800     03 RESP-KDPSLLOC        PIC 9(2).                                    
001900*                                 PRODUKTSLAG LOKALT                      
002000     03 RESP-OUTPUT          OCCURS 500 TIMES.                            
002100*                                                                         
002200        05 RESP-SELECT-LINE-ATTR                                          
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 RESP-SELECT-LINE  PIC X.                                       
002600        05 RESP-IDRADNR-LINE PIC 9(4).                                    
002700*                                 RADNUMMER                               
002800        05 RESP-ADLAGOMR-LINE                                             
002900                             PIC Z(2)9.                                   
003000*                                 LAGEROMRÅDE                             
003100        05 RESP-ADGANG-LINE  PIC Z(2)9.                                   
003200*                                 GÅNG                                    
003300        05 RESP-ADPLATS-LINE PIC Z(4)9.                                   
003400*                                 LAGERPLATSNUMMER                        
003500        05 RESP-UTRAD-LINE   PIC X(72).                                   
003600        05 RESP-LINE REDEFINES RESP-UTRAD-LINE.                           
003700*                                                                         
003800           07 RESP-REANTPSA-LINE                                          
003900                             PIC Z9.9(3).                                 
004000*                                 ANTAL PER SATS                          
004100           07 RESP-FILLER    PIC X(7).                                    
004200           07 RESP-IDARTNR-LINE                                           
004300                             PIC Z(9).                                    
004400*                                 ARTIKELNUMMER                           
004500           07 RESP-FILLER    PIC X(22).                                   
004600           07 RESP-BEART-LINE                                             
004700                             PIC X(15).                                   
004800           07 RESP-FILLER    PIC X.                                       
004900           07 RESP-IDSTRTYP-LINE                                          
005000                             PIC X.                                       
005100*                                 STRUKTURTYP                             
005200           07 RESP-FILLERX2  PIC X(2).                                    
005300           07 RESP-KDISATS-LINE                                           
005400                             PIC X.                                       
005500*                                 STATUSKOD I SATS                        
005600           07 RESP-FILLER    PIC X.                                       
005700           07 RESP-TIAAVV-LINE                                            
005800                             PIC Z(4).                                    
005900*                                 ÅR - VECKA  (ÅÅVV)                      
006000           07 RESP-FILLERX2  PIC X(2).                                    
006100           07 RESP-KDFARLIG-LINE                                          
006200                             PIC X.                                       
006300*                                 KOD FÖR FARLIGT GODS                    
006400*** END OF VILMAII-COPY LENGTH= 45047 BYTES                               
