000100 01  RESP-W10213O1.                                                       
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
003500        05 RESP-LINE-TYPE-LINE                                            
003600                             PIC X.                                       
003700*                                 TYP AV STRUKTURRAD                      
003800        05 RESP-LINE-DETAILS.                                             
003900*                                                                         
004000           07 RESP-REANTPSA-LINE                                          
004100                             PIC Z9.9(3).                                 
004200*                                 ANTAL PER SATS                          
004300           07 RESP-IDLEVNR-LINE                                           
004400                             PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600           07 RESP-BELEVART-LINE                                          
004700                             PIC X(30).                                   
004800*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
004900           07 RESP-IDARTNR-LINE                                           
005000                             PIC Z(9).                                    
005100*                                 ARTIKELNUMMER                           
005200           07 RESP-BEART-LINE                                             
005300                             PIC X(25).                                   
005400*                                 ARTIKELBENÄMNING                        
005500           07 RESP-IDSTRTYP-LINE                                          
005600                             PIC X.                                       
005700*                                 STRUKTURTYP                             
005800           07 RESP-KDISATS-LINE                                           
005900                             PIC X.                                       
006000*                                 STATUSKOD I SATS                        
006100           07 RESP-TIAAVV-LINE                                            
006200                             PIC 9(4).                                    
006300*                                 ÅR - VECKA  (ÅÅVV)                      
006400           07 RESP-KDFARLIG-LINE                                          
006500                             PIC X.                                       
006600*                                 KOD FÖR FARLIGT GODS                    
006700           07 RESP-IDANSK-LINE                                            
006800                             PIC 9(3).                                    
006900*                                 ANSKAFFARNUMMER                         
007000           07 RESP-PRARTSTD-LINE                                          
007100                             PIC -(7)9.9(2).                              
007200*                                 ARTIKELSTANDARDPRIS                     
007300           07 RESP-KVVECKOR-LT-LINE                                       
007400                             PIC Z9.                                      
007500*                                 ANTAL VECKOR LEDTID                     
007600           07 RESP-ARB-SALDO-LINE                                         
007700                             PIC -(7)9.                                   
007800*                                 LAGERTILLGÅNG                           
007900        05 RESP-NOTE-LINE-FILLER REDEFINES RESP-LINE-DETAILS.             
008000           07 RESP-NOTE-LINE.                                             
008100*                                                                         
008200              09 RESP-TESTRNOT-LINE                                       
008300                             PIC X(70).                                   
008400*                                 STRUKTURNOTERING                        
008500              09 RESP-CONTINUE-LINE                                       
008600                             PIC X.                                       
008700           07 FILLER         PIC X(35).                                   
008800*** END OF VILMAII-COPY LENGTH= 62547 BYTES                               
