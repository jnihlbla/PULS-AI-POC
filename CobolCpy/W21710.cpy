000100 01  W21710.                                                              
000200*                                 COPY-TEXT FÖR FILEN W21710              
000300     03 RAD1.                                                             
000400        05 IDUSER            PIC X(8).                                    
000500*                                 ANVÄNDARENS SÄKERHETS ID                
000600        05 FLAGGA            OCCURS 52 TIMES                              
000700                             PIC X.                                       
000800*                                 ALLMÄN FLAGGA                           
000900        05 FILLER            PIC X(20).                                   
001000     03 RAD2 REDEFINES RAD1.                                              
001100        05 IDANSK-FOM        PIC 9(3).                                    
001200*                                 ANSKAFFARNUMMER                         
001300        05 IDANSK-TOM        PIC 9(3).                                    
001400*                                 ANSKAFFARNUMMER                         
001500        05 IDANSK            OCCURS 5 TIMES                               
001600                             PIC 9(3).                                    
001700*                                 ANSKAFFARNUMMER                         
001800        05 IDLEVNR           OCCURS 9 TIMES                               
001900                             PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 KVVECKOR-AVROP    PIC 9(2).                                    
002200*                                 ANTAL VECKOR                            
002300        05 KVVECKOR-KVPB     PIC 9(2).                                    
002400*                                 ANTAL VECKOR                            
002500        05 KDPRODSL-FOM      PIC 9(2).                                    
002600*                                 PRODUKTSLAG                             
002700        05 KDPRODSL-TOM      PIC 9(2).                                    
002800*                                 PRODUKTSLAG                             
002900        05 IDPROJ-URV        PIC X(4).                                    
003000*                                 PARTS PROJEKTIDENTITET                  
003100        05 FLAGGA-IDLEVNR-SHIP                                            
003200                             PIC X.                                       
003300*                                 ALLMÄN FLAGGA                           
003400        05 FLAGGA-IDBERED    PIC X.                                       
003500*                                 ALLMÄN FLAGGA                           
003600     03 RAD3-FILLER REDEFINES RAD1.                                       
003700        05 RAD3.                                                          
003800           07 KDERS-FOM      PIC 9(2).                                    
003900*                                 ERSÄTTNINGSKOD                          
004000           07 KDERS-TOM      PIC 9(2).                                    
004100*                                 ERSÄTTNINGSKOD                          
004200           07 KDERS          OCCURS 5 TIMES                               
004300                             PIC 9(2).                                    
004400*                                 ERSÄTTNINGSKOD                          
004500           07 IDFKNGRP-FOM   PIC 9(4).                                    
004600*                                 FUNKTIONSGRUPP                          
004700           07 IDFKNGRP-TOM   PIC 9(4).                                    
004800*                                 FUNKTIONSGRUPP                          
004900           07 IDFKNGRP       OCCURS 4 TIMES                               
005000                             PIC 9(4).                                    
005100*                                 FUNKTIONSGRUPP                          
005200           07 BEFT           OCCURS 4 TIMES                               
005300                             PIC 9(2).                                    
005400*                                 FÖRPACKNINGSTYP                         
005500           07 BEART-SOEK     PIC X(25).                                   
005600*                                 ARTIKELBENÄMNING                        
005700           07 ADLAGOMR       PIC 9(2).                                    
005800*                                 LAGEROMRÅDE                             
005900           07 ADGANG-FOM     PIC 9(2).                                    
006000*                                 GÅNG FRÅN OCH MED                       
006100           07 ADGANG-TOM     PIC 9(2).                                    
006200*                                 GÅNG TILL OCH MED                       
006300           07 KDOTFREK       PIC X.                                       
006400*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
006500        05 FILLER            PIC X(2).                                    
006600*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
