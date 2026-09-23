000100 01  W2172010.                                                            
000200*                                 COPY-TEXT FÖR FILEN W2172010            
000300     03 RAD1-FILLER.                                                      
000400        05 RAD1.                                                          
000500           07 IDUSER         PIC X(8).                                    
000600*                                 ANVÄNDARENS SÄKERHETS ID                
000700           07 FLAGGA         OCCURS 47 TIMES                              
000800                             PIC X.                                       
000900*                                 ALLMÄN FLAGGA                           
001000           07 FILLER         PIC X(24).                                   
001100        05 FILLER            PIC X.                                       
001200     03 RAD2 REDEFINES RAD1-FILLER.                                       
001300        05 IDDC              PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500        05 IDANSK-FOM        PIC 9(3).                                    
001600*                                 ANSKAFFARNUMMER                         
001700        05 IDANSK-TOM        PIC 9(3).                                    
001800*                                 ANSKAFFARNUMMER                         
001900        05 IDANSK            OCCURS 5 TIMES                               
002000                             PIC 9(3).                                    
002100*                                 ANSKAFFARNUMMER                         
002200        05 FLAGGA-IDBERED    PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400        05 FLAGGA-IDLEVNR-SHIP                                            
002500                             PIC X.                                       
002600*                                 ALLMÄN FLAGGA                           
002700        05 IDLEVNR           OCCURS 5 TIMES                               
002800                             PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000        05 KDERS-FOM         PIC 9(2).                                    
003100*                                 ERSÄTTNINGSKOD                          
003200        05 KDERS-TOM         PIC 9(2).                                    
003300*                                 ERSÄTTNINGSKOD                          
003400        05 KDERS             OCCURS 5 TIMES                               
003500                             PIC 9(2).                                    
003600*                                 ERSÄTTNINGSKOD                          
003700        05 FLERSDAT-VIPS     PIC X.                                       
003800*                                 ALLMÄN FLAGGA                           
003900        05 FILLER            PIC X(15).                                   
004000     03 RAD3 REDEFINES RAD1-FILLER.                                       
004100        05 IDFKNGRP-FOM      PIC 9(4).                                    
004200*                                 FUNKTIONSGRUPP                          
004300        05 IDFKNGRP-TOM      PIC 9(4).                                    
004400*                                 FUNKTIONSGRUPP                          
004500        05 IDFKNGRP          OCCURS 5 TIMES                               
004600                             PIC 9(4).                                    
004700*                                 FUNKTIONSGRUPP                          
004800        05 BEFT              OCCURS 4 TIMES                               
004900                             PIC 9(2).                                    
005000*                                 FÖRPACKNINGSTYP                         
005100        05 BEART-SOEK        PIC X(25).                                   
005200*                                 ARTIKELBENÄMNING                        
005300        05 KDPRODSL-FOM      PIC 9(2).                                    
005400*                                 PRODUKTSLAG                             
005500        05 KDPRODSL-TOM      PIC 9(2).                                    
005600*                                 PRODUKTSLAG                             
005700        05 IDPROJ-URV        PIC X(4).                                    
005800*                                 PARTS PROJEKTIDENTITET                  
005900        05 ADLAGOMR          PIC 9(2).                                    
006000*                                 LAGEROMRÅDE                             
006100        05 ADGANG-FOM        PIC 9(2).                                    
006200*                                 GÅNG FRÅN OCH MED                       
006300        05 ADGANG-TOM        PIC 9(2).                                    
006400*                                 GÅNG TILL OCH MED                       
006500        05 KVVECKOR-AVROP    PIC 9(2).                                    
006600*                                 ANTAL VECKOR                            
006700        05 KVVECKOR-KVPB     PIC 9(2).                                    
006800*                                 ANTAL VECKOR                            
006900        05 FILLER            PIC X.                                       
007000*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
