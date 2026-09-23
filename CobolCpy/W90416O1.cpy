000100 01  MOD-W90416O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9041600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-BELEVART-IN      PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001100     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400     03 MOD-FILLER           PIC X.                                       
001500     03 MOD-FILLER           PIC X(2).                                    
001600     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 MOD-IDARTNR-SPAR     PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MOD-BELEVART-UT      PIC X(30).                                   
002100*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002200     03 MOD-ARTIKELNR REDEFINES MOD-BELEVART-UT.                          
002300*                                                                         
002400        05 MOD-IDARTNR-UT    PIC X(9).                                    
002500*                                 ARTIKELNUMMER                           
002600        05 MOD-FILLER        PIC X(21).                                   
002700     03 MOD-IDSKYLT-UT       PIC X(3).                                    
002800*                                 NATIONALITETSTECKEN                     
002900*                                 SPRÅKIDENTIFIKATION                     
003000     03 MOD-FILLER           PIC X.                                       
003100     03 MOD-FILLER           PIC X(2).                                    
003200     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
003300*                                 ARTIKELNUMMER                           
003400     03 MOD-KDSTRRAD-ENTER   PIC X.                                       
003500*                                 TYP AV STRUKTURRAD                      
003600     03 MOD-IDRADNR-ENTER    PIC 9(4).                                    
003700*                                 RADNUMMER                               
003800     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
003900*                                 ARTIKELNUMMER                           
004000     03 MOD-KDSTRRAD-NEXT    PIC X.                                       
004100*                                 TYP AV STRUKTURRAD                      
004200     03 MOD-IDRADNR-NEXT     PIC 9(4).                                    
004300*                                 RADNUMMER                               
004400     03 MOD-FILLER           PIC X(25).                                   
004500     03 MOD-FILLER           PIC X(40).                                   
004600     03 MOD-FILLER           PIC X(40).                                   
004700     03 MOD-RADER            OCCURS 10 TIMES.                             
004800*                                 RADINFORMATION                          
004900        05 MOD-FILLER        PIC X(2).                                    
005000        05 MOD-FILLER        PIC X.                                       
005100        05 MOD-IDRADNR       PIC Z(3)9.                                   
005200*                                 RADNUMMER                               
005300        05 MOD-IDARTNR       PIC Z(8)9.                                   
005400*                                 ARTIKELNUMMER                           
005500        05 MOD-BEART         PIC X(25).                                   
005600*                                 ARTIKELBENÄMNING                        
005700        05 MOD-REANTPSA      PIC Z9.9(3).                                 
005800*                                 ANTAL PER SATS                          
005900        05 MOD-IDSTRTYP      PIC X.                                       
006000*                                 STRUKTURTYP                             
006100        05 MOD-KDERS         PIC Z9.                                      
006200*                                 ERSÄTTNINGSKOD                          
006300        05 MOD-IDLEVNR       PIC X(5).                                    
006400*                                 LEVERANTÖRNUMMER                        
006500        05 MOD-KDPRODSL      PIC Z9.                                      
006600*                                 PRODUKTSLAG                             
006700        05 MOD-KDPSLLOC      PIC 9(2).                                    
006800*                                 PRODUKTSLAG LOKALT                      
006900     03 MOD-FILLER           PIC X(2).                                    
007000     03 MOD-FILLER           PIC X.                                       
007100     03 MOD-TEMFSINF         PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*** END OF VILMAII-COPY LENGTH= 916 BYTES                                 
