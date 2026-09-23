000100 01  MID-W2I34801.                                                        
000200*                                 MID-COPYTEXT FÖR W2034800               
000300     03 MID-KDARBTYP         PIC X(4).                                    
000400*                                 TYP AV ARBETE                           
000500     03 MID-IDPERSON         PIC X(3).                                    
000600*                                 PERSONKOD                               
000700     03 MID-IDDC             PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-FLKVROS          PIC X.                                       
001000     03 MID-IDPERSON-BUY-FOM PIC X(3).                                    
001100*                                 PERSONKOD REFILLANSVARIG                
001200     03 MID-IDPERSON-BUY-TOM PIC X(3).                                    
001300*                                 PERSONKOD REFILLANSVARIG                
001400     03 MID-IDPERSON-BUY2    PIC X(3).                                    
001500*                                 PERSONKOD REFILLANSVARIG                
001600     03 MID-IDPERSON-BUY3    PIC X(3).                                    
001700*                                 PERSONKOD REFILLANSVARIG                
001800     03 MID-IDPERSON-BUY4    PIC X(3).                                    
001900*                                 PERSONKOD REFILLANSVARIG                
002000     03 MID-FLONORDER        PIC X.                                       
002100     03 MID-IDPROJ           OCCURS 3 TIMES                               
002200                             PIC X(4).                                    
002300*                                 PARTS PROJEKTIDENTITET                  
002400     03 MID-FLAK-DC          PIC X.                                       
002500     03 MID-IDLEVNR-CDC      PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700     03 MID-KDPSLLOC         PIC X(2).                                    
002800*                                 PRODUKTSLAG LOKALT                      
002900     03 MID-FLASEAS          PIC X.                                       
003000     03 MID-IDLEVNR-DC       PIC X(5).                                    
003100*                                 LEVERANTÖRNUMMER                        
003200     03 MID-IDFKNGRP-FOM     PIC X(4).                                    
003300*                                 FUNKTIONSGRUPP-FROM                     
003400     03 MID-IDFKNGRP-TOM     PIC X(4).                                    
003500*                                 FUNKTIONSGRUPP-TOM                      
003600     03 MID-FLREFILL         PIC X.                                       
003700*                                 REFILLARTIKEL                           
003800     03 MID-PRISRAD          PIC X(2).                                    
003900     03 MID-PBRAD            PIC X.                                       
004000     03 MID-FLREFBEO         PIC X.                                       
004100*                                 AUTOMATISK REFILL BEORDRING?            
004200     03 MID-IDREFTAB         PIC X.                                       
004300*                                 IDENTITET REFILLTABELL                  
004400     03 MID-VKART-TKN        PIC X.                                       
004500     03 MID-VKART            PIC X(7).                                    
004600*                                 ARTIKELVIKT (G)                         
004700     03 MID-KVPB-FOM         PIC X(7).                                    
004800*                                 PERIODBEHOV FOM (PROGNOS)               
004900     03 MID-KVPB-TOM         PIC X(7).                                    
005000*                                 PERIODBEHOV TOM (PROGNOS)               
005100     03 MID-VLARTNTO-TKN     PIC X.                                       
005200     03 MID-VLARTNTO         PIC X(9).                                    
005300*                                 ARTIKELVOLYM NETTO (CM3)                
005400     03 MID-ADLAGOMR         PIC X(2).                                    
005500*                                 LAGEROMRÅDE                             
005600     03 MID-ADGANG           PIC X(2).                                    
005700*                                 GÅNG                                    
005800     03 MID-ADPLATS-FOM      PIC X(5).                                    
005900*                                 LAGERPLATSNUMMER                        
006000     03 MID-ADPLATS-TOM      PIC X(5).                                    
006100*                                 LAGERPLATSNUMMER                        
006200     03 MID-KDERS            PIC X(2).                                    
006300*                                 ERSÄTTNINGSKOD                          
006400     03 MID-PRARTSTD-TKN     PIC X.                                       
006500     03 MID-PRARTSTD         PIC X(9).                                    
006600*                                 ARTIKELSTANDARDPRIS                     
006700     03 MID-KVLS-TKN         PIC X.                                       
006800     03 MID-KVLS             PIC X(7).                                    
006900*                                 LAGERSALDO                              
007000     03 MID-TIFINLV-TKN      PIC X.                                       
007100     03 MID-TIFINLV          PIC X(5).                                    
007200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
007300     03 MID-TIREFEFT-TKN     PIC X.                                       
007400     03 MID-TIREFEFT         PIC X(6).                                    
007500*                                 DATUM SENAST EFTERFRÅGAD                
007600     03 MID-BEART            PIC X(25).                                   
007700*                                 ARTIKELBENÄMNING                        
007800     03 MID-BEMODELL         PIC X(15).                                   
007900*                                 BILENS MODELLBESKRIVNING.               
008000     03 MID-KDREFSTA         PIC X.                                       
008100*                                 STATUS REFILLARTIKEL                    
008200     03 MID-SUPERWEEK-TKN    PIC X.                                       
008300     03 MID-SUPERWEEK        PIC X(3).                                    
008400     03 MID-FLFLYG           PIC X.                                       
008500*                                 FLYGARTIKEL                             
008600*** END OF VILMAII-COPY LENGTH= 191 BYTES                                 
