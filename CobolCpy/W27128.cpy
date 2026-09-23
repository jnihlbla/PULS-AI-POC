000100 01  W27128.                                                              
000200*                                 COPYTEXT TILL FILEN W27128              
000300*                                 SKAPA EXTRAKTFIL FÖR                    
000400*                                 UTSÖKNINGAR FRÅN 2348                   
000500*                                 DENNA INNEHÅLLER RESULTATET             
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 FLKVROS              PIC X.                                       
001100     03 IDPERSON-BUY-FOM     PIC X(3).                                    
001200*                                 PERSONKOD REFILLANSVARIG                
001300     03 IDPERSON-BUY-TOM     PIC X(3).                                    
001400*                                 PERSONKOD REFILLANSVARIG                
001500     03 IDPERSON-BUY2        PIC X(3).                                    
001600*                                 PERSONKOD REFILLANSVARIG                
001700     03 IDPERSON-BUY3        PIC X(3).                                    
001800*                                 PERSONKOD REFILLANSVARIG                
001900     03 IDPERSON-BUY4        PIC X(3).                                    
002000*                                 PERSONKOD REFILLANSVARIG                
002100     03 FLONORDER            PIC X.                                       
002200     03 IDPROJ               OCCURS 3 TIMES                               
002300                             PIC X(4).                                    
002400*                                 PARTS PROJEKTIDENTITET                  
002500     03 FLAK-DC              PIC X.                                       
002600     03 FLASEAS              PIC X.                                       
002700     03 IDLEVNR-CDC          PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900     03 KDPRODSL             PIC X(2).                                    
003000*                                 PRODUKTSLAG                             
003100     03 IDLEVNR-DC           PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300     03 IDFKNGRP-FOM         PIC X(4).                                    
003400*                                 FUNKTIONSGRUPP-FROM                     
003500     03 IDFKNGRP-TOM         PIC X(4).                                    
003600*                                 FUNKTIONSGRUPP-TOM                      
003700     03 FLREFILL             PIC X.                                       
003800*                                 REFILLARTIKEL                           
003900     03 PRISRAD              PIC X(2).                                    
004000     03 PBRAD                PIC X.                                       
004100     03 FLREFBEO             PIC X.                                       
004200*                                 AUTOMATISK REFILL BEORDRING?            
004300     03 IDREFTAB             PIC X.                                       
004400*                                 IDENTITET REFILLTABELL                  
004500     03 VKART-TKN            PIC X.                                       
004600     03 VKART                PIC X(7).                                    
004700*                                 ARTIKELVIKT (G)                         
004800     03 KVPB-REF-FOM         PIC X(8).                                    
004900*                                 PERIODBEHOV REFILLING                   
005000     03 KVPB-REF-TOM         PIC X(8).                                    
005100*                                 PERIODBEHOV REFILLING                   
005200     03 VLARTNTO-TKN         PIC X.                                       
005300     03 VLARTNTO             PIC X(9).                                    
005400*                                 ARTIKELVOLYM NETTO (CM3)                
005500     03 ADLAGOMR             PIC X(2).                                    
005600*                                 LAGEROMRÅDE                             
005700     03 ADGANG               PIC X(2).                                    
005800*                                 GÅNG                                    
005900     03 ADPLATS-FOM          PIC X(5).                                    
006000*                                 LAGERPLATSNUMMER                        
006100     03 ADPLATS-TOM          PIC X(5).                                    
006200*                                 LAGERPLATSNUMMER                        
006300     03 KDERS                PIC X(2).                                    
006400*                                 ERSÄTTNINGSKOD                          
006500     03 PRARTSTD-TKN         PIC X.                                       
006600     03 PRARTSTD             PIC X(9).                                    
006700*                                 ARTIKELSTANDARDPRIS                     
006800     03 KVLS-TKN             PIC X.                                       
006900     03 KVLS                 PIC X(7).                                    
007000*                                 LAGERSALDO                              
007100     03 TIFINLV-TKN          PIC X.                                       
007200     03 TIFINLV              PIC X(5).                                    
007300*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
007400     03 TIREFEFT-TKN         PIC X.                                       
007500     03 TIREFEFT             PIC X(6).                                    
007600*                                 DATUM SENAST EFTERFRÅGAD                
007700     03 BEART                PIC X(25).                                   
007800*                                 ARTIKELBENÄMNING                        
007900     03 IDARTNR-RES          PIC Z(7)9.                                   
008000*                                 ARTIKELNUMMER                           
008100     03 IDDC-RES             PIC X(2).                                    
008200*                                 IDENTIFIERARE LAGER                     
008300     03 BEART-RES            PIC X(25).                                   
008400*                                 ARTIKELBENÄMNING                        
008500     03 KVLS-RES             PIC -(6)9.                                   
008600*                                 LAGERSALDO                              
008700     03 ADLAGOMR-RES         PIC Z(2).                                    
008800*                                 LAGEROMRÅDE                             
008900     03 ADGANG-RES           PIC Z(2).                                    
009000*                                 GÅNG                                    
009100     03 ADPLATS-RES          PIC Z(5).                                    
009200*                                 LAGERPLATSNUMMER                        
009300     03 KVPB-REF-RES         PIC Z(5)9.9.                                 
009400*                                 PERIODBEHOV REFILLING                   
009500     03 PRARTSTD-RES         PIC 9(7)V9(2).                               
009600*                                 ARTIKELSTANDARDPRIS                     
009700     03 BEMODELL             PIC X(15).                                   
009800*                                 BILENS MODELLBESKRIVNING.               
009900     03 KDREFSTA             PIC X.                                       
010000*                                 STATUS REFILLARTIKEL                    
010100     03 SUPERWEEK-TKN        PIC X.                                       
010200     03 SUPERWEEK            PIC Z(2)9.                                   
010300     03 FLFLYG               PIC X.                                       
010400*                                 FLYGARTIKEL                             
010500*** END OF VILMAII-COPY LENGTH= 257 BYTES                                 
