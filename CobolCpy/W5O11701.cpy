000100 01  MOD-W5O11701.                                                        
000200*                                 MOD-COPYTEXT FÖR W5011700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-KDPRIBEH-IN      PIC X.                                       
001000*                                 PRISBEHANDLINGSKOD                      
001100*                                  B = BORTTAGSMARKERAD. BEH EJ           
001200*                                  J = UPPDATERAS DIREKT                  
001300*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
001400*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
001500     03 MOD-IDARTNR-UT       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-KDPRIBEH-UT      PIC X.                                       
001800*                                 PRISBEHANDLINGSKOD                      
001900*                                  B = BORTTAGSMARKERAD. BEH EJ           
002000*                                  J = UPPDATERAS DIREKT                  
002100*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
002200*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
002300     03 MOD-W5O11701-GRP     OCCURS 13 TIMES.                             
002400        05 MOD-KDBEH-ATTR    PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-KDBEH         PIC X.                                       
002700*                                 BEHANDLINGSKOD                          
002800        05 MOD-IDARTNR       PIC Z(8)9.                                   
002900*                                 ARTIKELNUMMER                           
003000        05 MOD-PRARTBEL-PR   PIC Z(7)9.9(5).                              
003100*                                 DETTA BESTÄLLNINGSPRIS                  
003200*                                 (I LEVERANTÖRENS VALUTA)                
003300        05 MOD-KDVALISO      PIC X(3).                                    
003400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003500        05 MOD-PRINK-AKT     PIC Z(6)9.9(2).                              
003600*                                 INKÖPSPRIS AKTUELLT ÅR                  
003700        05 MOD-PRINK-KOM     PIC Z(6)9.9(2).                              
003800*                                 INKÖPSPRIS NÄSTA ÅR                     
003900        05 MOD-PRARTSTD-AKT  PIC Z(6)9.9(2).                              
004000*                                 ARTIKELSTANDARDPRIS                     
004100        05 MOD-PRARTSTD-KOM  PIC Z(6)9.9(2).                              
004200*                                 ARTIKELSTANDARDPRIS                     
004300        05 MOD-KDPRIBEH-ATTR PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-KDPRIBEH      PIC X.                                       
004600*                                 PRISBEHANDLINGSKOD                      
004700*                                  B = BORTTAGSMARKERAD. BEH EJ           
004800*                                  J = UPPDATERAS DIREKT                  
004900*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
005000*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
005100     03 MOD-TEMFSINF         PIC X(55).                                   
005200*                                 INFORMATIONSMEDDELANDE                  
005300*** END OF VILMAII-COPY LENGTH= 1055 BYTES                                
