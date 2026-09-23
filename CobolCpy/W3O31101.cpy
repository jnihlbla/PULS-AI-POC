000100 01  MOD-W3O31101.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W3O311                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPARTNR-IN      PIC X(9).                                    
000900*                                 FINANCIELL KUND                         
001000     03 MOD-IDPARTNR-UT      PIC X(9).                                    
001100*                                 FINANCIELL KUND                         
001200     03 MOD-IDFTG-IN         PIC X(2).                                    
001300*                                 FÖRETAGSID EKONOM REDOVISNING           
001400     03 MOD-IDFTG-UT         PIC X(2).                                    
001500*                                 FÖRETAGSID EKONOM REDOVISNING           
001600     03 MOD-IDDISTR-IN       PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 MOD-IDDISTR-UT       PIC X(4).                                    
001900*                                 DISTRIKTNUMMER                          
002000     03 MOD-IDPARTNR-ENTER   PIC X(9).                                    
002100*                                 FINANCIELL KUND                         
002200     03 MOD-IDDISTR-ENTER    PIC Z(3)9.                                   
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-BEBETRAD-1       PIC X(35).                                   
002500*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002600     03 MOD-BEBETRAD-2       PIC X(35).                                   
002700*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002800     03 MOD-IDPARTNR-INFO    PIC X(9).                                    
002900*                                 FINANCIELL KUND                         
003000     03 MOD-ADBETRAD-1       PIC X(35).                                   
003100*                                 ADRESSRAD BETALNINGSANSVARIG            
003200     03 MOD-ADBETRAD-2       PIC X(35).                                   
003300*                                 ADRESSRAD BETALNINGSANSVARIG            
003400     03 MOD-IDMARKBO-ATTR    PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-IDMARKBO-IN      PIC X.                                       
003700*                                 MARKNADSBOLAGSKOD                       
003800     03 MOD-IDMARKBO-UT      PIC X.                                       
003900*                                 MARKNADSBOLAGSKOD                       
004000     03 MOD-IDPROMR-ATTR     PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-IDPROMR-IN.                                                   
004300*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
004400        05 MOD-IDMARKBO      PIC X.                                       
004500*                                 MARKNADSBOLAGSKOD                       
004600        05 MOD-IDPROMRN      PIC X(2).                                    
004700*                                 PRISOMRÅDE LÖPNUMMER                    
004800     03 MOD-IDPROMR-UT.                                                   
004900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
005000        05 MOD-IDMARKBO      PIC X.                                       
005100*                                 MARKNADSBOLAGSKOD                       
005200        05 MOD-IDPROMRN      PIC X(2).                                    
005300*                                 PRISOMRÅDE LÖPNUMMER                    
005400     03 MOD-FLARTRAB-ATTR    PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-FLARTRAB-IN      PIC X.                                       
005700*                                 ALTERNATIV RABATTKOD ART.PRIS           
005800     03 MOD-FLARTRAB-UT      PIC X.                                       
005900*                                 ALTERNATIV RABATTKOD ART.PRIS           
006000     03 MOD-KDVALIS1         PIC X(3).                                    
006100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006200     03 MOD-KDVALIS2         PIC X(3).                                    
006300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006400     03 MOD-KDVALIS3         PIC X(3).                                    
006500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006600     03 MOD-KDVALIS5         PIC X(3).                                    
006700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006800     03 MOD-KDVALIS6         PIC X(3).                                    
006900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007000     03 MOD-TEMFSINF         PIC X(55).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END OF VILMAII-COPY LENGTH= 322 BYTES                                 
