000100 01  MOD-W3O31201.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W3O312                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPROMR-IN.                                                   
000900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001000        05 MOD-IDMARKBO      PIC X.                                       
001100*                                 MARKNADSBOLAGSKOD                       
001200*                                                                         
001300        05 MOD-IDPROMRN      PIC X(2).                                    
001400*                                 PRISOMRÅDE LÖPNUMMER                    
001500     03 MOD-IDPROMR-UT.                                                   
001600*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001700        05 MOD-IDMARKBO      PIC X.                                       
001800*                                 MARKNADSBOLAGSKOD                       
001900*                                                                         
002000        05 MOD-IDPROMRN      PIC X(2).                                    
002100*                                 PRISOMRÅDE LÖPNUMMER                    
002200     03 MOD-IDPARTNR-ENTER   PIC X(9).                                    
002300*                                 PARTNERNUMMER                           
002400     03 MOD-IDPARTNR-NEXT    PIC X(9).                                    
002500*                                 PARTNERNUMMER                           
002600     03 MOD-SPAR-IDPROMR.                                                 
002700*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
002800        05 MOD-IDMARKBO      PIC X.                                       
002900*                                 MARKNADSBOLAGSKOD                       
003000*                                                                         
003100        05 MOD-IDPROMRN      PIC X(2).                                    
003200*                                 PRISOMRÅDE LÖPNUMMER                    
003300     03 MOD-KDORDKL-DOG-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-KDORDKL-DOG-IN   PIC 9.                                       
003600*                                 DAGORDERKLASSGRÄNS FÖR BETALARE         
003700     03 MOD-KDORDKL-DOG-UT   PIC 9.                                       
003800*                                 DAGORDERKLASSGRÄNS FÖR BETALARE         
003900     03 MOD-W30312           OCCURS 8 TIMES.                              
004000        05 MOD-IDLANDX2      PIC X(2).                                    
004100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
004200        05 MOD-IDPARTNR      PIC X(9).                                    
004300*                                 PARTNERNUMMER                           
004400     03 MOD-KDVALIS1         PIC X(3).                                    
004500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004600     03 MOD-KDVALIS2         PIC X(3).                                    
004700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004800     03 MOD-KDVALIS3         PIC X(3).                                    
004900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005000     03 MOD-KDVALIS5         PIC X(3).                                    
005100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005200     03 MOD-KDVALIS6         PIC X(3).                                    
005300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005400     03 MOD-TEMFSINF         PIC X(55).                                   
005500*                                 INFORMATIONSMEDDELANDE                  
005600*** END OF VILMAII-COPY LENGTH= 233 BYTES                                 
