000100 01  MOD-W3O31701.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W3O317                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPROMR-IN.                                                   
000900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001000        05 MOD-IDMARKBO      PIC X.                                       
001100*                                 MARKNADSBOLAGSKOD                       
001200        05 MOD-IDPROMRN      PIC X(2).                                    
001300*                                 PRISOMRÅDE LÖPNUMMER                    
001400     03 MOD-IDPROMR-UT.                                                   
001500*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001600        05 MOD-IDMARKBO      PIC X.                                       
001700*                                 MARKNADSBOLAGSKOD                       
001800        05 MOD-IDPROMRN      PIC X(2).                                    
001900*                                 PRISOMRÅDE LÖPNUMMER                    
002000     03 MOD-IDARTNR-IN       PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDARTNR-UT       PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-IDDISTR-IN       PIC X(4).                                    
002500*                                 DISTRIKTNUMMER                          
002600     03 MOD-IDDISTR-UT       PIC X(4).                                    
002700*                                 DISTRIKTNUMMER                          
002800     03 MOD-IDPROMR-SPAR.                                                 
002900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
003000        05 MOD-IDMARKBO      PIC X.                                       
003100*                                 MARKNADSBOLAGSKOD                       
003200        05 MOD-IDPROMRN      PIC X(2).                                    
003300*                                 PRISOMRÅDE LÖPNUMMER                    
003400     03 MOD-IDARTNR-ENTER    PIC X(9).                                    
003500*                                 ARTIKELNUMMER                           
003600     03 MOD-IDARTNR-NEXT     PIC X(9).                                    
003700*                                 ARTIKELNUMMER                           
003800     03 MOD-TISTADAT-ENTER   PIC 9(6).                                    
003900*                                 GENERELLT STARTDATUM                    
004000     03 MOD-TISTADAT-NEXT    PIC 9(6).                                    
004100*                                 GENERELLT STARTDATUM                    
004200     03 MOD-W30317           OCCURS 13 TIMES.                             
004300        05 MOD-IDARTNR       PIC Z(8)9.                                   
004400*                                 ARTIKELNUMMER                           
004500        05 MOD-REARTRAB-BULK PIC Z9.9(2).                                 
004600*                                 ARTIKELRABATT BULKORDER                 
004700        05 MOD-REARTRAB-DO   PIC Z9.9(2).                                 
004800*                                 ARTIKELRABATT DAGORDER                  
004900        05 MOD-TISTADAT      PIC 9(6).                                    
005000*                                 GENERELLT STARTDATUM                    
005100        05 MOD-TISTODAT      PIC 9(6).                                    
005200*                                 GENERELLT STOPPDATUM                    
005300     03 MOD-TEMFSINF         PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*** END OF VILMAII-COPY LENGTH= 567 BYTES                                 
