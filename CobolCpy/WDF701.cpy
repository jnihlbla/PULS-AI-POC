000100 01  MPNR-WDF701.                                                         
000200*                                 EXTRAKT FORDNR - VOLVONR                
000300*                                 FYSISK NYCKEL: WDF701KY                 
000400*                                 IDARTNR, IDPRTNER                       
000500     03 MPNR-IDARTNR         PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 MPNR-IDPRTNER        PIC S9(5)           COMP-3.                  
000800*                                 PARTNER-ID  P.A.G.                      
000900     03 MPNR-KDARTUTF        PIC X(3).                                    
001000*                                 ARTIKELUTF. VOLVO-KDP                   
001100     03 MPNR-KDPRTNER        PIC X(4).                                    
001200*                                 PARTNERKOD  P.A.G.                      
001300     03 MPNR-IDARTMPNR       PIC X(50).                                   
001400*                                 MPNR ARTIKELIDENTITET                   
001500     03 MPNR-IDARTFMC-FILLER REDEFINES MPNR-IDARTMPNR.                    
001600        05 MPNR-IDARTFMC.                                                 
001700*                                 FORD ARTIKELIDENTITET KONSTR.           
001800           07 MPNR-IDARTPFX  PIC X(6).                                    
001900*                                 ARTIKELPREFIX FORD                      
002000           07 MPNR-IDARTBAS  PIC X(8).                                    
002100*                                 BASARTIKELNUMMER FORD                   
002200           07 MPNR-IDARTSFX  PIC X(8).                                    
002300*                                 FORD ARTIKELNUMMER SUFFIX               
002400        05 FILLER            PIC X(28).                                   
002500     03 MPNR-BEARTFMC-FORD   PIC X(34).                                   
002600*                                 ARTIKELBENÄMNING FORD                   
002700     03 MPNR-BEARTFMC-KDP    PIC X(34).                                   
002800*                                 ARTIKELBENÄMNING KDP                    
002900     03 MPNR-IDPRTNER-OWNER  PIC 9(5).                                    
003000*                                 OWNER PARTNER-ID  P.A.G.                
003100     03 MPNR-FLGEMFMC        PIC X.                                       
003200*                                 GEMENSAM FORD/MPNR ARTIKEL              
003300     03 MPNR-BETEXT-FMC      PIC X(20).                                   
003400*                                 KOMMENTAR FORD                          
003500     03 MPNR-BETEXT-KDP      PIC X(20).                                   
003600*                                 KOMMENTAR KDP                           
003700     03 MPNR-IDCDS           PIC X(8).                                    
003800*                                 ANVÄNDARENS CDS ID                      
003900     03 MPNR-DAREGFMC        PIC X(10).                                   
004000*                                 REG.DATUM FORD (ÅÅÅÅ-MM-DD)             
004100     03 MPNR-TIREGFMC        PIC X(8).                                    
004200*                                 REGISTRERINGSTID, FORD                  
004300     03 MPNR-TIDATETIME-9KOMPL                                            
004400                             PIC X(14).                                   
004500*                                 DATUMTIDS 9-KOMPLEMENT                  
004600     03 MPNR-FILLER          PIC X(20).                                   
004700*** END OF VILMAII-COPY LENGTH= 239 BYTES                                 
