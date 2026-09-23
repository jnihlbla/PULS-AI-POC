000100 01  W5O20701.                                                            
000200*                                 COPYTEXT FÖR MOD W5O20701               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDDC-IN              PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 IDDC-UT              PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 IDFTG-UT             PIC 9(2).                                    
001600*                                 FÖRETAGSID EKONOM REDOVISNING           
001700     03 BEART                PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900     03 PRIS-BEST            OCCURS 5 TIMES.                              
002000        05 BEST-PRIS-ATTR    PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200        05 BEST-PRIS.                                                     
002300           07 KDPRURSP       PIC X.                                       
002400*                                 PRISHÄRSTAMNING BESTÄLLNING             
002500           07 FILLER         PIC X.                                       
002600           07 TIPRLIST-PR    PIC 9(6).                                    
002700*                                 PRISLISTEDATUM (AAMMDD)                 
002800           07 FILLER         PIC X.                                       
002900           07 IDLEVNR-PR     PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100           07 FILLER         PIC X(2).                                    
003200           07 PRARTBES-PR    PIC Z(6)9.9(2).                              
003300*                                 BESTÄLLNINGSPRIS I KRONOR               
003400           07 FILLER         PIC X(2).                                    
003500           07 PRARTBEL-PR    PIC Z(7)9.9(5).                              
003600*                                 BESTPRIS LEVERANTÖRENS VALUTA           
003700           07 FILLER         PIC X(2).                                    
003800           07 KDSTATUS-PR    PIC X(5).                                    
003900           07 FILLER         PIC X(3).                                    
004000           07 KDVALISO       PIC X(3).                                    
004100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004200     03 TIPRLIST-U-ATTR      PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 TIPRLIST-U           PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 IDLEVNR-ATTR         PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 IDLEVNR              PIC X(5).                                    
004900*                                 LEVERANTÖRNUMMER                        
005000     03 TEMFSINF             PIC X(55).                                   
005100*                                 INFORMATIONSMEDDELANDE                  
005200*** END OF VILMAII-COPY LENGTH= 444 BYTES                                 
