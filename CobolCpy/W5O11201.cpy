000100 01  MOD-W5O11201.                                                        
000200*                                 COPYTEXT FÖR MOD W5O11201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-BEART            PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300     03 MOD-PRIS-BEST        OCCURS 5 TIMES.                              
001400        05 MOD-BEST-PRIS-ATTR                                             
001500                             PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700        05 MOD-BEST-PRIS.                                                 
001800           07 MOD-KDPRURSP   PIC X.                                       
001900*                                 PRISHÄRSTAMNING BESTÄLLNING             
002000           07 MOD-FILLER     PIC X.                                       
002100           07 MOD-TIPRLIST-PR                                             
002200                             PIC 9(6).                                    
002300*                                 PRISLISTEDATUM (AAMMDD)                 
002400           07 MOD-FILLER     PIC X.                                       
002500           07 MOD-IDLEVNR-PR PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700           07 MOD-FILLER     PIC X.                                       
002800           07 MOD-PRARTBES-PR                                             
002900                             PIC Z(6)9.9(2).                              
003000*                                 BESTÄLLNINGSPRIS I KRONOR               
003100           07 MOD-FILLER     PIC X.                                       
003200           07 MOD-PRARTBEL-PR                                             
003300                             PIC Z(7)9.9(5).                              
003400*                                 BESTPRIS LEVERANTÖRENS VALUTA           
003500           07 MOD-FILLER     PIC X(2).                                    
003600           07 MOD-KDSTATUS-PR                                             
003700                             PIC X(5).                                    
003800           07 MOD-FILLER     PIC X.                                       
003900           07 MOD-KDVALISO   PIC X(3).                                    
004000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004100     03 MOD-TIPRLIST-U-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-TIPRLIST-U       PIC X(2).                                    
004400*                                 MFS BEHANDLING AV INPUTFÄLT             
004500     03 MOD-IDLEVNR-ATTR     PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-IDLEVNR          PIC X(5).                                    
004800*                                 LEVERANTÖRNUMMER                        
004900     03 MOD-TEMFSINF         PIC X(55).                                   
005000*                                 INFORMATIONSMEDDELANDE                  
005100*** END OF VILMAII-COPY LENGTH= 418 BYTES                                 
