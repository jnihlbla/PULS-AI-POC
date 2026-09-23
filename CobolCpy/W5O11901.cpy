000100 01  W5O11901.                                                            
000200*                                 COPYTEXT FÖR MOD W5O11901               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDLEVNR-IN           PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 IDLEVNR-UT           PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 BEART                PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 PRINK                PIC Z(6)9.9(2).                              
001800*                                 INKÖPSPRIS                              
001900     03 PRARTSTD             PIC Z(6)9.9(2).                              
002000*                                 ARTIKELSTANDARDPRIS                     
002100     03 PRARTBES             PIC Z(6)9.9(2).                              
002200*                                 BESTÄLLNINGSPRIS I KRONOR               
002300     03 PRARTSJK             PIC Z(6)9.9(2).                              
002400*                                 ARTIKELNS SJÄLVKOSTNAD                  
002500     03 KDPSLLOC             PIC 9(2).                                    
002600*                                 PRODUKTSLAG LOKALT                      
002700     03 IDANSK               PIC Z(2)9.                                   
002800*                                 ANSKAFFARNUMMER                         
002900     03 IDINK                PIC X(4).                                    
003000*                                 INKÖPARNUMMER                           
003100     03 IDLEVNR              PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300     03 BEST-PRIS            OCCURS 5 TIMES.                              
003400        05 KDPRURSP-PR-ATTR  PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 KDPRURSP-PR       PIC X.                                       
003700*                                 PRISHÄRSTAMNING BESTÄLLNING             
003800        05 TIPRLIST-PR-ATTR  PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 TIPRLIST-PR       PIC 9(6).                                    
004100*                                 PRISLISTEDATUM (AAMMDD)                 
004200        05 PRARTBEL-PR-ATTR  PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 PRARTBEL-PR       PIC Z(7)9.9(5).                              
004500*                                 BESTPRIS LEVERANTÖRENS VALUTA           
004600        05 KDVALISO-PR-ATTR  PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 KDVALISO-PR       PIC X(3).                                    
004900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005000     03 KDPRURSP-ATTR        PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 KDPRURSP             PIC X.                                       
005300*                                 PRISHÄRSTAMNING BESTÄLLNING             
005400     03 TIPRLIST-ATTR        PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 TIPRLIST             PIC 9(6).                                    
005700*                                 PRISLISTEDATUM (AAMMDD)                 
005800     03 PRARTBEL-ATTR        PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 PRARTBEL             PIC X(14).                                   
006100*                                 BESTPRIS LEVERANTÖRENS VALUTA           
006200     03 KDVALISO-ATTR        PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 KDVALISO             PIC X(3).                                    
006500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006600     03 IDDC-ATTR            PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 IDDC                 PIC X(2).                                    
006900*                                 IDENTIFIERARE LAGER                     
007000     03 TEMFSINF             PIC X(55).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END OF VILMAII-COPY LENGTH= 402 BYTES                                 
