000100 01  MOD-W0O81501.                                                        
000200*                                 MOD-COPYTEXT FÖR W08150                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDSYSMOT-IN      PIC X(10).                                   
000900*                                 PULS MOTTAGANDE SYSTEMNAMN              
001000     03 MOD-IDSYSMOT-UT      PIC X(10).                                   
001100*                                 PULS MOTTAGANDE SYSTEMNAMN              
001200     03 MOD-IDDISTR-IN       PIC X(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
001700*                                 GRUPP MED TABELL RADER                  
001800        05 MOD-CMD-ATTR      PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-CMD           PIC X.                                       
002100        05 MOD-IDLANDX2-ATTR PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-IDLANDX2      PIC X(2).                                    
002400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002500        05 MOD-ADPOSTNR-FOM-ATTR                                          
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-ADPOSTNR-FOM  PIC X(10).                                   
002900*                                 POSTNUMMER I ADRESS                     
003000        05 MOD-ADPOSTNR-TOM-ATTR                                          
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-ADPOSTNR-TOM  PIC X(10).                                   
003400*                                 POSTNUMMER I ADRESS                     
003500        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-IDDISTR       PIC Z(3)9.                                   
003800*                                 DISTRIKTNUMMER                          
003900        05 MOD-IDKUNDNR-ATTR PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004200*                                 KUNDNUMMER                              
004300        05 MOD-IDDC-ATTR     PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-IDDC          PIC X(2).                                    
004600*                                 IDENTIFIERARE LAGER                     
004700     03 MOD-CMD-E-ATTR       PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-CMD-E            PIC X.                                       
005000     03 MOD-IDLANDX2-E-ATTR  PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-IDLANDX2-E       PIC X(2).                                    
005300*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
005400     03 MOD-ADPOSTNR-FOM-E-ATTR                                           
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-ADPOSTNR-FOM-E   PIC X(10).                                   
005800*                                 POSTNUMMER I ADRESS                     
005900     03 MOD-ADPOSTNR-TOM-E-ATTR                                           
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-ADPOSTNR-TOM-E   PIC X(10).                                   
006300*                                 POSTNUMMER I ADRESS                     
006400     03 MOD-IDDISTR-E-ATTR   PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-IDDISTR-E        PIC Z(3)9.                                   
006700*                                 DISTRIKTNUMMER                          
006800     03 MOD-IDKUNDNR-E-ATTR  PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-IDKUNDNR-E       PIC Z(5)9.                                   
007100*                                 KUNDNUMMER                              
007200     03 MOD-TEMFSINF         PIC X(55).                                   
007300*                                 INFORMATIONSMEDDELANDE                  
007400*** END OF VILMAII-COPY LENGTH= 760 BYTES                                 
