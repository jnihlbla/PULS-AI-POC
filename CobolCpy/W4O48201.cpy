000100 01  MOD-W4O48201.                                                        
000200*                                 COPYTEXT FÖR MID W4O48201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPARTNR-IN      PIC X(9).                                    
000800*                                 FINANCIELL KUND                         
000900     03 MOD-IDPARTNR-UT      PIC X(9).                                    
001000*                                 FINANCIELL KUND                         
001100     03 MOD-IDFTG-IN         PIC X(2).                                    
001200*                                 FÖRETAGSID EKONOM REDOVISNING           
001300     03 MOD-IDFTG-UT         PIC X(2).                                    
001400*                                 FÖRETAGSID EKONOM REDOVISNING           
001500     03 MOD-IDLAND-IN        PIC X(2).                                    
001600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001700     03 MOD-IDLAND-UT        PIC X(2).                                    
001800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001900     03 MOD-IDDISTR-IN       PIC X(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MOD-IDDISTR-UT       PIC X(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
002400*                                 KUNDNUMMER                              
002500     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002600*                                 KUNDNUMMER                              
002700     03 MOD-FLVISA-IN        PIC X.                                       
002800*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
002900     03 MOD-FLVISA-UT-ATTR   PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-FLVISA-UT        PIC X.                                       
003200*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
003300     03 MOD-OUTPUT           OCCURS 13 TIMES.                             
003400        05 MOD-IDPARTNR      PIC X(9).                                    
003500*                                 FINANCIELL KUND                         
003600        05 MOD-BEBET         PIC X(27).                                   
003700*                                 DEL AV NAMN        BEBET-DEL-00         
003800*                                 2                                       
003900        05 MOD-IDDISTR       PIC Z(3)9.                                   
004000*                                 DISTRIKTNUMMER                          
004100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004200*                                 KUNDNUMMER                              
004300        05 MOD-BEGODSM       PIC X(27).                                   
004400*                                 DEL AV GODSMOTTAGARNAMN                 
004500     03 MOD-TEMFSINF         PIC X(55).                                   
004600*                                 INFORMATIONSMEDDELANDE                  
004700*** END OF VILMAII-COPY LENGTH= 1098 BYTES                                
