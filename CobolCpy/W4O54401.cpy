000100 01  MOD-W4O54401.                                                        
000200*                                 MOD-COPYTEXT FÖR W40544                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-IDSKEPPN-IN      PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDSKEPPN-UT-GRP.                                              
002200        05 MOD-FILLERX2      PIC X(2).                                    
002300        05 MOD-IDSKEPPN-UT   PIC X(7).                                    
002400*                                 SKEPPNINGSNUMMER                        
002500     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002600*                                 MFS BEHANDLING AV INPUTFÄLT             
002700     03 MOD-IDPRODNR-UT      PIC X(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900     03 MOD-IDKUNDNR-FIRST   PIC 9(6).                                    
003000*                                 KUNDNUMMER                              
003100     03 MOD-KDFRAKT-FIRST    PIC 9(2).                                    
003200*                                 FRAKTSÄTT DC TILL KUND                  
003300     03 MOD-KDORDKLX-FIRST   PIC X.                                       
003400*                                 ORDERKLASS + BLANK                      
003500     03 MOD-IDTRPTNR-FIRST   PIC 9(3).                                    
003600*                                 TRANSPORTIDENTITET                      
003700     03 MOD-ADFLGEO-FIRST    PIC X(3).                                    
003800*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
003900     03 MOD-ADFLOMR-FIRST    PIC 9(3).                                    
004000*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
004100     03 MOD-ADRUTNIV-FIRST   PIC 9(3).                                    
004200*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
004300     03 MOD-IDKUNDNR-NEXT    PIC 9(6).                                    
004400*                                 KUNDNUMMER                              
004500     03 MOD-KDFRAKT-NEXT     PIC 9(2).                                    
004600*                                 FRAKTSÄTT DC TILL KUND                  
004700     03 MOD-KDORDKLX-NEXT    PIC X.                                       
004800*                                 ORDERKLASS + BLANK                      
004900     03 MOD-IDTRPTNR-NEXT    PIC 9(3).                                    
005000*                                 TRANSPORTIDENTITET                      
005100     03 MOD-ADFLGEO-NEXT     PIC X(3).                                    
005200*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
005300     03 MOD-ADFLOMR-NEXT     PIC 9(3).                                    
005400*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
005500     03 MOD-ADRUTNIV-NEXT    PIC 9(3).                                    
005600*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
005700     03 MOD-RAD              OCCURS 14 TIMES.                             
005800*                                                                         
005900        05 MOD-IDTRPTNR      PIC Z(2)9.                                   
006000*                                 TRANSPORTIDENTITET                      
006100        05 MOD-FILLERX2      PIC X(2).                                    
006200        05 MOD-IDKUNDNR      PIC Z(6).                                    
006300*                                 KUNDNUMMER                              
006400        05 MOD-FILLERX2      PIC X(2).                                    
006500        05 MOD-KDFRAKT       PIC Z(2).                                    
006600*                                 FRAKTSÄTT DC TILL KUND                  
006700        05 FILLER            PIC X(3).                                    
006800        05 MOD-KDORDKLX      PIC X.                                       
006900*                                 ORDERKLASS + BLANK                      
007000        05 MOD-FILLERX2      PIC X(2).                                    
007100        05 MOD-IDDC-CROSS    PIC X(2).                                    
007200*                                 DC FÖR CROSS DOCKING                    
007300        05 FILLER            PIC X(7).                                    
007400        05 MOD-ADFLGEO       PIC X(3).                                    
007500*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
007600        05 FILLER            PIC X.                                       
007700        05 MOD-ADFLOMR       PIC Z(2)9.                                   
007800*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
007900        05 FILLER            PIC X.                                       
008000        05 MOD-ADRUTNIV      PIC Z(2)9.                                   
008100*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
008200        05 MOD-FILLERX2      PIC X(2).                                    
008300        05 MOD-TEFLNOTE      PIC X(20).                                   
008400*                                 NOTERING FÄRDIGLAGRET                   
008500     03 MOD-TEMFSINF         PIC X(55).                                   
008600*                                 INFORMATIONSMEDDELANDE                  
008700*** END OF VILMAII-COPY LENGTH= 1061 BYTES                                
