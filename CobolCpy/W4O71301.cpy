000100 01  MOD-W4O71301.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4071300           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDRAPPNR-IN      PIC X(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-IDARTNR-IN       PIC X(8).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-IDARTNR-UT       PIC X(8).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDRADNR-IN       PIC X(4).                                    
002400*                                 RADNUMMER                               
002500     03 MOD-IDRADNR-UT       PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-IDARTNR-ENTER    PIC 9(8).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MOD-IDRADNR-ENTER    PIC 9(5).                                    
003000*                                 RADNUMMER                               
003100     03 MOD-IDARTNR-NEXT     PIC 9(8).                                    
003200*                                 ARTIKELNUMMER                           
003300     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
003400*                                 RADNUMMER                               
003500     03 MOD-TILEVANM         PIC 9(6).                                    
003600*                                 DATUM LEVERANSANMÄRKNING                
003700     03 MOD-IDKONTO-TXT      PIC X(13).                                   
003800     03 MOD-LEVANM-RAD       OCCURS 12 TIMES.                             
003900*                                 LEVERANSANM.RAD I PGM W4071300          
004000        05 MOD-IDARTNR       PIC Z(7)9.                                   
004100*                                 ARTIKELNUMMER                           
004200        05 MOD-IDRADNR       PIC Z(3)9.                                   
004300*                                 RADNUMMER                               
004400        05 MOD-IDKOLLI       PIC Z(4)9.                                   
004500*                                 KOLLINUMMER                             
004600        05 FILLER            PIC X.                                       
004700        05 MOD-IDORDNR       PIC Z(4)9.                                   
004800*                                 ORDERNUMMER UTGÅR PD90                  
004900        05 FILLER            PIC X.                                       
005000        05 MOD-BEART         PIC X(12).                                   
005100*                                                       BEART-004         
005200*                                 BENÄMNING ENLIGT SPRÅKKOD               
005300        05 FILLER            PIC X.                                       
005400        05 MOD-KVLEVANM      PIC Z(5)9.                                   
005500*                                 LEVERANSANMÄRKNINGSANTAL                
005600        05 FILLER            PIC X.                                       
005700        05 MOD-KDANMORS      PIC X(2).                                    
005800*                                 ORSAK TILL LEVERANSANMÄRKNING           
005900        05 FILLER            PIC X.                                       
006000        05 MOD-IDFTG         PIC 9(2).                                    
006100*                                 FÖRETAGSID EKONOM REDOVISNING           
006200        05 FILLER            PIC X.                                       
006300        05 MOD-IDANALYS      PIC Z(12).                                   
006400*                                 ANALYSNUMMER                            
006500        05 FILLER            PIC X.                                       
006600        05 MOD-IDKONTO       PIC Z(10).                                   
006700*                                 KONTO                                   
006800        05 MOD-IDKST REDEFINES MOD-IDKONTO                                
006900                             PIC X(10).                                   
007000*                                 KOSTNADSSTÄLLE                          
007100        05 FILLER            PIC X(6).                                    
007200     03 MOD-RAD19-IDARTNR-ATTR                                            
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-RAD19-IDARTNR    PIC X(8).                                    
007600*                                 ARTIKELNUMMER                           
007700     03 MOD-RAD19-IDRADNR-ATTR                                            
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-RAD19-IDRADNR    PIC X(4).                                    
008100*                                 RADNUMMER                               
008200     03 MOD-RAD19-IDFTG-ATTR PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-RAD19-IDFTG      PIC X(2).                                    
008500*                                 FÖRETAGSID EKONOM REDOVISNING           
008600     03 MOD-RAD19-IDANALYSNR-ATTR                                         
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-RAD19-IDANALYSNR PIC X(8).                                    
009000*                                 ANALYSNUMMER                            
009100     03 MOD-TEMFSINF         PIC X(55).                                   
009200*                                 INFORMATIONSMEDDELANDE                  
009300*** END OF VILMAII-COPY LENGTH= 1180 BYTES                                
