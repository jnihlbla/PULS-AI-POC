000100 01  MOD-W4O33401.                                                        
000200*                                 MOD-COPYTEXT FÖR W40334                 
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
001500     03 MOD-IDORDNR-IN       PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDORDNR-UT       PIC X(5).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDKOLLI-FIRST    PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900     03 MOD-IDKOLLI-NEXT     PIC 9(5).                                    
003000*                                 KOLLINUMMER                             
003100     03 MOD-RAD              OCCURS 14 TIMES.                             
003200*                                                                         
003300        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KDCMD         PIC X(2).                                    
003600*                                 MFS BEHANDLING AV INPUTFÄLT             
003700        05 MOD-IDKOLLI       PIC Z(4)9.                                   
003800*                                 KOLLINUMMER                             
003900        05 MOD-KDFLERK       PIC X.                                       
004000        05 MOD-ADFLGEO       PIC X(3).                                    
004100*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
004200        05 MOD-ADFLOMR       PIC Z(2)9.                                   
004300*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
004400        05 MOD-ADRUTNIV      PIC Z(2)9.                                   
004500*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
004600        05 MOD-ADVMODUL      PIC Z(3).                                    
004700*                                 VÄNSTER-MODUL                           
004800        05 MOD-ADHMODUL      PIC Z(3).                                    
004900*                                 HÖGER-MODUL                             
005000        05 MOD-ADFLGEO-NY-ATTR                                            
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-ADFLGEO-NY    PIC X(3).                                    
005400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
005500        05 MOD-ADFLOMR-NY-ATTR                                            
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-ADFLOMR-NY    PIC Z(2)9.                                   
005900*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
006000        05 MOD-ADRUTNIV-NY-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-ADRUTNIV-NY   PIC Z(2)9.                                   
006400*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
006500        05 MOD-KDKOLLI       PIC X(8).                                    
006600*                                 KOLLIKOD                                
006700     03 MOD-TEMFSINF         PIC X(55).                                   
006800*                                 INFORMATIONSMEDDELANDE                  
