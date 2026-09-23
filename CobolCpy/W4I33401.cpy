000100 01  MID-W4I33401.                                                        
000200*                                 MID-COPYTEXT FÖR W40334                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDORDNR-IN       PIC X(5).                                    
001200*                                 ORDERNUMMER                             
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER                             
001500     03 MID-IDKOLLI-IN       PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MID-IDKOLLI-UT       PIC X(5).                                    
001800*                                 KOLLINUMMER                             
001900     03 MID-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-IDKOLLI-FIRST    PIC 9(5).                                    
002400*                                 KOLLINUMMER                             
002500     03 MID-IDKOLLI-NEXT     PIC 9(5).                                    
002600*                                 KOLLINUMMER                             
002700     03 MID-RAD              OCCURS 14 TIMES.                             
002800*                                                                         
002900        05 MID-KDCMD         PIC X.                                       
003000         88 MID-KDCMD-INGENTING                                           
003100                             VALUE ' '.                                   
003200         88 MID-KDCMD-DELETE VALUE 'D'                                    
003300                             'B'.                                         
003400         88 MID-KDCMD-REPLACE                                             
003500                             VALUE 'R'                                    
003600                             'Ä'.                                         
003700         88 MID-KDCMD-INSERT VALUE 'I'                                    
003800                             'N'.                                         
003900*                                 RAD-UPPDATERINGSKOMMANDO                
004000        05 MID-IDKOLLI       PIC 9(5).                                    
004100*                                 KOLLINUMMER                             
004200        05 MID-ADFLGEO       PIC X(3).                                    
004300*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
004400        05 MID-ADFLOMR       PIC 9(3).                                    
004500*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
004600        05 MID-ADRUTNIV      PIC 9(3).                                    
004700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
004800        05 MID-ADVMODUL      PIC 9(3).                                    
004900*                                 VÄNSTER-MODUL                           
005000        05 MID-ADHMODUL      PIC 9(3).                                    
005100*                                 HÖGER-MODUL                             
005200        05 MID-ADFLGEO-NY    PIC X(3).                                    
005300*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
005400        05 MID-ADFLOMR-NY    PIC 9(3).                                    
005500*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
005600        05 MID-ADRUTNIV-NY   PIC 9(3).                                    
005700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
005800        05 MID-KDKOLLI       PIC X(8).                                    
005900*                                 KOLLIKOD                                
