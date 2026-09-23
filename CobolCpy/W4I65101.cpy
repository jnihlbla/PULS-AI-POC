000100 01  MID-W4I65101.                                                        
000200*                                 MID-COPYTEXT FÖR W40651                 
000300     03 MID-ADFLGEO-IN       PIC X(3).                                    
000400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
000500     03 MID-ADFLGEO-UT       PIC X(3).                                    
000600*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
000700     03 MID-ADFLOMR-IN       PIC X(3).                                    
000800*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
000900     03 MID-ADFLOMR-UT       PIC X(3).                                    
001000*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
001100     03 MID-ADRUTNIV-IN      PIC X(3).                                    
001200*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
001300     03 MID-ADRUTNIV-UT      PIC X(3).                                    
001400*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-ADRUTNIV-FIRST   PIC 9(3).                                    
002000*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002100     03 MID-DIDMODUL-FIRST   PIC 9(3).                                    
002200*                                 MODUL-DJUP                              
002300     03 MID-DIHMODUL-FIRST   PIC 9(3).                                    
002400*                                 MODUL-HÖJD                              
002500     03 MID-ADRUTNIV-NEXT    PIC 9(3).                                    
002600*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002700     03 MID-DIDMODUL-NEXT    PIC 9(3).                                    
002800*                                 MODUL-DJUP                              
002900     03 MID-DIHMODUL-NEXT    PIC 9(3).                                    
003000*                                 MODUL-HÖJD                              
003100     03 MID-ADVMODUL-SISTA   PIC 9(3).                                    
003200*                                 VÄNSTER-MODUL                           
003300     03 MID-ADHMODUL-SISTA   PIC 9(3).                                    
003400*                                 HÖGER-MODUL                             
003500     03 MID-ADVMODUL-FOERRA  PIC 9(3).                                    
003600*                                 VÄNSTER-MODUL                           
003700     03 MID-ADHMODUL-FOERRA  PIC 9(3).                                    
003800*                                 HÖGER-MODUL                             
003900     03 MID-RAD              OCCURS 13 TIMES.                             
004000*                                                                         
004100        05 MID-KDCMD         PIC X.                                       
004200         88 MID-KDCMD-INGENTING                                           
004300                             VALUE ' '.                                   
004400         88 MID-KDCMD-DELETE VALUE 'D'                                    
004500                             'B'.                                         
004600         88 MID-KDCMD-REPLACE                                             
004700                             VALUE 'R'                                    
004800                             'Ä'.                                         
004900         88 MID-KDCMD-INSERT VALUE 'I'                                    
005000                             'N'.                                         
005100*                                 RAD-UPPDATERINGSKOMMANDO                
005200        05 MID-ADRUTNIV      PIC 9(3).                                    
005300*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
005400        05 MID-DIDMODUL      PIC 9(3).                                    
005500*                                 MODUL-DJUP                              
005600        05 MID-DIHMODUL      PIC 9(3).                                    
005700*                                 MODUL-HÖJD                              
005800        05 MID-ADVMODUL      PIC 9(3).                                    
005900*                                 VÄNSTER-MODUL                           
006000        05 MID-ADHMODUL      PIC 9(3).                                    
006100*                                 HÖGER-MODUL                             
006200        05 MID-VLRUTNIV      PIC 9(3).                                    
006300*                                 TOT KOLLI-VOLYM I RUTA/NIV (M3)         
006400        05 MID-TESPAERR      PIC X(20).                                   
006500*                                 SPÄRRTEXT                               
006600        05 MID-VLRUTNIV-DOLT PIC 9(3).                                    
006700*                                 TOT KOLLI-VOLYM I RUTA/NIV (M3)         
006800        05 MID-FLAGGA-TRPT-DOLD                                           
006900                             PIC X.                                       
007000*                                 ALLMÄN FLAGGA (VAD ÄR DET?)             
007100     03 MID-NYUPPLAEGG.                                                   
007200*                                                                         
007300        05 MID-ADRUTNIV-NY   PIC 9(3).                                    
007400*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
007500        05 MID-DIDMODUL-NY   PIC 9(3).                                    
007600*                                 MODUL-DJUP                              
007700        05 MID-DIHMODUL-NY   PIC 9(3).                                    
007800*                                 MODUL-HÖJD                              
007900        05 MID-ADVMODUL-NY   PIC 9(3).                                    
008000*                                 VÄNSTER-MODUL                           
008100        05 MID-ADHMODUL-NY   PIC 9(3).                                    
008200*                                 HÖGER-MODUL                             
008300        05 MID-VLRUTNIV-NY   PIC 9(3).                                    
008400*                                 TOT KOLLI-VOLYM I RUTA/NIV (M3)         
008500        05 MID-TESPAERR-NY   PIC X(20).                                   
008600*                                 SPÄRRTEXT                               
