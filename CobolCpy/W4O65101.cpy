000100 01  MOD-W4O65101.                                                        
000200*                                 MOD-COPYTEXT FÖR W40651                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-ADFLGEO-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-ADFLGEO-UT       PIC X(3).                                    
001000*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
001100     03 MOD-ADFLOMR-IN       PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-ADFLOMR-UT       PIC X(3).                                    
001400*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
001500     03 MOD-ADRUTNIV-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-ADRUTNIV-UT      PIC X(3).                                    
001800*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-ADRUTNIV-FIRST   PIC 9(3).                                    
002400*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002500     03 MOD-DIDMODUL-FIRST   PIC 9(3).                                    
002600*                                 MODUL-DJUP                              
002700     03 MOD-DIHMODUL-FIRST   PIC 9(3).                                    
002800*                                 MODUL-HÖJD                              
002900     03 MOD-ADRUTNIV-NEXT    PIC 9(3).                                    
003000*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003100     03 MOD-DIDMODUL-NEXT    PIC 9(3).                                    
003200*                                 MODUL-DJUP                              
003300     03 MOD-DIHMODUL-NEXT    PIC 9(3).                                    
003400*                                 MODUL-HÖJD                              
003500     03 MOD-ADVMODUL-SISTA   PIC 9(3).                                    
003600*                                 VÄNSTER-MODUL                           
003700     03 MOD-ADHMODUL-SISTA   PIC 9(3).                                    
003800*                                 HÖGER-MODUL                             
003900     03 MOD-ADVMODUL-FOERRA  PIC 9(3).                                    
004000*                                 VÄNSTER-MODUL                           
004100     03 MOD-ADHMODUL-FOERRA  PIC 9(3).                                    
004200*                                 HÖGER-MODUL                             
004300     03 MOD-RAD              OCCURS 13 TIMES.                             
004400*                                                                         
004500        05 MOD-KDCMD-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-KDCMD         PIC X(2).                                    
004800*                                 MFS BEHANDLING AV INPUTFÄLT             
004900        05 MOD-ADRUTNIV      PIC Z(2)9.                                   
005000*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
005100        05 MOD-DIDMODUL      PIC Z(3).                                    
005200*                                 MODUL-DJUP                              
005300        05 MOD-DIHMODUL      PIC Z(3).                                    
005400*                                 MODUL-HÖJD                              
005500        05 MOD-ADVMODUL      PIC Z(3).                                    
005600*                                 VÄNSTER-MODUL                           
005700        05 MOD-ADHMODUL      PIC Z(3).                                    
005800*                                 HÖGER-MODUL                             
005900        05 MOD-VLRUTNIV-ATTR PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-VLRUTNIV      PIC Z(3).                                    
006200*                                 TOT KOLLI-VOLYM I RUTA/NIV (M3)         
006300        05 MOD-TESPAERR-ATTR PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-TESPAERR      PIC X(20).                                   
006600*                                 SPÄRRTEXT                               
006700        05 MOD-VLRUTNIV-DOLT PIC 9(3).                                    
006800*                                 TOT KOLLI-VOLYM I RUTA/NIV (M3)         
006900        05 MOD-FLAGGA-TRPT-DOLD                                           
007000                             PIC X.                                       
007100*                                 ALLMÄN FLAGGA (VAD ÄR DET?)             
007200     03 MOD-ADRUTNIV-NY-ATTR PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-ADRUTNIV-NY      PIC X(2).                                    
007500*                                 MFS BEHANDLING AV INPUTFÄLT             
007600     03 MOD-DIDMODUL-NY-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-DIDMODUL-NY      PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000     03 MOD-DIHMODUL-NY-ATTR PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-DIHMODUL-NY      PIC X(2).                                    
008300*                                 MFS BEHANDLING AV INPUTFÄLT             
008400     03 MOD-ADVMODUL-NY-ATTR PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-ADVMODUL-NY      PIC X(2).                                    
008700*                                 MFS BEHANDLING AV INPUTFÄLT             
008800     03 MOD-ADHMODUL-NY-ATTR PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-ADHMODUL-NY      PIC X(2).                                    
009100*                                 MFS BEHANDLING AV INPUTFÄLT             
009200     03 MOD-VLRUTNIV-NY-ATTR PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-VLRUTNIV-NY      PIC X(2).                                    
009500*                                 MFS BEHANDLING AV INPUTFÄLT             
009600     03 MOD-TESPAERR-NY-ATTR PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-TESPAERR-NY      PIC X(2).                                    
009900*                                 MFS BEHANDLING AV INPUTFÄLT             
010000     03 MOD-TEMFSINF         PIC X(55).                                   
010100*                                 INFORMATIONSMEDDELANDE                  
