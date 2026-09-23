000100 01  MOD-W4O33301.                                                        
000200*                                 MOD TILL FRÅGEBILD                      
000300*                                 LISTNING KOLLIFLAGGA                    
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-RAD-NYCKLAR.                                                  
000900        05 MOD-IDDISTR-IN    PIC X(2).                                    
001000*                                 MFS BEHANDLING AV INPUTFÄLT             
001100        05 MOD-IDDISTR-UT    PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300        05 MOD-IDKUNDNR-IN   PIC X(2).                                    
001400*                                 MFS BEHANDLING AV INPUTFÄLT             
001500        05 MOD-IDKUNDNR-UT   PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700        05 MOD-IDORDNR-IN    PIC X(2).                                    
001800*                                 MFS BEHANDLING AV INPUTFÄLT             
001900        05 MOD-IDORDNR-UT    PIC X(5).                                    
002000*                                 ORDERNUMMER UTGÅR PD90                  
002100        05 MOD-IDKOLLI-IN    PIC X(2).                                    
002200*                                 MFS BEHANDLING AV INPUTFÄLT             
002300        05 MOD-IDKOLLI-UT    PIC X(5).                                    
002400*                                 KOLLINUMMER                             
002500        05 MOD-IDDC-IN       PIC X(2).                                    
002600*                                 MFS BEHANDLING AV INPUTFÄLT             
002700        05 MOD-IDDC-UT       PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900        05 MOD-IDPRODNR-IN   PIC X(2).                                    
003000*                                 MFS BEHANDLING AV INPUTFÄLT             
003100        05 MOD-IDPRODNR-UT   PIC 9(7).                                    
003200*                                 PRODUKTIONSNUMMER                       
003300     03 MOD-RAD1.                                                         
003400        05 MOD-KDPRTVAL-IN-ATTR                                           
003500                             PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-KDPRTVAL-IN   PIC X(2).                                    
003800*                                 PRINTER-VAL KOD                         
003900        05 MOD-KDPRTVAL-UT   PIC X(2).                                    
004000*                                 PRINTER-VAL KOD                         
004100     03 MOD-RAD2.                                                         
004200        05 MOD-ADFLGEO       PIC X(3).                                    
004300*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
004400        05 MOD-ADFLOMR       PIC Z(2)9.                                   
004500*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
004600        05 MOD-ADRUTNIV      PIC Z(2)9.                                   
004700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
004800        05 MOD-ADVMODUL      PIC Z(3).                                    
004900*                                 VÄNSTER-MODUL                           
005000        05 MOD-ADHMODUL      PIC Z(3).                                    
005100*                                 HÖGER-MODUL                             
005200        05 MOD-KDFRAKT       PIC Z9.                                      
005300*                                 FRAKTSÄTT DC TILL KUND                  
005400     03 MOD-TEMFSINF         PIC X(55).                                   
005500*                                 INFORMATIONSMEDDELANDE                  
005600*** END OF VILMAII-COPY LENGTH= 163 BYTES                                 
