000100 01  MOD-W4O33701.                                                        
000200*                                 MOD-COPYTEXT FÖR W4033700               
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
001800*                                 ORDERNUMMER UTGÅR PD90                  
001900     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDPRODNR         PIC 9(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900     03 MOD-KDPRTVAL-FS      PIC X(2).                                    
003000*                                 PRINTER-VAL KOD                         
003100     03 MOD-KDPRTVAL-AF-IN   PIC X(2).                                    
003200*                                 PRINTER-VAL KOD                         
003300     03 MOD-KDPRTVAL-AF-UT   PIC X(2).                                    
003400*                                 PRINTER-VAL KOD                         
003500     03 MOD-VKORDBTO-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-VKORDBTO         PIC Z(5)9.9.                                 
003800*                                 ORDERVIKT BRUTTO (KG)                   
003900     03 MOD-ADRESS-TEXT      PIC X(7).                                    
004000     03 MOD-ADFLGEO          PIC X(3).                                    
004100*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
004200     03 MOD-ADFLOMR          PIC Z(2)9.                                   
004300*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
004400     03 MOD-ADRUTNIV         PIC Z(2)9.                                   
004500*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
004600     03 MOD-ADVMODUL         PIC Z(3).                                    
004700*                                 VÄNSTER-MODUL                           
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*** END OF VILMAII-COPY LENGTH= 173 BYTES                                 
