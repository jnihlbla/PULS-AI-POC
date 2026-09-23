000100 01  MOD-W4O33101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4033100               
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
002700     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 MOD-IDPRODNR-UT      PIC 9(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100     03 MOD-KDPRTVAL-AF-IN-ATTR                                           
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-KDPRTVAL-AF-IN   PIC X(2).                                    
003500*                                 PRINTER-VAL KOD                         
003600     03 MOD-KDPRTVAL-AF-UT   PIC X(2).                                    
003700*                                 PRINTER-VAL KOD                         
003800     03 MOD-KDPRTVAL-FS-IN-ATTR                                           
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KDPRTVAL-FS-IN   PIC X(2).                                    
004200*                                 PRINTER-VAL KOD                         
004300     03 MOD-KDPRTVAL-FS-UT   PIC X(2).                                    
004400*                                 PRINTER-VAL KOD                         
004500     03 MOD-KDKOLLI-ATTR     PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-KDKOLLI          PIC X(8).                                    
004800*                                 KOLLIKOD                                
004900     03 MOD-KDEMBTYP-ATTR    PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-KDEMBTYP         PIC 9.                                       
005200*                                 EMBALLAGETYP                            
005300     03 MOD-DIKOLLIL-ATTR    PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-DIKOLLIL         PIC Z(3)9.                                   
005600*                                 KOLLI-LÄNGD                             
005700     03 MOD-DIKOLLIB-ATTR    PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-DIKOLLIB         PIC Z(2)9.                                   
006000*                                 KOLLI-BREDD                             
006100     03 MOD-DIKOLLIH-ATTR    PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-DIKOLLIH         PIC Z(2)9.                                   
006400*                                 KOLLI-HÖJD                              
006500     03 MOD-ADRESS-TEXT      PIC X(7).                                    
006600     03 MOD-ADFLGEO          PIC X(3).                                    
006700*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
006800     03 MOD-ADFLOMR          PIC Z(2)9.                                   
006900*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
007000     03 MOD-ADRUTNIV         PIC Z(2)9.                                   
007100*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
007200     03 MOD-ADVMODUL         PIC Z(3).                                    
007300*                                 VÄNSTER-MODUL                           
007400     03 MOD-SLUT-TEXT        PIC X(15).                                   
007500     03 MOD-TEMFSINF         PIC X(55).                                   
007600*                                 INFORMATIONSMEDDELANDE                  
007700*** END OF VILMAII-COPY LENGTH= 215 BYTES                                 
