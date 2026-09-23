000100 01  MOD-W4O33501.                                                        
000200*                                 MOD-COPYTEXT FÖR W4033500               
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
002700     03 MOD-KDPRTVAL-IN-ATTR PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-KDPRTVAL-IN      PIC X(2).                                    
003000*                                 PRINTER-VAL KOD                         
003100     03 MOD-KDPRTVAL-UT      PIC X(2).                                    
003200*                                 PRINTER-VAL KOD                         
003300     03 MOD-KDKOLLI-BAER-ATTR                                             
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-KDKOLLI-BAER     PIC X(8).                                    
003700*                                 KOLLIKOD                                
003800     03 MOD-KDEMBTYP-BAER-ATTR                                            
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KDEMBTYP-BAER    PIC 9.                                       
004200*                                 EMBALLAGETYP                            
004300     03 MOD-DIKOLLIL-BAER-ATTR                                            
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-DIKOLLIL-BAER    PIC Z(3)9.                                   
004700*                                 KOLLI-LÄNGD                             
004800     03 MOD-DIKOLLIB-BAER-ATTR                                            
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-DIKOLLIB-BAER    PIC Z(2)9.                                   
005200*                                 KOLLI-BREDD                             
005300     03 MOD-DIKOLLIH-BAER-ATTR                                            
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-DIKOLLIH-BAER    PIC Z(2)9.                                   
005700*                                 KOLLI-HÖJD                              
005800     03 MOD-W4O33501-001     OCCURS 10 TIMES.                             
005900*                                 MOD-COPYTEXT FÖR W4O33501               
006000        05 MOD-IDKOLLI-RAD-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-IDKOLLI-RAD   PIC Z(4)9.                                   
006400*                                 KOLLINUMMER                             
006500        05 MOD-VKORDBTO-RAD-ATTR                                          
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-VKORDBTO-RAD  PIC Z(5)9.9.                                 
006900*                                 ORDERVIKT BRUTTO (KG)                   
007000        05 MOD-KDKOLLI-RAD-ATTR                                           
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-KDKOLLI-RAD   PIC X(8).                                    
007400*                                 KOLLIKOD                                
007500        05 MOD-KDEMBTYP-RAD-ATTR                                          
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-KDEMBTYP-RAD  PIC 9.                                       
007900*                                 EMBALLAGETYP                            
008000        05 MOD-DIKOLLIL-RAD-ATTR                                          
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-DIKOLLIL-RAD  PIC Z(3)9.                                   
008400*                                 KOLLI-LÄNGD                             
008500        05 MOD-DIKOLLIB-RAD-ATTR                                          
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-DIKOLLIB-RAD  PIC Z(2)9.                                   
008900*                                 KOLLI-BREDD                             
009000        05 MOD-DIKOLLIH-RAD-ATTR                                          
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-DIKOLLIH-RAD  PIC Z(2)9.                                   
009400*                                 KOLLI-HÖJD                              
009500     03 MOD-ADRESS-TEXT      PIC X(7).                                    
009600     03 MOD-ADFLGEO          PIC X(3).                                    
009700*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
009800     03 MOD-ADFLOMR          PIC Z(2)9.                                   
009900*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
010000     03 MOD-ADRUTNIV         PIC Z(2)9.                                   
010100*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
010200     03 MOD-ADVMODUL         PIC Z(3).                                    
010300*                                 VÄNSTER-MODUL                           
010400     03 MOD-TEMFSINF         PIC X(55).                                   
010500*                                 INFORMATIONSMEDDELANDE                  
010600*** END OF VILMAII-COPY LENGTH= 645 BYTES                                 
