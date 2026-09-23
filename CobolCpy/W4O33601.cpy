000100 01  MOD-W4O33601.                                                        
000200*                                 MOD-COPYTEXT FÖR W4033600               
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
002700     03 MOD-IDKOLLI-FLER     PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900     03 MOD-IDKOLLI-FIRST    PIC 9(5).                                    
003000*                                 KOLLINUMMER                             
003100     03 MOD-IDKOLLI-NEXT     PIC 9(5).                                    
003200*                                 KOLLINUMMER                             
003300     03 MOD-IDPRODNR         PIC 9(7).                                    
003400*                                 PRODUKTIONSNUMMER                       
003500     03 MOD-KDPRTVAL-IN-ATTR PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDPRTVAL-IN      PIC X(2).                                    
003800*                                 PRINTER-VAL KOD                         
003900     03 MOD-KDPRTVAL-UT      PIC X(2).                                    
004000*                                 PRINTER-VAL KOD                         
004100     03 MOD-KDKOLLI-BAER-UT  PIC X(8).                                    
004200*                                 KOLLIKOD                                
004300     03 MOD-KDKOLLI-BAER-IN-ATTR                                          
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-KDKOLLI-BAER-IN  PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800     03 MOD-KDEMBTYP-BAER-UT PIC 9.                                       
004900*                                 EMBALLAGETYP                            
005000     03 MOD-KDEMBTYP-BAER-IN-ATTR                                         
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KDEMBTYP-BAER-IN PIC X.                                       
005400     03 MOD-DIKOLLIL-BAER-UT PIC Z(3)9.                                   
005500*                                 KOLLI-LÄNGD                             
005600     03 MOD-DIKOLLIL-BAER-IN-ATTR                                         
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-DIKOLLIL-BAER-IN PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100     03 MOD-DIKOLLIB-BAER-UT PIC Z(2)9.                                   
006200*                                 KOLLI-BREDD                             
006300     03 MOD-DIKOLLIB-BAER-IN-ATTR                                         
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-DIKOLLIB-BAER-IN PIC X(2).                                    
006700*                                 MFS BEHANDLING AV INPUTFÄLT             
006800     03 MOD-DIKOLLIH-BAER-UT PIC Z(2)9.                                   
006900*                                 KOLLI-HÖJD                              
007000     03 MOD-DIKOLLIH-BAER-IN-ATTR                                         
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-DIKOLLIH-BAER-IN PIC X(2).                                    
007400*                                 MFS BEHANDLING AV INPUTFÄLT             
007500     03 MOD-W4O33601-001     OCCURS 8 TIMES.                              
007600*                                 MOD-COPYTEXT FÖR W4O33601               
007700        05 MOD-IDKOLLI-ING-UT                                             
007800                             PIC Z(4)9.                                   
007900*                                 KOLLINUMMER                             
008000        05 MOD-VKORDBTO-ING-UT                                            
008100                             PIC Z(5)9.9.                                 
008200*                                 ORDERVIKT BRUTTO (KG)                   
008300        05 MOD-KDKOLLI-ING-UT                                             
008400                             PIC X(8).                                    
008500*                                 KOLLIKOD                                
008600        05 MOD-KDEMBTYP-ING-UT                                            
008700                             PIC 9.                                       
008800*                                 EMBALLAGETYP                            
008900        05 MOD-DIKOLLIL-ING-UT                                            
009000                             PIC Z(3)9.                                   
009100*                                 KOLLI-LÄNGD                             
009200        05 MOD-DIKOLLIB-ING-UT                                            
009300                             PIC Z(2)9.                                   
009400*                                 KOLLI-BREDD                             
009500        05 MOD-DIKOLLIH-ING-UT                                            
009600                             PIC Z(2)9.                                   
009700*                                 KOLLI-HÖJD                              
009800     03 MOD-KDCMD-ATTR       PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-KDCMD            PIC X(2).                                    
010100*                                 MFS BEHANDLING AV INPUTFÄLT             
010200     03 MOD-IDKOLLI-ING-NY-ATTR                                           
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-IDKOLLI-ING-NY   PIC X(2).                                    
010600*                                 MFS BEHANDLING AV INPUTFÄLT             
010700     03 MOD-VKORDBTO-ING-NY-ATTR                                          
010800                             PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000     03 MOD-VKORDBTO-ING-NY  PIC X(2).                                    
011100*                                 MFS BEHANDLING AV INPUTFÄLT             
011200     03 MOD-KDKOLLI-ING-NY-ATTR                                           
011300                             PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-KDKOLLI-ING-NY   PIC X(2).                                    
011600*                                 MFS BEHANDLING AV INPUTFÄLT             
011700     03 MOD-KDEMBTYP-ING-NY-ATTR                                          
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-KDEMBTYP-ING-NY  PIC 9.                                       
012100*                                 EMBALLAGETYP                            
012200     03 MOD-DIKOLLIL-ING-NY-ATTR                                          
012300                             PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 MOD-DIKOLLIL-ING-NY  PIC X(2).                                    
012600*                                 MFS BEHANDLING AV INPUTFÄLT             
012700     03 MOD-DIKOLLIB-ING-NY-ATTR                                          
012800                             PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000     03 MOD-DIKOLLIB-ING-NY  PIC X(2).                                    
013100*                                 MFS BEHANDLING AV INPUTFÄLT             
013200     03 MOD-DIKOLLIH-ING-NY-ATTR                                          
013300                             PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500     03 MOD-DIKOLLIH-ING-NY  PIC X(2).                                    
013600*                                 MFS BEHANDLING AV INPUTFÄLT             
013700     03 MOD-ADRESS-TEXT      PIC X(7).                                    
013800     03 MOD-ADFLGEO          PIC X(3).                                    
013900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
014000     03 MOD-ADFLOMR          PIC Z(2)9.                                   
014100*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
014200     03 MOD-ADRUTNIV         PIC Z(2)9.                                   
014300*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
014400     03 MOD-ADVMODUL         PIC Z(3).                                    
014500*                                 VÄNSTER-MODUL                           
014600     03 MOD-ADHMODUL         PIC 9(3).                                    
014700*                                 HÖGER-MODUL                             
014800     03 MOD-DIDMODUL         PIC X(3).                                    
014900*                                 MODUL-DJUP                              
015000     03 MOD-DIHMODUL         PIC X(3).                                    
015100*                                 MODUL-HÖJD                              
015200     03 MOD-IDTRPTNR         PIC 9(3).                                    
015300*                                 TRANSPORTIDENTITET                      
015400     03 MOD-FLUTLAST         PIC X.                                       
015500*                                 KOLLI I UTLASTNINGSLAGER                
015600     03 MOD-TEMFSINF         PIC X(55).                                   
015700*                                 INFORMATIONSMEDDELANDE                  
015800*** END OF VILMAII-COPY LENGTH= 516 BYTES                                 
