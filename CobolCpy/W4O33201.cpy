000100 01  MOD-W4O33201.                                                        
000200*                                 MOD-COPYTEXT FÖR W4033200               
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
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-KDPRTVAL-IN-ATTR PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-KDPRTVAL-IN      PIC X(2).                                    
003000*                                 PRINTER-VAL KOD                         
003100     03 MOD-KDPRTVAL-UT      PIC X(2).                                    
003200*                                 PRINTER-VAL KOD                         
003300     03 MOD-KDKOLLI-UT       PIC X(8).                                    
003400*                                 KOLLIKOD                                
003500     03 MOD-KDKOLLI-IN-ATTR  PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDKOLLI-IN       PIC X(2).                                    
003800*                                 MFS BEHANDLING AV INPUTFÄLT             
003900     03 MOD-KDEMBTYP-UT      PIC 9.                                       
004000*                                 EMBALLAGETYP                            
004100     03 MOD-KDEMBTYP-IN-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-KDEMBTYP-IN      PIC X.                                       
004400*                                 EMBALLAGETYP                            
004500     03 MOD-DIKOLLIL-UT      PIC Z(3)9.                                   
004600*                                 KOLLI-LÄNGD                             
004700     03 MOD-DIKOLLIL-IN-ATTR PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-DIKOLLIL-IN      PIC X(2).                                    
005000*                                 MFS BEHANDLING AV INPUTFÄLT             
005100     03 MOD-DIKOLLIB-UT      PIC Z(2)9.                                   
005200*                                 KOLLI-BREDD                             
005300     03 MOD-DIKOLLIB-IN-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-DIKOLLIB-IN      PIC X(2).                                    
005600*                                 MFS BEHANDLING AV INPUTFÄLT             
005700     03 MOD-DIKOLLIH-UT      PIC Z(2)9.                                   
005800*                                 KOLLI-HÖJD                              
005900     03 MOD-DIKOLLIH-IN-ATTR PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-DIKOLLIH-IN      PIC X(2).                                    
006200*                                 MFS BEHANDLING AV INPUTFÄLT             
006300     03 MOD-VKORDBTO-UT      PIC Z(5)9.9.                                 
006400*                                 ORDERVIKT BRUTTO (KG)                   
006500     03 MOD-VKORDBTO-IN-ATTR PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-VKORDBTO-IN      PIC X(2).                                    
006800*                                 MFS BEHANDLING AV INPUTFÄLT             
006900     03 MOD-ADRESS-TEXT      PIC X(7).                                    
007000     03 MOD-ADFLGEO          PIC X(3).                                    
007100*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
007200     03 MOD-ADFLOMR          PIC Z(2)9.                                   
007300*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
007400     03 MOD-ADRUTNIV         PIC Z(2)9.                                   
007500*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
007600     03 MOD-ADVMODUL         PIC Z(3).                                    
007700*                                 VÄNSTER-MODUL                           
007800     03 MOD-TEMFSINF         PIC X(55).                                   
007900*                                 INFORMATIONSMEDDELANDE                  
008000*** END OF VILMAII-COPY LENGTH= 206 BYTES                                 
