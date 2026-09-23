000100 01  MOD-W4O31801-CTX.                                                    
000200*                                 MOD-COPYTEXT PGM W40318                 
000300*                                 KONTROLL EJ RAPPORTERADE RADER          
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDANSTNR-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDANSTNR-UT      PIC X(5).                                    
001100*                                 ANSTÄLLNINGSNUMMER                      
001200     03 MOD-IDDISTR-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-IDORDNR-IN       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDORDNR-UT       PIC X(5).                                    
002300*                                 ORDERNUMMER                             
002400     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002700*                                 KOLLINUMMER                             
002800     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200     03 MOD-IDDC-IN          PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400     03 MOD-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MOD-IDPLKLST         PIC X(3).                                    
003700*                                 PLOCKLISTNUMMER                         
003800     03 MOD-FLAGGA-ATTR      PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-FLAGGA           PIC X.                                       
004100*                                 ALLMÄN FLAGGA                           
004200     03 MOD-FLSVAR-ATTR      PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLSVAR           PIC X.                                       
004500*                                 ALLMÄN SVARSFLAGGA                      
004600     03 MOD-IDPLKLST-SPAR    PIC 9(3).                                    
004700*                                 PLOCKLISTNUMMER                         
004800     03 MOD-IDTRANS-START    PIC X(4).                                    
004900*                                 BILDNUMMER                              
005000     03 MOD-IDRADNR-SPAR     PIC 9(5).                                    
005100*                                 RADNUMMER                               
005200     03 MOD-ADFLGEO          PIC X(3).                                    
005300*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
005400     03 MOD-ADFLOMR          PIC Z(2)9.                                   
005500*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
005600     03 MOD-ADRUTNIV         PIC Z(2)9.                                   
005700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
005800     03 MOD-W4O31801-001-GRP OCCURS 25 TIMES.                             
005900        05 MOD-ADLAGOMR      PIC X(2).                                    
006000*                                 LAGEROMRÅDE                             
006100        05 MOD-IDRADNR-ATTR  PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-IDRADNR       PIC X(4).                                    
006400*                                 RADNUMMER                               
006500        05 MOD-FLNOLLJ       PIC X.                                       
006600*                                 UPPDATERAD AV NOLLJAGARE                
006700        05 MOD-KVORAPP       PIC 9(6).                                    
006800*                                 EJ-RAPPORTERAT-ANTAL                    
006900     03 MOD-TEPLATS          PIC X(38).                                   
007000     03 MOD-TEMFSINF         PIC X(55).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END OF VILMAII-COPY LENGTH= 590 BYTES                                 
