000100 01  MOD-W4O32301.                                                        
000200*                                 MOD-COPYTEXT FÖR FRÅGE-BILD             
000300*                                 PACK-UNDERLAG INFO                      
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDANSTNR-IN      PIC X(5).                                    
000900*                                 ANSTÄLLNINGSNUMMER                      
001000     03 MOD-IDANSTNR-UT      PIC X(5).                                    
001100*                                 ANSTÄLLNINGSNUMMER                      
001200     03 MOD-IDDISTR-IN       PIC X(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001700*                                 KUNDNUMMER                              
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-IDORDNR-IN       PIC X(5).                                    
002100*                                 ORDERNUMMER UTGÅR PD90                  
002200     03 MOD-IDORDNR-UT       PIC X(5).                                    
002300*                                 ORDERNUMMER UTGÅR PD90                  
002400     03 MOD-IDPURAD-IN       PIC X(5).                                    
002500     03 MOD-IDPURAD-UT       PIC X(5).                                    
002600     03 MOD-IDPRODNR-IN      PIC X(7).                                    
002700*                                 PRODUKTIONSNUMMER                       
002800     03 MOD-IDPRODNR-UT      PIC X(7).                                    
002900*                                 PRODUKTIONSNUMMER                       
003000     03 MOD-IDDC-IN          PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200     03 MOD-IDDC-UT          PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400     03 MOD-FORSTA-IDPURAD   PIC 9(4).                                    
003500*                                 RADNUMMER PÅ PACKUNDERLAG               
003600     03 MOD-FORSTA-IDKOLLI   PIC 9(5).                                    
003700*                                 KOLLINUMMER                             
003800     03 MOD-FORSTA-KOLLI-IDPLKLST                                         
003900                             PIC 9(3).                                    
004000*                                 PLOCKLISTNUMMER                         
004100     03 MOD-FORSTA-KOLLI-IDPRODNR                                         
004200                             PIC 9(7).                                    
004300*                                 PRODUKTIONSNUMMER                       
004400     03 MOD-FORSTA-Q4-IDORDER                                             
004500                             PIC X(7).                                    
004600*                                 VOLVO PARTS ORDERNUMMER                 
004700     03 MOD-FORSTA-Q4-IDARTNR                                             
004800                             PIC X(9).                                    
004900*                                 ARTIKELNUMMER                           
005000     03 MOD-FORSTA-Q4-IDLOPNR                                             
005100                             PIC 9(3).                                    
005200*                                 LÖPNUMMER                               
005300     03 MOD-RAD              OCCURS 14 TIMES.                             
005400*                                 TABELL INNEHÅLLANDE RADER.              
005500        05 MOD-IDRONR        PIC X(5).                                    
005600*                                 RESTORDERNUMMER                         
005700        05 MOD-ADLAGOMR      PIC Z9.                                      
005800*                                 LAGEROMRÅDE                             
005900        05 MOD-ADGANG        PIC Z9.                                      
006000*                                 GÅNG                                    
006100        05 MOD-ADPLATS       PIC Z(4)9.                                   
006200*                                 LAGERPLATSNUMMER                        
006300        05 MOD-IDPURAD       PIC Z(3)9.                                   
006400*                                 RADNUMMER PÅ PACKUNDERLAG               
006500        05 MOD-IDARTNR-MED-KSIFF.                                         
006600*                                 ARTIKELNR MED KONTROLLSIFFRA.           
006700           07 MOD-IDARTNR    PIC Z(7)9.                                   
006800*                                 ARTIKELNUMMER                           
006900           07 MOD-STRACK     PIC X.                                       
007000           07 MOD-REKSIFFR   PIC 9.                                       
007100*                                 KONTROLLSIFFRA                          
007200        05 MOD-KDARTURS      PIC X(2).                                    
007300*                                 ARTIKELURSPRUNGSKOD                     
007400        05 MOD-BEART         PIC X(25).                                   
007500*                                 ARTIKELBENÄMNING                        
007600        05 MOD-KVAVBART      PIC Z(5)9.                                   
007700*                                 AVBOKAT ANTAL ARTIKLAR                  
007800        05 MOD-KVLEVART      PIC Z(5)9.                                   
007900*                                 LEVERERAT ANTAL STYCK                   
008000        05 MOD-IDKOLLI       PIC X(5).                                    
008100*                                 KOLLINUMMER                             
008200     03 MOD-TEMFSINF         PIC X(55).                                   
008300*                                 INFORMATIONSMEDDELANDE                  
008400*** END OF VILMAII-COPY LENGTH= 1213 BYTES                                
