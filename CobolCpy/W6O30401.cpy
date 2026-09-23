000100 01  MOD-W6O30401.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-ADLAGOMR-UT      PIC Z9.                                      
001600*                                 LAGEROMRÅDE                             
001700     03 MOD-ADGANG-UT        PIC Z9.                                      
001800*                                 GÅNG                                    
001900     03 MOD-ADPLATS-UT       PIC Z(4)9.                                   
002000*                                 LAGERPLATSNUMMER                        
002100     03 MOD-ADLAGOMR-IN-ATTR PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-ADLAGOMR-IN      PIC X(2).                                    
002400*                                 LAGEROMRÅDE                             
002500     03 MOD-ADGANG-IN-ATTR   PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-ADGANG-IN        PIC X(2).                                    
002800*                                 GÅNG                                    
002900     03 MOD-ADPLATS-IN-ATTR  PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-ADPLATS-IN       PIC X(5).                                    
003200*                                 LAGERPLATSNUMMER                        
003300     03 MOD-VKART-UT         PIC Z(6)9.9(2).                              
003400*                                 ARTIKELVIKT (G/OZ)                      
003500     03 MOD-BESORT-VKART-UT  PIC X(6).                                    
003600*                                 BENÄMNING PÅ SORT/ENHET                 
003700     03 MOD-VKART-IN-ATTR    PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-VKART-IN         PIC X(10).                                   
004000*                                 ARTIKELVIKT (G/OZ)                      
004100     03 MOD-VLARTNTO-UT      PIC Z(7)9.9.                                 
004200*                                 ARTIKELVOLYM NETTO (CM3)                
004300     03 MOD-BESORT-VLARTNTO-UT                                            
004400                             PIC X(6).                                    
004500*                                 BENÄMNING PÅ SORT/ENHET                 
004600     03 MOD-VLARTNTO-IN-ATTR PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-VLARTNTO-IN      PIC X(10).                                   
004900*                                 ARTIKELVOLYM NETTO (CM3)                
005000     03 MOD-KDVSOP           PIC X(3).                                    
005100*                                 VSOP-KOD                                
005200     03 MOD-KDVSOP-IN-ATTR   PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-KDVSOP-IN        PIC X(3).                                    
005500*                                 VSOP-KOD                                
005600     03 MOD-TABELLRAD        OCCURS 5 TIMES.                              
005700*                                 GRUPP MED TABELLRADER                   
005800        05 MOD-IDARTNR       PIC Z(8)9.                                   
005900*                                 ARTIKELNUMMER                           
006000     03 MOD-INFO             PIC X(30).                                   
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 284 BYTES                                 
