000100 01  MOD-W3O18201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3182              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDKOLLI-IN-ATTR  PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDKOLLI-IN       PIC Z(4)9.                                   
001000*                                 KOLLINUMMER                             
001100     03 MOD-IDKOLLI-UT       PIC Z(4)9.                                   
001200*                                 KOLLINUMMER                             
001300     03 MOD-IDFAKT-UT        PIC X(7).                                    
001400*                                 FAKTURANUMMER                           
001500     03 MOD-NYTT-KOLLI-IN-ATTR                                            
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-NYTT-KOLLI-IN    PIC X.                                       
001900     03 MOD-KOLLI-VIKT-IN-ATTR                                            
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-KOLLI-VIKT-IN    PIC Z(5)9.9.                                 
002300*                                 ORDERVIKT BRUTTO (KG)                   
002400     03 MOD-KOLLI-VOL-IN-ATTR                                             
002500                             PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-KOLLI-VOL-IN     PIC Z(3)9.9(3).                              
002800*                                 ORDERVOLYM BRUTTO (M3)                  
002900     03 MOD-KOLLI-VIKT-UT    PIC Z(5)9.9.                                 
003000*                                 ORDERVIKT BRUTTO (KG)                   
003100     03 MOD-KOLLI-VOL-UT     PIC Z(3)9.9(3).                              
003200*                                 ORDERVOLYM BRUTTO (M3)                  
003300     03 MOD-FAKTURA-RAD      OCCURS 10 TIMES.                             
003400*                                 ARTIKELNUMMER I K-FAKTURA               
003500*                                 CLEARING FLEN BORN JAPAN AUSTRA         
003600*                                 LIEN                                    
003700        05 MOD-IDARTNR-OBJ-ATTR                                           
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-IDARTNR-OBJ   PIC Z(9).                                    
004100*                                 OBJEKTNUMMER                            
004200        05 MOD-KVCLEAR-ATTR  PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KVCLEAR       PIC Z(6)9.                                   
004500*                                 KVANTITET ATT CLEARA                    
004600        05 MOD-BEART         PIC X(25).                                   
004700*                                 ARTIKELBENÄMNING                        
004800        05 MOD-KVLS          PIC -(7)9.                                   
004900*                                 LAGERSALDO                              
005000     03 MOD-TOT-VIKT-IN-ATTR PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-TOT-VIKT-IN      PIC Z(5)9.9.                                 
005300*                                 ORDERVIKT BRUTTO (KG)                   
005400     03 MOD-TOT-VOL-IN-ATTR  PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-TOT-VOL-IN       PIC Z(3)9.9(3).                              
005700*                                 ORDERVOLYM BRUTTO (M3)                  
005800     03 MOD-CRE-INV-ATTR     PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-CRE-INV          PIC X.                                       
006100     03 MOD-DEL-ARTNR-IN-ATTR                                             
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-DEL-ARTNR-IN     PIC Z(9).                                    
006500*                                 OBJEKTNUMMER                            
006600     03 MOD-DEL-KVANT-IN-ATTR                                             
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-DEL-KVANT-IN     PIC Z(5)9.                                   
007000*                                 KVANTITET ATT CLEARA                    
007100     03 MOD-DEL-KOLLI-IN-ATTR                                             
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-DEL-KOLLI-IN     PIC Z(4)9.                                   
007500*                                 KOLLINUMMER                             
007600     03 MOD-TOT-VIKT-UT      PIC Z(5)9.9.                                 
007700*                                 ORDERVIKT BRUTTO (KG)                   
007800     03 MOD-TOT-VOL-UT       PIC Z(3)9.9(3).                              
007900*                                 ORDERVOLYM BRUTTO (M3)                  
008000     03 MOD-TEMFSINF         PIC X(55).                                   
008100*                                 INFORMATIONSMEDDELANDE                  
008200*** END OF VILMAII-COPY LENGTH= 752 BYTES                                 
