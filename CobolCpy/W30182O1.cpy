000100 01  RESP-W30182O1.                                                       
000200*                                 RESP-COPYTEXT FÖR BILD 3182             
000300*                                                                         
000400     03 RESP-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RESP-IDKOLLI-KEY-ATTR                                             
000700                             PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001000*                                 KOLLINUMMER                             
001100     03 RESP-IDFAKT          PIC X(7).                                    
001200*                                 FAKTURANUMMER                           
001300     03 RESP-NYTT-KOLLI-UPD-ATTR                                          
001400                             PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 RESP-NYTT-KOLLI-UPD  PIC X.                                       
001700     03 RESP-KOLLI-VIKT-UPD-ATTR                                          
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 RESP-KOLLI-VIKT-UPD  PIC Z(5)9.9.                                 
002100*                                 ORDERVIKT BRUTTO (KG)                   
002200     03 RESP-KOLLI-VOL-UPD-ATTR                                           
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 RESP-KOLLI-VOL-UPD   PIC Z(3)9.9(3).                              
002600*                                 ORDERVOLYM BRUTTO (M3)                  
002700     03 RESP-KOLLI-VIKT-UT   PIC Z(5)9.9.                                 
002800*                                 ORDERVIKT BRUTTO (KG)                   
002900     03 RESP-KOLLI-VOL-UT    PIC Z(3)9.9(3).                              
003000*                                 ORDERVOLYM BRUTTO (M3)                  
003100     03 RESP-TOT-VIKT-UPD-ATTR                                            
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 RESP-TOT-VIKT-UPD    PIC Z(5)9.9.                                 
003500*                                 ORDERVIKT BRUTTO (KG)                   
003600     03 RESP-TOT-VOL-UPD-ATTR                                             
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 RESP-TOT-VOL-UPD     PIC Z(3)9.9(3).                              
004000*                                 ORDERVOLYM BRUTTO (M3)                  
004100     03 RESP-SKAPA-PROFORMA-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 RESP-SKAPA-PROFORMA  PIC X.                                       
004500     03 RESP-DEL-IDARTNR-OBJ-ATTR                                         
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 RESP-DEL-IDARTNR-OBJ PIC Z(9).                                    
004900*                                 OBJEKTNUMMER                            
005000     03 RESP-DEL-KVANTAL-ATTR                                             
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 RESP-DEL-KVANTAL     PIC Z(5)9.                                   
005400*                                 ANTAL                                   
005500     03 RESP-DEL-IDKOLLI-ATTR                                             
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 RESP-DEL-IDKOLLI     PIC Z(4)9.                                   
005900*                                 KOLLINUMMER                             
006000     03 RESP-TOT-VIKT-UT     PIC Z(5)9.9.                                 
006100*                                 ORDERVIKT BRUTTO (KG)                   
006200     03 RESP-TOT-VOL-UT      PIC Z(3)9.9(3).                              
006300*                                 ORDERVOLYM BRUTTO (M3)                  
006400     03 RESP-KDDIAVAR        PIC X.                                       
006500*                                 DIALOGVARIANT                           
006600     03 RESP-FAKTURA-RAD     OCCURS 500 TIMES.                            
006700*                                 ARTIKELNUMMER I K-FAKTURA               
006800*                                 CLEARING FLEN BORN JAPAN AUSTRA         
006900*                                 LIEN                                    
007000        05 RESP-IDARTNR-OBJ-ATTR                                          
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 RESP-IDARTNR-OBJ  PIC Z(9).                                    
007400*                                 OBJEKTNUMMER                            
007500        05 RESP-KVANTAL-ATTR PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700        05 RESP-KVANTAL      PIC Z(6)9.                                   
007800*                                 ANTAL                                   
007900        05 RESP-BEART        PIC X(25).                                   
008000*                                 ARTIKELBENÄMNING                        
008100        05 RESP-KVLS         PIC -(7)9.                                   
008200*                                 LAGERSALDO                              
008300*** END OF VILMAII-COPY LENGTH= 26624 BYTES                               
