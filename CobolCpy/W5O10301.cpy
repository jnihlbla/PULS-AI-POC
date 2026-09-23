000100 01  W5O10301.                                                            
000200     03 TRANS-NUMMER.                                                     
000300*                                 TRANSNUMMER INGÅR I TRANSKOD            
000400        05 TRANS-SIFF-1      PIC X.                                       
000500        05 TRANS-SIFF-2      PIC X.                                       
000600        05 TRANS-SIFF-3      PIC X.                                       
000700        05 TRANS-SIFF-4      PIC X.                                       
000800     03 MESSAGE-1            PIC X(40).                                   
000900*                                 MEDDELANDE                              
001000     03 IDARTNR-IN           PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200     03 IDARTNR-UT           PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 STRECK               PIC X.                                       
001500     03 REKSIFFR             PIC X.                                       
001600*                                 KONTROLLSIFFRA                          
001700     03 FILLER               OCCURS 2 TIMES.                              
001800        05 KDSPARR           PIC X.                                       
001900*                                 SPÄRRKOD                                
002000     03 FLLSRDEL             PIC X.                                       
002100*                                 LEVERERAS SOM RESDEL                    
002200     03 FILLER               OCCURS 2 TIMES.                              
002300        05 KDERS             PIC Z9(2).                                   
002400*                                 ERSÄTTNINGSKOD                          
002500     03 KVQPACK-1            PIC Z(4)9.                                   
002600*                                 ANTAL I Q1 FÖRPACKNING                  
002700     03 IDANSK               PIC Z(2)9.                                   
002800*                                 ANSKAFFARNUMMER                         
002900     03 IDLEVNR              PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100     03 IDINK                PIC X(4).                                    
003200*                                 INKÖPARNUMMER                           
003300     03 FILLER               OCCURS 2 TIMES.                              
003400        05 ADLAGOMR          PIC Z9B.                                     
003500*                                 LAGEROMRÅDE                             
003600        05 ADGANG            PIC Z9B.                                     
003700*                                 GÅNG                                    
003800        05 ADPLATS           PIC Z(4)9.                                   
003900*                                 LAGERPLATSNUMMER                        
004000     03 FILLER               OCCURS 2 TIMES.                              
004100        05 KVDISP            PIC Z(6)9-.                                  
004200*                                 DISPONIBELT LAGER                       
004300     03 FILLER               OCCURS 2 TIMES.                              
004400        05 KVAKS             PIC Z(6)9-.                                  
004500*                                 ANKOMSTSALDO                            
004600     03 FILLER               OCCURS 2 TIMES.                              
004700        05 KVSPANT           PIC Z(6)9-.                                  
004800*                                 SPÄRRAT ANTAL                           
004900     03 KVPB                 PIC Z(5)9.9.                                 
005000*                                 PERIODBEHOV (PROGNOS)                   
005100     03 FILLER               OCCURS 2 TIMES.                              
005200        05 KVRESS            PIC Z(6)9.                                   
005300*                                 RESERVERAT ANTAL ARTIKLAR               
005400     03 KVBR                 PIC Z(6)9-.                                  
005500*                                 BESTÄLLNINGSREST                        
005600     03 KDSORT               PIC X(2).                                    
005700*                                 SORT-KOD                                
005800     03 PRINK                PIC Z(6)9.9(2).                              
005900*                                 INKÖPSPRIS                              
006000     03 PRARTSTD             PIC Z(6)9.9(2).                              
006100*                                 ARTIKELSTANDARDPRIS                     
006200     03 PRARTBES             PIC Z(6)9.9(2).                              
006300*                                 BESTÄLLNINGSPRIS I KRONOR               
006400     03 PRARTSJK             PIC Z(6)9.9(2).                              
006500*                                 ARTIKELNS SJÄLVKOSTNAD                  
006600     03 KDTIPPR              PIC 9.                                       
006700*                                 TIPPAT PRIS KOD                         
006800     03 IDARTNR-TILLK        PIC Z(8)9.                                   
006900*                                 ARTIKELNUMMER                           
007000     03 DIERS-TILLK          PIC Z(3)9.9(3).                              
007100*                                 TILLKOMMANDE ARTIKELANTAL               
007200     03 VKART                PIC Z(7)9.                                   
007300*                                 ARTIKELVIKT (G)                         
007400     03 FILLER               OCCURS 2 TIMES.                              
007500        05 FILLER            PIC X(2).                                    
007600        05 TIAVIDAT-SEN      PIC 9(6).                                    
007700*                                 SENASTE AVISERINGSDATUM  ÅÅMMDD         
007800        05 FILLER            PIC X(3).                                    
007900     03 KDHF                 PIC 9.                                       
008000*                                 HUVUDFÖRRÅDSMÄRKNING                    
008100     03 FILLER               OCCURS 2 TIMES.                              
008200        05 KVROS             PIC Z(6)9-.                                  
008300*                                 RESTORDERSALDO                          
008400        05 FILLER            PIC X(3).                                    
008500     03 FILLER               OCCURS 2 TIMES.                              
008600        05 KVEFRS            PIC Z(6)9-.                                  
008700*                                 EJ FAKTURERAT ANTAL STYCK               
008800        05 FILLER            PIC X(3).                                    
008900     03 KDKG                 PIC 9.                                       
009000*                                 KURANSGRUPP                             
009100     03 FILLER               PIC X.                                       
009200     03 PRHEMTAG             PIC Z(6)9.9(2).                              
009300*                                 HEMTAGNINGSKOSTNAD                      
009400     03 BEART-SVE            PIC X(25).                                   
009500*                                 SVENSK ARTIKELBENÄMNING                 
009600*** END OF VILMAII-COPY LENGTH= 355 BYTES                                 
