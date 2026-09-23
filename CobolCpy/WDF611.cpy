000100 01  PUDR-WDF611.                                                         
000200*                                 PU-DIREKTLEVERANTÖRER                   
000300*                                 ORDERRAD   INFO                         
000400*                                 FYSISK NYCKEL: IDPURAD                  
000500     03 PUDR-IDPURAD         PIC S9(5)           COMP-3.                  
000600*                                 RADNUMMER PÅ PACKUNDERLAG               
000700*                                 LINENO IN PACKINGDOCUMENT               
000800     03 PUDR-ADART.                                                       
000900*                                 ARTIKELADRESS I LAGRET                  
001000*                                 PARTS-ADRESS                            
001100        05 PUDR-ADLAGOMR     PIC S9(3)           COMP-3.                  
001200*                                 LAGEROMRÅDE                             
001300*                                 AREA                                    
001400        05 PUDR-ADGANG       PIC S9(3)           COMP-3.                  
001500*                                 GÅNG                                    
001600*                                 AISLE                                   
001700        05 PUDR-ADPLATS      PIC S9(5)           COMP-3.                  
001800*                                 LAGERPLATSNUMMER                        
001900*                                 LOCATION                                
002000     03 PUDR-BEART           PIC X(25).                                   
002100*                                 ARTIKELBENÄMNING                        
002200*                                 PART DESCRIPTION                        
002300     03 PUDR-BERADREF        PIC X(10).                                   
002400*                                 KUNDENS RADREFERENS                     
002500*                                 CUSTOMERS ITEM REF.                     
002600     03 PUDR-IDARTNR         PIC S9(9)           COMP-3.                  
002700*                                 ARTIKELNUMMER                           
002800*                                 PART NUMBER                             
002900     03 PUDR-KDARTURS        PIC X(2).                                    
003000*                                 ARTIKELURSPRUNGSKOD                     
003100*                                 COUNTRY OF ORIGIN                       
003200     03 PUDR-KVBEART         PIC S9(7)           COMP-3.                  
003300*                                 BESTÄLLT ANTAL STYCKEN                  
003400*                                 ORDERED QUANTITY                        
003500     03 PUDR-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
003600*                                 ARTIKELPRIS NETTO                       
003700*                                 NET PRICE EACH   (FOB NET)              
003800     03 PUDR-DEAL-PR-LINE.                                                
003900*                                 DEALERPRIS (RAD)                        
004000        05 PUDR-IDPRQUES     PIC 9(7).                                    
004100*                                 PRISFRÅGA NR                            
004200*                                 PRICE QUESTION NO                       
004300        05 PUDR-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
004400*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004500*                                 NET PRICE EACH LOCAL CURRENCY           
004600        05 PUDR-PRARTNTO-LOCPREL                                          
004700                             PIC S9(7)V9(2)      COMP-3.                  
004800*                                 PREL NETTO SLUTKUNDSPRIS I              
004900*                                 LOKAL VALUTA                            
005000*                                 PREL NET PRICE - LOCAL CURRENCY         
005100        05 PUDR-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
005200*                                 PRIS I LOKAL VALUTA                     
005300*                                 LOCAL GROSS SALES PRICE                 
005400        05 PUDR-KDVALISO     PIC X(3).                                    
005500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005600*                                 CURRENCY CODE BY ISO-STANDARD.          
005700        05 PUDR-KDVAT        PIC X(2).                                    
005800*                                 MOMSKOD                                 
005900*                                 VAT CODE                                
006000        05 PUDR-RERAB        PIC S9(2)V9(1)      COMP-3.                  
006100*                                 RABATTSATS (PROCENT)                    
006200        05 PUDR-KDRAB        PIC X(5).                                    
006300*                                 RABATTKOD                               
006400        05 PUDR-BEART-VIPS   PIC X(25).                                   
006500*                                 VIPS ARTIKELBENÄMNING                   
006600*                                 PÅ DEALERNS SPRÅK                       
006700*** END OF VILMAII-COPY LENGTH= 120 BYTES                                 
