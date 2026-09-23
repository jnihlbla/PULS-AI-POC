000100 01  SEQD-WDJ7D1.                                                         
000200*                                 ACS INVENTERINGS REGISTER               
000300*                                 SEK. INGÅNG ACSNR-PRIMECOUNT            
000400*                                 FYSISK NYCKEL: WDJ7D1KY                 
000500*                                 (IDDC + IDACSNR-P + ADLAGOMR +          
000600*                                  ADGANG + ADPLATS + IDARTNR)            
000700*                                 SECONDARY NYCKEL: WDJ7DSEQ              
000800*                                 (IDDC + IDACSNR-P)                      
000900     03 SEQD-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 SEQD-IDACSNR-P       PIC S9(7)           COMP-3.                  
001300*                                 ACS LISTNR FÖR RÄKNING INV.             
001400*                                 ACS REPORT NO PRIMECOUNT PARTS          
001500     03 SEQD-ADLAGOMR        PIC 9(2).                                    
001600*                                 LAGEROMRÅDE                             
001700*                                 AREA                                    
001800     03 SEQD-ADGANG          PIC 9(2).                                    
001900*                                 GÅNG                                    
002000*                                 AISLE                                   
002100     03 SEQD-ADPLATS         PIC 9(5).                                    
002200*                                 LAGERPLATSNUMMER                        
002300*                                 LOCATION                                
002400     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700     03 SEQD-BEART           PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900*                                 PART DESCRIPTION                        
003000     03 SEQD-KVLS            PIC S9(7)           COMP-3.                  
003100*                                 LAGERSALDO                              
003200*                                 STOCK BALANCE                           
003300*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
