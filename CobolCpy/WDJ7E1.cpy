000100 01  SEQE-WDJ7E1.                                                         
000200*                                 ACS INVENTERINGS REGISTER               
000300*                                 SEKUNDÄR INGÅNG ACSNR-RCOUNT            
000400*                                 FYSISK NYCKEL: WDJ7E1KY                 
000500*                                 (IDDC + IDACSNR-R + ADLAGOMR +          
000600*                                  ADGANG + ADPLATS + IDARTNR)            
000700*                                 SECONDARY NYCKEL: WDJ7ESEQ              
000800*                                 (IDDC + IDACSNR-R)                      
000900     03 SEQE-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 SEQE-IDACSNR-R       PIC S9(7)           COMP-3.                  
001300*                                 ACS LISTNR FÖR OMRÄKNING INV.           
001400*                                 ACS REPORT NO FOR RECOUNT PARTS         
001500     03 SEQE-ADLAGOMR        PIC 9(2).                                    
001600*                                 LAGEROMRÅDE                             
001700*                                 AREA                                    
001800     03 SEQE-ADGANG          PIC 9(2).                                    
001900*                                 GÅNG                                    
002000*                                 AISLE                                   
002100     03 SEQE-ADPLATS         PIC 9(5).                                    
002200*                                 LAGERPLATSNUMMER                        
002300*                                 LOCATION                                
002400     03 SEQE-IDARTNR         PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700     03 SEQE-BEART           PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900*                                 PART DESCRIPTION                        
003000     03 SEQE-KVLS            PIC S9(7)           COMP-3.                  
003100*                                 LAGERSALDO                              
003200*                                 STOCK BALANCE                           
003300*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
