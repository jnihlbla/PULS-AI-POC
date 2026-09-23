000100 01  SEQF-WDJ7F1.                                                         
000200*                                 ACS INVENTERINGS REGISTER               
000300*                                 SEKUNDÄR INGÅNG ACSNR-3RDCOUNT          
000400*                                 FYSISK NYCKEL: WDJ7F1KY                 
000500*                                 (IDDC + IDACSNR-T + ADLAGOMR +          
000600*                                  ADGANG + ADPLATS + IDARTNR)            
000700*                                 SECONDARY NYCKEL: WDJ7FSEQ              
000800*                                 (IDDC + IDACSNR-T)                      
000900     03 SEQF-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 SEQF-IDACSNR-T       PIC S9(7)           COMP-3.                  
001300*                                 ACS LISTNR FÖR 3E RÄKNING INV.          
001400*                                 ACS REPORTNO FOR 3RD COUNT PART         
001500     03 SEQF-ADLAGOMR        PIC 9(2).                                    
001600*                                 LAGEROMRÅDE                             
001700*                                 AREA                                    
001800     03 SEQF-ADGANG          PIC 9(2).                                    
001900*                                 GÅNG                                    
002000*                                 AISLE                                   
002100     03 SEQF-ADPLATS         PIC 9(5).                                    
002200*                                 LAGERPLATSNUMMER                        
002300*                                 LOCATION                                
002400     03 SEQF-IDARTNR         PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700     03 SEQF-BEART           PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900*                                 PART DESCRIPTION                        
003000     03 SEQF-KVLS            PIC S9(7)           COMP-3.                  
003100*                                 LAGERSALDO                              
003200*                                 STOCK BALANCE                           
003300*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
