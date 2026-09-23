000100 01  RESP-WL0104O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0104         
000300*                                 LDC STOCK DATA REGISTRATION             
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-IDARTNR-KEY     PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 RESP-BEART           PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200*                                 PART DESCRIPTION                        
001300     03 RESP-ADLAGOMR        PIC X(2).                                    
001400*                                 LAGEROMRÅDE                             
001500*                                 AREA                                    
001600     03 RESP-ADGANG          PIC X(2).                                    
001700*                                 GÅNG                                    
001800*                                 AISLE                                   
001900     03 RESP-ADPLATS         PIC X(5).                                    
002000*                                 LAGERPLATSNUMMER                        
002100*                                 LOCATION                                
002200     03 RESP-VKART2          PIC X(10).                                   
002300*                                 ARTIKELVIKT (G/OZ)                      
002400*                                 PART WEIGHT (G/OZ)                      
002500     03 RESP-BESORT-VKART    PIC X(6).                                    
002600*                                 BENÄMNING PÅ SORT/ENHET                 
002700*                                 NAME OF UNIT                            
002800     03 RESP-VLARTNTO        PIC X(10).                                   
002900*                                 ARTIKELVOLYM NETTO (CM3)                
003000*                                 PART NET VOLUME    (CM3)                
003100     03 RESP-BESORT-VLART    PIC X(6).                                    
003200*                                 BENÄMNING PÅ SORT/ENHET                 
003300*                                 NAME OF UNIT                            
003400     03 RESP-KDVSOP          PIC X(3).                                    
003500*                                 VSOP-KOD                                
003600*                                 VSOP-CODE                               
003700     03 RESP-TABELLRAD       OCCURS 5 TIMES.                              
003800*                                 GRUPP MED TABELLRADER                   
003900        05 RESP-IDARTNR      PIC Z(8)9.                                   
004000*                                 ARTIKELNUMMER                           
004100*                                 PART NUMBER                             
004200     03 RESP-KDDIAVAR        PIC X.                                       
004300*                                 DIALOGVARIANT                           
004400*                                 DIALOGUE VARIANT                        
004500*** END OF VILMAII-COPY LENGTH= 126 BYTES                                 
