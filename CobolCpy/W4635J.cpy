000100 01  W4635J.                                                              
000200*                                 JUSTERAD KOLLIVIKT                      
000300*                                                                         
000400     03 IDPRODNR             PIC 9(7).                                    
000500*                                 PRODUKTIONSNUMMER                       
000600*                                 PRODUCTION NUMBER                       
000700     03 IDSUPREF             PIC X(10).                                   
000800*                                 LEVERANTˆRSREF.                         
000900*                                 SUPPLIER REF.                           
001000     03 DAREGDAT             PIC 9(8).                                    
001100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001200*                                 REGISTRATION DATE (YYYYMMDD)            
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANT÷RNUMMER                        
001500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001600     03 IDDISTR              PIC 9(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900     03 IDKUNDNR             PIC 9(6).                                    
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200     03 IDORDNR7             PIC 9(7).                                    
002300*                                 ORDERNUMMER                             
002400*                                 ORDER NUMBER                            
002500     03 IDDC                 PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 IDKOLLI              PIC 9(5).                                    
002900*                                 KOLLINUMMER                             
003000*                                 CASE NUMBER                             
003100     03 VKORDBTO-KOLLI       PIC 9(6)V9(1).                               
003200*                                 ORDERVIKT BRUTTO PER KOLLI              
003300*                                 ORDER WEIGHT GROSS PER CASE             
003400     03 VKORDBTO-KOLLI-ADJ   PIC 9(6)V9(1).                               
003500*                                 ORDERVIKT BRUTTO PER KOLLI              
003600*                                 ORDER WEIGHT GROSS PER CASE             
003700     03 IDRADNR              PIC 9(4).                                    
003800*                                 RADNUMMER                               
003900*                                 LINE NO                                 
004000     03 IDARTNR              PIC 9(8).                                    
004100*                                 ARTIKELNUMMER                           
004200*                                 PART NUMBER                             
004300     03 KVLEVART             PIC 9(7).                                    
004400*                                 LEVERERAT ANTAL STYCK                   
004500*                                 DELIVERED QUANTITY                      
004600     03 VKART                PIC 9(7).                                    
004700*                                 ARTIKELVIKT (G)                         
004800*                                 PART WEIGHT (G)                         
004900*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  
