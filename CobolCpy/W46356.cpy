000100 01  W46356.                                                              
000200*                                 DIRECT DELIVERIES DDGS                  
000300*                                 FROM VENDOR TO RETAILER                 
000400*                                                                         
000500     03 IDPRODNR             PIC 9(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700*                                 PRODUCTION NUMBER                       
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 IDDISTR              PIC 9(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600*                                 DISTRICT NUMBER                         
001700     03 IDKUNDNR             PIC 9(6).                                    
001800*                                 KUNDNUMMER                              
001900*                                 CUSTOMER NO                             
002000     03 IDORDNR7             PIC 9(7).                                    
002100*                                 ORDERNUMMER                             
002200*                                 ORDER NUMBER                            
002300     03 IDKOLLI              PIC 9(5).                                    
002400*                                 KOLLINUMMER                             
002500*                                 CASE NUMBER                             
002600     03 FLFEL                PIC X.                                       
002700*                                 ALLMÄN FELFLAGGA                        
002800*                                 GENERAL ERROR FLAG                      
002900     03 IDSUPREF             PIC X(10).                                   
003000*                                 LEVERANTöRSREF.                         
003100*                                 SUPPLIER REF.                           
003200     03 DASUPREF             PIC 9(8).                                    
003300*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
003400*                                 SHIPPING DATE DIRECT SUPPLIER           
003500     03 TISUPTID             PIC 9(4).                                    
003600*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
003700*                                 SHIPPING TIME DIRECT SUPPLIER           
003800     03 VKORDBTO-KOLLI       PIC 9(6)V9(1).                               
003900*                                 ORDERVIKT BRUTTO PER KOLLI              
004000*                                 ORDER WEIGHT GROSS PER CASE             
004100     03 KDEMBTYP             PIC 9.                                       
004200*                                 EMBALLAGETYP                            
004300*                                 PACKAGE TYPE                            
004400     03 DIKOLLIL             PIC 9(4).                                    
004500*                                 KOLLI-LÄNGD                             
004600*                                 CASE LENGTH                             
004700     03 DIKOLLIB             PIC 9(3).                                    
004800*                                 KOLLI-BREDD                             
004900*                                 CASE WIDTH                              
005000     03 DIKOLLIH             PIC 9(3).                                    
005100*                                 KOLLI-HÖJD                              
005200*                                 CASE HEIGHT                             
005300     03 IDRADNR              PIC 9(4).                                    
005400*                                 RADNUMMER                               
005500*                                 LINE NO                                 
005600     03 IDARTNR              PIC 9(8).                                    
005700*                                 ARTIKELNUMMER                           
005800*                                 PART NUMBER                             
005900     03 KVLEVART             PIC 9(7).                                    
006000*                                 LEVERERAT ANTAL STYCK                   
006100*                                 DELIVERED QUANTITY                      
006200     03 KDARTURS             PIC X(2).                                    
006300*                                 ARTIKELURSPRUNGSKOD                     
006400*                                 COUNTRY OF ORIGIN                       
006500     03 VKART                PIC 9(7).                                    
006600*                                 ARTIKELVIKT (G)                         
006700*                                 PART WEIGHT (G)                         
006800     03 VLORDBTO-KOLLI       PIC 9(4)V9(3).                               
006900*                                 ORDERVOLYM BRUTTO KOLLI                 
007000*                                 ORDER VOL GR/CASE                       
007100*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 
