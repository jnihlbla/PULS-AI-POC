000100 01  W510A10.                                                             
000200*                                 TYPE A10, DIRECT DELIVERIES             
000300*                                 FROM VENDOR TO RETAILER                 
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700*                                 RECORD TYPE                             
000800     03 KDEKOHT              PIC X(3).                                    
000900*                                 KOD EKONOMISK HÄNDELSE                  
001000*                                 CODE ECONOMIC EVENT                     
001100     03 IDFTG                PIC 9(2).                                    
001200*                                 FÖRETAGSID EKONOM REDOVISNING           
001300*                                 COMPANY IDENTITY ACCOUNTING             
001400     03 IDDC-SEND            PIC X(2).                                    
001500*                                 SÄNDANDE LAGER                          
001600*                                 SENDING WAREHOUSE                       
001700     03 IDDC-REC             PIC X(2).                                    
001800*                                 MOTTAGANDE LAGER                        
001900*                                 RECEIVING WAREHOUSE                     
002000     03 IDDISTR              PIC 9(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200*                                 DISTRICT NUMBER                         
002300     03 IDKUNDNR             PIC 9(6).                                    
002400*                                 KUNDNUMMER                              
002500*                                 CUSTOMER NO                             
002600     03 IDPRODNR             PIC 9(7).                                    
002700*                                 PRODUKTIONSNUMMER                       
002800*                                 PRODUCTION NUMBER                       
002900     03 IDORDNR7             PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200     03 IDLEVNR              PIC X(5).                                    
003300*                                 LEVERANTÖRNUMMER                        
003400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003500     03 IDARTNR              PIC 9(8).                                    
003600*                                 ARTIKELNUMMER                           
003700*                                 PART NUMBER                             
003800     03 KDPRODSL             PIC 9(2).                                    
003900*                                 PRODUKTSLAG                             
004000*                                 PRODUCT GROUP                           
004100     03 KDPSLLOC             PIC 9(2).                                    
004200*                                 PRODUKTSLAG LOKALT                      
004300*                                 PRODUCT GROUP LOCAL                     
004400     03 KVBEART              PIC 9(6).                                    
004500*                                 BESTÄLLT ANTAL STYCKEN                  
004600*                                 ORDERED QUANTITY                        
004700     03 KVLEVART             PIC 9(7).                                    
004800*                                 LEVERERAT ANTAL STYCK                   
004900*                                 DELIVERED QUANTITY                      
005000     03 DAORDDAT             PIC 9(8).                                    
005100*                                 ORDERDATUM      (AAAAMMDD)              
005200*                                 ORDERING DATE   (YYYYMMDD)              
005300     03 PRARTBEU             PIC 9(5)V9(2).                               
005400*                                 BESTPRIS UTLÄNDSK VALUTA                
005500*                                 ORDER PRICE IN FOREIGN CURRENCY         
005600     03 PRARTNTO-GNB         PIC 9(7)V9(2).                               
005700*                                 NETTOPRIS FRÅN DIRLEV GNB               
005800*                                 NET PRICE FROM SUPPLIER GNB             
005900     03 IDFAKT-GNB           PIC X(8).                                    
006000*                                 FAKTURANUMMER GNB                       
006100*                                 INVOICE NO. GNB                         
006200     03 DAFAKT-GNB           PIC 9(8).                                    
006300*                                 FAKTURADATUM GNB (ÅÅÅÅMMDD)             
006400*                                 INVOICE DATE GNB (YYYYMMDD)             
006500*** END OF VILMAII-COPY LENGTH= 106 BYTES                                 
