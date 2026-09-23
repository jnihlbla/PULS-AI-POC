000100 01  W510L09.                                                             
000200*                                 TYPE L09, INVENTORY TRANSFER RE         
000300*                                 CORD                                    
000400*                                 FOR CHECK AND ANALYZE SALDO             
000500*                                                                         
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800*                                 RECORD TYPE                             
000900     03 KDEKOHT              PIC X(3).                                    
001000*                                 KOD EKONOMISK HÄNDELSE                  
001100*                                 CODE ECONOMIC EVENT                     
001200     03 IDFTG                PIC 9(2).                                    
001300*                                 FÖRETAGSID EKONOM REDOVISNING           
001400*                                 COMPANY IDENTITY ACCOUNTING             
001500     03 IDDC-SEND            PIC X(2).                                    
001600*                                 SÄNDANDE LAGER                          
001700*                                 SENDING WAREHOUSE                       
001800     03 IDDC-REC             PIC X(2).                                    
001900*                                 MOTTAGANDE LAGER                        
002000*                                 RECEIVING WAREHOUSE                     
002100     03 IDDISTR              PIC 9(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300*                                 DISTRICT NUMBER                         
002400     03 IDKUNDNR             PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700     03 IDFAKT               PIC 9(7).                                    
002800*                                 FAKTURANUMMER                           
002900*                                 INVOICE NO.                             
003000     03 DAFAKT               PIC 9(8).                                    
003100*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
003200*                                 INVOICING DATE   (YYYYMMDD)             
003300     03 IDORDNR7             PIC 9(7).                                    
003400*                                 ORDERNUMMER                             
003500*                                 ORDER NUMBER                            
003600     03 IDARTNR              PIC 9(8).                                    
003700*                                 ARTIKELNUMMER                           
003800*                                 PART NUMBER                             
003900     03 KDPRODSL             PIC 9(2).                                    
004000*                                 PRODUKTSLAG                             
004100*                                 PRODUCT GROUP                           
004200     03 KDPSLLOC             PIC 9(2).                                    
004300*                                 PRODUKTSLAG LOKALT                      
004400*                                 PRODUCT GROUP LOCAL                     
004500     03 KVLEVART             PIC 9(7).                                    
004600*                                 LEVERERAT ANTAL STYCK                   
004700*                                 DELIVERED QUANTITY                      
004800     03 PRAVCOST             PIC 9(7)V9(2).                               
004900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005000*                                 AVERAGE COST FOREIGN CURRENCY           
005100     03 FLOVRLEV             PIC X.                                       
005200*                                 ÖVERLEVERANS                            
005300*                                 OVER DELIVERY                           
005400*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
