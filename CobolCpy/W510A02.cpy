000100 01  W510A02.                                                             
000200*                                 TYPE A02,INVOICE LINE                   
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 KDEKOHT              PIC X(3).                                    
000800*                                 KOD EKONOMISK HÄNDELSE                  
000900*                                 CODE ECONOMIC EVENT                     
001000     03 IDFTG                PIC 9(2).                                    
001100*                                 FÖRETAGSID EKONOM REDOVISNING           
001200*                                 COMPANY IDENTITY ACCOUNTING             
001300     03 IDDC-SEND            PIC X(2).                                    
001400*                                 SÄNDANDE LAGER                          
001500*                                 SENDING WAREHOUSE                       
001600     03 IDDC-REC             PIC X(2).                                    
001700*                                 MOTTAGANDE LAGER                        
001800*                                 RECEIVING WAREHOUSE                     
001900     03 IDDISTR              PIC 9(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100*                                 DISTRICT NUMBER                         
002200     03 IDKUNDNR             PIC 9(6).                                    
002300*                                 KUNDNUMMER                              
002400*                                 CUSTOMER NO                             
002500     03 IDFAKT               PIC 9(7).                                    
002600*                                 FAKTURANUMMER                           
002700*                                 INVOICE NO.                             
002800     03 KDFAKTYP             PIC X.                                       
002900*                                 FAKTURATYP                              
003000*                                 INVOICE TYPE                            
003100     03 DAFAKT               PIC 9(8).                                    
003200*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
003300*                                 INVOICING DATE   (YYYYMMDD)             
003400     03 IDORDNR7             PIC 9(7).                                    
003500*                                 ORDERNUMMER                             
003600*                                 ORDER NUMBER                            
003700     03 IDARTNR              PIC 9(8).                                    
003800*                                 ARTIKELNUMMER                           
003900*                                 PART NUMBER                             
004000     03 KDPRODSL             PIC 9(2).                                    
004100*                                 PRODUKTSLAG                             
004200*                                 PRODUCT GROUP                           
004300     03 KDPSLLOC             PIC 9(2).                                    
004400*                                 PRODUKTSLAG LOKALT                      
004500*                                 PRODUCT GROUP LOCAL                     
004600     03 KVLEVART             PIC 9(7).                                    
004700*                                 LEVERERAT ANTAL STYCK                   
004800*                                 DELIVERED QUANTITY                      
004900     03 PRARTNTO             PIC 9(7)V9(2).                               
005000*                                 ARTIKELPRIS NETTO                       
005100*                                 NET PRICE EACH   (FOB NET)              
005200*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
