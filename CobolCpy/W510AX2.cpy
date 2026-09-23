000100 01  W510AX2.                                                             
000200*                                 TYPE AX2,INVOICE LINE,                  
000300*                                 SAME AS A02 + FLOVRLEV                  
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
002600     03 IDFAKT               PIC 9(7).                                    
002700*                                 FAKTURANUMMER                           
002800*                                 INVOICE NO.                             
002900     03 KDFAKTYP             PIC X.                                       
003000*                                 FAKTURATYP                              
003100*                                 INVOICE TYPE                            
003200     03 DAFAKT               PIC 9(8).                                    
003300*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
003400*                                 INVOICING DATE   (YYYYMMDD)             
003500     03 IDORDNR7             PIC 9(7).                                    
003600*                                 ORDERNUMMER                             
003700*                                 ORDER NUMBER                            
003800     03 IDARTNR              PIC 9(8).                                    
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100     03 KDPRODSL             PIC 9(2).                                    
004200*                                 PRODUKTSLAG                             
004300*                                 PRODUCT GROUP                           
004400     03 KDPSLLOC             PIC 9(2).                                    
004500*                                 PRODUKTSLAG LOKALT                      
004600*                                 PRODUCT GROUP LOCAL                     
004700     03 KVLEVART             PIC 9(7).                                    
004800*                                 LEVERERAT ANTAL STYCK                   
004900*                                 DELIVERED QUANTITY                      
005000     03 PRARTNTO             PIC 9(7)V9(2).                               
005100*                                 ARTIKELPRIS NETTO                       
005200*                                 NET PRICE EACH   (FOB NET)              
005300     03 FLOVRLEV             PIC X.                                       
005400*                                 ÖVERLEVERANS                            
005500*                                 OVER DELIVERY                           
005600*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
