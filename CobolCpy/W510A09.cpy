000100 01  W510A09.                                                             
000200*                                 TYPE A09, INVENTORY TRANSFER RE         
000300*                                 CORD                                    
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
002900     03 DAFAKT               PIC 9(8).                                    
003000*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
003100*                                 INVOICING DATE   (YYYYMMDD)             
003200     03 IDORDNR7             PIC 9(7).                                    
003300*                                 ORDERNUMMER                             
003400*                                 ORDER NUMBER                            
003500     03 IDARTNR              PIC 9(8).                                    
003600*                                 ARTIKELNUMMER                           
003700*                                 PART NUMBER                             
003800     03 KDPRODSL             PIC 9(2).                                    
003900*                                 PRODUKTSLAG                             
004000*                                 PRODUCT GROUP                           
004100     03 KDPSLLOC             PIC 9(2).                                    
004200*                                 PRODUKTSLAG LOKALT                      
004300*                                 PRODUCT GROUP LOCAL                     
004400     03 KVLEVART             PIC 9(7).                                    
004500*                                 LEVERERAT ANTAL STYCK                   
004600*                                 DELIVERED QUANTITY                      
004700     03 PRAVCOST             PIC 9(7)V9(2).                               
004800*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004900*                                 AVERAGE COST FOREIGN CURRENCY           
005000*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
