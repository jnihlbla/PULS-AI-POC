000100 01  W510A13.                                                             
000200*                                 TYPE A13, BINNING, VOR RECORD           
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
002800     03 DAFAKT               PIC 9(8).                                    
002900*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
003000*                                 INVOICING DATE   (YYYYMMDD)             
003100     03 IDORDNR7             PIC 9(7).                                    
003200*                                 ORDERNUMMER                             
003300*                                 ORDER NUMBER                            
003400     03 IDARTNR              PIC 9(8).                                    
003500*                                 ARTIKELNUMMER                           
003600*                                 PART NUMBER                             
003700     03 KDPRODSL             PIC 9(2).                                    
003800*                                 PRODUKTSLAG                             
003900*                                 PRODUCT GROUP                           
004000     03 KDPSLLOC             PIC 9(2).                                    
004100*                                 PRODUKTSLAG LOKALT                      
004200*                                 PRODUCT GROUP LOCAL                     
004300     03 KVLEVART             PIC 9(7).                                    
004400*                                 LEVERERAT ANTAL STYCK                   
004500*                                 DELIVERED QUANTITY                      
004600     03 PRARTNTO             PIC 9(7)V9(2).                               
004700*                                 ARTIKELPRIS NETTO                       
004800*                                 NET PRICE EACH   (FOB NET)              
004900     03 PRAVCOST             PIC 9(7)V9(2).                               
005000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005100*                                 AVERAGE COST FOREIGN CURRENCY           
005200     03 PRAVCOST-OLD         PIC 9(7)V9(2).                               
005300*                                 FÖREGÅENDE MEDELVÄRDESKOSTNAD I         
005400*                                  UTL.VALUTA                             
005500*                                 OLD AVERAGE COST FOREIGN CURREN         
005600*                                 CY                                      
005700     03 KVLS-OLD             PIC S9(7).                                   
005800*                                 LAGERSALDO FÖRE ÄNDRING                 
005900*                                 STOCK BALANCE BEFORE CHANGE             
006000     03 PRKURS               PIC 9(6)V9(5).                               
006100*                                 VALUTAKURS                              
006200*                                 CURRENCY EXCHANGE RATE                  
006300     03 REMARKUP             PIC 9V9(2).                                  
006400*                                 KOST UPPRÄKNINGSFAKTOR                  
006500*                                 COST MARK UP FACTOR                     
006600*** END OF VILMAII-COPY LENGTH= 111 BYTES                                 
