000100 01  W510A03.                                                             
000200*                                 TYPE A03,BINNIN,REFILL GOODS            
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
003400     03 IDKOLLI              PIC 9(5).                                    
003500*                                 KOLLINUMMER                             
003600*                                 CASE NUMBER                             
003700     03 IDARTNR              PIC 9(8).                                    
003800*                                 ARTIKELNUMMER                           
003900*                                 PART NUMBER                             
004000     03 KDPRODSL             PIC 9(2).                                    
004100*                                 PRODUKTSLAG                             
004200*                                 PRODUCT GROUP                           
004300     03 KDPSLLOC             PIC 9(2).                                    
004400*                                 PRODUKTSLAG LOKALT                      
004500*                                 PRODUCT GROUP LOCAL                     
004600     03 KDANMORS             PIC X(2).                                    
004700*                                 ORSAK TILL LEVERANSANMÄRKNING           
004800*                                 DISCREPANCY REPORT REASON CODE          
004900     03 KVLEVART             PIC 9(7).                                    
005000*                                 LEVERERAT ANTAL STYCK                   
005100*                                 DELIVERED QUANTITY                      
005200     03 PRARTNTO             PIC 9(7)V9(2).                               
005300*                                 ARTIKELPRIS NETTO                       
005400*                                 NET PRICE EACH   (FOB NET)              
005500     03 KDVALISO             PIC X(3).                                    
005600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005700*                                 CURRENCY CODE BY ISO-STANDARD.          
005800     03 DAINLINL             PIC 9(8).                                    
005900*                                 RAPPORTERINGSDATUM INLAGD (R32)         
006000*                                 DATE OF REPORTED IN STOCK (R32)         
006100     03 KVANTMOT             PIC 9(7).                                    
006200*                                 ANTAL MOTTAGET                          
006300*                                 QUANTITY RECEIVED                       
006400     03 KVSKROT              PIC 9(7).                                    
006500*                                 ANTAL SENASTE SKROTORDER                
006600*                                 QUANTITY LAST SCRAPORDER                
006700     03 PRAVCOST             PIC 9(7)V9(2).                               
006800*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
006900*                                 AVERAGE COST FOREIGN CURRENCY           
007000     03 PRAVCOST-OLD         PIC 9(7)V9(2).                               
007100*                                 FÖREGÅENDE MEDELVÄRDESKOSTNAD I         
007200*                                  UTL.VALUTA                             
007300*                                 OLD AVERAGE COST FOREIGN CURREN         
007400*                                 CY                                      
007500     03 KVLS-OLD             PIC S9(7).                                   
007600*                                 LAGERSALDO FÖRE ÄNDRING                 
007700*                                 STOCK BALANCE BEFORE CHANGE             
007800     03 FLSLUT               PIC X.                                       
007900*                                 AVSLUTNINGSFLAGGA                       
008000     03 PRKURS               PIC 9(6)V9(5).                               
008100*                                 VALUTAKURS                              
008200*                                 CURRENCY EXCHANGE RATE                  
008300     03 REMARKUP             PIC 9V9(2).                                  
008400*                                 KOST UPPRÄKNINGSFAKTOR                  
008500*                                 COST MARK UP FACTOR                     
008600*** END OF VILMAII-COPY LENGTH= 144 BYTES                                 
