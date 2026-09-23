000100 01  W510A01.                                                             
000200*                                 TYPE A01 INVOICE TOTAL                  
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
003400     03 KDFRAKT              PIC 9(2).                                    
003500*                                 FRAKTSÄTT DC TILL KUND                  
003600*                                 FREIGHT CODE                            
003700     03 SUFAKTRE             PIC 9(8)V9(2).                               
003800*                                 FSG FAKTURERAT PRIS RESERVDELAR         
003900*                                 SALES SPARE PARTS AT NET PRICE          
004000     03 PREMBHNT             PIC 9(7)V9(2).                               
004100*                                 EMBALLAGE O HANTERINGSKOST              
004200*                                 PACKING O HANDL COSTS                   
004300     03 PRFRAKT              PIC 9(7)V9(2).                               
004400*                                 FRAKTKOSTNAD                            
004500*                                 FREIGHT COST                            
004600     03 PRFOERS              PIC 9(7)V9(2).                               
004700*                                 FÖRSÄKRINGSPREMIE                       
004800*                                 INSURANCE FEE                           
004900     03 PRMOMS               PIC 9(7)V9(2).                               
005000*                                 MERVÄRDESSKATT                          
005100*                                 VAT                                     
005200     03 PRLEGKST             PIC 9(7)V9(2).                               
005300*                                 LEGALISERINSKOSTNAD                     
005400*                                 LEGALIZATION FEE                        
005500     03 SUFKTTILL            PIC 9(7)V9(2).                               
005600*                                 PRISTILLÄGG (KR)                        
005700*                                 ADDITIONAL COSTS (SEK)                  
005800     03 PRAVDRAG             PIC 9(7)V9(2).                               
005900*                                 AVDRAGSBELOPP                           
006000*                                 DEDUCTION                               
006100     03 SUFKTBEL             PIC 9(8)V9(2).                               
006200*                                 SUMMA FAKTURERAT BELOPP                 
006300*                                 TOTAL INVOICED AMOUNT                   
006400     03 SUFKTUTL             PIC 9(11)V9(2).                              
006500*                                 FAKTURABELOPP I UTLÄNDSK VALUTA         
006600*                                 INVOICE-SUM IN FOREIGN VALUE            
006700     03 PRKURS               PIC 9(6)V9(5).                               
006800*                                 VALUTAKURS                              
006900*                                 CURRENCY EXCHANGE RATE                  
007000     03 IDRAPPNR             PIC 9(7).                                    
007100*                                 RAPPORT NUMMER                          
007200*                                 DISCREPANCY REPORT NUMBER               
007300*** END OF VILMAII-COPY LENGTH= 154 BYTES                                 
