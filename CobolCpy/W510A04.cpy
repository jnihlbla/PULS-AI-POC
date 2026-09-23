000100 01  W510A04.                                                             
000200*                                 TYPE A04, PURCHASE CREDIT TOTAL         
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
002500     03 IDKNOTNR             PIC 9(7).                                    
002600*                                 KREDITNOTANUMMER                        
002700*                                 CREDIT NOTE NUMBER                      
002800     03 DAKRENOT             PIC 9(8).                                    
002900*                                 DATUM KREDITNOTA  (ÅÅÅÅMMDD)            
003000*                                 DATE CREDIT NOTE ISSUE (YYYYMMD         
003100*                                 D)                                      
003200     03 IDRAPPNR             PIC 9(7).                                    
003300*                                 RAPPORT NUMMER                          
003400*                                 DISCREPANCY REPORT NUMBER               
003500     03 SUKRENTO             PIC 9(11)V9(2).                              
003600*                                 KREDITERAT VARUVÄRDE NETTO              
003700*                                 CREDITED GOODSVALUE NET                 
003800     03 PRLANDCO             PIC 9(6)V9(2).                               
003900*                                 LANDING COST                            
004000*                                 LANDING COST                            
004100     03 PREMBHNT             PIC 9(7)V9(2).                               
004200*                                 EMBALLAGE O HANTERINGSKOST              
004300*                                 PACKING O HANDL COSTS                   
004400     03 PRFRAKT              PIC 9(7)V9(2).                               
004500*                                 FRAKTKOSTNAD                            
004600*                                 FREIGHT COST                            
004700     03 PRFOERS              PIC 9(7)V9(2).                               
004800*                                 FÖRSÄKRINGSPREMIE                       
004900*                                 INSURANCE FEE                           
005000     03 PRMOMS               PIC 9(7)V9(2).                               
005100*                                 MERVÄRDESSKATT                          
005200*                                 VAT                                     
005300     03 PRLEGKST             PIC 9(7)V9(2).                               
005400*                                 LEGALISERINSKOSTNAD                     
005500*                                 LEGALIZATION FEE                        
005600     03 SUKRENOT             PIC 9(7)V9(2).                               
005700*                                 KREDITNOTASUMMA                         
005800*                                 CREDIT NOTE TOTAL                       
005900     03 SUKREUTL             PIC 9(11)V9(2).                              
006000*                                 KREDITNOTASUMMA OMRÄKNAT I KUND         
006100*                                 ENS VALUTA                              
006200*                                 CREDIT NOTE TOTAL IN FOREIGN VA         
006300*                                 LUE                                     
006400     03 PRKURS               PIC 9(6)V9(5).                               
006500*                                 VALUTAKURS                              
006600*                                 CURRENCY EXCHANGE RATE                  
006700*** END OF VILMAII-COPY LENGTH= 143 BYTES                                 
