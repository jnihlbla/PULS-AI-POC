000100 01  W510A05.                                                             
000200*                                 TYPE A05, PURCASE CREDIT LINE           
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
003200     03 IDARTNR              PIC 9(8).                                    
003300*                                 ARTIKELNUMMER                           
003400*                                 PART NUMBER                             
003500     03 KDPRODSL             PIC 9(2).                                    
003600*                                 PRODUKTSLAG                             
003700*                                 PRODUCT GROUP                           
003800     03 KDPSLLOC             PIC 9(2).                                    
003900*                                 PRODUKTSLAG LOKALT                      
004000*                                 PRODUCT GROUP LOCAL                     
004100     03 KDANMORS             PIC X(2).                                    
004200*                                 ORSAK TILL LEVERANSANMÄRKNING           
004300*                                 DISCREPANCY REPORT REASON CODE          
004400     03 KVKREANT             PIC 9(6).                                    
004500*                                 KREDITERAT ANTAL                        
004600*                                 CREDITED QUANTITY                       
004700     03 PRARTNTO             PIC 9(7)V9(2).                               
004800*                                 ARTIKELPRIS NETTO                       
004900*                                 NET PRICE EACH   (FOB NET)              
005000*** END OF VILMAII-COPY LENGTH= 66 BYTES                                  
