000100 01  W510A06.                                                             
000200*                                 TYPE A06, BINNING GOODS RETURNS         
000300*                                  FROM RETAILER                          
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
002600     03 IDRAPPNR             PIC 9(7).                                    
002700*                                 RAPPORT NUMMER                          
002800*                                 DISCREPANCY REPORT NUMBER               
002900     03 DARETILL             PIC 9(8).                                    
003000*                                 RETURTILLSTÅNDSDATUM (AAAAMMDD)         
003100*                                 DATE RETURNPERMIT    (YYYYMMDD)         
003200     03 IDARTNR              PIC 9(8).                                    
003300*                                 ARTIKELNUMMER                           
003400*                                 PART NUMBER                             
003500     03 KDPRODSL             PIC 9(2).                                    
003600*                                 PRODUKTSLAG                             
003700*                                 PRODUCT GROUP                           
003800     03 KDPSLLOC             PIC 9(2).                                    
003900*                                 PRODUKTSLAG LOKALT                      
004000*                                 PRODUCT GROUP LOCAL                     
004100     03 KVLEVANM             PIC 9(6).                                    
004200*                                 LEVERANSANMÄRKNINGSANTAL                
004300*                                 DISCREPANCY REPORT QTY                  
004400     03 KDANMORS             PIC X(2).                                    
004500*                                 ORSAK TILL LEVERANSANMÄRKNING           
004600*                                 DISCREPANCY REPORT REASON CODE          
004700     03 KVRETINL             PIC 9(6).                                    
004800*                                 INLAGT ANTAL VID RETUR                  
004900*                                 RECEIVED QUANTITY ON RETURN             
005000     03 KVRETINL-SKR         PIC 9(6).                                    
005100*                                 INRPT ANTAL SOM SKROTATS                
005200*                                 REPORTED QTY SCRAPPED                   
005300     03 PRAVCOST             PIC 9(7)V9(2).                               
005400*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005500*                                 AVERAGE COST FOREIGN CURRENCY           
005600*** END OF VILMAII-COPY LENGTH= 78 BYTES                                  
