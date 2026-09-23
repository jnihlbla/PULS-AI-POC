000100 01  W510A14.                                                             
000200*                                 TYPE A14, BINNING, CORE RETURNS         
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
002600     03 IDBYTRAP             PIC 9(7).                                    
002700*                                 RAPPORTNUMMER  BYTES                    
002800*                                 REPORTNUMBER   EXCHANGE                 
002900     03 DAINLINL             PIC 9(8).                                    
003000*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003100*                                 DATE OF REPORTED IN STOCK (R32)         
003200     03 IDARTNR              PIC 9(8).                                    
003300*                                 ARTIKELNUMMER                           
003400*                                 PART NUMBER                             
003500     03 KDPRODSL             PIC 9(2).                                    
003600*                                 PRODUKTSLAG                             
003700*                                 PRODUCT GROUP                           
003800     03 KDPSLLOC             PIC 9(2).                                    
003900*                                 PRODUKTSLAG LOKALT                      
004000*                                 PRODUCT GROUP LOCAL                     
004100     03 KVRETUR              PIC 9(7).                                    
004200*                                 ANTAL I RETUR                           
004300*                                 QUANTITY IN RETURN                      
004400     03 PRAVCOST             PIC 9(7)V9(2).                               
004500*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004600*                                 AVERAGE COST FOREIGN CURRENCY           
004700*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
