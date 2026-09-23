000100 01  W510A11.                                                             
000200*                                 TYPE A11, BINNING LOCAL DELIVER         
000300*                                 IES                                     
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
002000     03 IDLEVNR              PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002300     03 IDFS                 PIC X(8).                                    
002400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002500*                                 ADVICE NOTE NUMBER ODETTE               
002600     03 DAINLINL             PIC 9(8).                                    
002700*                                 RAPPORTERINGSDATUM INLAGD (R32)         
002800*                                 DATE OF REPORTED IN STOCK (R32)         
002900     03 IDORDNR7             PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200     03 IDARTNR              PIC 9(8).                                    
003300*                                 ARTIKELNUMMER                           
003400*                                 PART NUMBER                             
003500     03 KDPRODSL             PIC 9(2).                                    
003600*                                 PRODUKTSLAG                             
003700*                                 PRODUCT GROUP                           
003800     03 KDPSLLOC             PIC 9(2).                                    
003900*                                 PRODUKTSLAG LOKALT                      
004000*                                 PRODUCT GROUP LOCAL                     
004100     03 PRARTBEU             PIC 9(5)V9(2).                               
004200*                                 BESTPRIS UTLÄNDSK VALUTA                
004300*                                 ORDER PRICE IN FOREIGN CURRENCY         
004400     03 KVAVIS               PIC 9(6).                                    
004500*                                 AVISERAT ANTAL                          
004600*                                 QUANTITY NOTIFIED                       
004700     03 KVANTMOT             PIC 9(7).                                    
004800*                                 ANTAL MOTTAGET                          
004900*                                 QUANTITY RECEIVED                       
005000     03 PRAVCOST             PIC 9(7)V9(2).                               
005100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005200*                                 AVERAGE COST FOREIGN CURRENCY           
005300     03 PRAVCOST-OLD         PIC 9(7)V9(2).                               
005400*                                 FÖREGÅENDE MEDELVÄRDESKOSTNAD I         
005500*                                  UTL.VALUTA                             
005600*                                 OLD AVERAGE COST FOREIGN CURREN         
005700*                                 CY                                      
005800     03 KVLS-OLD             PIC S9(7).                                   
005900*                                 LAGERSALDO FÖRE ÄNDRING                 
006000*                                 STOCK BALANCE BEFORE CHANGE             
006100     03 REMARKUP             PIC 9V9(2).                                  
006200*                                 KOST UPPRÄKNINGSFAKTOR                  
006300*                                 COST MARK UP FACTOR                     
006400*** END OF VILMAII-COPY LENGTH= 100 BYTES                                 
