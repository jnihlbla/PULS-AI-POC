000100 01  W510A18.                                                             
000200*                                 TYPE A18, INTERNAL INBOUND              
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
001900     03 IDLEVNR              PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002200     03 IDFS                 PIC X(8).                                    
002300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002400*                                 ADVICE NOTE NUMBER ODETTE               
002500     03 IDARTNR              PIC 9(8).                                    
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 KDPRODSL             PIC 9(2).                                    
002900*                                 PRODUKTSLAG                             
003000*                                 PRODUCT GROUP                           
003100     03 KDPSLLOC             PIC 9(2).                                    
003200*                                 PRODUKTSLAG LOKALT                      
003300*                                 PRODUCT GROUP LOCAL                     
003400     03 DAINLINL             PIC 9(8).                                    
003500*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003600*                                 DATE OF REPORTED IN STOCK (R32)         
003700     03 KVANTMOT             PIC 9(7).                                    
003800*                                 ANTAL MOTTAGET                          
003900*                                 QUANTITY RECEIVED                       
004000     03 PRAVCOST             PIC 9(7)V9(2).                               
004100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004200*                                 AVERAGE COST FOREIGN CURRENCY           
004300     03 IDKONTO              PIC 9(10).                                   
004400*                                 KONTO                                   
004500*                                 ACCOUNT                                 
004600*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
