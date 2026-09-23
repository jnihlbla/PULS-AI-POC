000100 01  W510A17.                                                             
000200*                                 TYPE A17, LOCAL PRODUCT CODE CH         
000300*                                 ANGES                                   
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
002000     03 IDARTNR              PIC 9(8).                                    
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300     03 KDPRODSL             PIC 9(2).                                    
002400*                                 PRODUKTSLAG                             
002500*                                 PRODUCT GROUP                           
002600     03 KDPSLLOC-OLD         PIC 9(2).                                    
002700*                                 PRODUKTSLAG LOKALT                      
002800*                                 PRODUCT GROUP LOCAL                     
002900     03 KDPSLLOC-NEW         PIC 9(2).                                    
003000*                                 PRODUKTSLAG LOKALT                      
003100*                                 PRODUCT GROUP LOCAL                     
003200     03 PRAVCOST             PIC 9(7)V9(2).                               
003300*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003400*                                 AVERAGE COST FOREIGN CURRENCY           
003500     03 KVLS                 PIC S9(7).                                   
003600*                                 LAGERSALDO                              
003700*                                 STOCK BALANCE                           
003800     03 KVEFRS               PIC S9(7).                                   
003900*                                 EJ FAKTURERAT ANTAL STYCK               
004000*                                 ORDERED NOT INVOICED QTY                
004100     03 DAJUSTDA             PIC 9(8).                                    
004200*                                 JUSTERINGSDATUM  (ÅÅÅÅMMDD)             
004300*                                 ADJUSTMENT DATE  (YYYYMMDD)             
004400*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
