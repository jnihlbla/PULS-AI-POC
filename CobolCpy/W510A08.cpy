000100 01  W510A08.                                                             
000200*                                 TYPE A08, INVENTORY ADJUSTMENT          
000300*                                 RECORD                                  
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
002000     03 DAJUSTDA             PIC 9(8).                                    
002100*                                 JUSTERINGSDATUM  (ÅÅÅÅMMDD)             
002200*                                 ADJUSTMENT DATE  (YYYYMMDD)             
002300     03 IDARTNR              PIC 9(8).                                    
002400*                                 ARTIKELNUMMER                           
002500*                                 PART NUMBER                             
002600     03 KDPRODSL             PIC 9(2).                                    
002700*                                 PRODUKTSLAG                             
002800*                                 PRODUCT GROUP                           
002900     03 KDPSLLOC             PIC 9(2).                                    
003000*                                 PRODUKTSLAG LOKALT                      
003100*                                 PRODUCT GROUP LOCAL                     
003200     03 KVJUSTKV             PIC S9(7).                                   
003300*                                 JUSTERAD KVANTITET                      
003400*                                 ADJUSTED QUANTITY                       
003500     03 PRAVCOST             PIC 9(7)V9(2).                               
003600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003700*                                 AVERAGE COST FOREIGN CURRENCY           
003800     03 KDINVKAT             PIC 9(2).                                    
003900*                                 INVENTERINGSKATEGORI                    
004000*                                 STOCKTAKING CATEGORY                    
004100     03 TEINVANM             PIC X(25).                                   
004200*                                 INVENTERINGSANMÄRKNING                  
004300*                                 STOCKTAKING COMMENT                     
004400*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
