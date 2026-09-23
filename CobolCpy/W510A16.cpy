000100 01  W510A16.                                                             
000200*                                 TYPE A16, COST PRICE RECORDS            
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
001900     03 IDARTNR              PIC 9(8).                                    
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 KDPRODSL             PIC 9(2).                                    
002300*                                 PRODUKTSLAG                             
002400*                                 PRODUCT GROUP                           
002500     03 KDPSLLOC             PIC 9(2).                                    
002600*                                 PRODUKTSLAG LOKALT                      
002700*                                 PRODUCT GROUP LOCAL                     
002800     03 DAJUSTDA             PIC 9(8).                                    
002900*                                 JUSTERINGSDATUM  (ÅÅÅÅMMDD)             
003000*                                 ADJUSTMENT DATE  (YYYYMMDD)             
003100     03 PRAVCOST             PIC 9(7)V9(2).                               
003200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003300*                                 AVERAGE COST FOREIGN CURRENCY           
003400     03 PRAVCOST-OLD         PIC 9(7)V9(2).                               
003500*                                 FÖREGÅENDE MEDELVÄRDESKOSTNAD I         
003600*                                  UTL.VALUTA                             
003700*                                 OLD AVERAGE COST FOREIGN CURREN         
003800*                                 CY                                      
003900     03 KVLS                 PIC S9(7).                                   
004000*                                 LAGERSALDO                              
004100*                                 STOCK BALANCE                           
004200     03 IDFS                 PIC X(8).                                    
004300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
004400*                                 ADVICE NOTE NUMBER ODETTE               
004500     03 SUAVCOST             PIC S9(9)V9(2).                              
004600*                                 SUMMA MEDELVÄRDESKOSTNAD I UTL.         
004700*                                 VALUTA                                  
004800*                                 SUM AVERAGE COST FOREIGN CURREN         
004900*                                 CY                                      
005000     03 IDUSER               PIC X(8).                                    
005100*                                 ANVÄNDARENS SÄKERHETS ID                
005200*                                 USER SECURITY-IDENTITY                  
005300     03 KDAVCOST             PIC X(2).                                    
005400*                                 ORSAKSKOD MANUELL KOST ÄNDRING          
005500*                                 REASON CODE AVERAGE COST CHANGE         
005600*** END OF VILMAII-COPY LENGTH= 86 BYTES                                  
