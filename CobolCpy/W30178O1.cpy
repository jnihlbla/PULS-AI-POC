000100 01  MOD-W30178O1.                                                        
000200*                                 MOD-COPYTEXT FÖR PGM W30178             
000300*                                 RENOVATOR CONFIRMATION                  
000400     03 MOD-IDDISTR          PIC Z(3)9.                                   
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 MOD-IDSPRAK          PIC X(2).                                    
000800*                                 2-STÄLLIG ISO SPRÅKKOD                  
000900*                                 2-LETTER ISO LANGUAGE CODE              
001000     03 MOD-IDDIARAD-START   PIC 9(6).                                    
001100*                                 FÖRSTA RAD ATT VISA                     
001200*                                 FIRST LINE TO SHOW                      
001300     03 MOD-KVDIARAD-AKTUELLT                                             
001400                             PIC 9(6).                                    
001500*                                 ANTAL RADER ATT VISA                    
001600*                                 NUMBER OF LINES TO BE SHOWN             
001700     03 MOD-KDFEL-IDARTNR-NY PIC X(3).                                    
001800*                                 FELKOD                                  
001900     03 MOD-IDARTNR-NY       PIC Z(8).                                    
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 MOD-KDFEL-AVBART-NY  PIC X(3).                                    
002300*                                 FELKOD                                  
002400     03 MOD-KVAVBART-NY      PIC Z(7).                                    
002500*                                 AVBOKAT ANTAL ARTIKLAR                  
002600*                                 ALLOCATED QUANTITY                      
002700     03 MOD-KVRAD-TOT        PIC Z(5)9.                                   
002800*                                 ANTAL ORDERRADER                        
002900*                                 NUMBER OF ITEMS                         
003000     03 MOD-TABELLRAD        OCCURS 500 TIMES.                            
003100*                                 GRUPP MED TABELLRADER                   
003200        05 MOD-DAORDREG-RAD  PIC X(8).                                    
003300*                                 ORDERDATUM (ÅÅÅÅMMDD)                   
003400*                                 ORDER DATE (YYYYMMDD)                   
003500        05 MOD-IDORDER-RAD   PIC Z(6)9.                                   
003600*                                 VOLVO PARTS ORDERNUMMER                 
003700*                                 VOLVO PARTS ORDER NUMBER                
003800        05 MOD-IDARTNR-RAD   PIC Z(7)9.                                   
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100        05 MOD-IDRADNR-RAD   PIC Z(3)9.                                   
004200*                                 RADNUMMER                               
004300*                                 LINE NO                                 
004400        05 MOD-BEART-RAD     PIC X(25).                                   
004500*                                 ARTIKELBENÄMNING                        
004600*                                 PART DESCRIPTION                        
004700        05 MOD-KVLEVART-RAD  PIC Z(6)9.                                   
004800*                                 LEVERERAT ANTAL STYCK                   
004900*                                 DELIVERED QUANTITY                      
005000        05 MOD-KDFEL-KVAVBART-RAD                                         
005100                             PIC X(3).                                    
005200*                                 FELKOD                                  
005300        05 MOD-KVAVBART-RAD  PIC Z(7).                                    
005400*                                 AVBOKAT ANTAL ARTIKLAR                  
005500*                                 ALLOCATED QUANTITY                      
005600        05 MOD-KVACCEPT-RAD  PIC Z(6)9.                                   
005700*                                 ANTAL                                   
005800*                                 NUMBER                                  
005900        05 MOD-FLORDER-RAD   PIC X.                                       
006000*                                 ALLMÄN FLAGGA                           
006100*                                 GENERAL FLAG                            
006200*** END OF VILMAII-COPY LENGTH= 38545 BYTES                               
