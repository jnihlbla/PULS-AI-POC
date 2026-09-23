000100 01  MOD-W30180O1.                                                        
000200*                                 MOD-COPYTEXT FÖR PGM W30180             
000300*                                 RENOVATOR ORDER                         
000400     03 MOD-IDDISTR          PIC Z(3)9.                                   
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 MOD-KVRADER          PIC Z(4)9.                                   
000800*                                 ANTAL RADER                             
000900*                                 NUMBER OF LINES                         
001000     03 MOD-TABELLRAD        OCCURS 1000 TIMES.                           
001100        05 MOD-IDARTNR-OBJ   PIC Z(7)9.                                   
001200*                                 OBJEKTNUMMER                            
001300        05 MOD-BEART         PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500*                                 PART DESCRIPTION                        
001600        05 MOD-KVLS-91       PIC -(7)9.                                   
001700*                                 LAGERSALDO                              
001800*                                 STOCK BALANCE                           
001900        05 MOD-KVLS-REM      PIC -(7)9.                                   
002000*                                 LAGERSALDO                              
002100*                                 STOCK BALANCE                           
002200        05 MOD-KVBYTPKO      PIC Z(2)9.                                   
002300*                                 ANTAL BYTESOBJEKT PER PALL              
002400*                                 NO OF EXCHANGE CORES PER PALLET         
002500        05 MOD-KVBEART-UPD   PIC Z(5)9.                                   
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700*                                 ORDERED QUANTITY                        
002800        05 MOD-KVANTAL-PREADV                                             
002900                             PIC Z(5)9.                                   
003000*                                 ANTAL                                   
003100*                                 NUMBER                                  
003200        05 MOD-KVAVROP       PIC Z(6)9.                                   
003300*                                 AVROPSKVANTITET                         
003400        05 MOD-KVAVROP-GAM   PIC Z(6)9.                                   
003500*                                 AVROPSKVANTITET                         
003600        05 MOD-IDORDER       PIC Z(6)9.                                   
003700*                                 VOLVO PARTS ORDERNUMMER                 
003800*                                 VOLVO PARTS ORDER NUMBER                
003900        05 MOD-DAORDDAT      PIC 9(8).                                    
004000*                                 ORDERDATUM      (AAAAMMDD)              
004100*                                 ORDERING DATE   (YYYYMMDD)              
004200        05 MOD-KVBEART       PIC Z(5)9.                                   
004300*                                 BESTÄLLT ANTAL STYCKEN                  
004400*                                 ORDERED QUANTITY                        
004500*** END OF VILMAII-COPY LENGTH= 99009 BYTES                               
