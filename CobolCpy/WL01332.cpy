000100 01  LINE-WL01332.                                                        
000200*                                 COPYTEXT FOR PACKING SPECIFICAT         
000300*                                 ION LDC PU LINE                         
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 LINE-ADLAGOMR        PIC 9(2).                                    
000700*                                 LAGEROMRÅDE                             
000800     03 LINE-ADGANG          PIC 9(2).                                    
000900*                                 GÅNG                                    
001000     03 LINE-ADPLATS         PIC 9(5).                                    
001100*                                 LAGERPLATSNUMMER                        
001200     03 LINE-BEART           PIC X(25).                                   
001300*                                 ARTIKELBENÄMNING                        
001400     03 LINE-BERADREF        PIC X(10).                                   
001500*                                 KUNDENS RADREFERENS                     
001600     03 LINE-WIPID           PIC X(10).                                   
001700*                                 KUNDENS RADREFERENS                     
001800     03 LINE-FLTILLK         PIC X.                                       
001900*                                 TILLKOMMANDE ARTIKEL ?                  
002000     03 LINE-IDARTNR         PIC Z(7)9.                                   
002100*                                 ARTIKELNUMMER                           
002200     03 LINE-IDKUNDRF-RO-URS PIC X(10).                                   
002300*                                 KUNDENS REFERENS (ORDERID)              
002400     03 LINE-IDPURAD         PIC Z(3)9.                                   
002500*                                 RADNUMMER PÅ PACKUNDERLAG               
002600     03 LINE-IDSPECEMB       PIC Z(4).                                    
002700*                                 SPECIALEMBALLAGEID                      
002800     03 LINE-KDARTURS        PIC X(2).                                    
002900*                                 ARTIKELURSPRUNGSKOD                     
003000     03 LINE-KDFARLIG        PIC X.                                       
003100*                                 KOD FÖR FARLIGT GODS                    
003200     03 LINE-KVBEART-Q       PIC Z(5)9.                                   
003300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003400     03 LINE-KVAVBART        PIC Z(5)9.                                   
003500*                                 AVBOKAT ANTAL ARTIKLAR                  
003600     03 LINE-REKSIFFR        PIC 9.                                       
003700*                                 KONTROLLSIFFRA                          
003800     03 LINE-IDLEVART        PIC X(10).                                   
003900     03 LINE-IDSYSTEM        PIC X(4).                                    
004000*                                 VOLVO VCCS SYSTEMNUMMER                 
004100*** END OF VILMAII-COPY LENGTH= 121 BYTES                                 
