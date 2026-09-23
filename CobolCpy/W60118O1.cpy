000100 01  RESP-W60118O1.                                                       
000200*                                 RESPONSE COPYTEXT FOR W6011810          
000300     03 RESP-IDRADNR-START   PIC 9(4).                                    
000400*                                 RADNUMMER                               
000500*                                 LINE NO                                 
000600     03 RESP-IDRADNR-NEXT    PIC 9(4).                                    
000700*                                 RADNUMMER                               
000800*                                 LINE NO                                 
000900     03 RESP-IDARTNR         PIC Z(7)9.                                   
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 RESP-KVAVIS          PIC Z(5)9.                                   
001300*                                 AVISERAT ANTAL                          
001400*                                 QUANTITY NOTIFIED                       
001500     03 RESP-BEART           PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700*                                 PART DESCRIPTION                        
001800     03 RESP-KDSORT          PIC X(2).                                    
001900*                                 SORT-KOD                                
002000*                                 UNIT OF MEASURE                         
002100     03 RESP-BEFT            PIC Z9.                                      
002200*                                 FÖRPACKNINGSTYP                         
002300*                                 PACKAGING TYPE                          
002400     03 RESP-KVRAPP          PIC Z(5)9.                                   
002500*                                 DELRAPPORTERAT ANTAL                    
002600*                                 PARTIAL REPORTED QUANTITY               
002700     03 RESP-BEFARLIG        PIC X(14).                                   
002800     03 RESP-KVRADER         PIC Z(4)9.                                   
002900*                                 ANTAL RADER                             
003000*                                 NUMBER OF LINES                         
003100     03 RESP-LINES           OCCURS 50 TIMES.                             
003200*                                 RADER                                   
003300        05 RESP-ADINLOMR-UPD-LINE-ATTR                                    
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 RESP-ADINLOMR-UPD-LINE                                         
003700                             PIC X(4).                                    
003800*                                 INLEVERANSOMRÅDE                        
003900*                                 RECEIVING AREA                          
004000        05 RESP-IDRADNR-LINE PIC Z(3)9.                                   
004100*                                 RADNUMMER                               
004200*                                 LINE NO                                 
004300        05 RESP-KVINLART-LINE                                             
004400                             PIC -(6)9.                                   
004500*                                 ANTAL I PARTIRAD                        
004600*                                 QTY/LINE IN A LOT                       
004700        05 RESP-ADINLOMR-LINE                                             
004800                             PIC X(4).                                    
004900*                                 INLEVERANSOMRÅDE                        
005000*                                 RECEIVING AREA                          
005100        05 RESP-KDINLSTA-LINE                                             
005200                             PIC X(3).                                    
005300*                                 SYSTEMSTATUS INLEVERANS                 
005400*                                 SYSTEM STATUS RECEIVING                 
005500        05 RESP-IDLEVNR-KOLLI-LINE                                        
005600                             PIC X(5).                                    
005700*                                 LEVERANTÖRNUMMER KOLLI                  
005800*                                 SUPPLIER NUMBER CASE                    
005900        05 RESP-IDOKOLLI-LINE                                             
006000                             PIC Z(8)9.                                   
006100*                                 ODETTE KOLLINUMMER                      
006200*                                 ODETTE CASE NUMBER                      
006300        05 RESP-STATUS-TEXT-LINE                                          
006400                             PIC X(25).                                   
006500*** END OF VILMAII-COPY LENGTH= 3226 BYTES                                
