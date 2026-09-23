000100 01  RESP-W60144O1.                                                       
000200*                                 COPYTEXT FOR RESP W60144O1              
000300*                                                                         
000400     03 RESP-IDRADNR-START   PIC 9(5).                                    
000500*                                 RADNUMMER                               
000600*                                 LINE NO                                 
000700     03 RESP-IDRADNR-NEXT    PIC 9(5).                                    
000800*                                 RADNUMMER                               
000900*                                 LINE NO                                 
001000     03 RESP-IDLOPNRM        PIC X(9).                                    
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300*                                 SERIAL NO RECEIVING REPORT              
001400*                                 (0WWDLLLLC)                             
001500     03 RESP-W60144O1-OUT.                                                
001600*                                                                         
001700*                                                                         
001800        05 RESP-IDARTNR      PIC Z(7)9.                                   
001900*                                 ARTIKELNUMMER                           
002000*                                 PART NUMBER                             
002100        05 RESP-KVAVIS       PIC Z(5)9.                                   
002200*                                 AVISERAT ANTAL                          
002300*                                 QUANTITY NOTIFIED                       
002400        05 RESP-BEART        PIC X(25).                                   
002500*                                 ARTIKELBENÄMNING                        
002600*                                 PART DESCRIPTION                        
002700        05 RESP-KDSORT       PIC X(2).                                    
002800*                                 SORT-KOD                                
002900*                                 UNIT OF MEASURE                         
003000        05 RESP-BEFT         PIC Z9.                                      
003100*                                 FÖRPACKNINGSTYP                         
003200*                                 PACKAGING TYPE                          
003300        05 RESP-KVRAPP       PIC Z(5)9.                                   
003400*                                 DELRAPPORTERAT ANTAL                    
003500*                                 PARTIAL REPORTED QUANTITY               
003600        05 RESP-BEFARLIG-TEXT                                             
003700                             PIC X(10).                                   
003800        05 RESP-ADINLOMR-NXT-ATTR                                         
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 RESP-ADINLOMR-NXT PIC X(4).                                    
004200*                                 INLEVERANSOMRÅDE NÄSTA                  
004300*                                 RECEIVING AREA NEXT                     
004400        05 RESP-ADLAGOMR     PIC Z9.                                      
004500*                                 LAGEROMRÅDE                             
004600*                                 AREA                                    
004700        05 RESP-ADGANG       PIC Z9.                                      
004800*                                 GÅNG                                    
004900*                                 AISLE                                   
005000        05 RESP-ADPLATS      PIC Z(4)9.                                   
005100*                                 LAGERPLATSNUMMER                        
005200*                                 LOCATION                                
005300        05 RESP-KVRADER      PIC 9(5).                                    
005400*                                 ANTAL RADER                             
005500*                                 NUMBER OF LINES                         
005600        05 RESP-FILLER       OCCURS 3 TIMES.                              
005700*                                 UPDATE                                  
005800           07 RESP-ADBUFFOMR PIC Z9.                                      
005900*                                 BUFFERTOMRÅDE                           
006000*                                 BUFFER AREA                             
006100           07 RESP-ADBUFFGANG                                             
006200                             PIC Z9.                                      
006300*                                 BUFFERT GÅNG                            
006400           07 RESP-ADBUFFPL  PIC Z(4)9.                                   
006500*                                 BUFFERPLATSNUMMER                       
006600*                                 LOCATION IN BUFFER                      
006700        05 RESP-IDANSTNR-ATTR                                             
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 RESP-IDANSTNR     PIC X(5).                                    
007100*                                 ANSTÄLLNINGSNUMMER                      
007200*                                 IDENTIFICATION NO EMPLOYEE              
007300        05 RESP-FILLER       OCCURS 50 TIMES.                             
007400*                                 UPDATE                                  
007500           07 RESP-KDCMDVAL-RAD-ATTR                                      
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800           07 RESP-KDCMDVAL-RAD                                           
007900                             PIC X(3).                                    
008000*                                 GENERELL KOMMANDOKOD                    
008100*                                 GENERAL COMMAND-CODE                    
008200           07 RESP-KVINLART-UPD-ATTR                                      
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500           07 RESP-KVINLART-UPD                                           
008600                             PIC X(6).                                    
008700*                                 ANTAL I PARTIRAD                        
008800*                                 QTY/LINE IN A LOT                       
008900           07 RESP-ADINLOMR-NXT-UPD-ATTR                                  
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200           07 RESP-ADINLOMR-NXT-UPD                                       
009300                             PIC X(4).                                    
009400*                                 INLEVERANSOMRÅDE                        
009500*                                 RECEIVING AREA                          
009600           07 RESP-IDRADNR-RAD                                            
009700                             PIC Z(2)9.                                   
009800*                                 RAD INOM ORDER      IDRADNR-002         
009900           07 RESP-KVINLART-RAD                                           
010000                             PIC Z(5)9.                                   
010100*                                 ANTAL I PARTIRAD                        
010200*                                 QTY/LINE IN A LOT                       
010300           07 RESP-ADINLOMR-RAD                                           
010400                             PIC X(4).                                    
010500*                                 INLEVERANSOMRÅDE                        
010600*                                 RECEIVING AREA                          
010700           07 RESP-KDINLSTA-RAD                                           
010800                             PIC X(3).                                    
010900*                                 SYSTEMSTATUS INLEVERANS                 
011000*                                 SYSTEM STATUS RECEIVING                 
011100           07 RESP-IDLEVNR-KOLLI-RAD                                      
011200                             PIC X(5).                                    
011300*                                 LEVERANTÖRNUMMER                        
011400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
011500           07 RESP-IDOKOLLI-RAD                                           
011600                             PIC Z(8)9.                                   
011700*                                 ODETTE KOLLINUMMER                      
011800*                                 ODETTE CASE NUMBER                      
011900           07 RESP-KVINLART-VOR-RAD                                       
012000                             PIC Z(5)9.                                   
012100*                                 ANTAL I PARTIRAD                        
012200*                                 QTY/LINE IN A LOT                       
012300*** END OF VILMAII-COPY LENGTH= 2882 BYTES                                
