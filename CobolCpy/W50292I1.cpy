000100 01  REQU-W50292I1.                                                       
000200*                                 REQUEST-COPYTEXT FÖR BILD 5292          
000300*                                 VID ANROP FRÅN WEBBEN                   
000400*                                 ACS COUNT SELECTION                     
000500     03 REQU-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 REQU-ADLAGOMR-FOM    PIC 9(2).                                    
000900*                                 LAGEROMRÅDE FRÅN OCH MED                
001000*                                 AREA ADDRESS FROM                       
001100     03 REQU-ADLAGOMR-TOM    PIC 9(2).                                    
001200*                                 LAGEROMRÅDE TILL OCH MED                
001300*                                 AREA ADDRESS TO                         
001400     03 REQU-ADGANG-FOM      PIC 9(2).                                    
001500*                                 GÅNG FRÅN OCH MED                       
001600*                                 AISLE ADDRESS FROM                      
001700     03 REQU-ADGANG-TOM      PIC 9(2).                                    
001800*                                 GÅNG TILL OCH MED                       
001900*                                 AISLE ADDRESS TO                        
002000     03 REQU-ADPLATS         PIC 9(5).                                    
002100*                                 LAGERPLATSNUMMER                        
002200*                                 LOCATION                                
002300     03 REQU-KDBUFFER        PIC X.                                       
002400*                                 BUFF.PL PÅVERKAN PÅ INV.URVAL           
002500*                                 HOW BUFFER LOC AFFECTS INVENT.          
002600     03 REQU-KVART-INVPRINT  PIC 9(7).                                    
002700*                                 ANT ART SKRIVAS UT & INVENTERAS         
002800*                                 QTY PARTS PRINTED FOR INVENTORY         
002900     03 REQU-IDPRTOMG        PIC 9.                                       
003000*                                 PRINTOMGÅNG FÖR INVENT/JUSTERIN         
003100*                                 PRINT ROUND OF INVENTORY/ADJUST         
003200     03 REQU-IDCOUNTER       PIC X(20).                                   
003300*                                 RÄKNARE/INVENTERARE                     
003400*                                 COUNTER'S NAME IN INVENTORY             
003500*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
