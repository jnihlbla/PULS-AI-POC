000100 01  REQU-WL0109I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0109             
000300*                                 LDC LOST CASE FOUND                     
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDFAKT-KEY      PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 REQU-IDORDNR-KEY     PIC X(5).                                    
001100*                                 ORDERNUMMER UTGÅR PD90                  
001200*                                 ORDER NUMBER                            
001300     03 REQU-IDKUNDNR-KEY    PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 REQU-IDKOLLI-KEY     PIC X(5).                                    
001700*                                 KOLLINUMMER                             
001800*                                 CASE NUMBER                             
001900     03 REQU-IDDC-SEND-KEY   PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 REQU-IDMSG-INFO      PIC X(3).                                    
002300*                                 INFORMATIONSMEDDELANDE ID               
002400*                                 INFORMATION MESSAGE ID                  
002500     03 REQU-KOLLI-KLAR      PIC X.                                       
002600     03 REQU-IDUSER-003      PIC X(5).                                    
002700*                                 ANSVARIGT USERID INLÄGGN.(R32)          
002800     03 REQU-IDTRACK         PIC X(25).                                   
002900*                                 TRACKING ID FROM CUSTOMS                
003000*                                 CUSTOMS TRACKING ID                     
003100     03 REQU-TABELL          OCCURS 12 TIMES.                             
003200*                                                                         
003300        05 REQU-IDARTNR      PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500*                                 PART NUMBER                             
003600        05 REQU-KVANTMOT     PIC 9(6).                                    
003700*                                 ANTAL MOTTAGET                          
003800*                                 QUANTITY RECEIVED                       
003900        05 REQU-KVSKROT      PIC 9(7).                                    
004000*                                 ANTAL SENASTE SKROTORDER                
004100*                                 QUANTITY LAST SCRAPORDER                
004200        05 REQU-ADLAGOMR     PIC 9(2).                                    
004300*                                 LAGEROMRÅDE                             
004400*                                 AREA                                    
004500        05 REQU-ADGANG       PIC 9(2).                                    
004600*                                 GÅNG                                    
004700*                                 AISLE                                   
004800        05 REQU-ADPLATS      PIC 9(5).                                    
004900*                                 LAGERPLATSNUMMER                        
005000*                                 LOCATION                                
005100*** END OF VILMAII-COPY LENGTH= 433 BYTES                                 
