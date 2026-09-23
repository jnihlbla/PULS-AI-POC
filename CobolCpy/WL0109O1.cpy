000100 01  RESP-WL0109O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WL0109         
000300*                                 LDC LOST CASE FOUND                     
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-IDFAKT-KEY      PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 RESP-IDORDNR-KEY     PIC X(5).                                    
001100*                                 ORDERNUMMER UTGÅR PD90                  
001200*                                 ORDER NUMBER                            
001300     03 RESP-IDKUNDNR-KEY    PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 RESP-IDKOLLI-KEY     PIC X(5).                                    
001700*                                 KOLLINUMMER                             
001800*                                 CASE NUMBER                             
001900     03 RESP-IDDC-SEND-KEY   PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 RESP-KOLLI-KLAR      PIC X.                                       
002300     03 RESP-IDUSER-003      PIC X(5).                                    
002400*                                 ANSVARIGT USERID INLÄGGN.(R32)          
002500     03 RESP-FLTRACK         PIC X.                                       
002600*                                 FLAG FOR TRACKING-ID FOR A DC           
002700*                                                                         
002800*                                 FLAG FOR TRACKING-ID FOR A DC           
002900*                                                                         
003000     03 RESP-IDTRACK         PIC X(25).                                   
003100*                                 TRACKING ID FROM CUSTOMS                
003200*                                 CUSTOMS TRACKING ID                     
003300     03 RESP-FLOLD-IDTRACK   PIC X.                                       
003400*                                 ALLMÄN FLAGGA                           
003500*                                 GENERAL FLAG                            
003600     03 RESP-TAB             OCCURS 12 TIMES.                             
003700*                                 GRUPP MED TABELLRADER INPUT             
003800        05 RESP-IDARTNR      PIC Z(8)9.                                   
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100        05 RESP-KVANTMOT     PIC Z(5)9.                                   
004200*                                 ANTAL MOTTAGET                          
004300*                                 QUANTITY RECEIVED                       
004400        05 RESP-KVSKROT      PIC Z(6)9.                                   
004500*                                 ANTAL SENASTE SKROTORDER                
004600*                                 QUANTITY LAST SCRAPORDER                
004700        05 RESP-ADLAGOMR     PIC 9(2).                                    
004800*                                 LAGEROMRÅDE                             
004900*                                 AREA                                    
005000        05 RESP-ADGANG       PIC 9(2).                                    
005100*                                 GÅNG                                    
005200*                                 AISLE                                   
005300        05 RESP-ADPLATS      PIC 9(5).                                    
005400*                                 LAGERPLATSNUMMER                        
005500*                                 LOCATION                                
005600        05 RESP-IDMSG-ERROR-LINE                                          
005700                             PIC X(3).                                    
005800*                                 FELMEDDELANDE ID                        
005900*                                 ERROR MESSAGE ID                        
006000*** END OF VILMAII-COPY LENGTH= 468 BYTES                                 
