000100 01  DP-HEAD-W50292.                                                      
000200*                                 PRINTDOCUMENT DATA HEADER               
000300     03 DP-HEAD-IDAFPRCD     PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 DP-HEAD-IDDC         PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 DP-HEAD-TIDATETIME   PIC X(14).                                   
001000*                                 DATUM OCH TID YYYYMMDDHHMMSS            
001100*                                 DATE AND TIME YYYYMMDDHHMMSS            
001200     03 DP-HEAD-ADLAGOMR-FOM PIC Z9.                                      
001300*                                 LAGEROMRÅDE FRÅN OCH MED                
001400*                                 AREA ADDRESS FROM                       
001500     03 DP-HEAD-ADLAGOMR-TOM PIC Z9.                                      
001600*                                 LAGEROMRÅDE TILL OCH MED                
001700*                                 AREA ADDRESS TO                         
001800     03 DP-HEAD-ADGANG-FOM   PIC Z9.                                      
001900*                                 GÅNG FRÅN OCH MED                       
002000*                                 AISLE ADDRESS FROM                      
002100     03 DP-HEAD-ADGANG-TOM   PIC Z9.                                      
002200*                                 GÅNG TILL OCH MED                       
002300*                                 AISLE ADDRESS TO                        
002400     03 DP-HEAD-ADPLATS      PIC Z(4)9.                                   
002500*                                 LAGERPLATSNUMMER                        
002600*                                 LOCATION                                
002700     03 DP-HEAD-IDCOUNTER    PIC X(20).                                   
002800*                                 RÄKNARE/INVENTERARE                     
002900*                                 COUNTER'S NAME IN INVENTORY             
003000     03 DP-HEAD-IDACSNR      PIC 9(6).                                    
003100*                                 NR.SERIE FÖR ACS-LISTOR                 
003200*                                 SERIAL NO. FOR ACS REPORTS              
003300*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
