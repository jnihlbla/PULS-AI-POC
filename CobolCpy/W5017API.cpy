000100 01  API-W5017API.                                                        
000200*                                 LINK AREA FOR W5017API                  
000300     03 API-INPUT-DATA.                                                   
000400        05 API-IDARTNR-IN    PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 API-OUTPUT-DATA.                                                  
000700        05 API-IDARTNR-OUT   PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900        05 API-PRARTSTD-OUT  PIC S9(7)V9(2)      COMP-3.                  
001000*                                 ARTIKELSTANDARDPRIS                     
001100        05 API-KDVALISO-OUT  PIC X(3).                                    
001200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001300     03 API-KDSVAR           PIC X.                                       
001400      88 API-KDSVAR-OK       VALUE ' '.                                   
001500      88 API-KDSVAR-FEL      VALUE 'F'.                                   
001600*                                                       KDSVAR-88         
001700*                                 SVARSKOD FRÅN SUBPROGRAM                
001800     03 API-IDMSG-ERROR      PIC X(3).                                    
001900*                                 FELMEDDELANDE ID                        
002000     03 API-IDELMT-ERROR     PIC X(16).                                   
002100*                                 DATAELEMENTIDENTITET                    
002200     03 API-FEL-TEXT         PIC X(25).                                   
002300*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
