000100 01  W22524.                                                              
000200*                                 LISTPOSTER FÖR                          
000300*                                 RESTORDER - RADER                       
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDLEVNR              PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 IDANSK               PIC S9(3)           COMP-3.                  
000900*                                 ANSKAFFARNUMMER                         
001000     03 SUROBEL              PIC S9(7)V9(2)      COMP-3.                  
001100*                                 RESTORDERVÄRDE STANDARDPRIS             
001200     03 KVRORAD              PIC S9(7)V9(2)      COMP-3.                  
001300*                                 RESTNOTERADE RADER  KVRORAD-003         
001400     03 TIRODAT-ORDER        PIC S9(5)           COMP-3.                  
001500*                                               TIRODAT-ORDER-002         
001600*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
001700     03 KVROS                PIC S9(7)           COMP-3.                  
001800*                                 RESTORDERSALDO                          
001900     03 SUAKBEL              PIC S9(7)V9(2)      COMP-3.                  
002000*                                 AK-VÄRDE                                
002100     03 KVAKS-CDC            PIC S9(7)           COMP-3.                  
002200*                                 DEL AV AK SOM LIGGER I CDC              
002300     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
002400*                                 DEL AV AK PÅ VÄG                        
002500     03 KVAKS-T              PIC S9(7)           COMP-3.                  
002600*                                 DEL AV AK I EN TERMINAL                 
002700     03 KVAVIS-LEVBESK-1     PIC S9(7)           COMP-3.                  
002800*                                 AVISERAT ANTAL                          
002900     03 TIAVIDAT-LEVBESK-1   PIC S9(5)           COMP-3.                  
003000*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
003100     03 KVAVIS-LEVBESK-2     PIC S9(7)           COMP-3.                  
003200*                                 AVISERAT ANTAL                          
003300     03 TIAVIDAT-LEVBESK-2   PIC S9(5)           COMP-3.                  
003400*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
003500     03 KVAVIS-LEVBESK-3     PIC S9(7)           COMP-3.                  
003600*                                 AVISERAT ANTAL                          
003700     03 TIAVIDAT-LEVBESK-3   PIC S9(5)           COMP-3.                  
003800*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
003900*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
