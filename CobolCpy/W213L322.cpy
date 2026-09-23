000100 01  W213L322.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W21332 MOT LEVERANTÖRSREGISTER          
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-LEVERANTORSDATA                                             
000700                             VALUE +201.                                  
000800*                                 ANROPSTYP FÖR SYSTEM R2XX               
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000      88 ANROP-OK            VALUE 'J'.                                   
001100      88 ANROP-FEL           VALUE 'N'.                                   
001200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 IDLEVNR-SHIP         PIC X(5).                                    
001600*                                 SKEPPANDE LEVERANTÖR                    
001700     03 IOAREA-LEVERANTOR.                                                
001800*                                                                         
001900        05 KDLEVTYP          PIC S9              COMP-3.                  
002000*                                 LEVERANTÖRTYP                           
002100        05 KDGK              PIC S9              COMP-3.                  
002200*                                 GODSMOTTAGAREKOD                        
002300        05 KVDAGAR-TTC1      PIC S9(3)           COMP-3.                  
002400*                                 DAGAR TULL- OCH TRANSPORT-TID           
002500*                                 C1                                      
002600        05 KVDAGAR-TTC2      PIC S9(3)           COMP-3.                  
002700*                                 DAGAR TULL- & TRANSPORT-TID  C2         
002800        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
002900*                                 ANTAL VECKOR LEDTID                     
003000        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
003100*                                 ANTAL VECKOR ANSKAFFNINGSTID            
003200        05 PG-TABELL.                                                     
003300           07 IDANSK-PG      OCCURS 8 TIMES                               
003400                             PIC S9(3)           COMP-3.                  
003500*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
003600     03 IDLEVNR-MOTSV        PIC X(5).                                    
003700*                                 MOTSVARANDE LEVERANTÖRSID               
003800*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
