000100 01  W222L226.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22222 MOT ART.REG FÖR SDC              
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-SDCINFO        VALUE +601.                                  
000700*                                            KDCALL-W222-W228-002         
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IOAREA.                                                           
001500*                                                                         
001600        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
001700*                                 DEL AV AK PÅ VÄG                        
001800        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
001900*                                 DEL AV AK SOM LIGGER I SDC              
002000        05 KVBEART           PIC S9(7)           COMP-3.                  
002100*                                 BESTÄLLT ANTAL STYCKEN                  
002200        05 KVLS              PIC S9(7)           COMP-3.                  
002300*                                 LAGERSALDO                              
002400        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
002500*                                 ORDERKÖSALDO, KLASS 1                   
002600        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
002700*                                 ORDERKÖSALDO, KLASS 2-4                 
002800        05 KVREFBER          PIC S9(7)           COMP-3.                  
002900*                                 BERÄKNAD REFILLINGKVANTITET             
003000        05 KVREFPKT          PIC S9(7)           COMP-3.                  
003100*                                 BERÄKNAD PÅFYLLNADSPUNKT                
003200        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
003300*                                 PERIODBEHOV REFILLING                   
003400*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
