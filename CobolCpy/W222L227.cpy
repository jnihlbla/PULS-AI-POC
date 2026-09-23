000100 01  W222L227.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22222 MOT ART.REG FÖR NDC              
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-NDCINFO        VALUE +701.                                  
000700*                                            KDCALL-W222-W228-002         
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IOAREA.                                                           
001500*                                                                         
001600        05 IDDC              PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800        05 FLWILSON          PIC X.                                       
001900*                                 WILSONFORMEL                            
002000        05 IDREFTAB          PIC X.                                       
002100*                                 IDENTITET REFILLTABELL                  
002200        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
002300*                                 DEL AV AK PÅ VÄG                        
002400        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
002500*                                 DEL AV AK SOM LIGGER I SDC              
002600        05 KVBEART           PIC S9(7)           COMP-3.                  
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800        05 KVLS              PIC S9(7)           COMP-3.                  
002900*                                 LAGERSALDO                              
003000        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
003100*                                 ORDERKÖSALDO, KLASS 1                   
003200        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
003300*                                 ORDERKÖSALDO, KLASS 2-4                 
003400        05 KVREFBER          PIC S9(7)           COMP-3.                  
003500*                                 BERÄKNAD REFILLINGKVANTITET             
003600        05 KVREFPKT          PIC S9(7)           COMP-3.                  
003700*                                 BERÄKNAD PÅFYLLNADSPUNKT                
003800        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
003900*                                 PERIODBEHOV REFILLING                   
004000        05 RESEASON          OCCURS 12 TIMES                              
004100                             PIC S9V9(2)         COMP-3.                  
004200*                                 SÄSONGSINDEX                            
004300*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
