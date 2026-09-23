000100 01  W221L461.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22146 MOT DATABASER                    
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ALLMAN-INFO     VALUE +461.                                  
000700*                                 ANROPSTYP     KDCALL-W221               
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 TIAAVV-AKT           PIC S9(5)           COMP-3.                  
001500*                                                                         
001600     03 IOAREA.                                                           
001700*                                                                         
001800        05 IDANSK            PIC S9(3)           COMP-3.                  
001900*                                 ANSKAFFARNUMMER                         
002000        05 IDLEVNR           PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200        05 BEART-GB          PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400        05 BEART-SVE         PIC X(25).                                   
002500*                                 ARTIKELBENÄMNING                        
002600        05 BELEV             PIC X(30).                                   
002700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002800        05 TEARTNOT          OCCURS 2 TIMES                               
002900                             PIC X(40).                                   
003000*                                 ARTIKEL NOTERING                        
003100        05 CLAGER            OCCURS 2 TIMES.                              
003200*                                                                         
003300           07 KVLS           PIC S9(7)           COMP-3.                  
003400*                                 LAGERSALDO                              
003500           07 KVRESS         PIC S9(7)           COMP-3.                  
003600*                                 RESERVERAT ANTAL ARTIKLAR               
003700           07 KVAKS          PIC S9(7)           COMP-3.                  
003800*                                 ANKOMSTSALDO                            
003900           07 KVROS          PIC S9(7)           COMP-3.                  
004000*                                 RESTORDERSALDO                          
004100           07 KDERS          PIC S9(3)           COMP-3.                  
004200*                                 ERSÄTTNINGSKOD                          
004300*** END OF VILMAII-COPY LENGTH= 214 BYTES                                 
