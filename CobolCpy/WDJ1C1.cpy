000100 01  SEQC-WDJ1C1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDJ111             
000300*                                 SATSSTRUKTUWREGISTER                    
000400*                                 FYSISK NYCKEL: WDJ1C1KY                 
000500*                                 (IDLEVNR + BELEVART + IDARTNR +         
000600*                                  IDARTNR-STR + KDSTRRAD +               
000700*                                  IDRADNR)                               
000800*                                 SEKUNDÄR NYCKEL: WDJ1CSEQ               
000900*                                 (IDLEVNR + BELEVART + IDARTNR)          
001000     03 SEQC-IDLEVNR         PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001300     03 SEQC-BELEVART        PIC X(30).                                   
001400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001500*                                 SUPPLIERS PART DESCRIPTION              
001600     03 SEQC-IDARTNR         PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900     03 SEQC-IDARTNR-STR     PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 SEQC-KDSTRRAD        PIC X.                                       
002300*                                 TYP AV STRUKTURRAD                      
002400*                                 TYPE OF LINE IN A STRUCTURE             
002500     03 SEQC-IDRADNR         PIC S9(5)           COMP-3.                  
002600*                                 RADNUMMER                               
002700*                                 LINE NO                                 
002800*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
