000100 01  W218LETA.                                                            
000200*                                 LÄNKAREA TILL  MODUL  ESTIMATED         
000300*                                 -TIME-OF-ARRIVAL W218ETA                
000400     03 KDCALL               PIC 9(3).                                    
000500      88 ORDER-AIR           VALUE 601.                                   
000600      88 ORDER-BOAT          VALUE 602.                                   
000700      88 INVOICING-AIR       VALUE 603.                                   
000800      88 INVOICING-BOAT      VALUE 604.                                   
000900      88 GOODS-REC-C         VALUE 605.                                   
001000      88 GOODS-REC-A         VALUE 606.                                   
001100      88 GOODS-REC-I         VALUE 607.                                   
001200      88 GOODS-REC-R         VALUE 608.                                   
001300      88 BACKORDER-AIR       VALUE 609.                                   
001400      88 BACKORDER-BOAT      VALUE 610.                                   
001500      88 ORDER-EXT-SUPPL     VALUE 611.                                   
001600      88 ORDER-CONFIRM       VALUE 612.                                   
001700      88 GOODS-REC-R-AIR     VALUE 613.                                   
001800*                                 ANROPSTYP     KDCALL-ETA                
001900     03 IDDC-SEND            PIC X(2).                                    
002000*                                 SÄNDANDE LAGER                          
002100     03 IDDC-REC             PIC X(2).                                    
002200*                                 MOTTAGANDE LAGER                        
002300     03 IDARTNR              PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500     03 IDLEVNR              PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700     03 TISEKEL-ANROP        PIC 9(2).                                    
002800*                                 SEKEL I ÅRTALET                         
002900     03 TIAAMMDD-ANROP       PIC S9(7)           COMP-3.                  
003000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003100     03 TISEKEL-SVAR         PIC 9(2).                                    
003200*                                 SEKEL I ÅRTALET                         
003300     03 TIAAMMDD-SVAR        PIC S9(7)           COMP-3.                  
003400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003500     03 SVAR-OK              PIC X.                                       
003600*                                 ALLMÄN SVARSFLAGGA                      
003700     03 KVAVIS-ETA           PIC S9(7)           COMP-3.                  
003800*                                 AVISERAT ANTAL FÖR ETABERÄKNING         
003900     03 KDFRAKT              PIC S9(3)           COMP-3.                  
004000*                                 FRAKTSÄTT DC TILL KUND                  
004100*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
