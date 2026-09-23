000100 01  SEQA-WDJ2A1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDJ201             
000300*                                 SATSORDERREGISTER                       
000400*                                 EXIT: INDEX FINNS NÄR                   
000500*                                 FLBYGGB   = J     OCH                   
000600*                                 KDSATSTA  = R                           
000700*                                 FYSISK NYCKEL: WDJ2A1KY                 
000800*                                 KDCLAGER, IDPRC, KVRORAD-9KOMPL         
000900*                                 RELSKVOT-L, FLSATPRI,                   
001000*                                 RELSKVOT-S, IDARTNR, DAREGDAT           
001100*                                 IDORDNST                                
001200*                                 SEKUNDÄR NYCKEL: WDJ2ASEQ               
001300*                                 KDCLAGER, IDPRC, KVRORAD-9KOMPL         
001400*                                 RELSKVOT-L, FLSATPRI,                   
001500*                                 RELSKVOT-S, IDARTNR, DAREGDAT           
001600     03 SEQA-KDCLAGER        PIC S9              COMP-3.                  
001700*                                 CENTRALLAGERKOD                         
001800*                                 CENTRAL WAREHOUSE CODE                  
001900     03 SEQA-IDPRC.                                                       
002000*                                 PRODUKTIONSKANAL                        
002100*                                 PRODUCTION CHANNEL                      
002200        05 SEQA-IDPRCBAS     PIC X(3).                                    
002300*                                 PRC-BAS                                 
002400*                                 PRC-BASIC                               
002500        05 SEQA-IDPRCVAR     PIC X.                                       
002600*                                 PRC-VARIANT                             
002700*                                 PRC-VARIANT                             
002800     03 SEQA-KVRORAD-9KOMPL  PIC S9(5)           COMP-3.                  
002900*                                 ANTAL RESTORDER-RADER 9-KOMPL           
003000*                                 NBR OF BACK ORDER ITEMS 9-COMPL         
003100     03 SEQA-RELSKVOT-L      PIC S9(3)V9(2)      COMP-3.                  
003200*                                 LAGERSALDO/PERIODBEH LITEN SATS         
003300*                                 STOCK BALANCE/PERIOD RQ SM KIT          
003400     03 SEQA-FLSATPRI        PIC X.                                       
003500*                                 MANUELL PRIORITERING AV SATS            
003600*                                 MANUAL PRIORITY OF KIT-ORDER            
003700     03 SEQA-RELSKVOT-S      PIC S9(3)V9(2)      COMP-3.                  
003800*                                 LAGERSALDO/PERIODBEH STOR  SATS         
003900*                                 STOCK BALANCE/PERIOD RQ GT KIT          
004000     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
004100*                                 ARTIKELNUMMER                           
004200*                                 PART NUMBER                             
004300     03 SEQA-DAREGDAT        PIC 9(8).                                    
004400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004500*                                 REGISTRATION DATE (YYYYMMDD)            
004600     03 SEQA-IDORDNST.                                                    
004700*                                 SATSORDERNUMMER-TOTALT                  
004800*                                 KIT-ORDER-NUMBER-TOTAL                  
004900        05 SEQA-IDORDNSB     PIC S9(5)           COMP-3.                  
005000*                                 SATSORDERNUMMER-BAS                     
005100*                                 KIT-ORDER-NUMBER-BASIC                  
005200        05 SEQA-IDORDNSS     PIC S9              COMP-3.                  
005300*                                 SATSORDERNUMMER-SUFFIX                  
005400*                                 KIT-ORDER-NUMBER-SUFFIX                 
005500*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
