000100 01  SEQD-WDJ2D1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDJ201             
000300*                                 SATSORDERREGISTER                       
000400*                                 FYSISK NYCKEL: WDJ2D1KY                 
000500*                                 (KDCLAGER,   KVRORAD-9KOMPL,            
000600*                                  RELSKVOT-L, KDSATPRI,                  
000700*                                  RELSKVOT-S, IDARTNR,                   
000800*                                  DAREGDAT,   IDORDNST)                  
000900*                                 SEKUNDÄR NYCKEL: WDJ2DSEQ               
001000*                                 (KDCLAGER,   KVRORAD-9KOMPL,            
001100*                                  RELSKVOT-L, KDSATPRI,                  
001200*                                  RELSKVOT-S, IDARTNR,                   
001300*                                  DAREGDAT)                              
001400     03 SEQD-KDCLAGER        PIC S9              COMP-3.                  
001500*                                 CENTRALLAGERKOD                         
001600*                                 CENTRAL WAREHOUSE CODE                  
001700     03 SEQD-KVRORAD-9KOMPL  PIC S9(5)           COMP-3.                  
001800*                                 ANTAL RESTORDER-RADER 9-KOMPL           
001900*                                 NBR OF BACK ORDER ITEMS 9-COMPL         
002000     03 SEQD-RELSKVOT-L      PIC S9(3)V9(2)      COMP-3.                  
002100*                                 LAGERSALDO/PERIODBEH LITEN SATS         
002200*                                 STOCK BALANCE/PERIOD RQ SM KIT          
002300     03 SEQD-FLSATPRI        PIC X.                                       
002400*                                 MANUELL PRIORITERING AV SATS            
002500*                                 MANUAL PRIORITY OF KIT-ORDER            
002600     03 SEQD-RELSKVOT-S      PIC S9(3)V9(2)      COMP-3.                  
002700*                                 LAGERSALDO/PERIODBEH STOR  SATS         
002800*                                 STOCK BALANCE/PERIOD RQ GT KIT          
002900     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
003000*                                 ARTIKELNUMMER                           
003100*                                 PART NUMBER                             
003200     03 SEQD-DAREGDAT        PIC 9(8).                                    
003300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003400*                                 REGISTRATION DATE (YYYYMMDD)            
003500     03 SEQD-IDORDNST.                                                    
003600*                                 SATSORDERNUMMER-TOTALT                  
003700*                                 KIT-ORDER-NUMBER-TOTAL                  
003800        05 SEQD-IDORDNSB     PIC S9(5)           COMP-3.                  
003900*                                 SATSORDERNUMMER-BAS                     
004000*                                 KIT-ORDER-NUMBER-BASIC                  
004100        05 SEQD-IDORDNSS     PIC S9              COMP-3.                  
004200*                                 SATSORDERNUMMER-SUFFIX                  
004300*                                 KIT-ORDER-NUMBER-SUFFIX                 
004400*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
