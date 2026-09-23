000100 01  SEQK-WDQ3K1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 PLANERING PROD.KANAL(STA = R)           
000500*                                 FYSISK NYCKEL: WDQ3K1KY                 
000600*                                 (IDDC,    DARFS, IDPRC,                 
000700*                                  IDPRODNR, IDPLKLST, IDORDER)           
000800*                                 SECONDARY NYCKEL: WDQ3KSEQ              
000900*                                 (IDDC,    DARFS, IDPRC)                 
001000     03 SEQK-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQK-DARFS           PIC 9(12).                                   
001400*                                 KLART FÖR TRANSPORT                     
001500*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
001600     03 SEQK-IDPRC.                                                       
001700*                                 PRODUKTIONSKANAL                        
001800*                                 PRODUCTION CHANNEL                      
001900        05 SEQK-IDPRCBAS     PIC X(3).                                    
002000*                                 PRC-BAS                                 
002100*                                 PRC-BASIC                               
002200        05 SEQK-IDPRCVAR     PIC X.                                       
002300*                                 PRC-VARIANT                             
002400*                                 PRC-VARIANT                             
002500     03 SEQK-IDPRODNR        PIC S9(7)           COMP-3.                  
002600*                                 PRODUKTIONSNUMMER                       
002700*                                 PRODUCTION NUMBER                       
002800     03 SEQK-IDPLKLST        PIC S9(3)           COMP-3.                  
002900*                                 PLOCKLISTNUMMER                         
003000*                                 PICKING LIST NUMBER                     
003100     03 SEQK-IDORDER         PIC S9(7)           COMP-3.                  
003200*                                 VOLVO PARTS ORDERNUMMER                 
003300*                                 VOLVO PARTS ORDER NUMBER                
003400     03 SEQK-KVRADER         PIC S9(5)           COMP-3.                  
003500*                                 ANTAL RADER                             
003600*                                 NUMBER OF LINES                         
003700     03 SEQK-IDTRP.                                                       
003800*                                 TRANSPORTIDENTITET                      
003900*                                 TRANSPORTIDENTITY                       
004000        05 SEQK-IDTRPLOS     PIC X(3).                                    
004100*                                 TRANSPORTLÖSNING                        
004200*                                 TRANSPORTSOLUTION                       
004300        05 SEQK-IDTRPVAR     PIC X(2).                                    
004400*                                 TRANSPORTLÖSNINGSGRUPP                  
004500*                                 TRANSPORTSOLUTIONGROUP                  
004600     03 SEQK-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
004700*                                 ORDERVIKT NETTO (KG)                    
004800*                                 WEIGHT PER ORDER NETTO (KG)             
004900     03 SEQK-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
005000*                                 ORDERVOLYM NETTO (M3)                   
005100*                                 NET VOLUME PER ORDER (M3)               
005200     03 SEQK-IDWDQ301        PIC X(12).                                   
005300*                                 NYCKEL TILL WDQ301                      
005400*                                 KEY TO WDQ301                           
005500*** END OF VILMAII-COPY LENGTH= 56 BYTES                                  
