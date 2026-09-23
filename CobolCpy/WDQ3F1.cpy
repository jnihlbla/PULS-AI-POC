000100 01  SEQF-WDQ3F1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDƒRT INDEX TILL WDQ301             
000400*                                 PLANERING PRODUKTIONSKANAL              
000500*                                 FYSISK NYCKEL: WDQ3F1KY                 
000600*                                 (IDDC,    DARFSDAT, IDPRC,              
000700*                                  IDORDER, IDPRODNR, IDPLKLST)           
000800*                                 SECONDARY NYCKEL: WDQ3FSEQ              
000900*                                 (IDDC,    DARFSDAT, IDPRC)              
001000     03 SEQF-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQF-DARFSDAT        PIC 9(8).                                    
001400*                                 KLART F÷R TRANSPORT ≈≈≈≈MMDD            
001500*                                 READY FOR SHIPMENT  YYYYMMDD            
001600     03 SEQF-IDPRC.                                                       
001700*                                 PRODUKTIONSKANAL                        
001800*                                 PRODUCTION CHANNEL                      
001900        05 SEQF-IDPRCBAS     PIC X(3).                                    
002000*                                 PRC-BAS                                 
002100*                                 PRC-BASIC                               
002200        05 SEQF-IDPRCVAR     PIC X.                                       
002300*                                 PRC-VARIANT                             
002400*                                 PRC-VARIANT                             
002500     03 SEQF-IDORDER         PIC S9(7)           COMP-3.                  
002600*                                 VOLVO PARTS ORDERNUMMER                 
002700*                                 VOLVO PARTS ORDER NUMBER                
002800     03 SEQF-IDPRODNR        PIC S9(7)           COMP-3.                  
002900*                                 PRODUKTIONSNUMMER                       
003000*                                 PRODUCTION NUMBER                       
003100     03 SEQF-IDPLKLST        PIC S9(3)           COMP-3.                  
003200*                                 PLOCKLISTNUMMER                         
003300*                                 PICKING LIST NUMBER                     
003400     03 SEQF-KDODELSTA       PIC X.                                       
003500*                                 ORDERDELSTATUS                          
003600*                                 ORDER PART STATUS                       
003700     03 SEQF-KVRADER         PIC S9(5)           COMP-3.                  
003800*                                 ANTAL RADER                             
003900*                                 NUMBER OF LINES                         
004000     03 SEQF-IDWDQ301        PIC X(12).                                   
004100*                                 NYCKEL TILL WDQ301                      
004200*                                 KEY TO WDQ301                           
004300*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
