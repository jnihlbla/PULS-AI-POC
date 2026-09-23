000100 01  SEQD-WDQ3D1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDQ301             
000400*                                 PRODNR-PLKLST                           
000500*                                 FYSISK NYCKEL: WDQ3D1KY                 
000600*                                 (IDPRODNR, IDPLKLST,                    
000700*                                  IDORDER,  IDDC)                        
000800*                                 SECONDARY NYCKEL: WDQ3DSEQ              
000900*                                 (IDPRODNR, IDPLKLST)                    
001000     03 SEQD-IDPRODNR        PIC S9(7)           COMP-3.                  
001100*                                 PRODUKTIONSNUMMER                       
001200*                                 PRODUCTION-NUMBER                       
001300     03 SEQD-IDPLKLST        PIC S9(3)           COMP-3.                  
001400*                                 PLOCKLISTNUMMER                         
001500*                                 PICKING LIST NUMBER                     
001600     03 SEQD-IDORDER         PIC S9(7)           COMP-3.                  
001700*                                 VOLVO PARTS ORDERNUMMER                 
001800*                                 VOLVO PARTS ORDER NUMBER                
001900     03 SEQD-IDDC            PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 SEQD-IDWDQ301        PIC X(12).                                   
002300*                                 NYCKEL TILL WDQ301                      
002400*                                 KEY TO WDQ301                           
002500*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
