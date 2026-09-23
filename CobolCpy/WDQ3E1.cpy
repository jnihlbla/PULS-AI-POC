000100 01  SEQE-WDQ3E1.                                                         
000200*                                 ORDERDELSREGISTER                       
000300*                                 SEKUNDƒRT INDEX TILL WDQ301             
000400*                                 TRANSPORT-PLANERING                     
000500*                                 FYSISK NYCKEL: WDQ3E1KY                 
000600*                                 (IDDC,    DARFSDAT, IDTRP,              
000700*                                  IDORDER, IDPRODNR, IDPLKLST)           
000800*                                 SECONDARY NYCKEL: WDQ3ESEQ              
000900*                                 (IDDC,    DARFSDAT, IDTRP)              
001000     03 SEQE-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQE-DARFSDAT        PIC 9(8).                                    
001400*                                 KLART F÷R TRANSPORT ≈≈≈≈MMDD            
001500*                                 READY FOR SHIPMENT  YYYYMMDD            
001600     03 SEQE-IDTRP.                                                       
001700*                                 TRANSPORTIDENTITET                      
001800*                                 TRANSPORTIDENTITY                       
001900        05 SEQE-IDTRPLOS     PIC X(3).                                    
002000*                                 TRANSPORTL÷SNING                        
002100*                                 TRANSPORTSOLUTION                       
002200        05 SEQE-IDTRPVAR     PIC X(2).                                    
002300*                                 TRANSPORTL÷SNINGSGRUPP                  
002400*                                 TRANSPORTSOLUTIONGROUP                  
002500     03 SEQE-IDORDER         PIC S9(7)           COMP-3.                  
002600*                                 VOLVO PARTS ORDERNUMMER                 
002700*                                 VOLVO PARTS ORDER NUMBER                
002800     03 SEQE-IDPRODNR        PIC S9(7)           COMP-3.                  
002900*                                 PRODUKTIONSNUMMER                       
003000*                                 PRODUCTION NUMBER                       
003100     03 SEQE-IDPLKLST        PIC S9(3)           COMP-3.                  
003200*                                 PLOCKLISTNUMMER                         
003300*                                 PICKING LIST NUMBER                     
003400     03 SEQE-KDODELSTA       PIC X.                                       
003500*                                 ORDERDELSTATUS                          
003600*                                 ORDER PART STATUS                       
003700     03 SEQE-KVRADER         PIC S9(5)           COMP-3.                  
003800*                                 ANTAL RADER                             
003900*                                 NUMBER OF LINES                         
004000     03 SEQE-IDWDQ301        PIC X(12).                                   
004100*                                 NYCKEL TILL WDQ301                      
004200*                                 KEY TO WDQ301                           
004300*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
