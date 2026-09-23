000100 01  4492-WDGX4492.                                                       
000200*                                 TRANSPORTINFO. BOLLA                    
000300*                                 LÖPNUMMERSERIER                         
000400*                                 OCH FASTA VÄRDEN                        
000500*                                 FYSISK NYCKEL: TIAA                     
000600     03 4492-TIAA            PIC S9(3)           COMP-3.                  
000700*                                 ÅR    (ÅÅ)                              
000800*                                 YEAR  (YY)                              
000900     03 4492-IDTRPBON        PIC S9(7)           COMP-3.                  
001000*                                 BOLLA-DOKUMENT NUMMER                   
001100*                                 TRANSPORT BOLLA DOCUMENT NO.            
001200     03 4492-IDTRPBOR        PIC S9(5)           COMP-3.                  
001300*                                 BOLLA-DOKUMENT REFERENSNUMMER           
001400*                                 BOLLA DOCUMENT REFERENCE NO.            
001500     03 4492-TRPFIR          OCCURS 10 TIMES.                             
001600*                                 TRANSPORTÖR UPPGIGTER                   
001700        05 4492-IDTRPTNR     PIC S9(3)           COMP-3.                  
001800*                                 TRANSPORTIDENTITET                      
001900*                                 TRANSPORT IDENTITY                      
002000        05 4492-KDFRAKT      PIC S9(3)           COMP-3.                  
002100*                                 FRAKTSÄTT DC TILL KUND                  
002200*                                 FREIGHT CODE                            
002300        05 4492-BETRPFIR     PIC X(45).                                   
002400*                                 TRANSPORTFIRMANS NAMN                   
002500*                                 NAME OF THE TRANSPORTCOMPANY            
002600        05 4492-ADTRPFIR.                                                 
002700*                                 TRANSPORTFIRMA ADRESS                   
002800*                                 TRANSPORT COMPANY ADDRESS               
002900           07 4492-ADTRPFIR-RAD1                                          
003000                             PIC X(35).                                   
003100*                                 TRANSPORTFIRMA ADRESSRAD-1              
003200*                                 ADDRESS OF TRANSPORTCOMPANY             
003300           07 4492-ADTRPFIR-RAD2                                          
003400                             PIC X(35).                                   
003500*                                 TRANSPORTFIRMA ADRESSRAD-2              
003600*                                 ADDRESS OF TRANSPORTCOMPANY             
003700*** END OF VILMAII-COPY LENGTH= 1199 BYTES                                
