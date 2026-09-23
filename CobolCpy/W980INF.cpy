000100 01  W980INF.                                                             
000200*                                 DATAPARAMETER TILL ATTRIBUT INF         
000300     03 INFO-LENGD           PIC S9(4)           COMP                     
000400                             VALUE ZEROS.                                 
000500*                                 FÄLTLÄNGD                               
000600     03 INF-VAERDE.                                                       
000700        05 KDPROCSTAT        PIC X                                        
000800                             VALUE SPACE.                                 
000900*                                 PROCESS-STATUS                          
001000        05 TIAPDAT           PIC S9(7)           COMP-3                   
001100                             VALUE ZEROS.                                 
001200*                                 AKTIVERINGS- PASSIVERINGS-DATUM         
001300*                                 (ÅÅMMDD)                                
001400        05 TIEXDAT           PIC S9(7)           COMP-3                   
001500                             VALUE ZEROS.                                 
001600*                                 EXEKVERINGSDATUM     (ÅÅMMDD)           
001700        05 TIMINUT-START     PIC S9(5)           COMP-3                   
001800                             VALUE ZEROS.                                 
001900*                                 STARTTID    (HH.MM)                     
002000        05 TIMINUT-STOPP     PIC S9(5)           COMP-3                   
002100                             VALUE ZEROS.                                 
002200*                                 STOPPTID  (HH.MM)                       
002300        05 TIEXEC-SENAST     PIC S9(5)           COMP-3                   
002400                             VALUE ZEROS.                                 
002500*                                 KÖRNINGSTID (MINUTER)                   
002600        05 TIEXEC-MEDEL      PIC S9(5)           COMP-3                   
002700                             VALUE ZEROS.                                 
002800*                                 KÖRNINGSTID (MINUTER)                   
002900        05 KDANR             PIC S9(4)           COMP                     
003000                             VALUE ZEROS.                                 
003100*                                 AKTIVERINGSNUMMER                       
003200*** END COPY W980INFCC0  LENGTH=25                                        
