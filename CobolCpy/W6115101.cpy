000100 01  W61151.                                                              
000200*                                 GENOMLOPPSTID PER UPPFÖLJ-              
000300*                                 NINGSSTATUS                             
000400*                                                               .         
000500*                                 PROCESS TIME  PER FOLLOW-UP             
000600*                                 STATUS                                  
000700*                                                               .         
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000*                                 RECORD TYPE                             
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 KDINLUPF             PIC X(4).                                    
001500*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
001600*                                 FOLLOW-UP STATUS RECEIVING              
001700     03 KVRADER              PIC S9(5)           COMP-3.                  
001800*                                 ANTAL RADER                             
001900*                                 NUMBER OF LINES                         
002000     03 KVRADER-PRIO         PIC S9(5)           COMP-3.                  
002100*                                 ANTAL RADER                             
002200*                                 NUMBER OF LINES                         
002300     03 KVART                PIC S9(7)           COMP-3.                  
002400*                                 ANTAL ARTNR PER BRYTBEGREPP             
002500*                                 NO OF PARTNOS PER TYPE                  
002600     03 SUBEL                PIC S9(9)V9(2).                              
002700*                                 SUMMABELOPP                             
002800*                                 SUM AMOUNT                              
002900     03 SUBEL-PRIO           PIC S9(9)V9(2).                              
003000*                                 SUMMABELOPP                             
003100*                                 SUM AMOUNT                              
003200     03 TIGLT                PIC S9(3)V9(2)      COMP-3.                  
003300*                                 UTFÖRD ARBETSTID (TIMMAR)               
003400*                                 PERFORMED LABOUR TIME (HOURS)           
003500     03 TIGLT-PRIO           PIC S9(3)V9(2)      COMP-3.                  
003600*                                 UTFÖRD ARBETSTID (TIMMAR)               
003700*                                 PERFORMED LABOUR TIME (HOURS)           
003800     03 KVRADER-MAAL         PIC S9(5)           COMP-3.                  
003900*                                 ANTAL RADER                             
004000*                                 NUMBER OF LINES                         
004100     03 KVRADER-MAAL-PRIO    PIC S9(5)           COMP-3.                  
004200*                                 ANTAL RADER                             
004300*                                 NUMBER OF LINES                         
004400*** END OF VILMAII-COPY LENGTH= 53 BYTES                                  
