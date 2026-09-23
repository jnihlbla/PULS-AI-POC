000100 01  WXTRF1.                                                              
000200*                                 BELÄGGNING OCH GENOMLOPPSTID            
000300*                                 PER UPPFÖLJNINGSSTATUS                  
000400*                                                               .         
000500*                                 WORK FLOW TIME  AND                     
000600*                                 WORK LOAD                               
000700*                                 PER FOLLOW-UP STATUS.                   
000800*                                                               .         
000900     03 IDDC                 PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 TIAAVV               PIC S9(5)           COMP-3.                  
001300*                                 ÅR - VECKA  (ÅÅVV)                      
001400*                                 YEAR - WEEK  (YYWW)                     
001500     03 KDINLUPF             PIC X(4).                                    
001600*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
001700*                                 FOLLOW-UP STATUS RECEIVING              
001800     03 KVART                PIC S9(7)           COMP-3.                  
001900*                                 ANTAL ARTNR PER BRYTBEGREPP             
002000*                                 NO OF PARTNOS PER TYPE                  
002100     03 KVKOLLI              PIC S9(5)           COMP-3.                  
002200*                                 ANTAL KOLLI                             
002300*                                 NBR OF CASES                            
002400     03 KVKOLLI-PRIO         PIC S9(5)           COMP-3.                  
002500*                                 ANTAL KOLLI                             
002600*                                 NBR OF CASES                            
002700     03 SUBEL                PIC S9(9)V9(2).                              
002800*                                 SUMMABELOPP                             
002900*                                 SUM AMOUNT                              
003000     03 SUBEL-PRIO           PIC S9(9)V9(2).                              
003100*                                 SUMMABELOPP                             
003200*                                 SUM AMOUNT                              
003300     03 KVRADER-GLT          PIC S9(5)           COMP-3.                  
003400*                                 ANTAL RADER                             
003500*                                 NUMBER OF LINES                         
003600     03 KVRADER-PRIO-GLT     PIC S9(5)           COMP-3.                  
003700*                                 ANTAL RADER                             
003800*                                 NUMBER OF LINES                         
003900     03 SUBEL-GLT            PIC S9(9)V9(2).                              
004000*                                 SUMMABELOPP                             
004100*                                 SUM AMOUNT                              
004200     03 SUBEL-PRIO-GLT       PIC S9(9)V9(2).                              
004300*                                 SUMMABELOPP                             
004400*                                 SUM AMOUNT                              
004500     03 TIGLT                PIC S9(3)V9(2)      COMP-3.                  
004600*                                 UTFÖRD ARBETSTID (TIMMAR)               
004700*                                 PERFORMED LABOUR TIME (HOURS)           
004800     03 TIGLT-PRIO           PIC S9(3)V9(2)      COMP-3.                  
004900*                                 UTFÖRD ARBETSTID (TIMMAR)               
005000*                                 PERFORMED LABOUR TIME (HOURS)           
005100*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
