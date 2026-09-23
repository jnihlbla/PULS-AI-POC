000100 01  W61150.                                                              
000200*                                 BELÄGGNING PER UPPFÖLJNINGS-            
000300*                                 STATUS                                  
000400*                                                            .            
000500*                                 WORK LOAD  PER FOLLOW-UP                
000600*                                 STATUS                                  
000700*                                                            .            
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000*                                 RECORD TYPE                             
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 KDINLUPF             PIC X(4).                                    
001500*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
001600*                                 FOLLOW-UP STATUS RECEIVING              
001700     03 KVART                PIC S9(7)           COMP-3.                  
001800*                                 ANTAL ARTNR PER BRYTBEGREPP             
001900*                                 NO OF PARTNOS PER TYPE                  
002000     03 KVRADER              PIC S9(5)           COMP-3.                  
002100*                                 ANTAL RADER                             
002200*                                 NUMBER OF LINES                         
002300     03 KVRADER-PRIO         PIC S9(5)           COMP-3.                  
002400*                                 ANTAL RADER                             
002500*                                 NUMBER OF LINES                         
002600     03 KVKOLLI              PIC S9(5)           COMP-3.                  
002700*                                 ANTAL KOLLI                             
002800*                                 NBR OF CASES                            
002900     03 KVKOLLI-PRIO         PIC S9(5)           COMP-3.                  
003000*                                 ANTAL KOLLI                             
003100*                                 NBR OF CASES                            
003200     03 SUBEL                PIC S9(9)V9(2).                              
003300*                                 SUMMABELOPP                             
003400     03 SUBEL-PRIO           PIC S9(9)V9(2).                              
003500*                                 SUMMABELOPP                             
003600*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
