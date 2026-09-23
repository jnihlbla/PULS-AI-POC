000100 01  W6115D01.                                                            
000200*                                 GENOMLOPPSTID PER UPPFÖLJ-              
000300*                                 NINGSSTATUS/ VECKA                      
000400*                                                               .         
000500*                                 WORK FLOW TIME PER FOLLOW-UP            
000600*                                 STATUS AND WEEK                         
000700*                                                               .         
000800     03 IDAVD                PIC X(5).                                    
000900*                                 DEN ANSTÄLLDES AVDELNING/               
001000*                                 KOSTNADSSTÄLLE                          
001100*                                 DEPARTMENT OF EMPLOYED/                 
001200*                                 COST CENTER                             
001300     03 IDGRUPP              PIC X(2).                                    
001400*                                 GRUPPIDENTITET                          
001500     03 KVRADER              PIC S9(5)           COMP-3.                  
001600*                                 ANTAL RADER                             
001700*                                 NUMBER OF LINES                         
001800     03 KVRADER-PRIO         PIC S9(5)           COMP-3.                  
001900*                                 ANTAL RADER                             
002000*                                 NUMBER OF LINES                         
002100     03 KVMIN                PIC S9(9)           COMP-3.                  
002200*                                 ANTAL MINUTER                           
002300*                                 NO OF MINUTES                           
002400     03 KVMIN-PRIO           PIC S9(9)           COMP-3.                  
002500*                                 ANTAL MINUTER PRIO                      
002600*                                 NO OF MINUTES PRIO                      
002700     03 KVART-MAAL           PIC S9(7)           COMP-3.                  
002800*                                 ANTAL ARTNR PER BRYTBEGREPP             
002900*                                 NO OF PARTNOS PER TYPE                  
003000     03 KVART-MAAL-PRIO      PIC S9(7)           COMP-3.                  
003100*                                 ANTAL ARTNR PER BRYTBEGREPP             
003200*                                 NO OF PARTNOS PER TYPE                  
003300     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
003400*                                 ARTIKELVOLYM NETTO (CM3)                
003500*                                 PART NET VOLUME    (CM3)                
003600     03 SUBEL                PIC S9(9)V9(2).                              
003700*                                 SUMMABELOPP                             
003800*                                 SUM AMOUNT                              
003900*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
