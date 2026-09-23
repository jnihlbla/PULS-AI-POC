000100 01  PTID-W416PTID.                                                       
000200*                                 LÄNKAREA TILL W416PTID - BERÄK-         
000300*                                 NING AV PRODUKTIONSTID FÖR              
000400*                                 BYGGBAR SATS                            
000500     03 PTID-IDSYSTEM        PIC X(4).                                    
000600*                                 SKAPANDE SYSTEMNUMMER                   
000700     03 PTID-IDORDNST.                                                    
000800*                                 SATSORDERNUMMER-TOTALT                  
000900        05 PTID-IDORDNSB     PIC S9(5)           COMP-3.                  
001000*                                 SATSORDERNUMMER-BAS                     
001100        05 PTID-IDORDNSS     PIC S9              COMP-3.                  
001200*                                 SATSORDERNUMMER-SUFFIX                  
001300     03 PTID-IDPRC           PIC X(4).                                    
001400*                                 PRODUKTIONSKANAL                        
001500     03 PTID-IDARTNR         PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 PTID-KDCLAGER        PIC S9              COMP-3.                  
001800      88 PTID-KDCLAGER-C1    VALUE +1.                                    
001900      88 PTID-KDCLAGER-C2    VALUE +2.                                    
002000*                                 CENTRALLAGERKOD                         
002100     03 PTID-KVBYGGB         PIC S9(7)           COMP-3.                  
002200*                                 ANTAL BYGGBARA SATSER                   
002300     03 PTID-KVRADER         PIC S9(5)           COMP-3.                  
002400*                                 ANTAL RADER                             
002500     03 PTID-KVANTART        PIC S9(5)           COMP-3.                  
002600*                                 ANTAL-ARTIKLAR                          
002700     03 PTID-SUSATPTI        PIC S9(3)V9(2)      COMP-3.                  
002800*                                 TOT PRODUKTIONSTID SATS TIM MIN         
002900     03 PTID-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
003000*                                 ORDERVIKT NETTO (KG)                    
003100     03 PTID-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
003200*                                 ORDERVOLYM NETTO (M3)                   
003300     03 PTID-KDSVAR          PIC X.                                       
003400*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003500*** END COPY W416PTIDC0  LENGTH=40                                        
