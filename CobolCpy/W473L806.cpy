000100 01  6-W473L806.                                                          
000200*                                 CALL6 COPYTEXT TILL SUBPROG.            
000300*                                 W4738310                                
000400*                                                                         
000500     03 6-KVMANTIM-TOT       PIC S9(5)V9(2)      COMP-3.                  
000600*                                 TOTALT ANTAL MANTIMMAR                  
000700     03 6-KDMARKN            PIC S9              COMP-3.                  
000800*                                 MARKNADSKOD                             
000900     03 6-VLORDNTO           PIC S9(4)V9(3)      COMP-3.                  
001000*                                 ORDERVOLYM NETTO (M3)                   
001100     03 6-TABELL.                                                         
001200        05 6-LAGEROMR-INF    OCCURS 25 TIMES                              
001300                             INDEXED 6-LOIX.                              
001400           07 6-ADLAGOMR     PIC S9(3)           COMP-3.                  
001500*                                 LAGEROMRÅDE                             
001600           07 6-KVMANTIM-RAD PIC S9(5)V9(2)      COMP-3.                  
001700*                                 ANTAL MANTIMMAR BASERAT PÅ              
001800*                                 ORDERRADER                              
001900           07 6-KVMANTIM-VOL PIC S9(5)V9(2)      COMP-3.                  
002000*                                 ANTAL MANTIMMAR BASERAT PÅ              
002100*                                 VOLYM                                   
002200           07 6-VLORDBTO     PIC S9(4)V9(3)      COMP-3.                  
002300*                                 ORDERVOLYM BRUTTO (M3)                  
002400           07 6-IDRADNR-ORD-FROM                                          
002500                             PIC S9(5)           COMP-3.                  
002600*                                 RADNUMMER PÅ VOLVOORDER FROM            
002700           07 6-IDRADNR-ORD-TOM                                           
002800                             PIC S9(5)           COMP-3.                  
002900*                                 RADNUMMER PÅ VOLVOORDER TOM             
003000*** END COPY W473L806C0  LENGTH=509                                       
