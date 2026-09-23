000100 01  SPAR-AREA.                                                           
000200     03 SPAR-IDTRANS              PIC XXXX    VALUE '2147'.               
000300     03 SPAR-SIDNR                PIC S9(3)   COMP-3.                     
000400     03 SPAR-KVRADER              PIC 9(4).                               
000420                                                                          
000500*    -- Enternycklar WDD6  (Samma eller Första sida)                      
000600     03 SPAR-KEY-WDD601KY-ENTER.                                          
000800        05 SPAR-KEY-IDDC-ENTER    PIC X(2).                               
000810        05 SPAR-KEY-IDLEVNR-ENTER PIC X(5).                               
000900        05 SPAR-KEY-IDARTNR-ENTER PIC S9(9)      COMP-3.                  
001000        05 SPAR-KEY-IDANSK-ENTER  PIC S9(3)      COMP-3.                  
001100                                                                          
001200*    -- Next-nycklar  WDD6                                                
001300     03 SPAR-KEY-WDD601KY-NEXT.                                           
001400        05 SPAR-KEY-IDDC-NEXT     PIC X(2).                               
001500        05 SPAR-KEY-IDLEVNR-NEXT  PIC X(5).                               
001600        05 SPAR-KEY-IDARTNR-NEXT  PIC S9(9)      COMP-3.                  
001610        05 SPAR-KEY-IDANSK-NEXT   PIC S9(3)      COMP-3.                  
001700                                                                          
001800*    -- Previous-nycklar WDD6. Indexerade av SIDX                         
001900     03 SPAR-KEY-PREV    Occurs 10.                                       
002000        05 SPAR-KEY-WDD601KY-PREV.                                        
002100           07 SPAR-KEY-IDDC-PREV    PIC X(2).                             
002200           07 SPAR-KEY-IDLEVNR-PREV PIC X(5).                             
002300           07 SPAR-KEY-IDARTNR-PREV PIC S9(9)    COMP-3.                  
002310           07 SPAR-KEY-IDANSK-PREV  PIC S9(3)    COMP-3.                  
002400                                                                          
002500*    -- Ytterligare spar-fält.                                            
002600     03 SPAR-KEY-IDANSK-TO        PIC S9(3)      COMP-3.                  
002700     03 SPAR-KEY-IDLEVNR          PIC X(5).                               
002800     03 SPAR-KEY-KDLPORS          PIC S9(3)      COMP-3.                  
002900     03 SPAR-KEY-KDLEVPLF         PIC X.                                  
003100     03 SPAR-SISTA-SIDAN          PIC X.                                  
003200     03 SPAR-KEY-IDANSK-FROM      PIC S9(3)      COMP-3.                  
