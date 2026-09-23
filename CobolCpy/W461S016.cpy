000100 01  AVST-W461S001.                                                       
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 AVSTÄMMNINGSINFO TILL NOAC              
000400     03 AVST-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 AVST-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 AVST-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 AVST-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 AVST-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 AVST-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 AVST-W461016.                                                     
001700*                                 SALDO AVSTÄMMNING                       
001800*                                 TILL NOAC PT-016                        
001900        05 AVST-IDPTYP       PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 AVST-IDDISTR      PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 AVST-IDKUNDNR     PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 AVST-IDORDNR      PIC S9(7)           COMP-3.                  
002600*                                 ORDERNR             IDORDNR-002         
002700        05 AVST-IDARTNR      PIC S9(9)           COMP-3.                  
002800*                                 ARTIKELNUMMER                           
002900        05 AVST-REKSIFFR     PIC S9              COMP-3.                  
003000*                                 KONTROLLSIFFRA                          
003100        05 AVST-BEVOLREF     PIC X(10).                                   
003200*                                 VOLVO REFERENS                          
003300        05 AVST-KVBEART      PIC S9(7)           COMP-3.                  
003400*                                 BESTÄLLT ANTAL ARTIKLAR                 
003500        05 AVST-KVRO         PIC S9(7)           COMP-3.                  
003600*                                 ANTAL RESTNOTERADE ARTIKLAR             
003700        05 FILLER            PIC X(6).                                    
003800*** END COPY W461S016C0  LENGTH=65                                        
