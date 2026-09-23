000100 01  BACK-W461S043-CTX.                                                   
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 RKH      POST TILL NOAC                 
000400     03 BACK-SOR0-IDDISTR    PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 BACK-SOR0-IDKUNDNR   PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 BACK-SOR0-IDRONR     PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 BACK-SOR0-TIRODAT    PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 BACK-SOR0-IDPTYP     PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 BACK-SOR0-IDLOPNR    PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 BACK-W461RKHN-CTX.                                                
001700*                                 BACKNINGSTRANS ERSÄTTNINGAR             
001800*                                 RECORD TYP  RKG                         
001900        05 BACK-IDPTYP       PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 BACK-IDARTNR      PIC 9(9).                                    
002200*                                 ARTIKELNUMMER                           
002300        05 BACK-REKSIFFR     PIC 9.                                       
002400*                                 KONTROLLSIFFRA                          
002500        05 BACK-KDERS-OLD    PIC 9(2).                                    
002600*                                 ERSÄTTNINGSKOD                          
002700        05 BACK-KDERS-NEW    PIC 9(2).                                    
002800*                                 ERSÄTTNINGSKOD                          
002900        05 BACK-FILLERX63    PIC X(63).                                   
003000*** END OF VILMAII-COPY LENGTH= 101 BYTES                                 
