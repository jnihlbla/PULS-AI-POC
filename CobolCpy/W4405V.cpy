000100 01  W4405V.                                                              
000200*                                 UPPF. POST VORK÷ NY                     
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDRF-GRP.                                                     
001000*                                 KUNDENS REFERENS (ORDERID)              
001100        05 IDKUNDRF          PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
001400           07 IDORDNR5       PIC 9(5).                                    
001500*                                 ORDERNUMMER                             
001600           07 FILLER         PIC X(5).                                    
001700        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
001800           07 IDORDNR7       PIC 9(7).                                    
001900*                                 ORDERNUMMER                             
002000           07 FILLER         PIC X(3).                                    
002100     03 TIREGDAT-URSP        PIC S9(7)           COMP-3.                  
002200*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002300     03 TIREGTID-URSP        PIC S9(9)           COMP-3.                  
002400*                                 KLOCKSLAG (TTMMSSTH)                    
002500     03 IDARTNR              PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700     03 TIREGDAT-AVV         PIC S9(7)           COMP-3.                  
002800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002900     03 TIREGTID-AVV         PIC S9(9)           COMP-3.                  
003000*                                 KLOCKSLAG (TTMMSSTH)                    
003100     03 KDVORATG             PIC X.                                       
003200*                                 TYP AV ≈TGƒRD F÷R POST VOR-K÷N          
003300     03 TIKLAR               PIC S9(7)           COMP-3.                  
003400*                                 KLARDATUM          (≈≈MMDD)             
003500     03 TIKLATID             PIC S9(7)           COMP-3.                  
003600*                                 KLARTID                                 
003700*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
