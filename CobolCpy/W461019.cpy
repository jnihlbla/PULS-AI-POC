000100 01  SERVGR-W461019.                                                      
000200*                                 SERVICE GRAD TILL NOAC PT-019           
000300     03 SERVGR-IDPTYP        PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 SERVGR-IDDC          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 SERVGR-IDDISTR       PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 SERVGR-IDKUNDNR      PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 SERVGR-IDORDNR       PIC S9(7)           COMP-3.                  
001200*                                 ORDERNR             IDORDNR-002         
001300     03 SERVGR-KDORDKL       PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500     03 SERVGR-IDARTNR       PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 SERVGR-REKSIFFR      PIC S9              COMP-3.                  
001800*                                 KONTROLLSIFFRA                          
001900     03 SERVGR-KDPRODSL      PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTSLAG                             
002100     03 SERVGR-KVBEART       PIC S9(7)           COMP-3.                  
002200*                                 BESTÄLLT ANTAL STYCKEN                  
002300     03 SERVGR-KVLEVART      PIC S9(7)           COMP-3.                  
002400*                                 LEVERERAT ANTAL STYCK                   
002500     03 SERVGR-TIORDREG      PIC S9(7)           COMP-3.                  
002600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002700     03 FILLER               PIC X(6).                                    
002800*** END COPY W461019     LENGTH=43                                        
