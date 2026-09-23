000100 01  SERV-W461009.                                                        
000200*                                 SERVICE GRAD TILL NOAC PT-009           
000300     03 SERV-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 SERV-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 SERV-IDDISTR         PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 SERV-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 SERV-IDORDNR         PIC S9(7)           COMP-3.                  
001200*                                 ORDERNR             IDORDNR-002         
001300     03 SERV-KDORDKL         PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500     03 SERV-IDARTNR         PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 SERV-REKSIFFR        PIC S9              COMP-3.                  
001800*                                 KONTROLLSIFFRA                          
001900     03 SERV-KDPRODSL        PIC S9(3)           COMP-3.                  
002000*                                 PRODUKTSLAG                             
002100     03 SERV-KVBEART         PIC S9(7)           COMP-3.                  
002200*                                 BESTÄLLT ANTAL STYCKEN                  
002300     03 SERV-TIORDREG        PIC S9(7)           COMP-3.                  
002400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002500     03 SERV-KDFAKTYP        PIC X.                                       
002600*                                 FAKTURATYP                              
002700     03 FILLER               PIC X(6).                                    
002800*** END COPY W461009     LENGTH=40                                        
