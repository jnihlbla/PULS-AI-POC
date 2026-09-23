000100 01  2502-WDGX2502.                                                       
000200*                                 REFILL PARAMETERTABELL                  
000300*                                 FYSISK NYCKEL: IDREFTAB                 
000400     03 2502-IDREFTAB        PIC X.                                       
000500*                                 IDENTITET REFILLTABELL                  
000600*                                 REFILLINGTABLE IDENTIFIER               
000700     03 2502-KVPB-REF        OCCURS 8 TIMES                               
000800                             PIC S9(6)V9(1)      COMP-3.                  
000900*                                 PERIODBEHOV REFILLING                   
001000*                                 FORECAST REFILLING                      
001100     03 2502-PRIS-RAD        OCCURS 10 TIMES.                             
001200*                                 PRISRAD                                 
001300        05 2502-PRARTBES     PIC S9(7)V9(2)      COMP-3.                  
001400*                                 BESTÄLLNINGSPRIS I KRONOR               
001500*                                 ORDER PRICE SWEDISH CURRENCY            
001600        05 2502-PROGNOS-KOLUMN                                            
001700                             OCCURS 8 TIMES.                              
001800*                                 PROGNOS KOLUMN                          
001900           07 2502-KVREFLIM  PIC S9(5)           COMP-3.                  
002000*                                 FAKTOR FÖR REFILLPUNKT.                 
002100*                                 FACTOR FOR REFILLING POINT.             
002200           07 2502-KDREFPKT-LIM                                           
002300                             PIC X.                                       
002400*                                 TYP AV FAKTOR FÖR REFILLPKT             
002500*                                 TYPE OF FACTOR FOR REF POINT            
002600           07 2502-KVREFKVA  PIC S9(5)           COMP-3.                  
002700*                                 FAKTOR FÖR REFILLKVANTITET.             
002800*                                 FACTOR FOR REFILLING QUANTITY.          
002900           07 2502-KDREFPKT-KVA                                           
003000                             PIC X.                                       
003100*                                 TYP AV FAKTOR FÖR REFILLKVANT           
003200*                                 TYPE OF FACTOR FOR REF QUANTITY         
003300     03 2502-TEREFLIM        PIC X(70).                                   
003400*                                 REFILLORDERFÖRSLAGTAB. GRÄNS            
003500*                                 REFILLORDERPROPOSAL LIMIT               
003600*** END OF VILMAII-COPY LENGTH= 793 BYTES                                 
