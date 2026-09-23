000100 01  RYI-WDGZRYI.                                                         
000200*                                 RYI                                     
000300*                                 SKAPAS FÖR SATSORDERRADER VID           
000400*                                 UTSKRIFT AV ORDER. ANVÄNDS VID          
000500*                                 TRANSAKTIONSSKAPANDE TILL               
000600*                                 ÖVRIGA SYSTEM.                          
000700     03 RYI-IDPTYP           PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 RYI-IDARTNR          PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 RYI-IDDISTR          PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 RYI-IDDC             PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 RYI-KDORDKL          PIC S9              COMP-3.                  
001600*                                 ORDERKLASS                              
001700     03 RYI-KDRORELS         PIC S9(3)           COMP-3.                  
001800*                                 RÖRELSE I LAGER                         
001900     03 RYI-KDUPPD           PIC X.                                       
002000*                                 UPPDATERINGSTYP                         
002100     03 RYI-KVAVBART         PIC S9(7)           COMP-3.                  
002200*                                 AVBOKAT ANTAL ARTIKLAR                  
002300     03 RYI-TIUTSKR          PIC S9(7)           COMP-3.                  
002400*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
002500*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
