000100 01  RYJ-WDGZRYJ.                                                         
000200*                                 RYJ                                     
000300*                                 SKAPAS FÖR SATSORDERRADER VID           
000400*                                 AVVIKELSERAPPORTERING AV UT-            
000500*                                 SKRIVEN ORDER                           
000600*                                 ANVÄNDS VID TRANSAKTION-                
000700*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000800     03 RYJ-IDPTYP           PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 RYJ-IDARTNR          PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 RYJ-IDDISTR          PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 RYJ-IDDC             PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 RYJ-KDORDKL          PIC S9              COMP-3.                  
001700*                                 ORDERKLASS                              
001800     03 RYJ-KDAVVORS         PIC S9              COMP-3.                  
001900*                                 AVVIKELSEORSAK                          
002000*                                 1 = FYSISK NOLLNING                     
002100*                                 2 = ANNULLERING                         
002200     03 RYJ-KDRORELS         PIC S9(3)           COMP-3.                  
002300*                                 RÖRELSE I LAGER                         
002400     03 RYJ-KDUPPD           PIC X.                                       
002500*                                 UPPDATERINGSTYP                         
002600     03 RYJ-KVANNANT         PIC S9(7)           COMP-3.                  
002700*                                 ANNULLERAT ANTAL ARTIKLAR               
002800     03 RYJ-KVAVART          PIC S9(7)           COMP-3.                  
002900*                                 AVVIKANDE ANTAL ARTIKLAR                
003000     03 RYJ-TIUTSKR          PIC S9(7)           COMP-3.                  
003100*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
003200*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
