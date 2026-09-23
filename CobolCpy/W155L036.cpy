000100 01  6-W155L036.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W15503         
000300     03 6-IDKATRAD           PIC S9(5)           COMP-3.                  
000400*                                 RADNUMMER                               
000500     03 6-TIUPPDAT           PIC S9(7)           COMP-3.                  
000600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
000700     03 6-KDRADST            PIC X.                                       
000800*                                 RAD-     L = LÅNAD.                     
000900*                                 STATUS   Ä = ÄNDRAD.                    
001000*                                          N = NYREGISTRERAD.             
001100*                                          C = L,Ä,N EFTER OMBRYT         
001200*                                              FRAM TILL VADGEN.          
001300*                                      SPACE = OFÖRÄNDRAD.                
001400     03 6-KDFBX              PIC X.                                       
001500*                                 FBX-KOD                                 
001600     03 6-IDKATPOS           PIC X(4).                                    
001700*                                 POSITIONSNUMMER                         
001800     03 6-IDARTNR            PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 6-KVKOL-GRP.                                                      
002100        05 6-KVKOL           OCCURS 5 TIMES                               
002200                             PIC X(3).                                    
002300*                                 ANTAL AV ARTIKEL I RESP KOLUMN          
002400     03 6-KDPS               PIC X(2).                                    
002500*                                 ARTIKELSTATUS                           
002600     03 6-KVPUNKT            PIC S9              COMP-3.                  
002700*                                 ANTAL INDRAGNINGSPUNKTER                
002800     03 6-IDTTEXNR           PIC S9(5)           COMP-3.                  
002900*                                 TILLÄGGSTEXT-NR                         
003000*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
