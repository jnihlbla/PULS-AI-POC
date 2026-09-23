000100 01  SEQE-WDM6E1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDM601             
000300*                                 BYTESREGISTER LAGRING RETURER           
000400*                                 SEKUNDÄR NYCKEL: WDM6ESEQ               
000500*                                 (IDARTNR)                               
000600*                                 FYSISK NYCKEL: WDM6E1KY                 
000700*                                 (IDARTNR + IDDISTR +                    
000800*                                  IDBYTRAP-9KOMPL + IDBYTRAD)            
000900     03 SEQE-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 SEQE-IDDISTR         PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 SEQE-IDBYTRAP-9KOMPL PIC S9(7)           COMP-3.                  
001600*                                 RAPPORTNUMMER BYTES 9-KOMPL             
001700*                                 REPORTNUMBER  EXCHANGE 9-COMPL          
001800     03 SEQE-IDBYTRAD        PIC S9(5)           COMP-3.                  
001900*                                 RADNUMMER                               
002000*                                 LINE NO                                 
002100*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
