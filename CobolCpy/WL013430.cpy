000100 01  3430-WL013430.                                                       
000200*                                 REQUEST TO PGM WL013430                 
000300     03 3430-IDLIST          PIC X(10).                                   
000400*                                 LISTIDENTITET                           
000500     03 3430-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 3430-IDPRODNR-KEY    PIC 9(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900     03 3430-IDPLKLST-KEY    PIC 9(3).                                    
001000*                                 PLOCKLISTNUMMER                         
001100     03 3430-IDUSER          PIC X(8).                                    
001200*                                 ANVÄNDARENS SÄKERHETS ID                
001300     03 3430-IDTRANS         PIC X(4).                                    
001400*                                 BILDNUMMER                              
001500     03 3430-ORDDEL          OCCURS 99 TIMES.                             
001600*                                 GRUPP MED ORDERDELAR FÖR UTSKRI         
001700*                                 FT                                      
001800        05 3430-ADFLGEO      PIC X(3).                                    
001900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002000        05 3430-ADFLOMR      PIC S9(3)           COMP-3.                  
002100*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002200        05 3430-ADRUTNIV     PIC S9(3)           COMP-3.                  
002300*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002400        05 3430-IDTRPTNR     PIC S9(3)           COMP-3.                  
002500*                                 TRANSPORTIDENTITET                      
002600        05 3430-VKORDNTO     PIC S9(6)V9(1)      COMP-3.                  
002700*                                 ORDERVIKT NETTO (KG)                    
002800        05 3430-DATRPAVT.                                                 
002900*                                 TRANSPORTAVGÅNGSTIDPUNKT                
003000           07 3430-DATRPAVD  PIC 9(8).                                    
003100*                                 TRANSPORTAVGÅNGSDATUM                   
003200           07 3430-TIHHMM    PIC S9(5)           COMP-3.                  
003300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003400        05 3430-TIRFSDAT     PIC 9(6).                                    
003500*                                 KLART FÖR TRANSPORT ÅÅMMDD              
003600        05 3430-TIRFSTID     PIC 9(4).                                    
003700*                                 KLART FÖR TRANSPORT (TTMM)              
003800        05 3430-IDORDER      PIC S9(7)           COMP-3.                  
003900*                                 VOLVO PARTS ORDERNUMMER                 
004000        05 3430-IDPRODNR     PIC S9(7)           COMP-3.                  
004100*                                 PRODUKTIONSNUMMER                       
004200        05 3430-IDPLKLST     PIC S9(3)           COMP-3.                  
004300*                                 PLOCKLISTNUMMER                         
004400        05 3430-IDDC-CROSS   PIC X(2).                                    
004500*                                 DC FÖR CROSS DOCKING                    
004600*** END OF VILMAII-COPY LENGTH= 4588 BYTES                                
