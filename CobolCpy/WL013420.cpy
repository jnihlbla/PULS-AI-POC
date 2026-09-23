000100 01  3420-WL013420.                                                       
000200*                                 REQUEST TO PGM WL013420                 
000300     03 3420-IDLIST          PIC X(10).                                   
000400*                                 LISTIDENTITET                           
000500     03 3420-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 3420-IDPRODNR-KEY    PIC 9(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900     03 3420-IDPLKLST-KEY    PIC 9(3).                                    
001000*                                 PLOCKLISTNUMMER                         
001100     03 3420-IDUSER          PIC X(8).                                    
001200*                                 ANVÄNDARENS SÄKERHETS ID                
001300     03 3420-IDTRANS         PIC X(4).                                    
001400*                                 BILDNUMMER                              
001500     03 3420-IDPRC.                                                       
001600*                                 PRODUKTIONSKANAL                        
001700        05 3420-IDPRCBAS     PIC X(3).                                    
001800*                                 PRC-BAS                                 
001900        05 3420-IDPRCVAR     PIC X.                                       
002000*                                 PRC-VARIANT                             
002100     03 3420-ORDDEL          OCCURS 99 TIMES.                             
002200*                                 GRUPP MED ORDERDELAR FÖR UTSKRI         
002300*                                 FT                                      
002400        05 3420-ADFLGEO      PIC X(3).                                    
002500*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002600        05 3420-ADFLOMR      PIC S9(3)           COMP-3.                  
002700*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002800        05 3420-ADRUTNIV     PIC S9(3)           COMP-3.                  
002900*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003000        05 3420-IDTRPTNR     PIC S9(3)           COMP-3.                  
003100*                                 TRANSPORTIDENTITET                      
003200        05 3420-VKORDNTO     PIC S9(6)V9(1)      COMP-3.                  
003300*                                 ORDERVIKT NETTO (KG)                    
003400        05 3420-DATRPAVT.                                                 
003500*                                 TRANSPORTAVGÅNGSTIDPUNKT                
003600           07 3420-DATRPAVD  PIC 9(8).                                    
003700*                                 TRANSPORTAVGÅNGSDATUM                   
003800           07 3420-TIHHMM    PIC S9(5)           COMP-3.                  
003900*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
004000        05 3420-TIRFSDAT     PIC 9(6).                                    
004100*                                 KLART FÖR TRANSPORT ÅÅMMDD              
004200        05 3420-TIRFSTID     PIC 9(4).                                    
004300*                                 KLART FÖR TRANSPORT (TTMM)              
004400        05 3420-IDORDER      PIC S9(7)           COMP-3.                  
004500*                                 VOLVO PARTS ORDERNUMMER                 
004600        05 3420-IDPRODNR     PIC S9(7)           COMP-3.                  
004700*                                 PRODUKTIONSNUMMER                       
004800        05 3420-IDPLKLST     PIC S9(3)           COMP-3.                  
004900*                                 PLOCKLISTNUMMER                         
005000        05 3420-IDDC-CROSS   PIC X(2).                                    
005100*                                 DC FÖR CROSS DOCKING                    
005200*** END OF VILMAII-COPY LENGTH= 4592 BYTES                                
