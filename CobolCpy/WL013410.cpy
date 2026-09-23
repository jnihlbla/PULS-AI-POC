000100 01  3410-WL013410.                                                       
000200*                                 REQUEST TO PGM WL013410                 
000300     03 3410-IDBORD          PIC X(3).                                    
000400*                                 PACK-BORD                               
000500     03 3410-IDLOPNR         PIC 9(3).                                    
000600*                                 LÖPNUMMER                               
000700     03 3410-IDUSER          PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900     03 3410-IX              PIC 9(3).                                    
001000*                                 LÖPINDEX                                
001100     03 3410-IDPRODNR-KEY    PIC 9(7).                                    
001200*                                 PRODUKTIONSNUMMER                       
001300     03 3410-IDPLKLST-KEY    PIC 9(3).                                    
001400*                                 PLOCKLISTNUMMER                         
001500     03 3410-ORDDEL-2-LW     PIC X.                                       
001600*                                 ALLMÄN FLAGGA                           
001700     03 3410-TIAAMMDD        PIC 9(6).                                    
001800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001900     03 3410-TIAADDD         PIC 9(5).                                    
002000*                                 ÅR - DAGNUMMER    (ÅÅDDD)               
002100     03 3410-TIHHMMSS        PIC 9(6).                                    
002200*                                 TIM - MIN - SEK   (HHMMSS)              
002300     03 3410-IDTRANS         PIC X(4).                                    
002400*                                 BILDNUMMER                              
002500     03 3410-ORDDEL          OCCURS 99 TIMES.                             
002600*                                 GRUPP MED ORDERDELAR FÖR UTSKRI         
002700*                                 FT                                      
002800        05 3410-IDORDER      PIC S9(7)           COMP-3.                  
002900*                                 VOLVO PARTS ORDERNUMMER                 
003000        05 3410-IDDC         PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200        05 3410-IDPRODNR     PIC S9(7)           COMP-3.                  
003300*                                 PRODUKTIONSNUMMER                       
003400        05 3410-IDPLKLST     PIC S9(3)           COMP-3.                  
003500*                                 PLOCKLISTNUMMER                         
003600        05 3410-VKORDNTO     PIC S9(6)V9(1)      COMP-3.                  
003700*                                 ORDERVIKT NETTO (KG)                    
003800        05 3410-ADFLGEO      PIC X(3).                                    
003900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
004000        05 3410-ADFLOMR      PIC S9(3)           COMP-3.                  
004100*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
004200        05 3410-ADRUTNIV     PIC S9(3)           COMP-3.                  
004300*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
004400        05 3410-IDTRPTNR     PIC S9(3)           COMP-3.                  
004500*                                 TRANSPORTIDENTITET                      
004600        05 3410-TIRFSDAT     PIC 9(6).                                    
004700*                                 KLART FÖR TRANSPORT ÅÅMMDD              
004800        05 3410-TIRFSTID     PIC 9(4).                                    
004900*                                 KLART FÖR TRANSPORT (TTMM)              
005000        05 3410-IDDC-CROSS   PIC X(2).                                    
005100*                                 DC FÖR CROSS DOCKING                    
005200*** END OF VILMAII-COPY LENGTH= 3712 BYTES                                
