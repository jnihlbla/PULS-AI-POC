000100 01  MID-W2I13201.                                                        
000200*                                 MID-COPYTEXT FÖR W2013200               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-C2FAELT-SW       PIC X.                                       
000800     03 MID-INPUT.                                                        
000900        05 MID-REDIRLEV-C1   PIC X(4).                                    
001000*                                 DIREKTLEVERANSANDEL                     
001100        05 MID-FLFSP         PIC X.                                       
001200*                                 FÖRDELNINGSSPÄRR                        
001300        05 MID-KVSLUTKP      PIC 9(7).                                    
001400*                                 SLUTKÖPSSALDO                           
001500        05 MID-KDKSP         PIC X.                                       
001600*                                 KÖPSPÄRR                                
001700        05 MID-KDAVT         PIC X.                                       
001800*                                 AVTALSMÄRKNING                          
001900        05 MID-IDINK         PIC X(4).                                    
002000*                                 INKÖPARNUMMER                           
002100        05 MID-TISLUTKP      PIC 9(5).                                    
002200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
002300        05 MID-FLNYBER       PIC X.                                       
002400*                                 FLAGGA BERÄKN HEMTAGN NYTT SÄTT         
002500        05 MID-KDSOP         PIC X.                                       
002600*                                 VISAR NÄR START DAT ART GÄLLER          
002700        05 MID-KVEOP         PIC 9(2).                                    
002800*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
002900        05 MID-FLJIT         PIC X.                                       
003000*                                 JUST-IN-TIME FLAGGA                     
003100        05 MID-FLBSNES       PIC X.                                       
003200*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
003300        05 MID-FLBRAND       PIC X.                                       
003400*                                 ARTIKEL MED VARUMÄRKESBILD              
003500        05 MID-TISTODAT-LARM PIC X(6).                                    
003600*                                 STOPPDATUM FÖR LARM-223                 
003700        05 MID-KVKP          PIC 9(7).                                    
003800*                                 KÖPPUNKT                                
003900        05 MID-FLMANKP       PIC X.                                       
004000*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
004100        05 MID-TIFINLV       PIC 9(4).                                    
004200        05 MID-TIURPROD      PIC 9(4).                                    
004300*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
004400        05 MID-FLRELSP       PIC X.                                       
004500*                                 RELEASEBLOCKAD ARTIKEL .                
004600        05 MID-KVVECKOR-LVAR PIC X(4).                                    
004700*                                 VARIANS I LEDTIDEN                      
004800        05 MID-INPUT-TILEVDAGAR.                                          
004900*                                                                         
005000           07 MID-DAG-POS    OCCURS 5 TIMES                               
005100                             PIC X(2).                                    
005200        05 MID-TEARTNOT1     PIC X(40).                                   
005300*                                 ARTIKEL NOTERING                        
005400        05 MID-TEARTNOT2     PIC X(40).                                   
005500*                                 ARTIKEL NOTERING                        
005600     03 MID-SPAR-TILEVDAGAR.                                              
005700*                                                                         
005800        05 MID-SPAR-DAG      OCCURS 5 TIMES                               
005900                             PIC X(2).                                    
006000*** END OF VILMAII-COPY LENGTH= 176 BYTES                                 
