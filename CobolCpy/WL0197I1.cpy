000100 01  REQU-WL0197I1.                                                       
000200*                                 REQUEST TO PGM  WL0197                  
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDTRPTNR-KEY    PIC 9(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 REQU-TIRFSDAT-KEY    PIC 9(6).                                    
000800*                                 KLART FÖR TRANSPORT ÅÅMMDD              
000900     03 REQU-TIRFSTID-KEY    PIC 9(4).                                    
001000*                                 KLART FÖR TRANSPORT (TTMM)              
001100     03 REQU-IDPRC-KEY.                                                   
001200*                                 PRODUKTIONSKANAL                        
001300        05 REQU-IDPRCBAS     PIC X(3).                                    
001400*                                 PRC-BAS                                 
001500        05 REQU-IDPRCVAR     PIC X.                                       
001600*                                 PRC-VARIANT                             
001700     03 REQU-IDLOTNR-KEY     PIC 9(3).                                    
001800*                                 VAGN-NUMMER                             
001900     03 REQU-KVRADER         PIC 9(5).                                    
002000*                                 ANTAL RADER                             
002100     03 REQU-FLOK-PACKRAPP   PIC X.                                       
002200*                                 JA/NEJ-FLAGGA                           
002300     03 REQU-FLSKRIV-CLABEL  PIC X.                                       
002400*                                 J/Y = SKRIV BEGÄRD LISTA                
002500     03 REQU-FLSKRIV-DELNOTE PIC X.                                       
002600*                                 J/Y = SKRIV BEGÄRD LISTA                
002700     03 REQU-RESTART-IX-SPAR PIC 9(5).                                    
002800     03 REQU-RAD             OCCURS 500 TIMES.                            
002900*                                 MID-COPYTEXT FÖR WL0197                 
003000        05 REQU-IDPRODNR     PIC 9(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200        05 REQU-IDPLKLST     PIC 9(3).                                    
003300*                                 PLOCKLISTNUMMER                         
003400        05 REQU-IDKOLLI      PIC 9(5).                                    
003500*                                 KOLLINUMMER                             
003600        05 REQU-KDKOLLI      PIC X(8).                                    
003700*                                 KOLLIKOD                                
003800        05 REQU-KDKOLLI-IN   PIC X(8).                                    
003900*                                 KOLLIKOD                                
004000*** END OF VILMAII-COPY LENGTH= 15535 BYTES                               
