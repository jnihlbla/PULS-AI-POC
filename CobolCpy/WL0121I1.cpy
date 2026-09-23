000100 01  REQU-WL0121I1.                                                       
000200*                                 REQUEST TO PGM  WL0121                  
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-FLSKRIV-CLABEL  PIC X.                                       
000600*                                 J/Y = SKRIV BEGÄRD LISTA                
000700     03 REQU-FLSKRIV-DELNOTE PIC X.                                       
000800*                                 J/Y = SKRIV BEGÄRD LISTA                
000900     03 REQU-IDKOLLI-SAMP    PIC 9(5).                                    
001000*                                 SAMPACKNINGSKOLLINUMMER                 
001100     03 REQU-KDMATT          PIC X.                                       
001200*                                 MÅTTKOD                                 
001300     03 REQU-RAD             OCCURS 15 TIMES.                             
001400*                                 MID-COPYTEXT FÖR WL0121                 
001500        05 REQU-IDPRODNR     PIC 9(7).                                    
001600*                                 PRODUKTIONSNUMMER                       
001700        05 REQU-IDPLKLST     PIC 9(3).                                    
001800*                                 PLOCKLISTNUMMER                         
001900        05 REQU-KOLLI-INFO.                                               
002000*                                 MID-COPYTEXT FÖR WL0121                 
002100           07 REQU-IDKOLLI   PIC 9(5).                                    
002200*                                 KOLLINUMMER                             
002300           07 REQU-KDKOLLI   PIC X(8).                                    
002400*                                 KOLLIKOD                                
002500           07 REQU-VKORDBTO-KOLLI                                         
002600                             PIC X(8).                                    
002700*                                 ORDERVIKT BRUTTO PER KOLLI              
002800           07 REQU-KDEMBTYP  PIC 9.                                       
002900*                                 EMBALLAGETYP                            
003000           07 REQU-DIKOLLIL  PIC 9(4).                                    
003100*                                 KOLLI-LÄNGD                             
003200           07 REQU-DIKOLLIB  PIC 9(3).                                    
003300*                                 KOLLI-BREDD                             
003400           07 REQU-DIKOLLIH  PIC 9(3).                                    
003500*                                 KOLLI-HÖJD                              
003600           07 REQU-FLAVSP    PIC X.                                       
003700*                                 ALLMÄN FELFLAGGA                        
003800*** END OF VILMAII-COPY LENGTH= 655 BYTES                                 
