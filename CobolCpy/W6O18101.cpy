000100 01  MOD-W6O18101.                                                        
000200*                                 MOD FÖR PROGRAM W60181                  
000300*                                 PROGRAMMET VISAR OCH UPPDATERAR         
000400*                                 FÖRPACKNINGSTYP                         
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDDC-UT          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-BEART-UT         PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900     03 MOD-BEFT-UT          PIC Z9.                                      
002000*                                 FÖRPACKNINGSTYP                         
002100     03 MOD-KDFORP-UT.                                                    
002200*                                 FÖRPACKNINGSKOD                         
002300        05 MOD-KDFORPPL      PIC 9.                                       
002400*                                 FÖRPACKNINGSPLATS                       
002500        05 MOD-KDFORPGP      PIC 9(2).                                    
002600*                                 FÖRPACKNINGSGRUPP                       
002700        05 MOD-KDFORPUF      PIC 9.                                       
002800*                                 UPPRÄKNINGSFAKTOR                       
002900     03 MOD-IDUSER-UT        PIC X(8).                                    
003000*                                 ANVÄNDARENS SÄKERHETS ID                
003100     03 MOD-TIREGDAT-UT      PIC X(6).                                    
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD OR Å         
003300*                                 ÅÅÅ-MM-DD)                              
003400     03 MOD-TEBEFT-UT        PIC X(40).                                   
003500*                                 TEXT FÖRPACKNINGSINSTRUKTION            
003600     03 MOD-TEBEFT-79-UT     PIC X(79).                                   
003700     03 MOD-TEBEFT02-79-UT   PIC X(79).                                   
003800     03 MOD-BEFT-IN-ATTR     PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-BEFT-IN          PIC Z9.                                      
004100*                                 FÖRPACKNINGSTYP                         
004200     03 MOD-KDFORP-IN-ATTR   PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-KDFORP-IN.                                                    
004500*                                 FÖRPACKNINGSKOD                         
004600        05 MOD-KDFORPPL      PIC 9.                                       
004700*                                 FÖRPACKNINGSPLATS                       
004800        05 MOD-KDFORPGP      PIC 9(2).                                    
004900*                                 FÖRPACKNINGSGRUPP                       
005000        05 MOD-KDFORPUF      PIC 9.                                       
005100*                                 UPPRÄKNINGSFAKTOR                       
005200     03 MOD-TEBEFT-IN-ATTR   PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-TEBEFT-IN        PIC X(40).                                   
005500*                                 TEXT FÖRPACKNINGSINSTRUKTION            
005600     03 MOD-TEBEFT-79-IN-ATTR                                             
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-TEBEFT-79-IN     PIC X(79).                                   
006000     03 MOD-TEBEFT02-79-IN-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-TEBEFT02-79-IN   PIC X(79).                                   
006400     03 MOD-W6O18101-GRP     OCCURS 2 TIMES.                              
006500*                                 RADER SOM VISAR HISTORIK PÅ             
006600*                                 FÖRPACKNINGSINSTRUKTIONER               
006700        05 MOD-BEFT-HIST     PIC Z9.                                      
006800*                                 FÖRPACKNINGSTYP                         
006900        05 MOD-KDFORP-HIST.                                               
007000*                                 FÖRPACKNINGSKOD                         
007100           07 MOD-KDFORPPL   PIC 9.                                       
007200*                                 FÖRPACKNINGSPLATS                       
007300           07 MOD-KDFORPGP   PIC 9(2).                                    
007400*                                 FÖRPACKNINGSGRUPP                       
007500           07 MOD-KDFORPUF   PIC 9.                                       
007600*                                 UPPRÄKNINGSFAKTOR                       
007700        05 MOD-IDUSER-HIST   PIC X(8).                                    
007800*                                 ANVÄNDARENS SÄKERHETS ID                
007900        05 MOD-TIREGDAT-HIST PIC X(6).                                    
008000*                                 REGISTRERINGSDATUM (ÅÅMMDD OR Å         
008100*                                 ÅÅÅ-MM-DD)                              
008200        05 MOD-TEBEFT-HIST   PIC X(40).                                   
008300*                                 TEXT FÖRPACKNINGSINSTRUKTION            
008400        05 MOD-TEBEFT-79-HIST                                             
008500                             PIC X(79).                                   
008600        05 MOD-TEBEFT02-79-HIST                                           
008700                             PIC X(79).                                   
008800     03 MOD-TEMFSINF         PIC X(55).                                   
008900*                                 INFORMATIONSMEDDELANDE                  
009000*** END OF VILMAII-COPY LENGTH= 1014 BYTES                                
