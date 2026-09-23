000100 01  MID-W2I32101.                                                        
000200     03 MID-SPAR-FAELT.                                                   
000300*                                       SPARADE NYCKLAR                   
000400        05 MID-IDUSER-SPAR   PIC X(8).                                    
000500*                                 ANVÄNDARENS SÄKERHETS ID                
000600        05 MID-TIREGDAT-SPAR PIC 9(6).                                    
000700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000800        05 MID-TIREGTID-SPAR PIC 9(6).                                    
000900*                                 REGISTRERINGSTID                        
001000     03 MID-INPUT.                                                        
001100*                                 RADINFORMATION                          
001200        05 MID-IDANSK-FOM    PIC X(3).                                    
001300*                                 ANSKAFFARNUMMER                         
001400        05 MID-IDANSK-TOM    PIC X(3).                                    
001500*                                 ANSKAFFARNUMMER                         
001600        05 MID-KDSORT1       PIC X.                                       
001700*                                 SORTERINGSKOD                           
001800        05 MID-IDLEVNR       PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000        05 MID-KDPRODSL      PIC X(2).                                    
002100*                                 PRODUKTSLAG                             
002200        05 MID-IDDISTR-FOM   PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400        05 MID-IDDISTR-TOM   PIC X(4).                                    
002500*                                 DISTRIKTNUMMER                          
002600        05 MID-KDBASLM-FOM   PIC X(6).                                    
002700*                                 BASLAGERMARKNAD                         
002800        05 MID-KDBASLM-TOM   PIC X(6).                                    
002900*                                 BASLAGERMARKNAD                         
003000        05 MID-KDTPOTYP-FOM  PIC X.                                       
003100*                                 TYP AV TIDPLANERAD ORDER                
003200        05 MID-KDTPOTYP-TOM  PIC X.                                       
003300*                                 TYP AV TIDPLANERAD ORDER                
003400        05 MID-TITPO-FOM     PIC X(4).                                    
003500*                                 ÅR - VECKA  (ÅÅVV)                      
003600        05 MID-TITPO-TOM     PIC X(4).                                    
003700*                                 ÅR - VECKA  (ÅÅVV)                      
003800     03 MID-IDARTNR-GRP.                                                  
003900*                                       ARTIKEL-GRUPP                     
004000        05 MID-IDARTNR       OCCURS 42 TIMES                              
004100                             PIC X(9).                                    
004200*                                 ARTIKELNUMMER                           
004300*** END COPY W2I32101C0  LENGTH=442                                       
