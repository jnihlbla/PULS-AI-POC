000100 01  W22532.                                                              
000200*                                 MATCHANDE TPO OCH URVAL                 
000300     03 URVALS-ID.                                                        
000400*                                 IDENTIFIERING AV URVAL                  
000500*                                 OBS DENNA GRUPP ANVÄNDS I               
000600*                                 FLERA COPYTEXTER                        
000700        05 IDUSER            PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900        05 TIREGDAT          PIC S9(7)           COMP-3.                  
001000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001100        05 TIREGTID          PIC S9(7)           COMP-3.                  
001200*                                 REGISTRERINGSTID                        
001300     03 URVAL.                                                            
001400*                                 URVAL                                   
001500        05 URV-IDURVNR       PIC S9(2).                                   
001600        05 URV-IDANSK-FOM    PIC S9(3)           COMP-3.                  
001700*                                 ANSKAFFARNUMMER                         
001800        05 URV-IDANSK-TOM    PIC S9(3)           COMP-3.                  
001900*                                 ANSKAFFARNUMMER                         
002000        05 URV-KDSORT1       PIC S9              COMP-3.                  
002100*                                 SORTERINGSKOD                           
002200        05 URV-IDLEVNR       PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400        05 URV-KDPRODSL      PIC S9(3)           COMP-3.                  
002500*                                 PRODUKTSLAG                             
002600        05 URV-IDDISTR-FOM   PIC S9(5)           COMP-3.                  
002700*                                 DISTRIKTNUMMER                          
002800        05 URV-IDDISTR-TOM   PIC S9(5)           COMP-3.                  
002900*                                 DISTRIKTNUMMER                          
003000        05 URV-KDBASLM-FOM   PIC X(6).                                    
003100*                                 BASLAGERMARKNAD                         
003200        05 URV-KDBASLM-TOM   PIC X(6).                                    
003300*                                 BASLAGERMARKNAD                         
003400        05 URV-KDTPOTYP-FOM  PIC S9              COMP-3.                  
003500*                                 TYP AV TIDPLANERAD ORDER                
003600        05 URV-KDTPOTYP-TOM  PIC S9              COMP-3.                  
003700*                                 TYP AV TIDPLANERAD ORDER                
003800        05 URV-TITPO-FOM     PIC S9(5)           COMP-3.                  
003900*                                 ÅR - VECKA  (ÅÅVV)                      
004000        05 URV-TITPO-TOM     PIC S9(5)           COMP-3.                  
004100*                                 ÅR - VECKA  (ÅÅVV)                      
004200        05 URV-IDARTNR       OCCURS 100 TIMES                             
004300                             PIC S9(9)           COMP-3.                  
004400*                                 ARTIKELNUMMER                           
004500     03 IDANSK               PIC S9(3)           COMP-3.                  
004600*                                 ANSKAFFARNUMMER                         
004700     03 IDLEVNR              PIC X(5).                                    
004800*                                 LEVERANTÖRNUMMER                        
004900     03 IDARTNR              PIC S9(9)           COMP-3.                  
005000*                                 ARTIKELNUMMER                           
005100     03 KDTPOTYP             PIC S9              COMP-3.                  
005200*                                 TYP AV TIDPLANERAD ORDER                
005300     03 KVART                PIC S9(7)           COMP-3.                  
005400*                                 ANTAL ARTNR PER BRYTBEGREPP             
005500     03 TITPO                PIC S9(5)           COMP-3.                  
005600*                                 ÅR - VECKA  (ÅÅVV)                      
005700     03 IDDISTR              PIC S9(5)           COMP-3.                  
005800*                                 DISTRIKTNUMMER                          
005900     03 KDBASLM              PIC X(6).                                    
006000*                                 BASLAGERMARKNAD                         
006100     03 IDORDER              PIC S9(7)           COMP-3.                  
006200*                                 VOLVO PARTS ORDERNUMMER                 
006300*** END OF VILMAII-COPY LENGTH= 589 BYTES                                 
