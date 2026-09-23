000100 01  W22530.                                                              
000200*                                 LISTURVAL TPO 1-6                       
000300*                                                                         
000400     03 URVALS-ID.                                                        
000500*                                 IDENTIFIERING AV URVAL                  
000600*                                 OBS DENNA GRUPP ANVÄNDS I               
000700*                                 FLERA COPYTEXTER                        
000800        05 IDUSER            PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000        05 TIREGDAT          PIC S9(7)           COMP-3.                  
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200        05 TIREGTID          PIC S9(7)           COMP-3.                  
001300*                                 REGISTRERINGSTID                        
001400     03 IDANSK-FOM           PIC S9(3)           COMP-3.                  
001500*                                 ANSKAFFARNUMMER                         
001600     03 IDANSK-TOM           PIC S9(3)           COMP-3.                  
001700*                                 ANSKAFFARNUMMER                         
001800     03 KDSORT1              PIC S9              COMP-3.                  
001900*                                 SORTERINGSKOD                           
002000     03 IDLEVNR              PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002300*                                 PRODUKTSLAG                             
002400     03 IDDISTR-FOM          PIC S9(5)           COMP-3.                  
002500*                                 DISTRIKTNUMMER                          
002600     03 IDDISTR-TOM          PIC S9(5)           COMP-3.                  
002700*                                 DISTRIKTNUMMER                          
002800     03 KDTPOTYP-FOM         PIC S9              COMP-3.                  
002900*                                 TYP AV TIDPLANERAD ORDER                
003000     03 KDTPOTYP-TOM         PIC S9              COMP-3.                  
003100*                                 TYP AV TIDPLANERAD ORDER                
003200     03 TITPO-FOM            PIC S9(7)           COMP-3.                  
003300*                                 PLANERAD ORDERDATUM                     
003400     03 TITPO-TOM            PIC S9(7)           COMP-3.                  
003500*                                 PLANERAD ORDERDATUM                     
003600     03 IDARTNR              OCCURS 100 TIMES                             
003700                             PIC S9(9)           COMP-3.                  
003800*                                 ARTIKELNUMMER                           
003900*** END OF VILMAII-COPY LENGTH= 544 BYTES                                 
