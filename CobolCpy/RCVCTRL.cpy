000100* GENERATION OF COBOL HOST STRUCTURE FROM RCVCTRL-TAB                     
000200  01 RCVCTRL.                                                             
000300*              ALL REPORTED COMPANYS SALES VALUES                         
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 DAFSGVV-FIRST                     PIC S9(7) COMP-3.                 
000700*              FÖRSTA FÖRSÄLJNINGSVECKA ARTIKEL                           
000800   03 DAFSGVV-LAST                      PIC S9(7) COMP-3.                 
000900*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
001000   03 IDDSN                             PIC X(44).                        
001100*              DATASET NAMN                                               
001200   03 DADATTID                          PIC X(14).                        
001300   03 SURCVREC                          PIC S9(9) COMP-3.                 
001400*              SUMMA MOTTAGNA RECANT                                      
001500*              FÖR EN VECKA                                               
001600   03 SURCVREC-REJ                      PIC S9(9) COMP-3.                 
001700*              SUMMA MOTTAGNA FELAKTIGA RECANT                            
001800*              FÖR EN VECKA                                               
001900   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
002000*              VÄRDE TILL SUGGESTED RETAIL                                
002100   03 SURET                             PIC S9(13)V9(2) COMP-3.           
002200*              VÄRDE TILL SUGGESTED RETAIL                                
002300   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
002400*              VÄRDE TILL DEALER NET                                      
002500   03 SULANDCO                          PIC S9(13)V9(2) COMP-3.           
002600*              VÄRDE TILL LANDED COST                                     
002700   03 SUPNET                            PIC S9(13)V9(2) COMP-3.           
002800*              VÄRDE TILL PURCHASE NET                                    
002900   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
003000*              VÄRDE TILL STANDARD LANDING COST                           
003100   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
003200*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
003300   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
003400*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
003500   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
003600*              VÄRDE TILL DEALER NET                                      
003700   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
003800*              VÄRDE TILL DEALER NET                                      
003900   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
004000*              VÄRDE TILL LANDED COST BERÄKNAT                            
004100   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
004200*              VÄRDE TILL LANDED COST BERÄKNAT                            
004300   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
004400*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004500   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
004600*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
004700   03 SUPNET-LOC                        PIC S9(13)V9(2) COMP-3.           
004800*              VÄRDE TILL PURCHASE NET                                    
004900   03 SUBERPNP-LOCSTO                   PIC S9(13)V9(2) COMP-3.           
005000*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
005100   03 SUBERPNP-LOCDAY                   PIC S9(13)V9(2) COMP-3.           
005200*              VÄRDE TILL PURCHASE NET BERÄKNAT                           
005300   03 SURCVREC-IAG1                     PIC S9(9) COMP-3.                 
005400*              SUMMERADE OCH TILLAGDA RECORDS                             
005500*              FÖR AGGREGERINGSNIVÅ 1                                     
005600   03 SURCVREC-UAG1                     PIC S9(9) COMP-3.                 
005700*              SUMMERADE RECORDS VILKA UPPDATERAR                         
005800*              AGGREGERINGS NIVÅ 1                                        
005900   03 SURCVREC-IAG2                     PIC S9(9) COMP-3.                 
006000*              SUMMERADE OCH TILLAGDA RECORDS                             
006100*              FÖR AGGREGERINGSNIVÅ 2                                     
006200   03 SURCVREC-UAG2                     PIC S9(9) COMP-3.                 
006300*              SUMMERADE RECORDS VILKA UPPDATERAR                         
006400*              AGGREGERINGS NIVÅ 2                                        
006500   03 SURCVREC-IAG3                     PIC S9(9) COMP-3.                 
006600*              SUMMERADE OCH TILLAGDA RECORDS                             
006700*              FÖR AGGREGERINGSNIVÅ 3                                     
006800   03 SURCVREC-UAG3                     PIC S9(9) COMP-3.                 
006900*              SUMMERADE RECORDS VILKA UPPDATERAR                         
007000*              AGGREGERINGS NIVÅ 3                                        
007100   03 SURCVREC-IAG4                     PIC S9(9) COMP-3.                 
007200*              SUMMERADE OCH TILLAGDA RECORDS                             
007300*              FÖR AGGREGERINGSNIVÅ 4                                     
007400   03 SURCVREC-UAG4                     PIC S9(9) COMP-3.                 
007500*              SUMMERADE RECORDS VILKA UPPDATERAR                         
007600*              AGGREGERINGS NIVÅ 4                                        
007700*                                                                         
007800*** END OF VILMAII-COPY LENGTH= 254 OLD LENGTH=                           
