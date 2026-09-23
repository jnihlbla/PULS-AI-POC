000100* GENERATION OF COBOL HOST STRUCTURE FROM CWPPF-TAB                       
000200  01 CWPPF.                                                               
000300*              SC PLAN FORECAST PRODGRP AND COUNTRY                       
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 KDTARGTY                          PIC X(1).                         
000700*              TARGET TYPE                                                
000800   03 DAFSGVV                           PIC S9(7) COMP-3.                 
000900*              FÖRSÄLJNINGSVECKA ARTIKEL                                  
001000   03 KDPRODSL                          PIC S9(3) COMP-3.                 
001100*              PRODUKTSLAG                                                
001200   03 FLLOKINK                          PIC X(1).                         
001300*              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
001400*              AV IMPORTÖREN                                              
001500   03 TIAAAA                            PIC S9(5) COMP-3.                 
001600*              ÅRTAL (ÅÅÅÅ)                                               
001700   03 DAAARP                            PIC S9(7) COMP-3.                 
001800*              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
001900*              12 PER ÅR                                                  
002000   03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
002100*              VÄRDE TILL SUGGESTED RETAIL                                
002200   03 SURET                             PIC S9(13)V9(2) COMP-3.           
002300*              VÄRDE TILL SUGGESTED RETAIL                                
002400   03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
002500*              VÄRDE TILL DEALER NET                                      
002600   03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
002700*              VÄRDE TILL DEALER NET                                      
002800   03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
002900*              VÄRDE TILL LANDED COST BERÄKNAT                            
003000   03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
003100*              VÄRDE TILL LANDED COST BERÄKNAT                            
003200   03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
003300*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
003400   03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
003500*              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
003600   03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
003700*              VÄRDE TILL STANDARD LANDING COST                           
003800   03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
003900*              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
004000   03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
004100*              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
004200   03 KVFORC                            PIC S9(9) COMP-3.                 
004300*              FÖRVÄNTAD FÖRSÄLJNING                                      
004400   03 RETOTFSG-STOCK                    PIC S9(3)V9(1) COMP-3.            
004500*              MÅNADSORDER I FÖRH TILL TOTFSG                             
004600   03 RESEAS                            PIC S9(3)V9(2) COMP-3.            
004700*              SÄSONGSINDEX                                               
004800   03 KDVALISO                          PIC X(3).                         
004900*              VALUTAKOD ENLIGT ISO-STANDARD.                             
005000   03 DADATTID                          PIC X(14).                        
005100*                                                                         
005200*** END OF VILMAII-COPY LENGTH= 133 OLD LENGTH=                           
