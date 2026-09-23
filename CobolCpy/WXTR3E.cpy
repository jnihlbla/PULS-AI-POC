000100 01  WXTR3E-CTX.                                                          
000200*                                 PRIMARY EXTRACT                         
000300*                                                                         
000400*                                 PRICE CHECK (PP95)                      
000500*                                                                         
000600*                                 CREATED PER ORDERLINE WHEN              
000700*                                 EMPTYING TRANSACTIONS                   
000800*                                 REGARDING PRICE-ERRORS                  
000900*                                                                         
001000     03 IDPTYP               PIC X(3).                                    
001100*                                 RECORD TYPE                             
001200     03 IDMARKBO             PIC X.                                       
001300*                                 MARKET COMPANY CODE                     
001400*                                 A = VCS                                 
001500*                                 B = VCEM                                
001600*                                 C = NORDIC WITHOUT SWEDEN               
001700*                                 D = VCUK                                
001800*                                 E = VCNA                                
001900*                                 F = VCI                                 
002000*                                 G = VCAS                                
002100     03 IDDISTR              PIC S9(5)           COMP-3.                  
002200*                                 DISTRICT NUMBER                         
002300     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
002400*                                 CUSTOMER NO                             
002500     03 IDORDER              PIC S9(7)           COMP-3.                  
002600*                                 VOLVO PARTS ORDER NUMBER                
002700     03 KDORDKL              PIC S9              COMP-3.                  
002800*                                 ORDER CLASS                             
002900     03 KDFAKTYP             PIC X.                                       
003000*                                 INVOICE TYPE                            
003100     03 IDARTNR              PIC S9(9)           COMP-3.                  
003200*                                 PART NUMBER                             
003300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
003400*                                 PRODUCT GROUP                           
003500     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003600*                                 FUNCTION GROUP                          
003700     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003800*                                 NET PRICE EACH   (FOB NET)              
003900     03 KVBEART              PIC S9(7)           COMP-3.                  
004000*                                 ORDERED QUANTITY                        
004100     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
004200*                                 COST OF SALES                           
004300     03 TIREGDAT             PIC S9(7)           COMP-3.                  
004400*                                 REGISTRATION DATE (YYMMDD)              
004500     03 TIRODAT              PIC S9(7)           COMP-3.                  
004600*                                 BACK ORDER DATE        (YYMMDD)         
004700     03 PRBPRIS              PIC S9(7)V9(2)      COMP-3.                  
004800     03 IDFELKOD             PIC X(3).                                    
004900*                                 ERROR CODE                              
005000*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
