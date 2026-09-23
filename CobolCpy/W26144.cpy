000100 01  W26144.                                                              
000200*                                 VISSA ARTIKLAR MED TIURPROD > 1         
000300*                                 0 ÅR KOMPLETERADE                       
000400     03 IDANSK               PIC S9(3)           COMP-3.                  
000500*                                 ANSKAFFARNUMMER                         
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001100*                                 FUNKTIONSGRUPP                          
001200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001300*                                 PRODUKTSLAG                             
001400     03 TIURPROD             PIC S9(5)           COMP-3.                  
001500*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
001600     03 FLFKNPRI             PIC X.                                       
001700*                                 FLAGGA=FKNGRP VIKTIG                    
001800     03 KVLS                 PIC S9(7)           COMP-3.                  
001900*                                 LAGERSALDO                              
002000     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002100*                                 ARTIKELSTANDARDPRIS                     
002200     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
002300*                                 ANTAL LEV ART RULLANDE ÅR               
002400*                                 (AF2)                                   
002500     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
002600*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
002700     03 SUTOTBV-FRAAR        PIC S9(11)V9(2)     COMP-3.                  
002800*                                 TÄCKNINGSBIDRAG FÖR RULLANDE            
002900*                                 FÖREGÅENDE ÅR (AF1)                     
003000     03 KDERS                PIC S9(3)           COMP-3.                  
003100*                                 ERSÄTTNINGSKOD                          
003200     03 FLAGGA15             PIC X.                                       
003300*                                 ALLMÄN FLAGGA                           
003400     03 TISKPREL             PIC S9(5)           COMP-3.                  
003500*                                 PREL. SKROTNINGSDATUM (AAVV)            
003600*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
