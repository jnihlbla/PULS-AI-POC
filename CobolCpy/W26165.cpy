000100 01  W26165.                                                              
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
001800     03 TISLUTKP             PIC S9(7)           COMP-3.                  
001900*                                 DÅ SLUTKÖPSDATUM BÖRJAR GÄLLA           
002000     03 KVLS                 PIC S9(7)           COMP-3.                  
002100*                                 LAGERSALDO                              
002200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002300*                                 ARTIKELSTANDARDPRIS                     
002400     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
002500*                                 SLUTKÖPSSALDO                           
002600     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
002700*                                 ANTAL LEV ART RULLANDE ÅR               
002800*                                 (AF2)                                   
002900     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
003000*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
003100     03 SUTOTBV-FRAAR        PIC S9(11)V9(2)     COMP-3.                  
003200*                                 TÄCKNINGSBIDRAG FÖR RULLANDE            
003300*                                 FÖREGÅENDE ÅR (AF1)                     
003400     03 BEART                PIC X(25).                                   
003500*                                 ARTIKELBENÄMNING                        
003600     03 KDERS                PIC S9(3)           COMP-3.                  
003700*                                 ERSÄTTNINGSKOD                          
003800     03 FLAGGA15             PIC X.                                       
003900*                                 ALLMÄN FLAGGA                           
004000     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
004100*                                 LAGEROMRÅDE                             
004200     03 ADPLATS              PIC S9(5)           COMP-3.                  
004300*                                 LAGERPLATSNUMMER                        
004400*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
