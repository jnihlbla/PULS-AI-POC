000100 01  W26123.                                                              
000200*                                 ARTIKLAR MED TIURPROD > 10/15/7         
000300*                                  ÅR                                     
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BEART                PIC X(25).                                   
000700*                                 ARTIKELBENÄMNING                        
000800     03 KVLS                 PIC S9(7)           COMP-3.                  
000900*                                 LAGERSALDO                              
001000     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001100*                                 FUNKTIONSGRUPP                          
001200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001300*                                 PRODUKTSLAG                             
001400     03 IDLEVNR              PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 KDERS                PIC S9(3)           COMP-3.                  
001700*                                 ERSÄTTNINGSKOD                          
001800     03 IDANSK               PIC S9(3)           COMP-3.                  
001900*                                 ANSKAFFARNUMMER                         
002000     03 TIFINLV              PIC S9(5)           COMP-3.                  
002100*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
002200     03 TIURPROD             PIC S9(5)           COMP-3.                  
002300*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
002400     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002500*                                 ARTIKELSTANDARDPRIS                     
002600     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
002700*                                 ANTAL LEV ART RULLANDE ÅR               
002800*                                 (AF2)                                   
002900     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
003000*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
003100     03 SUTOTBV-FRAAR        PIC S9(11)V9(2)     COMP-3.                  
003200*                                 TÄCKNINGSBIDRAG FÖR RULLANDE            
003300*                                 FÖREGÅENDE ÅR (AF1)                     
003400     03 IDINK                PIC X(4).                                    
003500*                                 INKÖPARNUMMER                           
003600     03 KDARTURS             PIC X(2).                                    
003700*                                 ARTIKELURSPRUNGSKOD                     
003800     03 PRARTBTO-MARK        PIC S9(7)V9(2)      COMP-3.                  
003900*                                 BRUTTOPRIS PER MARKNAD (FOB)            
004000     03 FLSATART             PIC X.                                       
004100*                                 ALLMÄN FLAGGA                           
004200     03 VKART                PIC S9(7)           COMP-3.                  
004300*                                 ARTIKELVIKT (G)                         
004400     03 KDSORT               PIC X(2).                                    
004500*                                 SORT-KOD                                
004600     03 KVOI-TOT-CURRENT-YEAR                                             
004700                             PIC X(10).                                   
004800     03 KVOI-TOT-ONE-YEAR-AGO                                             
004900                             PIC X(10).                                   
005000     03 KVSKROT              PIC S9(7)           COMP-3.                  
005100*                                 ANTAL SENASTE SKROTORDER                
005200     03 DASKROT              PIC 9(8).                                    
005300*                                 SKROTNINGSDATUM (ÅÅÅÅMMDD)              
005400     03 KDFARLIG             PIC S9              COMP-3.                  
005500*                                 KOD FÖR FARLIGT GODS                    
005600*** END OF VILMAII-COPY LENGTH= 129 BYTES                                 
