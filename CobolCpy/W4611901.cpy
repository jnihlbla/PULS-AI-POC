000100 01  W4611901.                                                            
000200*                                 NOAC                                    
000300*                                 ARTIKELINFO TILL W461V1                 
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 REKSIFFR             PIC S9              COMP-3.                  
000700*                                 KONTROLLSIFFRA                          
000800     03 KDSORT               PIC X(2).                                    
000900*                                 SORT-KOD                                
001000     03 VKART                PIC S9(7)           COMP-3.                  
001100*                                 ARTIKELVIKT (G)                         
001200     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
001300*                                 ARTIKELVOLYM NETTO (CM3)                
001400     03 KDVSOP               PIC S9(3)           COMP-3.                  
001500*                                 VSOP-KOD                                
001600     03 KDBPSR               PIC S9              COMP-3.                  
001700*                                 BASLAGERFÖRSLAGSNIVÅ                    
001800     03 PRARTBTO-EXP         PIC S9(7)V9(2)      COMP-3.                  
001900*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
002000     03 TIERSDAT             PIC S9(5)           COMP-3.                  
002100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
002200     03 TIFINLV              PIC S9(5)           COMP-3.                  
002300*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
002400     03 IDLEVNR              PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600     03 IDLKTO               PIC S9(7)           COMP-3.                  
002700*                                 LAGERKONTO (FFHHHUU)                    
002800     03 FLLSRDEL             PIC X.                                       
002900*                                 LEVERERAS SOM RESDEL                    
003000     03 KDUART               PIC X.                                       
003100*                                 UNDANTAGSARTIKEL                        
003200     03 KDLTK                PIC S9              COMP-3.                  
003300*                                 LAGERTILLHÖRIGHETSKOD                   
003400     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003500*                                 FUNKTIONSGRUPP                          
003600     03 KDPRODSL             PIC S9(3)           COMP-3.                  
003700*                                 PRODUKTSLAG                             
003800     03 KDERS                PIC S9(3)           COMP-3.                  
003900*                                 ERSÄTTNINGSKOD                          
004000     03 KDARTURS             PIC X(2).                                    
004100*                                 ARTIKELURSPRUNGSKOD                     
004200     03 KDSRA                PIC S9(3)           COMP-3.                  
004300*                                 SRA-KOD                                 
004400     03 IDSTATNR             OCCURS 6 TIMES                               
004500                             PIC S9(9)           COMP-3.                  
004600*                                 STATISTISKT NUMMER                      
004700*                                 1 = NORSKT                              
004800*                                 2 = ENGELSKT                            
004900*                                 3 = BELGISKT                            
005000*                                 4 = PERUANSKT                           
005100*                                 5 = SVENSKT                             
005200*                                 6 =                                     
005300     03 IDSKYLT              OCCURS 10 TIMES                              
005400                             PIC X(3).                                    
005500*                                 NATIONALITETSTECKEN                     
005600*                                 SPRÅKIDENTIFIKATION                     
005700     03 BEART                OCCURS 10 TIMES                              
005800                             PIC X(25).                                   
005900*                                 ARTIKELBENÄMNING                        
006000     03 KDBBCL               PIC 9.                                       
006100*                                 RETURNERBAR ARTIKEL                     
006200     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
006300*                                 ANTAL KVANTITETFÖRPACKNINGAR            
006400     03 KDARTRAB-A           PIC 9(2).                                    
006500*                                 RABATTKOD (ARTIKELPRIS)                 
006600     03 PRARTBTO-MARK-A      PIC S9(7)V9(2)      COMP-3.                  
006700*                                 BRUTTOPRIS PER MARKNAD (FOB)            
006800     03 KDARTRAB-B           PIC 9(2).                                    
006900*                                 RABATTKOD (ARTIKELPRIS)                 
007000     03 PRARTBTO-MARK-B      PIC S9(7)V9(2)      COMP-3.                  
007100*                                 BRUTTOPRIS PER MARKNAD (FOB)            
007200     03 KDARTRAB-C           PIC 9(2).                                    
007300*                                 RABATTKOD (ARTIKELPRIS)                 
007400     03 PRARTBTO-MARK-C      PIC S9(7)V9(2)      COMP-3.                  
007500*                                 BRUTTOPRIS PER MARKNAD (FOB)            
007600     03 KDARTRAB-D           PIC 9(2).                                    
007700*                                 RABATTKOD (ARTIKELPRIS)                 
007800     03 PRARTBTO-MARK-D      PIC S9(7)V9(2)      COMP-3.                  
007900*                                 BRUTTOPRIS PER MARKNAD (FOB)            
008000     03 KDARTRAB-E           PIC 9(2).                                    
008100*                                 RABATTKOD (ARTIKELPRIS)                 
008200     03 PRARTBTO-MARK-E      PIC S9(7)V9(2)      COMP-3.                  
008300*                                 BRUTTOPRIS PER MARKNAD (FOB)            
008400     03 KDARTRAB-F           PIC 9(2).                                    
008500*                                 RABATTKOD (ARTIKELPRIS)                 
008600     03 PRARTBTO-MARK-F      PIC S9(7)V9(2)      COMP-3.                  
008700*                                 BRUTTOPRIS PER MARKNAD (FOB)            
008800     03 KDARTRAB-G           PIC 9(2).                                    
008900*                                 RABATTKOD (ARTIKELPRIS)                 
009000     03 PRARTBTO-MARK-G      PIC S9(7)V9(2)      COMP-3.                  
009100*                                 BRUTTOPRIS PER MARKNAD (FOB)            
009200     03 KDAGE                PIC X.                                       
009300*                                 AGE-CODE                                
009400*** END OF VILMAII-COPY LENGTH= 418 BYTES                                 
