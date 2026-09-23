000100 01  W480W001.                                                            
000200*                                 INFORMATION FRÅN W480AA                 
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 TIFINLV              PIC S9(5)           COMP-3.                  
000700*                                 PUBLICERINGSVECKA                       
000800     03 FLABORT-UTG          PIC S9              COMP-3.                  
000900*                                 MÄRKNING ATT ARTIKELN UTGÅTT            
001000     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001100*                                 FUNKTIONSGRUPP                          
001200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001300*                                 PRODUKTSLAG                             
001400     03 VKART                PIC S9(7)           COMP-3.                  
001500*                                 ARTIKELVIKT (G)                         
001600     03 KDCLPOST             PIC S9              COMP-3.                  
001700*                                 CENTRALLAGERPOST                        
001800     03 BEART-SVE            PIC X(25).                                   
001900*                                 ARTIKELBENÄMNING                        
002000     03 IDLKTO               PIC S9(7).                                   
002100*                                 LAGERKONTO                              
002200     03 FILLER REDEFINES IDLKTO.                                          
002300        05 FILLER            PIC 9(2).                                    
002400        05 HUVKTO            PIC 9(3).                                    
002500*                                 HUVUDKONTO                              
002600        05 FILLER            PIC S9(2).                                   
002700     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
002800*                                 ARTIKELNS SJÄLVKOSTNAD                  
002900     03 PRARTBTO-SVE         PIC S9(7)V9(2)      COMP-3.                  
003000*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003100     03 REOMRTAL-MO          PIC S9(2)V9(3)      COMP-3.                  
003200*                                 OMRÄKNINGSTAL KVANTORDER                
003300     03 MOPRIS               PIC S9(7)V9(2)      COMP-3.                  
003400*                                 PRARTBTO-SVE   REOMRTAL-MO              
003500     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003600*                                 ARTIKELSTANDARDPRIS                     
003700     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
003800*                                 SATS-PERIODBEHOV                        
003900     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
004000*                                 SEPARAT PERIODBEHOV                     
004100     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
004200*                                 ANTAL KVANTITETFÖRPACKNINGAR            
004300     03 ADLAGOMR             PIC S9              COMP-3.                  
004400*                                 LAGEROMRÅDE        ADLAGOMR-002         
004500     03 ADPLATS              PIC S9(5)           COMP-3.                  
004600*                                 LAGERPLATSNUMMER                        
004700     03 KVAKS                PIC S9(7)           COMP-3.                  
004800*                                 ANKOMSTSALDO                            
004900     03 KVLS                 PIC S9(7)           COMP-3.                  
005000*                                 LAGERSALDO                              
005100     03 FLLSRDEL             PIC X.                                       
005200*                                 LEVERERAS SOM RESDEL                    
005300*** END COPY W480W001C0  LENGTH=98                                        
