000100* GENERATION OF COBOL HOST STRUCTURE FROM TP3HUNT-TAB                     
000200  01 TP3HUNT.                                                             
000300*              TP3HUNT                                                    
000400   03 IDPGM                             PIC X(8).                         
000500*              PROGRAM IDENTITET                                          
000600   03 IDLOPNR                           PIC S9(7) COMP-3.                 
000700*              LÖPNUMMER                                                  
000800   03 IDDC                              PIC X(2).                         
000900*              IDENTIFIERARE LAGER                                        
001000   03 IDDISTR                           PIC S9(5) COMP-3.                 
001100*              DISTRIKTNUMMER                                             
001200   03 IDKUNDNR                          PIC S9(7) COMP-3.                 
001300*              KUNDNUMMER                                                 
001400   03 IDORDNR7                          PIC X(7).                         
001500*              ORDERNUMMER                                                
001600   03 DARFS                             PIC X(12).                        
001700*              KLART FÖR TRANSPORT                                        
001800   03 TIRODAT                           PIC S9(7) COMP-3.                 
001900*              RESTORDERDATUM         (ÅÅMMDD)                            
002000   03 BERADREF                          PIC X(10).                        
002100*              KUNDENS RADREFERENS                                        
002200   03 IDARTNR                           PIC S9(9) COMP-3.                 
002300*              ARTIKELNUMMER                                              
002400   03 BEART-ENG                         PIC X(25).                        
002500*              ENGELSK ARTIKELBENÄMNING                                   
002600   03 KVART                             PIC S9(7) COMP-3.                 
002700*              ANTAL ARTNR PER BRYTBEGREPP                                
002800   03 KVRO                              PIC S9(7) COMP-3.                 
002900*              ANTAL RESTNOTERADE ARTIKLAR                                
003000   03 IDANSK                            PIC S9(3) COMP-3.                 
003100*              ANSKAFFARNUMMER                                            
003200   03 TIREGDAT                          PIC S9(7) COMP-3.                 
003300*              REGISTRERINGSDATUM (ÅÅMMDD)                                
003400   03 TIREGTID                          PIC S9(7) COMP-3.                 
003500*              REGISTRERINGSTID                                           
003600   03 TIREPDAT                          PIC S9(7) COMP-3.                 
003700*              REPAIR DATE                                                
003800*                                                                         
003900*** END OF VILMAII-COPY LENGTH= 106 OLD LENGTH=                           
