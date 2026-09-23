000100 01  WSUPERSX.                                                            
000200*                                 A COPY OF W01103 - TO CREATE WX         
000300*                                 TR FILE IN EDITABLE FORMAT              
000400     03 IDARTNR              PIC Z(7)9                                    
000500                             VALUE ZEROS.                                 
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 DIERS-ERS            PIC Z(2)9.9(3)                               
000900                             VALUE ZEROS.                                 
001000*                                 ERSATT ARTIKELANTAL                     
001100*                                 NUMBER OF SUPERSEDED                    
001200     03 IDKORTNR             PIC Z9                                       
001300                             VALUE ZEROS.                                 
001400*                                 KORTNUMMER                              
001500*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001600*                                 SEQUENCE NUMBER FOR EACH RECORD         
001700*                                  IN A SUPERSESSION                      
001800     03 FLTEXT               PIC X                                        
001900                             VALUE SPACE.                                 
002000*                                 FINNS TEXTINFORMATION ?                 
002100     03 IDARTNR-TILLK        PIC Z(7)9                                    
002200                             VALUE ZEROS.                                 
002300*                                 TILLKOMMANDE ARTIKELNUMMER              
002400*                                 REPLACEMENT PART NO.                    
002500     03 DIERS-TILLK          PIC Z(2)9.9(3)                               
002600                             VALUE ZEROS.                                 
002700*                                 TILLKOMMANDE ARTIKELANTAL               
002800*                                 NUMBER OF SUPERSEDING                   
002900     03 BEERS                PIC X(20)                                    
003000                             VALUE SPACES.                                
003100*                                 ERSÄTTNINGSTEXT                         
003200*                                 REPLACEMENT TEXT                        
003300*** END OF VILMAII-COPY LENGTH= 53 BYTES                                  
