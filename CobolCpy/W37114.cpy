000100 01  W37114.                                                              
000200*                                 KOPPLINGSTRANS MELLAN BYTES             
000300*                                 OCH EKONOMISYSTEM                       
000400*                                 (GODKÄNDA BYTES-RETURER)                
000500*                                                                         
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
001500*                                 OBJEKTNUMMER                            
001600     03 IDLKTO               PIC S9(7)           COMP-3.                  
001700*                                 LAGERKONTO (FFHHHUU)                    
001800     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001900*                                 ARTIKELSTANDARDPRIS                     
002000     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
002100*                                 ARTIKELNS SJÄLVKOSTNAD                  
002200     03 KVRETUR              PIC S9(5)           COMP-3.                  
002300*                                 ANTAL I RETUR                           
002400*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
