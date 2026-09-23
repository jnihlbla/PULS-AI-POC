000100 01  W1116A.                                                              
000200*                                 UNDERLAG LISTA KEMI.INSP.               
000300*                                 I RUTIN W111Y1                          
000400     03 IDARTNR              PIC 9(9)                                     
000500                             VALUE ZEROS.                                 
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 FILLER               PIC X                                        
000900                             VALUE ';'.                                   
001000*                                 SEMIKOLON                               
001100*                                 SEMICOLON                               
001200     03 SVEBEN               PIC X(25)                                    
001300                             VALUE SPACES.                                
001400*                                 ARTIKELBENÄMNING                        
001500*                                 PART DESCRIPTION                        
001600     03 FILLER               PIC X                                        
001700                             VALUE ';'.                                   
001800*                                 SEMIKOLON                               
001900*                                 SEMICOLON                               
002000     03 IDANMNR              PIC 9(6)                                     
002100                             VALUE ZEROS.                                 
002200*                                 A-NUMMER                                
002300*                                 A NUMBER                                
002400     03 FILLER               PIC X                                        
002500                             VALUE ';'.                                   
002600*                                 SEMIKOLON                               
002700*                                 SEMICOLON                               
002800     03 IDVARINF             PIC X(4)                                     
002900                             VALUE SPACES.                                
003000*                                 ID VARUINFO KEMISKA PRODUKTER           
003100*                                 ID INFO. CHEMICAL PRODUCTS              
003200     03 FILLER               PIC X                                        
003300                             VALUE ';'.                                   
003400*                                 SEMIKOLON                               
003500*                                 SEMICOLON                               
003600     03 HELTAL               PIC 9(6)                                     
003700                             VALUE ZEROS.                                 
003800     03 PUNKT                PIC X                                        
003900                             VALUE SPACE.                                 
004000     03 DECIMAL              PIC 9(3)                                     
004100                             VALUE ZEROS.                                 
004200     03 FILLER               PIC X                                        
004300                             VALUE ';'.                                   
004400*                                 SEMIKOLON                               
004500*                                 SEMICOLON                               
004600     03 SULEVANT             PIC 9(9)                                     
004700                             VALUE ZEROS.                                 
004800*                                 SUMMA LEVERERAT ANTAL                   
004900*                                 AV 1 ARTIKEL                            
005000*                                 SUMMARY DELIVERED OF AN ITEM            
005100     03 FILLER               PIC X                                        
005200                             VALUE ';'.                                   
005300*                                 SEMIKOLON                               
005400*                                 SEMICOLON                               
005500     03 KDERS                PIC 9(2)                                     
005600                             VALUE ZEROS.                                 
005700*                                 ERSÄTTNINGSKOD                          
005800*                                 SUPERSESSION CODE                       
005900*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
