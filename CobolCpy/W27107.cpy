000100 01  W27107.                                                              
000200*                                 RESTORDER MER ÄN 6 VECKOR GAMML         
000300*                                 A                                       
000400     03 IDPERSON             PIC Z(4)9                                    
000500                             VALUE ZEROS.                                 
000600     03 HORIZTAB             PIC X                                        
000700                             VALUE X'05'.                                 
000800*                                 HORIZTAB                                
000900*                                 HORIZTAB                                
001000     03 IDARTNR              PIC Z(8)9                                    
001100                             VALUE ZEROS.                                 
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 HORIZTAB             PIC X                                        
001500                             VALUE X'05'.                                 
001600*                                 HORIZTAB                                
001700*                                 HORIZTAB                                
001800     03 TOTALSUM             PIC Z(11)9                                   
001900                             VALUE ZEROS.                                 
002000     03 HORIZTAB             PIC X                                        
002100                             VALUE X'05'.                                 
002200*                                 HORIZTAB                                
002300*                                 HORIZTAB                                
002400     03 PRARTSTD             PIC Z(6)9.9(2)                               
002500                             VALUE ZEROS.                                 
002600*                                 ARTIKELSTANDARDPRIS                     
002700*                                 STANDARD PRICE                          
002800     03 HORIZTAB             PIC X                                        
002900                             VALUE X'05'.                                 
003000*                                 HORIZTAB                                
003100*                                 HORIZTAB                                
003200     03 KVPB-REF             PIC Z(5)9.9                                  
003300                             VALUE ZEROS.                                 
003400*                                 PERIODBEHOV REFILLING                   
003500*                                 FORECAST REFILLING                      
003600     03 HORIZTAB             PIC X                                        
003700                             VALUE X'05'.                                 
003800*                                 HORIZTAB                                
003900*                                 HORIZTAB                                
004000     03 AVAILABLE            PIC -(7)9                                    
004100                             VALUE ZEROS.                                 
004200     03 HORIZTAB             PIC X                                        
004300                             VALUE X'05'.                                 
004400*                                 HORIZTAB                                
004500*                                 HORIZTAB                                
004600     03 IDDC                 PIC X(2)                                     
004700                             VALUE SPACES.                                
004800*                                 IDENTIFIERARE LAGER                     
004900*                                 WAREHOUSE IDENTIFIER                    
005000     03 HORIZTAB             PIC X                                        
005100                             VALUE X'05'.                                 
005200*                                 HORIZTAB                                
005300*                                 HORIZTAB                                
005400*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
