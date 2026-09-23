000100 01  MAIL-WF106202.                                                       
000200*                                 MAIL-HEADER FOR PGM WF1062              
000300     03 MAIL-BETEXT-07       PIC X(20)                                    
000400                             VALUE SPACES.                                
000500     03 MAIL-SEMICOLON       PIC X                                        
000600                             VALUE ';'.                                   
000700*                                 SEMIKOLON                               
000800*                                 SEMICOLON                               
000900     03 MAIL-SUNTO-MAX       PIC Z(10)9.9(2)                              
001000                             VALUE ZEROS.                                 
001100*                                 TOTAL SALES AMOUNT EXCL. VAT            
001200     03 MAIL-SEMICOLON       PIC X                                        
001300                             VALUE ';'.                                   
001400*                                 SEMIKOLON                               
001500*                                 SEMICOLON                               
001600     03 MAIL-BETEXT-08       PIC X(20)                                    
001700                             VALUE SPACES.                                
001800     03 MAIL-SEMICOLON       PIC X                                        
001900                             VALUE ';'.                                   
002000*                                 SEMIKOLON                               
002100*                                 SEMICOLON                               
002200     03 MAIL-FLSOFT          PIC X                                        
002300                             VALUE SPACE.                                 
002400*                                 FLAGGA SOFTVARA                         
002500*                                 SOFTWARE MARK                           
002600     03 MAIL-SEMICOLON       PIC X                                        
002700                             VALUE ';'.                                   
002800*                                 SEMIKOLON                               
002900*                                 SEMICOLON                               
003000     03 MAIL-BETEXT-09       PIC X(20)                                    
003100                             VALUE SPACES.                                
003200     03 MAIL-SEMICOLON       PIC X                                        
003300                             VALUE ';'.                                   
003400*                                 SEMIKOLON                               
003500*                                 SEMICOLON                               
003600     03 MAIL-FLFREE          PIC X                                        
003700                             VALUE SPACE.                                 
003800*                                 GRATISFATURA                            
003900*                                 FREE INVOICE                            
004000     03 MAIL-SEMICOLON       PIC X                                        
004100                             VALUE ';'.                                   
004200*                                 SEMIKOLON                               
004300*                                 SEMICOLON                               
004400     03 MAIL-BETEXT-10       PIC X(20)                                    
004500                             VALUE SPACES.                                
004600     03 MAIL-SEMICOLON       PIC X                                        
004700                             VALUE ';'.                                   
004800*                                 SEMIKOLON                               
004900*                                 SEMICOLON                               
005000     03 MAIL-FLSERV          PIC X                                        
005100                             VALUE SPACE.                                 
005200*                                 FLAGGA SERVICE                          
005300*                                 SERVICE FLAG                            
005400     03 MAIL-SEMICOLON       PIC X                                        
005500                             VALUE ';'.                                   
005600*                                 SEMIKOLON                               
005700*                                 SEMICOLON                               
005800     03 MAIL-BETEXT-11       PIC X(20)                                    
005900                             VALUE SPACES.                                
006000     03 MAIL-SEMICOLON       PIC X                                        
006100                             VALUE ';'.                                   
006200*                                 SEMIKOLON                               
006300*                                 SEMICOLON                               
006400     03 MAIL-FLINVOIC        PIC X                                        
006500                             VALUE SPACE.                                 
006600*                                 FLAGGA INVOICE                          
006700*                                 INVOICE DLAG                            
006800     03 MAIL-SEMICOLON       PIC X                                        
006900                             VALUE ';'.                                   
007000*                                 SEMIKOLON                               
007100*                                 SEMICOLON                               
007200     03 MAIL-BETEXT-12       PIC X(20)                                    
007300                             VALUE SPACES.                                
007400     03 MAIL-SEMICOLON       PIC X                                        
007500                             VALUE ';'.                                   
007600*                                 SEMIKOLON                               
007700*                                 SEMICOLON                               
007800     03 MAIL-DAREGDAT        PIC Z(8)                                     
007900                             VALUE ZEROS.                                 
008000*                                 REGISTRERINGSDATUM (電電MMDD)           
008100*                                 REGISTRATION DATE (YYYYMMDD)            
008200     03 MAIL-SEMICOLON       PIC X                                        
008300                             VALUE ';'.                                   
008400*                                 SEMIKOLON                               
008500*                                 SEMICOLON                               
008600     03 MAIL-BETEXT-13       PIC X(20)                                    
008700                             VALUE SPACES.                                
008800     03 MAIL-SEMICOLON       PIC X                                        
008900                             VALUE ';'.                                   
009000*                                 SEMIKOLON                               
009100*                                 SEMICOLON                               
009200     03 MAIL-DAUPPDAT        PIC Z(8)                                     
009300                             VALUE ZEROS.                                 
009400*                                 UPPDATERINGSDATUM  (電電MMDD)           
009500*                                                                         
009600*                                 UPDATING DATE     (YYYYMMDD)            
009700*                                                                         
009800*** END OF VILMAII-COPY LENGTH= 187 BYTES                                 
