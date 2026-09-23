000100 01  MAIL-WF106201.                                                       
000200*                                 MAIL-HEADER FOR PGM WF1062              
000300     03 MAIL-BETEXT-01       PIC X(20)                                    
000400                             VALUE SPACES.                                
000500     03 MAIL-SEMICOLON       PIC X                                        
000600                             VALUE ';'.                                   
000700*                                 SEMIKOLON                               
000800*                                 SEMICOLON                               
000900     03 MAIL-FLPAYTE         PIC X                                        
001000                             VALUE SPACE.                                 
001100*                                 FLAGGA PAYTE                            
001200*                                 PAYTE FLAG                              
001300     03 MAIL-SEMICOLON       PIC X                                        
001400                             VALUE ';'.                                   
001500*                                 SEMIKOLON                               
001600*                                 SEMICOLON                               
001700     03 MAIL-BETEXT-02       PIC X(20)                                    
001800                             VALUE SPACES.                                
001900     03 MAIL-SEMICOLON       PIC X                                        
002000                             VALUE ';'.                                   
002100*                                 SEMIKOLON                               
002200*                                 SEMICOLON                               
002300     03 MAIL-FLDELTE         PIC X                                        
002400                             VALUE SPACE.                                 
002500*                                 FLAGGA DELTE                            
002600*                                 DELTE FLAG                              
002700     03 MAIL-SEMICOLON       PIC X                                        
002800                             VALUE ';'.                                   
002900*                                 SEMIKOLON                               
003000*                                 SEMICOLON                               
003100     03 MAIL-BETEXT-03       PIC X(20)                                    
003200                             VALUE SPACES.                                
003300     03 MAIL-SEMICOLON       PIC X                                        
003400                             VALUE ';'.                                   
003500*                                 SEMIKOLON                               
003600*                                 SEMICOLON                               
003700     03 MAIL-REARTRAB        PIC Z9.9(2)                                  
003800                             VALUE ZEROS.                                 
003900*                                 ARTIKELRABATT                           
004000*                                 PARTS DISCOUNT PERCENT                  
004100     03 MAIL-SEMICOLON       PIC X                                        
004200                             VALUE ';'.                                   
004300*                                 SEMIKOLON                               
004400*                                 SEMICOLON                               
004500     03 MAIL-BETEXT-04       PIC X(20)                                    
004600                             VALUE SPACES.                                
004700     03 MAIL-SEMICOLON       PIC X                                        
004800                             VALUE ';'.                                   
004900*                                 SEMIKOLON                               
005000*                                 SEMICOLON                               
005100     03 MAIL-PRARTNTO-MIN    PIC Z(6)9.9(2)                               
005200                             VALUE ZEROS.                                 
005300*                                 ARTIKELPRIS NETTO                       
005400*                                 NET PRICE EACH   (FOB NET)              
005500     03 MAIL-SEMICOLON       PIC X                                        
005600                             VALUE ';'.                                   
005700*                                 SEMIKOLON                               
005800*                                 SEMICOLON                               
005900     03 MAIL-BETEXT-05       PIC X(20)                                    
006000                             VALUE SPACES.                                
006100     03 MAIL-SEMICOLON       PIC X                                        
006200                             VALUE ';'.                                   
006300*                                 SEMIKOLON                               
006400*                                 SEMICOLON                               
006500     03 MAIL-PRARTNTO-MAX    PIC Z(6)9.9(2)                               
006600                             VALUE ZEROS.                                 
006700*                                 ARTIKELPRIS NETTO                       
006800*                                 NET PRICE EACH   (FOB NET)              
006900     03 MAIL-SEMICOLON       PIC X                                        
007000                             VALUE ';'.                                   
007100*                                 SEMIKOLON                               
007200*                                 SEMICOLON                               
007300     03 MAIL-BETEXT-06       PIC X(20)                                    
007400                             VALUE SPACES.                                
007500     03 MAIL-SEMICOLON       PIC X                                        
007600                             VALUE ';'.                                   
007700*                                 SEMIKOLON                               
007800*                                 SEMICOLON                               
007900     03 MAIL-SUNTO-MIN       PIC Z(10)9.9(2)                              
008000                             VALUE ZEROS.                                 
008100*                                 TOTAL SALES AMOUNT EXCL. VAT            
008200*** END OF VILMAII-COPY LENGTH= 172 BYTES                                 
