000100 01  W55374.                                                              
000200*                                 COPYTEXT FÖR ERROR LIST                 
000300     03 IDARTNR              PIC Z(8)9                                    
000400                             VALUE ZEROS.                                 
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 SEMICOLON            PIC X                                        
000800                             VALUE ';'.                                   
000900*                                 SEMIKOLON                               
001000*                                 SEMICOLON                               
001100     03 BETEXT               PIC X(40)                                    
001200                             VALUE SPACES.                                
001300     03 SEMICOLON            PIC X                                        
001400                             VALUE ';'.                                   
001500*                                 SEMIKOLON                               
001600*                                 SEMICOLON                               
001700     03 IDLEVNR              PIC X(5)                                     
001800                             VALUE SPACES.                                
001900*                                 LEVERANTÖRNUMMER                        
002000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002100     03 SEMICOLON            PIC X                                        
002200                             VALUE ';'.                                   
002300*                                 SEMIKOLON                               
002400*                                 SEMICOLON                               
002500     03 PRARTBEL             PIC Z(7)9.9(5)                               
002600                             VALUE ZEROS.                                 
002700*                                 BESTPRIS LEVERANTÖRENS VALUTA           
002800*                                 ORDER PRICE SUPL.CUR                    
002900     03 SEMICOLON            PIC X                                        
003000                             VALUE ';'.                                   
003100*                                 SEMIKOLON                               
003200*                                 SEMICOLON                               
003300     03 KDVALISO             PIC X(3)                                     
003400                             VALUE SPACES.                                
003500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003600*                                 CURRENCY CODE BY ISO-STANDARD.          
003700     03 SEMICOLON            PIC X                                        
003800                             VALUE ';'.                                   
003900*                                 SEMIKOLON                               
004000*                                 SEMICOLON                               
004100     03 TIPRLIST             PIC X(6)                                     
004200                             VALUE SPACES.                                
004300*                                 PRISLISTEDATUM (AAMMDD)                 
004400     03 SEMICOLON            PIC X                                        
004500                             VALUE ';'.                                   
004600*                                 SEMIKOLON                               
004700*                                 SEMICOLON                               
004800     03 KDFPKPRI             PIC X                                        
004900                             VALUE SPACE.                                 
005000*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
005100*                                 IF PACKING INCLUDED IN PRICE            
005200     03 SEMICOLON            PIC X                                        
005300                             VALUE ';'.                                   
005400*                                 SEMIKOLON                               
005500*                                 SEMICOLON                               
005600     03 IDDC                 PIC X(2)                                     
005700                             VALUE SPACES.                                
005800*                                 IDENTIFIERARE LAGER                     
005900*                                 WAREHOUSE IDENTIFIER                    
006000     03 SEMICOLON            PIC X                                        
006100                             VALUE ';'.                                   
006200*                                 SEMIKOLON                               
006300*                                 SEMICOLON                               
006400     03 IDNAMN               PIC X(40)                                    
006500                             VALUE SPACES.                                
006600*                                 NAMN                                    
006700     03 SEMICOLON            PIC X                                        
006800                             VALUE ';'.                                   
006900*                                 SEMIKOLON                               
007000*                                 SEMICOLON                               
007100     03 IDINK                PIC Z(2)9                                    
007200                             VALUE ZEROS.                                 
007300*                                 INKÖPARNUMMER                           
007400*                                 PURCHASE IDENTIFICATION NUMBER          
007500     03 SEMICOLON            PIC X                                        
007600                             VALUE ';'.                                   
007700*                                 SEMIKOLON                               
007800*                                 SEMICOLON                               
007900     03 IDMAIL               PIC X(60)                                    
008000                             VALUE SPACES.                                
008100*                                 MAIL ADRESS                             
008200*                                 MAIL ADDRESS                            
008300*** END OF VILMAII-COPY LENGTH= 193 BYTES                                 
