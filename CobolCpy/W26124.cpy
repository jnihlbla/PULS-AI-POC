000100 01  W26124.                                                              
000200*                                 ARTIKLAR MED TIURPROD = 10/15/7         
000300*                                  ÅR                                     
000400     03 IDARTNR              PIC Z(7)9                                    
000500                             VALUE ZEROS.                                 
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 SEMICOLON            PIC X                                        
000900                             VALUE ';'.                                   
001000*                                 SEMIKOLON                               
001100*                                 SEMICOLON                               
001200     03 BEART                PIC X(25)                                    
001300                             VALUE SPACES.                                
001400*                                 ARTIKELBENÄMNING                        
001500*                                 PART DESCRIPTION                        
001600     03 SEMICOLON            PIC X                                        
001700                             VALUE ';'.                                   
001800*                                 SEMIKOLON                               
001900*                                 SEMICOLON                               
002000     03 KVLS                 PIC -(7)9                                    
002100                             VALUE ZEROS.                                 
002200*                                 LAGERSALDO                              
002300*                                 STOCK BALANCE                           
002400     03 SEMICOLON            PIC X                                        
002500                             VALUE ';'.                                   
002600*                                 SEMIKOLON                               
002700*                                 SEMICOLON                               
002800     03 IDFKNGRP             PIC Z(3)9                                    
002900                             VALUE ZEROS.                                 
003000*                                 FUNKTIONSGRUPP                          
003100*                                 FUNCTION GROUP                          
003200     03 SEMICOLON            PIC X                                        
003300                             VALUE ';'.                                   
003400*                                 SEMIKOLON                               
003500*                                 SEMICOLON                               
003600     03 KDPRODSL             PIC Z9                                       
003700                             VALUE ZEROS.                                 
003800*                                 PRODUKTSLAG                             
003900*                                 PRODUCT GROUP                           
004000     03 SEMICOLON            PIC X                                        
004100                             VALUE ';'.                                   
004200*                                 SEMIKOLON                               
004300*                                 SEMICOLON                               
004400     03 IDLEVNR              PIC X(5)                                     
004500                             VALUE SPACES.                                
004600*                                 LEVERANTÖRNUMMER                        
004700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004800     03 SEMICOLON            PIC X                                        
004900                             VALUE ';'.                                   
005000*                                 SEMIKOLON                               
005100*                                 SEMICOLON                               
005200     03 KDERS                PIC Z9                                       
005300                             VALUE ZEROS.                                 
005400*                                 ERSÄTTNINGSKOD                          
005500*                                 SUPERSESSION CODE                       
005600     03 SEMICOLON            PIC X                                        
005700                             VALUE ';'.                                   
005800*                                 SEMIKOLON                               
005900*                                 SEMICOLON                               
006000     03 IDANSK               PIC Z(2)9                                    
006100                             VALUE ZEROS.                                 
006200*                                 ANSKAFFARNUMMER                         
006300*                                 PROCURER NO.                            
006400     03 SEMICOLON            PIC X                                        
006500                             VALUE ';'.                                   
006600*                                 SEMIKOLON                               
006700*                                 SEMICOLON                               
006800     03 TIFINLV              PIC Z(4)9                                    
006900                             VALUE ZEROS.                                 
007000*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
007100*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
007200     03 SEMICOLON            PIC X                                        
007300                             VALUE ';'.                                   
007400*                                 SEMIKOLON                               
007500*                                 SEMICOLON                               
007600     03 TIURPROD             PIC 9(4)                                     
007700                             VALUE ZEROS.                                 
007800*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
007900*                                 OUT OF PRODUCTION DATE (YYWW)           
008000     03 SEMICOLON            PIC X                                        
008100                             VALUE ';'.                                   
008200*                                 SEMIKOLON                               
008300*                                 SEMICOLON                               
008400     03 PRARTSTD             PIC Z(6)9.9(2)                               
008500                             VALUE ZEROS.                                 
008600*                                 ARTIKELSTANDARDPRIS                     
008700*                                 STANDARD PRICE                          
008800     03 SEMICOLON            PIC X                                        
008900                             VALUE ';'.                                   
009000*                                 SEMIKOLON                               
009100*                                 SEMICOLON                               
009200     03 FLCLART              PIC X                                        
009300                             VALUE SPACE.                                 
009400*                                 ALLMÄN FLAGGA                           
009500*                                 GENERAL FLAG                            
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
010000     03 FLKAMP               PIC X                                        
010100                             VALUE SPACE.                                 
010200*                                 ALLMÄN FLAGGA                           
010300*                                 GENERAL FLAG                            
010400     03 SEMICOLON            PIC X                                        
010500                             VALUE ';'.                                   
010600*                                 SEMIKOLON                               
010700*                                 SEMICOLON                               
010800     03 IDINK                PIC X(4)                                     
010900                             VALUE SPACES.                                
011000*                                 INKÖPARNUMMER                           
011100*                                 PURCHASE IDENTIFICATION NUMBER          
011200     03 SEMICOLON            PIC X                                        
011300                             VALUE ';'.                                   
011400*                                 SEMIKOLON                               
011500*                                 SEMICOLON                               
011600     03 KDARTURS             PIC X(2)                                     
011700                             VALUE SPACES.                                
011800*                                 ARTIKELURSPRUNGSKOD                     
011900*                                 COUNTRY OF ORIGIN                       
012000     03 SEMICOLON            PIC X                                        
012100                             VALUE ';'.                                   
012200*                                 SEMIKOLON                               
012300*                                 SEMICOLON                               
012400     03 PRARTBTO-MARK        PIC Z(6)9.9(2)                               
012500                             VALUE ZEROS.                                 
012600*                                 BRUTTOPRIS PER MARKNAD (FOB)            
012700*                                 SUGGESTED RETAIL PER MARKET.            
012800     03 SEMICOLON            PIC X                                        
012900                             VALUE ';'.                                   
013000*                                 SEMIKOLON                               
013100*                                 SEMICOLON                               
013200     03 FLSATART             PIC X                                        
013300                             VALUE SPACE.                                 
013400*                                 ALLMÄN FLAGGA                           
013500*                                 GENERAL FLAG                            
013600     03 SEMICOLON            PIC X                                        
013700                             VALUE ';'.                                   
013800*                                 SEMIKOLON                               
013900*                                 SEMICOLON                               
014000     03 VKART                PIC Z(6)9                                    
014100                             VALUE ZEROS.                                 
014200*                                 ARTIKELVIKT (G)                         
014300*                                 PART WEIGHT (G)                         
014400     03 SEMICOLON            PIC X                                        
014500                             VALUE ';'.                                   
014600*                                 SEMIKOLON                               
014700*                                 SEMICOLON                               
014800     03 KDSORT               PIC X(2)                                     
014900                             VALUE SPACES.                                
015000*                                 SORT-KOD                                
015100*                                 UNIT OF MEASURE                         
015200     03 SEMICOLON            PIC X                                        
015300                             VALUE ';'.                                   
015400*                                 SEMIKOLON                               
015500*                                 SEMICOLON                               
015600     03 KVOI-TOT-CURRENT-YEAR                                             
015700                             PIC X(10)                                    
015800                             VALUE SPACES.                                
015900     03 SEMICOLON            PIC X                                        
016000                             VALUE ';'.                                   
016100*                                 SEMIKOLON                               
016200*                                 SEMICOLON                               
016300     03 KVOI-TOT-ONE-YEAR-AGO                                             
016400                             PIC X(10)                                    
016500                             VALUE SPACES.                                
016600     03 SEMICOLON            PIC X                                        
016700                             VALUE ';'.                                   
016800*                                 SEMIKOLON                               
016900*                                 SEMICOLON                               
017000     03 KVSKROT              PIC Z(6)9                                    
017100                             VALUE ZEROS.                                 
017200*                                 ANTAL SENASTE SKROTORDER                
017300*                                 QUANTITY LAST SCRAPORDER                
017400     03 SEMICOLON            PIC X                                        
017500                             VALUE ';'.                                   
017600*                                 SEMIKOLON                               
017700*                                 SEMICOLON                               
017800     03 DASKROT              PIC 9(8)                                     
017900                             VALUE ZEROS.                                 
018000*                                 SKROTNINGSDATUM (ÅÅÅÅMMDD)              
018100*                                 DATE OF SCRAPPING (YYYYMMDD)            
018200     03 SEMICOLON            PIC X                                        
018300                             VALUE ';'.                                   
018400*                                 SEMIKOLON                               
018500*                                 SEMICOLON                               
018600     03 KDFARLIG             PIC 9                                        
018700                             VALUE ZERO.                                  
018800*                                 KOD FÖR FARLIGT GODS                    
018900*                                 DANGEROUS GOODS CODE                    
019000     03 SEMICOLON            PIC X                                        
019100                             VALUE ';'.                                   
019200*                                 SEMIKOLON                               
019300*                                 SEMICOLON                               
019400*** END OF VILMAII-COPY LENGTH= 164 BYTES                                 
