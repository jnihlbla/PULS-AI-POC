000010*** EDIT ALLOWED                                                          
000020 01  W26124-RUBRIK.                                                       
000030*                            ARTIKLAR MED TIURPROD = 10 ÅR OSV            
000040*                                                                         
000050     03 FILLER               PIC X(8)                                     
000060                             VALUE ' PART NO'.                            
000070*                                 ARTIKELNUMMER                           
000080*                                 PART NUMBER                             
000090     03 SEMICOLON            PIC X                                        
000100                             VALUE ';'.                                   
000200*                                 SEMIKOLON                               
000300*                                 SEMICOLON                               
000400     03 FILLER               PIC X(25)                                    
000500                             VALUE 'DESCRIPTION     '.                    
000600*                                 ARTIKELBENÄMNING                        
000700*                                 PART DESCRIPTION                        
000800     03 SEMICOLON            PIC X                                        
000900                             VALUE ';'.                                   
001000*                                 SEMIKOLON                               
001100*                                 SEMICOLON                               
001200     03 FILLER               PIC X(9)                                     
001300                             VALUE 'STOCK CDC'.                           
001400*                                 LAGERSALDO                              
001500*                                 STOCK BALANCE                           
001600     03 SEMICOLON            PIC X                                        
001700                             VALUE ';'.                                   
001800*                                 SEMIKOLON                               
001900*                                 SEMICOLON                               
002000     03 FILLER               PIC X(4)                                     
002100                             VALUE 'FGRP'.                                
002200*                                 FUNKTIONSGRUPP                          
002300*                                 FUNCTION GROUP                          
002400     03 SEMICOLON            PIC X                                        
002500                             VALUE ';'.                                   
002600*                                 SEMIKOLON                               
002700*                                 SEMICOLON                               
002800     03 FILLER               PIC X(4)                                     
002900                             VALUE 'PGRP'.                                
003000*                                 PRODUKTSLAG                             
003100*                                 PRODUCT GROUP                           
003200     03 SEMICOLON            PIC X                                        
003300                             VALUE ';'.                                   
003400*                                 SEMIKOLON                               
003500*                                 SEMICOLON                               
003600     03 FILLER               PIC X(5)                                     
003700                             VALUE 'SUPPL'.                               
003800*                                 LEVERANTÖRNUMMER                        
003900*                                 SUPPLIER NUMBER (VENDOR NU              
004000     03 SEMICOLON            PIC X                                        
004100                             VALUE ';'.                                   
004200*                                 SEMIKOLON                               
004300*                                 SEMICOLON                               
004400     03 FILLER               PIC X(12)                                    
004500                             VALUE 'SUPERSESSION'.                        
004600*                                 ERSÄTTNINGSKOD                          
004700*                                 SUPERSESSION CODE                       
004800     03 SEMICOLON            PIC X                                        
004900                             VALUE ';'.                                   
005000*                                 SEMIKOLON                               
005100*                                 SEMICOLON                               
005200     03 FILLER               PIC X(8)                                     
005300                             VALUE 'PURCH.PL'.                            
005400*                                 ANSKAFFARNUMMER                         
005500*                                 PROCURER NO.                            
005600     03 SEMICOLON            PIC X                                        
005700                             VALUE ';'.                                   
005800*                                 SEMIKOLON                               
005900*                                 SEMICOLON                               
006000     03 FILLER               PIC X(9)                                     
006100                             VALUE 'PUBL.WEEK'.                           
006200*                                 PUBLICERINGSVECKA, (ÅÅVVD               
006300*                                 DATE 1:ST GOODS REC,(YYWWD              
006400     03 SEMICOLON            PIC X                                        
006500                             VALUE ';'.                                   
006600*                                 SEMIKOLON                               
006700*                                 SEMICOLON                               
006800     03 FILLER               PIC X(9)                                     
006900                             VALUE 'PROD STOP'.                           
007000*                                 DATUM UTGÅTT UR PROD   (ÅÅ              
007100*                                 OUT OF PRODUCTION DATE (YY              
007200     03 SEMICOLON            PIC X                                        
007300                             VALUE ';'.                                   
007400*                                 SEMIKOLON                               
007500*                                 SEMICOLON                               
007600     03 FILLER               PIC X(10)                                    
007700                             VALUE ' STD.PRICE'.                          
007800*                                 ARTIKELSTANDARDPRIS                     
007900*                                 STANDARD PRICE                          
008000     03 SEMICOLON            PIC X                                        
008100                             VALUE ';'.                                   
008200*                                 SEMIKOLON                               
008300*                                 SEMICOLON                               
008400     03 FILLER               PIC X(7)                                     
008500                             VALUE 'CLASSIC'.                             
008600*                                 ALLMÄN FLAGGA                           
008700*                                 GENERAL FLAG                            
008800     03 SEMICOLON            PIC X                                        
008900                             VALUE ';'.                                   
009000*                                 SEMIKOLON                               
009100*                                 SEMICOLON                               
009200     03 FILLER               PIC X(8)                                     
009300                             VALUE 'CAMPAIGN'.                            
009400*                                 ALLMÄN FLAGGA                           
009500*                                 GENERAL FLAG                            
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(11)                                    
009300                             VALUE 'PURCHASE.NO'.                         
009400*                                 INKÖPARNUMMER                           
009500*                                 PURCHASE IDENTIFICATION NUMBER          
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(6)                                     
009300                             VALUE 'ORIGIN'.                              
009400*                                 ARTIKELURSPRUNGSKOD                     
009500*                                 COUNTRY OF ORIGIN                       
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(11)                                    
009300                             VALUE 'SUGG.RETAIL'.                         
009400*                                 BRUTTOPRIS PER MARKNAD (FOB)            
009500*                                 SUGGESTED RETAIL PER MARKET.            
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(9)                                     
009300                             VALUE 'STRUCTURE'.                           
009400*                                 ALLMÄN FLAGGA                           
009500*                                 GENERAL FLAG                            
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(12)                                    
009300                             VALUE 'WEIGHT IN KG'.                        
009400*                                 ARTIKELVIKT (G)                         
009500*                                 PART WEIGHT (G)                         
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(9)                                     
009300                             VALUE 'SORT CODE'.                           
009400*                                 SORT-KOD                                
009500*                                 UNIT OF MEASURE                         
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(18)                                    
009300                             VALUE 'SALES CURRENT YEAR'.                  
009400*                                 ORDERINGÅNG TOTALT                      
009500*                                 TOTAL ORDER INCOMING                    
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(18)                                    
009300                             VALUE 'SALES ONE YEAR AGO'.                  
009400*                                 ORDERINGÅNG TOTALT                      
009500*                                 TOTAL ORDER INCOMING 1 YRS AGO          
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(9)                                     
009300                             VALUE 'SCRAP QTY'.                           
009400*                                 ANTAL SENASTE SKROTORDER                
009500*                                 QUANTITY LAST SCRAPORDER                
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(10)                                    
009300                             VALUE 'SCRAP DATE'.                          
009400*                                 SKROTNINGSDATUM (ÅÅÅÅMMDD)              
009500*                                 DATE OF SCRAPPING (YYYYMMDD)            
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
009200     03 FILLER               PIC X(10)                                    
009300                             VALUE 'DANG.GOODS'.                          
009400*                                 KOD FÖR FARLIGT GODS                    
009500*                                 DANGEROUS GOODS CODE                    
009600     03 SEMICOLON            PIC X                                        
009700                             VALUE ';'.                                   
009800*                                 SEMIKOLON                               
009900*                                 SEMICOLON                               
010000*** END OF VILMAII-COPY LENGTH= 265 BYTES                                 
