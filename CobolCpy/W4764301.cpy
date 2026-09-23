000100 01  W4764301.                                                            
000200*                                 HIT-TRANSPORTÖR                         
000300*                                 KOLLI DOKUMENT SORT-1                   
000400     03 IDPTYP               PIC X(3)                                     
000500                             VALUE SPACES.                                
000600*                                 POSTTYP                                 
000700     03 FILLER               PIC X                                        
000800                             VALUE '+'.                                   
000900     03 IDKLIID-GRP.                                                      
001000*                                 KOLLI ID PÅ HIT-TRANSPORTDOK.           
001100        05 IDKLIID           PIC 9(11)                                    
001200                             VALUE ZEROS.                                 
001300*                                 KOLLIID NUMMER TILL HIT                 
001400        05 IDLANDX2          PIC X(2)                                     
001500                             VALUE SPACES.                                
001600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001700     03 FILLER               PIC X                                        
001800                             VALUE '+'.                                   
001900     03 DAREGDAT             PIC 9(8)                                     
002000                             VALUE ZEROS.                                 
002100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002200     03 FILLER               PIC X                                        
002300                             VALUE '+'.                                   
002400     03 TIHHMM               PIC 9(4)                                     
002500                             VALUE ZEROS.                                 
002600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002700     03 FILLER               PIC X                                        
002800                             VALUE '+'.                                   
002900     03 IDKUND               PIC X(10)                                    
003000                             VALUE SPACES.                                
003100*                                 KUND/LEV ID                             
003200     03 FILLER               PIC X                                        
003300                             VALUE '+'.                                   
003400     03 KDPRODUKT            PIC 9(2)                                     
003500                             VALUE ZEROS.                                 
003600*                                 PRODUKTKOD HIT TRANSPORTÖR              
003700     03 FILLER               PIC X                                        
003800                             VALUE '+'.                                   
003900     03 ADPOSTNR-005         PIC 9(5)                                     
004000                             VALUE ZEROS.                                 
004100*                                 POSTNUMMER I ADRESS                     
004200     03 FILLER               PIC X                                        
004300                             VALUE '+'.                                   
004400     03 VKORDBTO-KOLLI       PIC 9(5).9                                   
004500                             VALUE ZEROS.                                 
004600*                                 ORDERVIKT BRUTTO PER KOLLI              
004700     03 FILLER               PIC X                                        
004800                             VALUE '+'.                                   
004900     03 VLORDBTO-KOLLI       PIC 9(4).9(3)                                
005000                             VALUE ZEROS.                                 
005100*                                 ORDERVOLYM BRUTTO KOLLI                 
005200     03 FILLER               PIC X                                        
005300                             VALUE '+'.                                   
005400     03 IDGODS               PIC 9(2)                                     
005500                             VALUE ZEROS.                                 
005600*                                 GODSTYP PÅ TRANSPORTDOKUMENT            
005700     03 FILLER               PIC X                                        
005800                             VALUE '+'.                                   
005900     03 IDKUNDNR             PIC 9(6)                                     
006000                             VALUE ZEROS.                                 
006100*                                 KUNDNUMMER                              
006200     03 FILLER               PIC X                                        
006300                             VALUE '+'.                                   
006400     03 IDLANDX2-SORT        PIC X(2)                                     
006500                             VALUE SPACES.                                
006600*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
006700     03 ADFLGEO              PIC X(3)                                     
006800                             VALUE SPACES.                                
006900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
007000     03 FILLER               PIC X                                        
007100                             VALUE '+'.                                   
007200     03 KDPOD                PIC 9                                        
007300                             VALUE ZERO.                                  
007400*                                 POD KOD FÖR HIT-TRANSP.DOKUMENT         
007500     03 FILLER               PIC X                                        
007600                             VALUE '+'.                                   
007700     03 KUNDREFERENS         PIC X(27)                                    
007800                             VALUE SPACES.                                
007900*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
