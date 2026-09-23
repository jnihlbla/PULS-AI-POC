000100 01  W4764302.                                                            
000200*                                 HIT-TRANSPORTÖR                         
000300*                                 KOLLI DOKUMENT SORT-2                   
000400     03 IDPTYP               PIC X(3)                                     
000500                             VALUE SPACES.                                
000600*                                 POSTTYP                                 
000700     03 FILLER               PIC X                                        
000800                             VALUE '+'.                                   
000900     03 IDKLIID-DDGS         PIC X(22)                                    
001000                             VALUE SPACES.                                
001100     03 FILLER               PIC X                                        
001200                             VALUE '+'.                                   
001300     03 DAREGDAT             PIC 9(8)                                     
001400                             VALUE ZEROS.                                 
001500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001600     03 FILLER               PIC X                                        
001700                             VALUE '+'.                                   
001800     03 TIHHMM               PIC 9(4)                                     
001900                             VALUE ZEROS.                                 
002000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
002100     03 FILLER               PIC X                                        
002200                             VALUE '+'.                                   
002300     03 IDKUND               PIC X(10)                                    
002400                             VALUE SPACES.                                
002500*                                 KUND/LEV ID                             
002600     03 FILLER               PIC X                                        
002700                             VALUE '+'.                                   
002800     03 KDPRODUKT            PIC 9(2)                                     
002900                             VALUE ZEROS.                                 
003000*                                 PRODUKTKOD HIT TRANSPORTÖR              
003100     03 FILLER               PIC X                                        
003200                             VALUE '+'.                                   
003300     03 ADPOSTNR-005         PIC 9(5)                                     
003400                             VALUE ZEROS.                                 
003500*                                 POSTNUMMER I ADRESS                     
003600     03 FILLER               PIC X                                        
003700                             VALUE '+'.                                   
003800     03 VKORDBTO-KOLLI       PIC 9(5).9                                   
003900                             VALUE ZEROS.                                 
004000*                                 ORDERVIKT BRUTTO PER KOLLI              
004100     03 FILLER               PIC X                                        
004200                             VALUE '+'.                                   
004300     03 VLORDBTO-KOLLI       PIC 9(4).9(3)                                
004400                             VALUE ZEROS.                                 
004500*                                 ORDERVOLYM BRUTTO KOLLI                 
004600     03 FILLER               PIC X                                        
004700                             VALUE '+'.                                   
004800     03 IDGODS               PIC 9(2)                                     
004900                             VALUE ZEROS.                                 
005000*                                 GODSTYP PÅ TRANSPORTDOKUMENT            
005100     03 FILLER               PIC X                                        
005200                             VALUE '+'.                                   
005300     03 IDKUNDNR             PIC 9(6)                                     
005400                             VALUE ZEROS.                                 
005500*                                 KUNDNUMMER                              
005600     03 FILLER               PIC X                                        
005700                             VALUE '+'.                                   
005800     03 IDLANDX2-SORT        PIC X(2)                                     
005900                             VALUE SPACES.                                
006000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
006100     03 ADFLGEO              PIC X(3)                                     
006200                             VALUE SPACES.                                
006300*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
006400     03 FILLER               PIC X                                        
006500                             VALUE '+'.                                   
006600     03 KDPOD                PIC 9                                        
006700                             VALUE ZERO.                                  
006800*                                 POD KOD FÖR HIT-TRANSP.DOKUMENT         
006900     03 FILLER               PIC X                                        
007000                             VALUE '+'.                                   
007100     03 KUNDREFERENS         PIC X(27)                                    
007200                             VALUE SPACES.                                
007300*** END OF VILMAII-COPY LENGTH= 123 BYTES                                 
