000100 01  MOD-W1O53101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 KATALOGIDENTITET                        
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDCATNR-IN       PIC X(5).                                    
000900*                                 KATALOG-ID                              
001000     03 MOD-IDCATNR-UT       PIC X(5).                                    
001100*                                 KATALOG-ID                              
001200     03 MOD-BECAT.                                                        
001300        05 FILLER.                                                        
001400           07 MOD-BECAT-RAD1-ATTR                                         
001500                             PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700           07 MOD-BECAT-RAD1 PIC X(40).                                   
001800*                                 KATALOGBETECKNING                       
001900*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
002000        05 FILLER.                                                        
002100           07 MOD-BECAT-RAD2-ATTR                                         
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400           07 MOD-BECAT-RAD2 PIC X(20).                                   
002500*                                 KATALOGBETECKNING                       
002600*                                 SE ÄVEN BEEMBLEM RESP BEMASTER          
002700     03 MOD-BEEMBLEM-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-BEEMBLEM         PIC X(5).                                    
003000*                                 EMBLEM                                  
003100     03 MOD-BEMASTER-ATTR    PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-BEMASTER         PIC X(12).                                   
003400*                                 MASTERNAMN FÖR FORDON                   
003500     03 MOD-KDFORDON-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDFORDON         PIC X(2).                                    
003800*                                 FORDONSSLAG                             
003900     03 MOD-FLKOPIE-ATTR     PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-FLKOPIE          PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300     03 MOD-TIOMBRYT-F-ATTR  PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-TIOMBRYT-F       PIC 9(6).                                    
004600*                                 FÖRSTA OMBRYTNINGSDATUM                 
004700     03 MOD-TIAAVV-PUBL      PIC 9(4).                                    
004800*                                 PUBLICERINGSVECKA OMBRYTNING            
004900     03 MOD-KDCATPUB-R-FOM   PIC X(3).                                    
005000*                                 DE 3 HÖGRA TECKNEN I KDCATPUB           
005100     03 MOD-FLKATVAD         PIC X.                                       
005200*                                 KATALOG TILL VADIS?                     
005300     03 MOD-TIREGDAT         PIC 9(6).                                    
005400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005500     03 MOD-IDILLU-ATTR      PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-IDILLU           PIC 9(5).                                    
005800*                                 ILLUSTRATIONENS NR                      
005900     03 MOD-TIHIST           PIC Z(5)9.                                   
006000*                                 FLYTTNINGSDATUM                         
006100     03 MOD-TENOTE-ATTR      PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-TENOTE           PIC X(40).                                   
006400*                                 NOTERINGSFÄLT                           
006500     03 MOD-OMBRYTNINGSDATUMGRUPP                                         
006600                             OCCURS 17 TIMES.                             
006700        05 MOD-TIOMBRYT-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-TIOMBRYT      PIC 9(6).                                    
007000*                                 OMBRYTNINGSDATUM                        
007100     03 MOD-TIOMBRYT-SEN     PIC 9(6).                                    
007200*                                 SENASTE OMBRYTNINGSDATUM                
007300     03 MOD-TEMFSINF         PIC X(55).                                   
007400*                                 INFORMATIONSMEDDELANDE                  
007500*** END OF VILMAII-COPY LENGTH= 421 BYTES                                 
