000100 01  MOD-W4O70601.                                                        
000200*                                 MOD-COPYTEXT FÖR W4070600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KVDAGAR-LAP-UT-ATTR                                           
000800                             PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 MOD-KVDAGAR-LAP-UT   PIC Z9.                                      
001100*                                 MAX LIGGTID I DAGAR FÖR LEVANM          
001200*                                 INNAN RAD MARKERAS                      
001300     03 MOD-KVDAGAR-LAP-IN-ATTR                                           
001400                             PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-KVDAGAR-LAP-IN   PIC X(2).                                    
001700*                                 MAX LIGGTID I DAGAR FÖR LEVANM          
001800*                                 INNAN RAD MARKERAS                      
001900     03 MOD-KVDAGAR-RTAOSEA-UT-ATTR                                       
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-KVDAGAR-RTAOSEA-UT                                            
002300                             PIC Z9.                                      
002400*                                 MAX LIGGTID I DGR FÖR AVISERAT          
002500*                                 OVERSEAS-RT INNAN RAD MARKERAS          
002600     03 MOD-KVDAGAR-RTAOSEA-IN-ATTR                                       
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-KVDAGAR-RTAOSEA-IN                                            
003000                             PIC X(2).                                    
003100*                                 MAX LIGGTID I DGR FÖR AVISERAT          
003200*                                 OVERSEAS-RT INNAN RAD MARKERAS          
003300     03 MOD-KVDAGAR-RTAOVR-UT-ATTR                                        
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-KVDAGAR-RTAOVR-UT                                             
003700                             PIC Z9.                                      
003800*                                 MAX LIGGTID I DGR FÖR AVISERAT          
003900*                                 EUROPA-RT INNAN RAD MARKERAS            
004000     03 MOD-KVDAGAR-RTAOVR-IN-ATTR                                        
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-KVDAGAR-RTAOVR-IN                                             
004400                             PIC X(2).                                    
004500*                                 MAX LIGGTID I DGR FÖR AVISERAT          
004600*                                 EUROPA-RT INNAN RAD MARKERAS            
004700     03 MOD-KVDAGAR-RTM-UT-ATTR                                           
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-KVDAGAR-RTM-UT   PIC Z9.                                      
005100*                                 MAX LIGGTID I DAGAR FÖR                 
005200*                                 MOTTAGET RT INNAN RAD MARKERAS          
005300     03 MOD-KVDAGAR-RTM-IN-ATTR                                           
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KVDAGAR-RTM-IN   PIC X(2).                                    
005700*                                 MAX LIGGTID I DAGAR FÖR                 
005800*                                 MOTTAGET RT INNAN RAD MARKERAS          
005900     03 MOD-KVDAGAR-RTP-UT-ATTR                                           
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-KVDAGAR-RTP-UT   PIC Z9.                                      
006300*                                 MAX LIGGTID I DAGAR FÖR                 
006400*                                 PÅBÖRJAT RT INNAN RAD MARKERAS          
006500     03 MOD-KVDAGAR-RTP-IN-ATTR                                           
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KVDAGAR-RTP-IN   PIC X(2).                                    
006900*                                 MAX LIGGTID I DAGAR FÖR                 
007000*                                 PÅBÖRJAT RT INNAN RAD MARKERAS          
007100     03 MOD-KVDAGAR-KLIARB-UT-ATTR                                        
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-KVDAGAR-KLIARB-UT                                             
007500                             PIC Z9.                                      
007600*                                 MAX LIGGTID I DAGAR FÖR KOLLI           
007700*                                 INNAN RAD MARKERAS                      
007800     03 MOD-KVDAGAR-KLIARB-IN-ATTR                                        
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-KVDAGAR-KLIARB-IN                                             
008200                             PIC X(2).                                    
008300*                                 MAX LIGGTID I DAGAR FÖR KOLLI           
008400*                                 INNAN RAD MARKERAS                      
008500     03 MOD-KVDAGAR-KLIAVV-UT-ATTR                                        
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-KVDAGAR-KLIAVV-UT                                             
008900                             PIC Z9.                                      
009000*                                 ANTAL DAGAR INNAN ETT SAKNAT            
009100*                                 RETURKOLLI VISAS                        
009200     03 MOD-KVDAGAR-KLIAVV-IN-ATTR                                        
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-KVDAGAR-KLIAVV-IN                                             
009600                             PIC X(2).                                    
009700*                                 ANTAL DAGAR INNAN ETT SAKNAT            
009800*                                 RETURKOLLI VISAS                        
009900     03 MOD-TEMFSINF         PIC X(55).                                   
010000*                                 INFORMATIONSMEDDELANDE                  
010100*** END OF VILMAII-COPY LENGTH= 155 BYTES                                 
