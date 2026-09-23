000100 01  MOD-W5O21901.                                                        
000200*                                 MOD-COPYTEXT FÖR W50219                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDEKHHT-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-KDEKHHT-UT       PIC X(3).                                    
001100*                                 EKONOMISK HUVUDHÄNDELSE                 
001200     03 MOD-KDEKSHT-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-KDEKSHT-UT       PIC X(3).                                    
001500*                                 EKONOMISK SUBHÄNDELSE                   
001600     03 MOD-KDEKNIVA-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-KDEKNIVA-UT      PIC X(5).                                    
001900*                                 EKONOMISK HÄNDELSENIVÅ                  
002000     03 MOD-IDFTG-UT         PIC 9(2).                                    
002100*                                 FÖRETAGSID EKONOM REDOVISNING           
002200     03 MOD-BEEKHHT          PIC X(25).                                   
002300*                                 BESKR. EKONOMISK HUVUDHÄNDELSE          
002400     03 MOD-BEEKSHT          PIC X(25).                                   
002500*                                 BESKR. EKONOMISK SUBHÄNDELSE            
002600     03 MOD-FLARTNTO-ATTR    PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-FLARTNTO         PIC X.                                       
002900*                                 INDIKERAR PRARTNTO                      
003000     03 MOD-FLLSBOK-ATTR     PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-FLLSBOK          PIC X.                                       
003300*                                 LAGERAVBOKNING                          
003400     03 MOD-FLARTSJK-ATTR    PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-FLARTSJK         PIC X.                                       
003700*                                 INDIKERAR PRARTSJK                      
003800     03 MOD-FLARTSTD-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-FLARTSTD         PIC X.                                       
004100*                                 INDIKERAR PRARTSTD                      
004200     03 MOD-FLAVCOST-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLAVCOST         PIC X.                                       
004500*                                 INDIKERAR PRAVCOST                      
004600     03 MOD-FLINK-ATTR       PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-FLINK            PIC X.                                       
004900*                                 INDIKERAR PRARTINK                      
005000     03 MOD-FLDIRLON-ATTR    PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-FLDIRLON         PIC X.                                       
005300*                                 INDIKERAR PRDIRLON                      
005400     03 MOD-FLDMTRL-ATTR     PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-FLDMTRL          PIC X.                                       
005700*                                 INDIKERAR PRDMTRL                       
005800     03 MOD-FLOVRPAL-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-FLOVRPAL         PIC X.                                       
006100*                                 INDIKERAR PROVRPAL                      
006200     03 MOD-FLHEMTAG-ATTR    PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-FLHEMTAG         PIC X.                                       
006500*                                 INDIKERAR PRHEMTAG                      
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 198 BYTES                                 
