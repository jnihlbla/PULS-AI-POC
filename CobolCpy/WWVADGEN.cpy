000010*** EDIT ALLOWED                                                          
000300* WWVADGEN                                                                
000600*                                                                         
000700* Detta är de av katalog-redaktionen fastställda veckonumren              
000800* för generering av katalogdata till VADIS.                               
000900* Tolv VADIS-genererings-perioder per år.                                 
001000* Synkroniserat med WLKATM11-TAB-PERIOD.                                  
001100                                                                          
001110 01  VADIS-PERIOD-TABELL.                                                 
001200     03 PERIOD-TABELL.                                                    
001300         05 FILLER            PIC 99      VALUE  04.                      
001400         05 FILLER            PIC 99      VALUE  08.                      
001500         05 FILLER            PIC 99      VALUE  13.                      
001600         05 FILLER            PIC 99      VALUE  17.                      
001700         05 FILLER            PIC 99      VALUE  21.                      
001800         05 FILLER            PIC 99      VALUE  26.                      
001900         05 FILLER            PIC 99      VALUE  30.                      
002000         05 FILLER            PIC 99      VALUE  34.                      
002100         05 FILLER            PIC 99      VALUE  39.                      
002200         05 FILLER            PIC 99      VALUE  43.                      
002300         05 FILLER            PIC 99      VALUE  47.                      
002400         05 FILLER            PIC 99      VALUE  52.                      
002500                                                                          
002600     03  PERIOD-TAB REDEFINES PERIOD-TABELL OCCURS 12.                    
002700         05  VADIS-PERIOD-VV      PIC 99.                                 
002710                                                                          
002720*                                                                         
002721 01  VADIS-PUBKOD-FROM-TEST.                                              
002730     03  FILLER       PIC X.                                              
002740     03  VADIS-PERIOD PIC XX.                                             
002750         88  PER1       VALUE '04'.                                       
002760         88  PER2       VALUE '08'.                                       
002770         88  PER3       VALUE '13'.                                       
002780         88  PER4       VALUE '17'.                                       
002790         88  PER5       VALUE '21'.                                       
002791         88  PER6       VALUE '26'.                                       
002792         88  PER7       VALUE '30'.                                       
002793         88  PER8       VALUE '34'.                                       
002794         88  PER9       VALUE '39'.                                       
002795         88  PER10      VALUE '43'.                                       
002796         88  PER11      VALUE '47'.                                       
002797         88  PER12      VALUE '52'.                                       
002798         88  GODK-PER   VALUE '04' '08' '13' '17' '21' '26'               
002799                              '30' '34' '39' '43' '47' '52' .             
002800                                                                          
002900* END-WWVADGEN                                                            
