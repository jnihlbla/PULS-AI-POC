000100 01  W488FEL1.                                                            
000200*                                 FELAKTIGA POSTER FRÅN PDP               
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 IDARTNR              PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 IDDC                 PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 KVBUFF-F             PIC X(7).                                    
001300*                                 FÖRÄDLAT BUFFERSALDO                    
001400*                                 BUFFER QUANTANTITY PREPARED             
001500     03 KVBUFF-OF            PIC X(7).                                    
001600*                                 BUFFERSALDO OFÖRÄDLAT GODS              
001700*                                 BUFFER BALANCE UNPREPARED GOODS         
001800     03 KVKOLLI-F            PIC X(4).                                    
001900*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
002000*                                 Q PREPARED CASES IN BUFFER              
002100     03 KVKOLLI-OF           PIC X(4).                                    
002200*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
002300*                                 Q UNPREPARED CASES IN BUFFER            
002400     03 FILLER               PIC X(2).                                    
