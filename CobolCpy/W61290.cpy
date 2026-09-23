000100 01  W61290.                                                              
000200*                                 LOCATION FILE EXTRACT                   
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 ADLAGOMR             PIC 9(2).                                    
001000*                                 LAGEROMRÅDE                             
001100*                                 AREA                                    
001200     03 ADGANG               PIC 9(2).                                    
001300*                                 GÅNG                                    
001400*                                 AISLE                                   
001500     03 ADPLATS              PIC 9(5).                                    
001600*                                 LAGERPLATSNUMMER                        
001700*                                 LOCATION                                
001800     03 KDLOC                PIC X.                                       
001900*                                 TYPE OF LOCATION                        
002000*                                 TYPE OF LOCATION                        
002100     03 KDFREQ               PIC X(2).                                    
002200*                                 FREQUENCY CODE                          
002300*                                 FREQUENCY CODE                          
002400     03 KDSTOR               PIC X(3).                                    
002500*                                 STORAGE CODE                            
002600*                                 STORAGE CODE                            
002700     03 KVMPART              PIC 9(2).                                    
002800*                                 NUMBER OF PARTNUMBERS ON A LOCA         
002900*                                 TION                                    
003000*                                 NUMBER OF PARTNUMBER                    
003100     03 TELOC                PIC X(15).                                   
003200*                                 LOCATION INFORMATION                    
003300*                                 LOCATION INFORMATION                    
