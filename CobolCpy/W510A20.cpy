000100 01  W510A20.                                                             
000200*                                 TYPE A20, EXCHANGE RATE / DAILY         
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 KDEKOHT              PIC X(3).                                    
000800*                                 KOD EKONOMISK HÄNDELSE                  
000900*                                 CODE ECONOMIC EVENT                     
001000     03 IDFTG                PIC 9(2).                                    
001100*                                 FÖRETAGSID EKONOM REDOVISNING           
001200*                                 COMPANY IDENTITY ACCOUNTING             
001300     03 IDDC-SEND            PIC X(2).                                    
001400*                                 SÄNDANDE LAGER                          
001500*                                 SENDING WAREHOUSE                       
001600     03 IDDC-REC             PIC X(2).                                    
001700*                                 MOTTAGANDE LAGER                        
001800*                                 RECEIVING WAREHOUSE                     
001900     03 DASTADAT             PIC 9(8).                                    
002000*                                 GENERELLT STARTDATUM                    
002100*                                 GENERAL START DATE                      
002200     03 PRKURS-SU            PIC 9(6)V9(5).                               
002300*                                 VALUTAKURS                              
002400*                                 CURRENCY EXCHANGE RATE                  
002500     03 PRKURS-SC            PIC 9(6)V9(5).                               
002600*                                 VALUTAKURS                              
002700*                                 CURRENCY EXCHANGE RATE                  
002800     03 PRKURS-UC            PIC 9(6)V9(5).                               
002900*                                 VALUTAKURS                              
003000*                                 CURRENCY EXCHANGE RATE                  
003100     03 PRKURS-CU            PIC 9(6)V9(5).                               
003200*                                 VALUTAKURS                              
003300*                                 CURRENCY EXCHANGE RATE                  
003400*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
