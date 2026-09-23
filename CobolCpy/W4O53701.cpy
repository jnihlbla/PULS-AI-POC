000100 01  W4O53701.                                                            
000200*                                 COPYTEXT FÖR MOD W4O53701               
000300*                                                                         
000400     03 TRANS-NUMMER         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MESSAGE-RAD1         PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 IDDISTR-IN-ATTR      PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 IDDISTR-IN           PIC X(5).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDPRODNR-IN-ATTR     PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 IDPRODNR-IN          PIC X(7).                                    
001500*                                 PRODUKTIONSNUMMER                       
001600     03 IDDISTR-UT           PIC X(5).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 IDPRODNR-UT          PIC X(7).                                    
001900*                                 PRODUKTIONSNUMMER                       
002000     03 ORDER-INFO.                                                       
002100*                                                                         
002200        05 IDKUNDNR          PIC Z(7).                                    
002300*                                 KUNDNUMMER                              
002400        05 FILLER            PIC X(2).                                    
002500        05 IDKUNDRF          PIC X(10).                                   
002600*                                 KUNDENS REFERENS                        
002700        05 FILLER            PIC X(2).                                    
002800        05 KDFRAKT           PIC Z(2).                                    
002900*                                 FRAKTSÄTT C1-C2 TILL KUND               
003000        05 FILLER            PIC X(3).                                    
003100        05 TIPACKN           PIC 9(6).                                    
003200*                                 PACKNINGSDATUM         (ÅÅMMDD)         
003300     03 PRFRAKT-ATTR         PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 PRFRAKT              PIC X(10).                                   
003600*                                 FRAKTKOSTNAD (KR)                       
003700     03 MESSAGE-RAD23        PIC X(79).                                   
003800*                                 MEDDELANDEFÄLT PÅ RAD 23                
003900*** END COPY W4O53701C0  LENGTH=195                                       
