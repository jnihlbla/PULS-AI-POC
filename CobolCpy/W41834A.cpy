000100 01  W41834A.                                                             
000200*                                 KREDITNOTA-HUVUD FRÅN BILLIT.           
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDRAPPNR             PIC 9(7).                                    
001100*                                 RAPPORT NUMMER                          
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDKNOTNR             PIC 9(7).                                    
001500*                                 KREDITNOTANUMMER                        
001600     03 TIKNOTA              PIC 9(6).                                    
001700*                                 KREDITNOTADATUM                         
001800     03 KDVALISO             PIC X(3).                                    
001900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002000     03 PRKURS               PIC 9(6)V9(5).                               
002100*                                 VALUTAKURS                              
002200     03 SUKRENTO             PIC 9(11)V9(2).                              
002300*                                 KREDITERAT VARUVÄRDE NETTO              
002400     03 SUVAT-FAKT           PIC 9(11)V9(2).                              
002500*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
002600*                                 EDITNOTA                                
002700     03 SUKRETOT             PIC 9(11)V9(2).                              
002800*                                 TOTALT KREDITERAT VÄRDE                 
002900     03 PRLANDCO             PIC 9(7)V9(2).                               
003000*                                 LANDING COST                            
003100     03 PRFRAKT              PIC 9(7)V9(2).                               
003200*                                 FRAKTKOSTNAD                            
003300     03 PRFOERS              PIC 9(7)V9(2).                               
003400*                                 FÖRSÄKRINGSPREMIE                       
003500     03 PRLEGKST             PIC 9(7)V9(2).                               
003600*                                 LEGALISERINSKOSTNAD                     
003700     03 FLSLUT               PIC X.                                       
003800*                                 AVSLUTNINGSFLAGGA                       
003900     03 KDTRADP              PIC X(4).                                    
004000*                                 TRADING PARTNER                         
004100*** END OF VILMAII-COPY LENGTH= 129 BYTES                                 
