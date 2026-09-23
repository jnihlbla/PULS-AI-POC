000100 01  W41832.                                                              
000200*                                 KREDITNOTARADER FRÅN BILLIT.            
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
001800     03 IDARTNR              PIC 9(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 IDRADNR              PIC 9(5).                                    
002100*                                 RADNUMMER                               
002200     03 BEART                PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400     03 KDVAT                PIC X(2).                                    
002500*                                 MOMSKOD                                 
002600     03 KDVALISO             PIC X(3).                                    
002700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002800     03 PRKURS               PIC 9(6)V9(5).                               
002900*                                 VALUTAKURS                              
003000     03 KDVALISO-BET         PIC X(3).                                    
003100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003200     03 PRKURS-BET           PIC 9(6)V9(5).                               
003300*                                 VALUTAKURS                              
003400     03 PRKURS-FAKBET        PIC 9(6)V9(5).                               
003500*                                 VALUTAKURS                              
003600     03 KVKREANT             PIC 9(6).                                    
003700*                                 KREDITERAT ANTAL                        
003800     03 PRARTNTO             PIC 9(7)V9(2).                               
003900*                                 ARTIKELPRIS NETTO                       
004000     03 SULNELOC             PIC 9(9)V9(2).                               
004100*                                 FAKTURARADSUMMA EXKL MOMS               
004200     03 SUVAT-LINE           PIC 9(11)V9(2).                              
004300*                                 MOMSVÄRDE PER FAKTURARAD                
004400     03 PRLANDCO-RAD         PIC 9(7)V9(2).                               
004500*                                 LANDING COST                            
004600     03 SUKRENTO             PIC 9(11)V9(2).                              
004700*                                 KREDITERAT VARUVÄRDE NETTO              
004800     03 SUVAT-FAKT           PIC 9(11)V9(2).                              
004900*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
005000*                                 EDITNOTA                                
005100     03 SUKRETOT             PIC 9(11)V9(2).                              
005200*                                 TOTALT KREDITERAT VÄRDE                 
005300     03 PRFRAKT              PIC 9(7)V9(2).                               
005400*                                 FRAKTKOSTNAD                            
005500     03 PRFOERS              PIC 9(7)V9(2).                               
005600*                                 FÖRSÄKRINGSPREMIE                       
005700     03 PRLEGKST             PIC 9(7)V9(2).                               
005800*                                 LEGALISERINSKOSTNAD                     
005900     03 KDTRADP              PIC X(4).                                    
006000*                                 TRADING PARTNER                         
006100*** END OF VILMAII-COPY LENGTH= 233 BYTES                                 
