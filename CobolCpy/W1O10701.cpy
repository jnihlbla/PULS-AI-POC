000100 01  W1O10701.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W1O10701                                
000400     03 TRANS-NUMMER.                                                     
000500*                                 TRANSAKTIONS-NUMMER                     
000600        05 TRANS-SIFF-1      PIC X.                                       
000700        05 TRANS-SIFF-2      PIC X.                                       
000800        05 TRANS-SIFF-3      PIC X.                                       
000900        05 TRANS-SIFF-4      PIC X.                                       
001000     03 MESSAGE-RAD1         PIC X(40).                                   
001100*                                 MEDDELANDEFÄLT PÅ RAD 1                 
001200     03 IDLEVNR-IN           PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 BELEV-IN             PIC X(30).                                   
001500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001600     03 IDARTNR-IN           PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 IDLEVNR-UT           PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 BELEV-UT             PIC X(30).                                   
002100*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002200     03 IDARTNR-UT           PIC 9(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 BELEV-SPAR           PIC X(30).                                   
002500*                                 SPARAD BENÄMNING                        
002600     03 AREA.                                                             
002700        05 LINES             OCCURS 14 TIMES                              
002800                             INDEXED IX-LINE.                             
002900           07 BELEV-ATTR     PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100           07 BELEV          PIC X(30).                                   
003200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
003300           07 KDFTAG         PIC 9.                                       
003400*                                 FÖRETAGSKOD                             
003500           07 FLTLVM         PIC X.                                       
003600*                                 TILLVERKARMÄRKNING                      
003700           07 IDBENR         PIC 9.                                       
003800*                                 BENÄMNINGSNUMMER                        
003900           07 IDARTNR        PIC 9(9).                                    
004000*                                 ARTIKELNUMMER                           
004100           07 FLLSRDEL       PIC X.                                       
004200*                                 LEVERERAS SOM RESDEL                    
004300        05 LINE23.                                                        
004400           07 MESSAGE-23     PIC X(80).                                   
004500*                                 MEDDELANDEFÄLT PÅ RAD 23                
004600*** END COPY W1O10701C0  LENGTH=872                                       
