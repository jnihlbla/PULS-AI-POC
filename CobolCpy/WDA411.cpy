000100 01  BART-WDA411.                                                         
000200*                                 BUY BACK RETURER                        
000300*                                 ARTIKEL INFO                            
000400*                                 FYSISK NYCKEL: WDA411KY                 
000500*                                 (FLMATCH + FLPRGRNS + IDARTNR)          
000600     03 BART-FLMATCH         PIC X.                                       
000700*                                 FLAGGA MATCH                            
000800*                                 MATCH FLAG                              
000900     03 BART-FLPRGRNS        PIC X.                                       
001000*                                 RAD VÄRDE STÖRRE ÄN PRISGRÄNS           
001100*                                 VALUE GREATER THEN PRICE LIMIT          
001200     03 BART-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 BART-BEART           PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700*                                 PART DESCRIPTION                        
001800     03 BART-KDLTK           PIC S9              COMP-3.                  
001900*                                 LAGERTILLHÖRIGHETSKOD                   
002000*                                 STOCK BELONGING CODE                    
002100     03 BART-KDVALISO        PIC X(3).                                    
002200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002300*                                 CURRENCY CODE BY ISO-STANDARD.          
002400     03 BART-KVANTAL         PIC S9(7)           COMP-3.                  
002500*                                 ANTAL                                   
002600*                                 NUMBER                                  
002700     03 BART-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
002800*                                 ARTIKELPRIS NETTO                       
002900*                                 NET PRICE EACH   (FOB NET)              
003000     03 BART-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
003100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003200*                                 NET PRICE EACH LOCAL CURRENCY           
003300     03 BART-PRARTNTO-NEW    PIC S9(7)V9(2)      COMP-3.                  
003400*                                 NYTT ARTIKELPRIS * FOBNET               
003500*                                 NEW PRICE PARTS PRICE * FOBNET          
003600     03 BART-PRARTNTO-TOT    PIC S9(7)V9(2)      COMP-3.                  
003700*                                 NYTT ARTIKELPRIS * ANTAL                
003800*                                 NEW PRICE PARTS PRICE * AMOUNT          
003900     03 BART-REKSIFFR        PIC S9              COMP-3.                  
004000*                                 KONTROLLSIFFRA                          
004100*                                 PART NO CHECK DIGIT                     
004200     03 BART-FILLER          PIC X(10).                                   
004300*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
