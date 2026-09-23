000100 01  W009W030.                                                            
000200*                                 COPYTEXT FÖR OMVANDLING AV              
000300*                                 PERIODSALDON TILL AF1 - AF5             
000400*                                 START-STOPP  TILL AF6                   
000500     03 NUV-TIAAP            PIC S9(3)           COMP-3.                  
000600*                                 ÅR - PLANERINGSPERIOD (ÅÅP)             
000700     03 TISTART-AAP          PIC S9(3)           COMP-3.                  
000800*                                 START-ÅR-PERIOD FSG-MÅL KONCERN         
000900*                                                                         
001000     03 TISTOPP-AAP          PIC S9(3)           COMP-3.                  
001100*                                 STOPP-ÅR-PERIOD KONCERN                 
001200     03 TIANTPER             PIC S9(3)           COMP-3.                  
001300*                                 FÖRS.PERIODER                           
001400     03 INGRUPP.                                                          
001500*                                 INDATA FÖR OMVANDLING AV PERIOD         
001600*                                 DATA TILL AF1 - AF5                     
001700        05 STATGRUPP         OCCURS 16 TIMES.                             
001800*                                 STATISTIKGRUPP PER PERIOD               
001900           07 TIFSGPER       PIC S9(3)           COMP-3.                  
002000*                                 FÖRS.PERIODER                           
002100           07 KVLEVART       PIC S9(7)           COMP-3.                  
002200*                                 LEVERERAT ANTAL ARTIKLAR                
002300           07 PRARTNTO       PIC S9(7)V9(2)      COMP-3.                  
002400*                                 ARTIKELPRIS NETTO                       
002500           07 PRARTSJK       PIC S9(7)V9(2)      COMP-3.                  
002600*                                 ARTIKELNS SJÄLVKOSTNAD                  
002700     03 SVARSGRUPP.                                                       
002800*                                 PERIODDATA OMVANDLAT TILL AF(X)         
002900*                                 DÄR X = 1 - 5                           
003000*                                 START - STOPP INTERVALL X = 6           
003100        05 AFGRUPP           OCCURS 6 TIMES.                              
003200*                                 AF(IX), IX = AF1 - AF5                  
003300           07 AF-KVLEVART    PIC S9(7)           COMP-3.                  
003400*                                 LEVERERAT ANTAL ARTIKLAR                
003500           07 AF-PRARTNTO    PIC S9(7)V9(2)      COMP-3.                  
003600*                                 ARTIKELPRIS NETTO                       
003700           07 AF-PRARTSJK    PIC S9(7)V9(2)      COMP-3.                  
003800*                                 ARTIKELNS SJÄLVKOSTNAD                  
003900*** END COPY W009W030C0  LENGTH=348                                       
