000100 01  MID-W6I21101.                                                        
000200     03 MID-IDARTNR-IN       PIC X(9).                                    
000300*                                 ARTIKELNUMMER                           
000400     03 MID-IDARTNR-UT       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDLEVNR-IN       PIC X(5).                                    
000700*                                 LEVERANT÷RNUMMER                        
000800     03 MID-IDLEVNR-UT       PIC X(5).                                    
000900*                                 LEVERANT÷RNUMMER                        
001000     03 MID-IDKVAINF-IN      PIC X(2).                                    
001100*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
001200     03 MID-IDKVAINF-UT      PIC X(2).                                    
001300*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
001400     03 MID-TIREGDAT-IN      PIC X(6).                                    
001500*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001600     03 MID-TIREGDAT-UT      PIC X(6).                                    
001700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001800     03 MID-KDKVAINF-IN      PIC X.                                       
001900*                                 TYP AV KVAL.INFO F÷R ARTIKEL            
002000     03 MID-KDKVAINF-UT      PIC X.                                       
002100*                                 TYP AV KVAL.INFO F÷R ARTIKEL            
002200     03 MID-IDFORDON-ENTER   PIC 9(3).                                    
002300*                                 FORDONSSLAG                             
002400     03 MID-IDFORDON-NEXT    PIC 9(3).                                    
002500*                                 FORDONSSLAG                             
002600     03 MID-TIOMBRYT-1-ENTER PIC 9(6).                                    
002700*                                 OMBRYTNINGSDATUM                        
002800     03 MID-TIOMBRYT-1-NEXT  PIC 9(6).                                    
002900*                                 OMBRYTNINGSDATUM                        
003000     03 MID-INPUT1.                                                       
003100        05 MID-KDYTBEH       PIC 9(2).                                    
003200*                                 YTBEHANDLINGSKOD                        
003300        05 MID-KDFARLIG      PIC 9.                                       
003400*                                 KOD F÷R FARLIGT GODS                    
003500     03 MID-INPUT3.                                                       
003600        05 MID-IDLIKARE      OCCURS 4 TIMES                               
003700                             PIC X(9).                                    
003800*                                 LIKARIDENTITET                          
003900     03 MID-INPUT2.                                                       
004000        05 MID-KDKVATYP      PIC X.                                       
004100*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
004200        05 MID-IDPROVPL-PRI  PIC X.                                       
004300*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
004400        05 MID-IDPROVPL-SEK  PIC X.                                       
004500*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
004600        05 MID-KDKVAULG      PIC X.                                       
004700*                                 UNDERLAG F÷R KVALITETSKONTROLL          
004800        05 MID-ADKVAULG      PIC X(2).                                    
004900*                                 PLATS UNDERLAG KVAL.KONTROLL            
005000*** END OF VILMAII-COPY LENGTH= 109 BYTES                                 
