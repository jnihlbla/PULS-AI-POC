000100 01  MID-W6I21201.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANT÷RNUMMER                        
000900     03 MID-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANT÷RNUMMER                        
001100     03 MID-IDKVAINF-IN      PIC X(2).                                    
001200*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
001300     03 MID-IDKVAINF-UT      PIC X(2).                                    
001400*                                 RADNR F÷R KVALITETSKONTROLLTEXT         
001500     03 MID-TIREGDAT-IN      PIC X(6).                                    
001600*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001700     03 MID-TIREGDAT-UT      PIC X(6).                                    
001800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001900     03 MID-FLNYSEG          PIC X.                                       
002000*                                 NYTT SEGMENT                            
002100     03 MID-TIREGDAT-9KOMPL-ENTER                                         
002200                             PIC 9(7).                                    
002300*                                 DATUMETS 9-KOMPLEMENT                   
002400     03 MID-TIKLOCK-9KOMPL-ENTER                                          
002500                             PIC 9(9).                                    
002600*                                 TID LAGRAT SOM 9-KOMPLEMENT             
002700     03 MID-TIREGDAT-9KOMPL-NEXT                                          
002800                             PIC 9(7).                                    
002900*                                 DATUMETS 9-KOMPLEMENT                   
003000     03 MID-TIKLOCK-9KOMPL-NEXT                                           
003100                             PIC 9(9).                                    
003200*                                 TID LAGRAT SOM 9-KOMPLEMENT             
003300     03 MID-INPUT.                                                        
003400*                                 INDATA F÷R UPPDATERING                  
003500        05 MID-KDPERSON      PIC 9(3).                                    
003600*                                 PERSONKOD                               
003700        05 MID-TEKVAINP      PIC X(20).                                   
003800*                                 KVALITETS INFORMATION ARTIKEL-I         
003900*                                 NPUT                                    
004000        05 MID-TIKLAR-QUAL   PIC 9(6).                                    
004100*                                 KLARDATUM          (≈≈MMDD)             
004200        05 MID-FLTABORT      PIC X.                                       
004300*                                 BORTTAGSFLAGGA                          
004400        05 MID-TEKVAINF-INT-RAD1                                          
004500                             PIC X(63).                                   
004600*                                 KVALITETS INFORMATION INTERNT           
004700        05 MID-TEKVAINF-INT-RAD2                                          
004800                             PIC X(79).                                   
004900*                                 KVALITETS INFORMATION INTERNT           
005000        05 MID-TEKVAINF-INT-RAD3                                          
005100                             PIC X(79).                                   
005200*                                 KVALITETS INFORMATION INTERNT           
005300        05 MID-TEKVAINF-INT-RAD4                                          
005400                             PIC X(79).                                   
005500*                                 KVALITETS INFORMATION INTERNT           
005600        05 MID-TEKVAINF-INT-RAD5                                          
005700                             PIC X(79).                                   
005800*                                 KVALITETS INFORMATION INTERNT           
005900        05 MID-TEKVAINF-INT-RAD6                                          
006000                             PIC X(79).                                   
006100*                                 KVALITETS INFORMATION INTERNT           
006200        05 MID-TEKVAINF-INT-RAD7                                          
006300                             PIC X(79).                                   
006400*                                 KVALITETS INFORMATION INTERNT           
006500        05 MID-KDKVAINF      PIC X.                                       
006600*                                 TYP AV KVAL.INFO F÷R ARTIKEL            
006700        05 MID-TIKLAR-LEV    PIC 9(6).                                    
006800*                                 KLARDATUM          (≈≈MMDD)             
006900        05 MID-FLSTOCH       PIC X.                                       
007000*                                 STOCKCHECK FLAGGA                       
007100*                                                                         
007200        05 MID-FLQPA         PIC X.                                       
007300*                                 QUALITY POINT ASSURED FLAGGA            
007400*                                                                         
007500        05 MID-TEKVAINF-EXT-RAD1                                          
007600                             PIC X(63).                                   
007700*                                 KVALITETS INFORMATION EXTERNT           
007800        05 MID-TEKVAINF-EXT-RAD2                                          
007900                             PIC X(79).                                   
008000*                                 KVALITETS INFORMATION EXTERNT           
008100        05 MID-TEKVAINF-EXT-RAD3                                          
008200                             PIC X(79).                                   
008300*                                 KVALITETS INFORMATION EXTERNT           
008400        05 MID-TEKVAINF-EXT-RAD4                                          
008500                             PIC X(79).                                   
008600*                                 KVALITETS INFORMATION EXTERNT           
008700        05 MID-TEKVAINF-EXT-RAD5                                          
008800                             PIC X(79).                                   
008900*                                 KVALITETS INFORMATION EXTERNT           
009000        05 MID-TEKVAINF-EXT-RAD6                                          
009100                             PIC X(79).                                   
009200*                                 KVALITETS INFORMATION EXTERNT           
009300        05 MID-TEKVAINF-EXT-RAD7                                          
009400                             PIC X(79).                                   
009500*                                 KVALITETS INFORMATION EXTERNT           
009600*** END OF VILMAII-COPY LENGTH= 1190 BYTES                                
