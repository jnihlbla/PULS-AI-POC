000100*** EDIT ALLOWED                                                          
000200 01  W479T011.                                                            
000300*                      *** TABELLEN ÄR UPPBYGGD AV:                       
000400*                                                                         
000500*                      *** IDDISTR-FR.O.M.                                
000600*                      *** IDDISTR-T.O.M.                                 
000700*                      *** LEDTID FÖR RENSNING (ENHET:I VECKA)            
000800*                                                                         
000900*OBS! ANTALET RADER NEDAN (80 IDAG) PÅVERKAR TABELL I PGM W47910!!        
001000*OBS! VID TILLÄGG ELLER BORTTAG ÄNDRA ÄVEN I PGM W4791000.                
001100*OBS!!TA BORT DOCK ALDRIG SISTA 0001-9999 INTERVALLET                     
001200*                                                                         
001210*IF NEW LYNK/ECOM DISTRICT IS ADDED,CHECK IF WE NEED TO ADD HERE          
001220*RETENTION PERIOD IS 6 MONTHS FOR LYNK/ECOM ORDERS.                       
001300*                                                                         
001400     03  FILLER  PIC X(12)  VALUE '0071 0072 02'.                         
001500     03  FILLER  PIC X(12)  VALUE '0738 0739 24'.                         
001600     03  FILLER  PIC X(12)  VALUE '0748 0748 04'.                         
001700     03  FILLER  PIC X(12)  VALUE '0001 0799 04'.                         
001800     03  FILLER  PIC X(12)  VALUE '0838 0838 24'.                         
001900     03  FILLER  PIC X(12)  VALUE '0848 0848 04'.                         
002000     03  FILLER  PIC X(12)  VALUE '0938 0938 24'.                         
002100     03  FILLER  PIC X(12)  VALUE '0948 0948 04'.                         
002200     03  FILLER  PIC X(12)  VALUE '0800 0999 04'.                         
002300     03  FILLER  PIC X(12)  VALUE '1030 1030 24'.                         
002400     03  FILLER  PIC X(12)  VALUE '1040 1040 24'.                         
002500     03  FILLER  PIC X(12)  VALUE '1000 1090 05'.                         
002600     03  FILLER  PIC X(12)  VALUE '1130 1130 24'.                         
002700     03  FILLER  PIC X(12)  VALUE '1140 1140 24'.                         
002800     03  FILLER  PIC X(12)  VALUE '1238 1239 24'.                         
002900     03  FILLER  PIC X(12)  VALUE '1248 1248 04'.                         
003000     03  FILLER  PIC X(12)  VALUE '1091 1282 04'.                         
003100     03  FILLER  PIC X(12)  VALUE '1338 1338 24'.                         
003200     03  FILLER  PIC X(12)  VALUE '1348 1348 04'.                         
003300     03  FILLER  PIC X(12)  VALUE '1320 1378 08'.                         
003400     03  FILLER  PIC X(12)  VALUE '1438 1438 24'.                         
003500     03  FILLER  PIC X(12)  VALUE '1448 1448 04'.                         
003600     03  FILLER  PIC X(12)  VALUE '1538 1538 24'.                         
003700     03  FILLER  PIC X(12)  VALUE '1548 1548 04'.                         
003800     03  FILLER  PIC X(12)  VALUE '1284 1626 04'.                         
003900     03  FILLER  PIC X(12)  VALUE '1627 1627 01'.                         
004000     03  FILLER  PIC X(12)  VALUE '1638 1638 24'.                         
004100     03  FILLER  PIC X(12)  VALUE '1648 1648 04'.                         
004200     03  FILLER  PIC X(12)  VALUE '1738 1738 24'.                         
004300     03  FILLER  PIC X(12)  VALUE '1748 1748 04'.                         
004400     03  FILLER  PIC X(12)  VALUE '1778 1778 08'.                         
004500     03  FILLER  PIC X(12)  VALUE '1832 1832 24'.                         
004600     03  FILLER  PIC X(12)  VALUE '1842 1842 04'.                         
004700     03  FILLER  PIC X(12)  VALUE '1938 1938 24'.                         
004800     03  FILLER  PIC X(12)  VALUE '1948 1948 04'.                         
004900     03  FILLER  PIC X(12)  VALUE '2038 2038 24'.                         
005000     03  FILLER  PIC X(12)  VALUE '2048 2048 04'.                         
005100     03  FILLER  PIC X(12)  VALUE '2138 2138 24'.                         
005200     03  FILLER  PIC X(12)  VALUE '2148 2148 04'.                         
005300     03  FILLER  PIC X(12)  VALUE '2238 2238 24'.                         
005400     03  FILLER  PIC X(12)  VALUE '2248 2248 04'.                         
005500     03  FILLER  PIC X(12)  VALUE '2330 2348 24'.                         
005600     03  FILLER  PIC X(12)  VALUE '2600 2699 06'.                         
005700     03  FILLER  PIC X(12)  VALUE '2838 2838 24'.                         
005800     03  FILLER  PIC X(12)  VALUE '2848 2848 04'.                         
005900     03  FILLER  PIC X(12)  VALUE '1628 3020 04'.                         
006000     03  FILLER  PIC X(12)  VALUE '3130 6586 06'.                         
006100     03  FILLER  PIC X(12)  VALUE '6588 6589 12'.                         
006200     03  FILLER  PIC X(12)  VALUE '6590 7039 06'.                         
006300     03  FILLER  PIC X(12)  VALUE '7040 7050 12'.                         
006400     03  FILLER  PIC X(12)  VALUE '7051 7573 06'.                         
006500     03  FILLER  PIC X(12)  VALUE '7574 7574 08'.                         
006600     03  FILLER  PIC X(12)  VALUE '7575 7639 06'.                         
006700     03  FILLER  PIC X(12)  VALUE '7674 7674 08'.                         
006800     03  FILLER  PIC X(12)  VALUE '7830 7920 06'.                         
006900     03  FILLER  PIC X(12)  VALUE '8000 8002 02'.                         
007000     03  FILLER  PIC X(12)  VALUE '8032 8032 04'.                         
007010     03  FILLER  PIC X(12)  VALUE '8035 8039 12'.                         
007020     03  FILLER  PIC X(12)  VALUE '8065 8096 12'.                         
007100     03  FILLER  PIC X(12)  VALUE '8145 8145 18'.                         
007110     03  FILLER  PIC X(12)  VALUE '8141 8149 12'.                         
007200     03  FILLER  PIC X(12)  VALUE '8151 8153 18'.                         
007300     03  FILLER  PIC X(12)  VALUE '8162 8165 18'.                         
007310     03  FILLER  PIC X(12)  VALUE '8161 8167 12'.                         
007400     03  FILLER  PIC X(12)  VALUE '8171 8174 12'.                         
007500     03  FILLER  PIC X(12)  VALUE '8181 8187 12'.                         
007510     03  FILLER  PIC X(12)  VALUE '8193 8193 18'.                         
007520     03  FILLER  PIC X(12)  VALUE '8341 8347 12'.                         
007530     03  FILLER  PIC X(12)  VALUE '8361 8392 12'.                         
007540     03  FILLER  PIC X(12)  VALUE '8402 8411 12'.                         
007550     03  FILLER  PIC X(12)  VALUE '8440 8460 12'.                         
007600     03  FILLER  PIC X(12)  VALUE '8500 8500 12'.                         
007610     03  FILLER  PIC X(12)  VALUE '8560 8566 12'.                         
007620     03  FILLER  PIC X(12)  VALUE '8570 8574 12'.                         
007630     03  FILLER  PIC X(12)  VALUE '8720 8751 12'.                         
007640     03  FILLER  PIC X(12)  VALUE '8792 8792 12'.                         
007650     03  FILLER  PIC X(12)  VALUE '8839 8839 12'.                         
007660     03  FILLER  PIC X(12)  VALUE '8871 8872 12'.                         
007700     03  FILLER  PIC X(12)  VALUE '9111 9899 12'.                         
008100     03  FILLER  PIC X(12)  VALUE '0001 9999 02'.                         
008200*                                                                         
008300*OBS! ANTALET RADER OVAN (80 IDAG) PÅVERKAR TABELL I PGM W47910!!         
008400*OBS! VID TILLÄGG ELLER BORTTAG ÄNDRA ÄVEN I PGM W4791000.                
008500*OBS!!TA BORT DOCK ALDRIG SISTA 0001-9999 INTERVALLET                     
008600*                                                                         
008700*** END COPY W479T011                                                     
