000010*** EDIT ALLOWED                                                          
000100 01     WWTEXT01.                                                         
000200*                                                                         
000300     03 TEXT-401.                                                         
000400         05  FILLER              PIC X(40)   VALUE                        
000500             '401 FEL NYCKEL                         '.                   
000600         05  FILLER              PIC X(40)   VALUE                        
000700             '401 WRONG KEY                          '.                   
000800     03  FILLER REDEFINES TEXT-401.                                       
000900         05  TEXT-0401 OCCURS 2      PIC X(40).                           
001000*                                                                         
001100     03 TEXT-402.                                                         
001200         05  FILLER              PIC X(40)   VALUE                        
001300             '402 FLER RADER FINNS                   '.                   
001400         05  FILLER              PIC X(40)   VALUE                        
001500             '402 MORE LINES                         '.                   
001600     03  FILLER REDEFINES TEXT-402.                                       
001700         05  TEXT-0402 OCCURS 2      PIC X(40).                           
001800*                                                                         
001900     03 TEXT-403.                                                         
002000         05  FILLER              PIC X(40)   VALUE                        
002100             '403 RESTORDER SAKNAS                   '.                   
002200         05  FILLER              PIC X(40)   VALUE                        
002300             '403 BACKORDER MISSING                  '.                   
002400     03  FILLER REDEFINES TEXT-403.                                       
002500         05  TEXT-0403 OCCURS 2      PIC X(40).                           
002600*                                                                         
002700     03 TEXT-404.                                                         
002800         05  FILLER              PIC X(40)   VALUE                        
002900             '404 UPPDATERING UTFÖRD                 '.                   
003000         05  FILLER              PIC X(40)   VALUE                        
003100             '404 UPDATED                            '.                   
003200     03  FILLER REDEFINES TEXT-404.                                       
003300         05  TEXT-0404 OCCURS 2      PIC X(40).                           
003400*                                                                         
003500     03 TEXT-405.                                                         
003600         05  FILLER              PIC X(40)   VALUE                        
003700             '405 OBEHÖRIG ANVÄNDARE                 '.                   
003800         05  FILLER              PIC X(40)   VALUE                        
003900             '405 USER NOT AUTHORIZED                '.                   
004000     03  FILLER REDEFINES TEXT-405.                                       
004100         05  TEXT-0405 OCCURS 2      PIC X(40).                           
004200*                                                                         
004300     03 TEXT-406.                                                         
004400         05  FILLER              PIC X(40)   VALUE                        
004500             '406 PRIORITET SAKNAS                   '.                   
004600         05  FILLER              PIC X(40)   VALUE                        
004700             '406 PRIORITY MISSING                   '.                   
004800     03  FILLER REDEFINES TEXT-406.                                       
004900         05  TEXT-0406 OCCURS 2      PIC X(40).                           
005000*                                                                         
005100     03 TEXT-407.                                                         
005200         05  FILLER              PIC X(40)   VALUE                        
005300             '407 TRYCK PF11 FÖR UPPDATERING         '.                   
005400         05  FILLER              PIC X(40)   VALUE                        
005500             '407 PRESS PF11 TO UPDATE               '.                   
005600     03  FILLER REDEFINES TEXT-407.                                       
005700         05  TEXT-0407 OCCURS 2      PIC X(40).                           
005800*                                                                         
005900     03 TEXT-408.                                                         
006000         05  FILLER              PIC X(40)   VALUE                        
006100             '408 PRIORITET EJ NUMERISK              '.                   
006200         05  FILLER              PIC X(40)   VALUE                        
006300             '408 PRIORITY NOT NUMERIC               '.                   
006400     03  FILLER REDEFINES TEXT-408.                                       
006500         05  TEXT-0408 OCCURS 2      PIC X(40).                           
006600*                                                                         
006700     03 TEXT-409.                                                         
006800         05  FILLER              PIC X(40)   VALUE                        
006900             '409 UPPLYSTA FÄLT FEL                  '.                   
007000         05  FILLER              PIC X(40)   VALUE                        
007100             '409 HIGHLIGHTED FIELDS WRONG           '.                   
007200     03  FILLER REDEFINES TEXT-409.                                       
007300         05  TEXT-0409 OCCURS 2      PIC X(40).                           
007400*                                                                         
007500     03 TEXT-410.                                                         
007600         05  FILLER              PIC X(40)   VALUE                        
007700             '410 DETTA ÄR FÖRSTA SIDAN              '.                   
007800         05  FILLER              PIC X(40)   VALUE                        
007900             '410 THIS IS THE FIRST PAGE             '.                   
008000     03  FILLER REDEFINES TEXT-410.                                       
008100         05  TEXT-0410 OCCURS 2      PIC X(40).                           
008200*                                                                         
008300     03 TEXT-411.                                                         
008400         05  FILLER              PIC X(40)   VALUE                        
008500             '411 MAX 25 OLIKA PRIORITETER           '.                   
008600         05  FILLER              PIC X(40)   VALUE                        
008700             '411 MAX 25 DIFFRENT PRIORITY           '.                   
008800     03  FILLER REDEFINES TEXT-411.                                       
008900         05  TEXT-0411 OCCURS 2      PIC X(40).                           
009000*                                                                         
009100     03 TEXT-412.                                                         
009200         05  FILLER              PIC X(40)   VALUE                        
009300             '412 DISTRIKT/KUND SAKNAS PÅ KUNDREG.   '.                   
009400         05  FILLER              PIC X(40)   VALUE                        
009500             '412 DISTR/CUST. MISSING ON CUST.FILE   '.                   
009600     03  FILLER REDEFINES TEXT-412.                                       
009700         05  TEXT-0412 OCCURS 2      PIC X(40).                           
009800*                                                                         
009900     03 TEXT-413.                                                         
010000         05  FILLER              PIC X(40)   VALUE                        
010100             '413 INFORMATION SAKNAS                 '.                   
010200         05  FILLER              PIC X(40)   VALUE                        
010300             '413 INFORMATION MISSING                '.                   
010400     03  FILLER REDEFINES TEXT-413.                                       
010500         05  TEXT-0413 OCCURS 2      PIC X(40).                           
010600*                                                                         
010700     03 TEXT-414.                                                         
010800         05  FILLER              PIC X(40)   VALUE                        
010900             '414 INGET ÄNDRAT, UPPDATERING EJ UTFÖRD'.                   
011000         05  FILLER              PIC X(40)   VALUE                        
011100             '414 NO CHANGE, UPDATE HAS NOT BEEN DONE'.                   
011200     03  FILLER REDEFINES TEXT-414.                                       
011300         05  TEXT-0414 OCCURS 2      PIC X(40).                           
011400*                                                                         
011500     03 TEXT-415.                                                         
011600         05  FILLER              PIC X(40)   VALUE                        
011700             '415 ORDERINFORMATION BORTTAGEN         '.                   
011800         05  FILLER              PIC X(40)   VALUE                        
011900             '415 ORDER INFORMATION DELETED          '.                   
012000     03  FILLER REDEFINES TEXT-415.                                       
012100         05  TEXT-0415 OCCURS 2      PIC X(40).                           
012200*                                                                         
012300     03 TEXT-416.                                                         
012400         05  FILLER              PIC X(40)   VALUE                        
012500             '416 FEL VALKOD                         '.                   
012600         05  FILLER              PIC X(40)   VALUE                        
012700             '416 WRONG SELECTION CODE               '.                   
012800     03  FILLER REDEFINES TEXT-416.                                       
012900         05  TEXT-0416 OCCURS 2      PIC X(40).                           
013000*                                                                         
013100     03 TEXT-417.                                                         
013200         05  FILLER              PIC X(40)   VALUE                        
013300             '417 ORDERHUVUD SAKNAS                  '.                   
013400         05  FILLER              PIC X(40)   VALUE                        
013500             '417 ORDER HEADING MISSING              '.                   
013600     03  FILLER REDEFINES TEXT-417.                                       
013700         05  TEXT-0417 OCCURS 2      PIC X(40).                           
013800*                                                                         
013900     03 TEXT-418.                                                         
014000         05  FILLER              PIC X(40)   VALUE                        
014100             '418 VECKODAG EJ IFYLLD                 '.                   
014200         05  FILLER              PIC X(40)   VALUE                        
014300             '418 DAY NOT REGISTRED                  '.                   
014400     03  FILLER REDEFINES TEXT-418.                                       
014500         05  TEXT-0418 OCCURS 2      PIC X(40).                           
014600*                                                                         
014700     03 TEXT-419.                                                         
014800         05  FILLER              PIC X(40)   VALUE                        
014900             '419 EJ DELNING AV KVANTITETSFÖRPACKNING'.                   
015000         05  FILLER              PIC X(40)   VALUE                        
015100             '419SPLIT OF QUANTITYBULKPACK NOT ALLOWED'.                  
015200     03  FILLER REDEFINES TEXT-419.                                       
015300         05  TEXT-0419 OCCURS 2      PIC X(40).                           
015400*                                                                         
015500     03 TEXT-420.                                                         
015600         05  FILLER              PIC X(40)   VALUE                        
015700             '420 KUNDNR. MÅSTE VARA IFYLLT          '.                   
015800         05  FILLER              PIC X(40)   VALUE                        
015900             '420 CUSTOMER NO. MUST BE REGISTRED      '.                  
016000     03  FILLER REDEFINES TEXT-420.                                       
016100         05  TEXT-0420 OCCURS 2      PIC X(40).                           
016200*                                                                         
016300     03 TEXT-421.                                                         
016400         05  FILLER              PIC X(40)   VALUE                        
016500             '421 LEVERANSBESKED SAKNAS              '.                   
016600         05  FILLER              PIC X(40)   VALUE                        
016700             '421 BO AVAILABILITY MISSING             '.                  
016800     03  FILLER REDEFINES TEXT-421.                                       
016900         05  TEXT-0421 OCCURS 2      PIC X(40).                           
017000*                                                                         
017100     03 TEXT-422.                                                         
017200         05  FILLER              PIC X(40)   VALUE                        
017300             '422 BORTTAG AV SATS-ORDERRAD EJ TILLÅTET'.                  
017400         05  FILLER              PIC X(40)   VALUE                        
017500             '422 DELETE OF KITLINE NOT ALLOWED       '.                  
017600     03  FILLER REDEFINES TEXT-422.                                       
017700         05  TEXT-0422 OCCURS 2      PIC X(40).                           
017800*                                                                         
