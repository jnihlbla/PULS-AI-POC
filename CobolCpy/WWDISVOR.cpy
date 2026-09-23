000100*** EDIT ALLOWED                                                          
000200*                                                                         
000300******************************************************************        
000400* THIS TABLE IS USED IN W41218 BMP TO CHECK IF THE DIST/DC                
000500* COMBINATION IS FOUND OR NOT. IF FOUND, THEN WE DON'T RELEASE            
000600* VOR ORDERS FROM THESE DC'S WITH ORDER LINES GREATER THAN                
000700* 20KG OR 0.1M3                                                           
000800******************************************************************        
000900*                                                                         
001000*                                                                         
001100 01  WWDC00-TAB-DIST-DC.                                                  
001200     03  FILLER PIC X(6) VALUE '077824'.                                  
001300     03  FILLER PIC X(6) VALUE '077826'.                                  
001400     03  FILLER PIC X(6) VALUE '07783C'.                                  
001500     03  FILLER PIC X(6) VALUE '07783F'.                                  
001600     03  FILLER PIC X(6) VALUE '07783O'.                                  
001700     03  FILLER PIC X(6) VALUE '07783P'.                                  
001800     03  FILLER PIC X(6) VALUE '07783R'.                                  
001900     03  FILLER PIC X(6) VALUE '07783S'.                                  
002000*                                                                         
002100     03  FILLER PIC X(6) VALUE '097424'.                                  
002200     03  FILLER PIC X(6) VALUE '097426'.                                  
002300     03  FILLER PIC X(6) VALUE '09743C'.                                  
002400     03  FILLER PIC X(6) VALUE '09743F'.                                  
002500     03  FILLER PIC X(6) VALUE '09743O'.                                  
002600     03  FILLER PIC X(6) VALUE '09743P'.                                  
002700     03  FILLER PIC X(6) VALUE '09743R'.                                  
002800     03  FILLER PIC X(6) VALUE '09743S'.                                  
002900*                                                                         
003000     03  FILLER PIC X(6) VALUE '097824'.                                  
003100     03  FILLER PIC X(6) VALUE '097826'.                                  
003200     03  FILLER PIC X(6) VALUE '09783C'.                                  
003300     03  FILLER PIC X(6) VALUE '09783F'.                                  
003400     03  FILLER PIC X(6) VALUE '09783O'.                                  
003500     03  FILLER PIC X(6) VALUE '09783P'.                                  
003600     03  FILLER PIC X(6) VALUE '09783R'.                                  
003700     03  FILLER PIC X(6) VALUE '09783S'.                                  
003800*                                                                         
003900     03  FILLER PIC X(6) VALUE '109024'.                                  
004000     03  FILLER PIC X(6) VALUE '109026'.                                  
004100     03  FILLER PIC X(6) VALUE '10903C'.                                  
004200     03  FILLER PIC X(6) VALUE '10903F'.                                  
004300     03  FILLER PIC X(6) VALUE '10903P'.                                  
004400     03  FILLER PIC X(6) VALUE '10903R'.                                  
004500     03  FILLER PIC X(6) VALUE '10903S'.                                  
004600*                                                                         
004700     03  FILLER PIC X(6) VALUE '125824'.                                  
004800     03  FILLER PIC X(6) VALUE '125826'.                                  
004900     03  FILLER PIC X(6) VALUE '12583F'.                                  
005000     03  FILLER PIC X(6) VALUE '12583O'.                                  
005100     03  FILLER PIC X(6) VALUE '12583P'.                                  
005200     03  FILLER PIC X(6) VALUE '12583S'.                                  
005300*                                                                         
005400     03  FILLER PIC X(6) VALUE '147824'.                                  
005500     03  FILLER PIC X(6) VALUE '147826'.                                  
005600     03  FILLER PIC X(6) VALUE '14783C'.                                  
005700     03  FILLER PIC X(6) VALUE '14783F'.                                  
005710     03  FILLER PIC X(6) VALUE '14783G'.                                  
005720     03  FILLER PIC X(6) VALUE '14783M'.                                  
005800     03  FILLER PIC X(6) VALUE '14783O'.                                  
005900     03  FILLER PIC X(6) VALUE '14783S'.                                  
006000*                                                                         
006100     03  FILLER PIC X(6) VALUE '155824'.                                  
006200     03  FILLER PIC X(6) VALUE '155826'.                                  
006300     03  FILLER PIC X(6) VALUE '15583C'.                                  
006310     03  FILLER PIC X(6) VALUE '15583G'.                                  
006320     03  FILLER PIC X(6) VALUE '15583M'.                                  
006400     03  FILLER PIC X(6) VALUE '15583O'.                                  
006500     03  FILLER PIC X(6) VALUE '15583P'.                                  
006600     03  FILLER PIC X(6) VALUE '15583S'.                                  
006700*                                                                         
006800     03  FILLER PIC X(6) VALUE '167824'.                                  
006900     03  FILLER PIC X(6) VALUE '167826'.                                  
007000     03  FILLER PIC X(6) VALUE '16783F'.                                  
007010     03  FILLER PIC X(6) VALUE '16783G'.                                  
007020     03  FILLER PIC X(6) VALUE '16783M'.                                  
007100     03  FILLER PIC X(6) VALUE '16783O'.                                  
007200     03  FILLER PIC X(6) VALUE '16783P'.                                  
007300     03  FILLER PIC X(6) VALUE '16783S'.                                  
007400*                                                                         
007500     03  FILLER PIC X(6) VALUE '177824'.                                  
007600     03  FILLER PIC X(6) VALUE '177826'.                                  
007700     03  FILLER PIC X(6) VALUE '17783C'.                                  
007800     03  FILLER PIC X(6) VALUE '17783F'.                                  
007810     03  FILLER PIC X(6) VALUE '17783G'.                                  
007820     03  FILLER PIC X(6) VALUE '17783M'.                                  
007830     03  FILLER PIC X(6) VALUE '17783O'.                                  
007900     03  FILLER PIC X(6) VALUE '17783P'.                                  
008000     03  FILLER PIC X(6) VALUE '17783S'.                                  
008100*                                                                         
008200     03  FILLER PIC X(6) VALUE '182224'.                                  
008300     03  FILLER PIC X(6) VALUE '182226'.                                  
008400     03  FILLER PIC X(6) VALUE '18223C'.                                  
008410     03  FILLER PIC X(6) VALUE '18223G'.                                  
008420     03  FILLER PIC X(6) VALUE '18223M'.                                  
008500     03  FILLER PIC X(6) VALUE '18223O'.                                  
008600     03  FILLER PIC X(6) VALUE '18223P'.                                  
008700     03  FILLER PIC X(6) VALUE '18223S'.                                  
008800*                                                                         
008900     03  FILLER PIC X(6) VALUE '195826'.                                  
009000     03  FILLER PIC X(6) VALUE '19583C'.                                  
009100     03  FILLER PIC X(6) VALUE '19583F'.                                  
009110     03  FILLER PIC X(6) VALUE '19583G'.                                  
009120     03  FILLER PIC X(6) VALUE '19583M'.                                  
009200     03  FILLER PIC X(6) VALUE '19583O'.                                  
009300     03  FILLER PIC X(6) VALUE '19583P'.                                  
009400     03  FILLER PIC X(6) VALUE '19583S'.                                  
009500*                                                                         
009600     03  FILLER PIC X(6) VALUE '217826'.                                  
009700     03  FILLER PIC X(6) VALUE '21783C'.                                  
009800     03  FILLER PIC X(6) VALUE '21783F'.                                  
009810     03  FILLER PIC X(6) VALUE '21783G'.                                  
009820     03  FILLER PIC X(6) VALUE '21783M'.                                  
009900     03  FILLER PIC X(6) VALUE '21783O'.                                  
010000     03  FILLER PIC X(6) VALUE '21783P'.                                  
010100     03  FILLER PIC X(6) VALUE '21783S'.                                  
010200*                                                                         
010210     03  FILLER PIC X(6) VALUE '227824'.                                  
010300     03  FILLER PIC X(6) VALUE '227826'.                                  
010400     03  FILLER PIC X(6) VALUE '22783C'.                                  
010500     03  FILLER PIC X(6) VALUE '22783F'.                                  
010510     03  FILLER PIC X(6) VALUE '22783G'.                                  
010520     03  FILLER PIC X(6) VALUE '22783M'.                                  
010600     03  FILLER PIC X(6) VALUE '22783O'.                                  
010700     03  FILLER PIC X(6) VALUE '22783P'.                                  
010800     03  FILLER PIC X(6) VALUE '22783S'.                                  
010900*                                                                         
011000     03  FILLER PIC X(6) VALUE '237824'.                                  
011100     03  FILLER PIC X(6) VALUE '23783C'.                                  
011200     03  FILLER PIC X(6) VALUE '23783F'.                                  
011300     03  FILLER PIC X(6) VALUE '23783O'.                                  
011400     03  FILLER PIC X(6) VALUE '23783P'.                                  
011500     03  FILLER PIC X(6) VALUE '23783S'.                                  
011600*                                                                         
011700     03  FILLER PIC X(6) VALUE '236424'.                                  
011800     03  FILLER PIC X(6) VALUE '23643C'.                                  
011900     03  FILLER PIC X(6) VALUE '23643F'.                                  
012000     03  FILLER PIC X(6) VALUE '23643O'.                                  
012100     03  FILLER PIC X(6) VALUE '23643P'.                                  
012200     03  FILLER PIC X(6) VALUE '23643S'.                                  
012300*                                                                         
012400     03  FILLER PIC X(6) VALUE '236524'.                                  
012500     03  FILLER PIC X(6) VALUE '23653C'.                                  
012600     03  FILLER PIC X(6) VALUE '23653F'.                                  
012700     03  FILLER PIC X(6) VALUE '23653O'.                                  
012800     03  FILLER PIC X(6) VALUE '23653P'.                                  
012900     03  FILLER PIC X(6) VALUE '23653S'.                                  
013000*                                                                         
013100     03  FILLER PIC X(6) VALUE '287824'.                                  
013200     03  FILLER PIC X(6) VALUE '287826'.                                  
013300     03  FILLER PIC X(6) VALUE '28783C'.                                  
013400     03  FILLER PIC X(6) VALUE '28783F'.                                  
013500     03  FILLER PIC X(6) VALUE '28783O'.                                  
013600     03  FILLER PIC X(6) VALUE '28783P'.                                  
013700*                                                                         
013800*                                                                         
013900 01  WWDC00-DIST-DC-TAB REDEFINES WWDC00-TAB-DIST-DC.                     
014000     03  WWDC00-DIST-DC   OCCURS 127 TIMES                                
014100                       ASCENDING KEY IS WWDC00-SOK-DIST-DC                
014200                       INDEXED BY WWDC00-IX.                              
014300       05  WWDC00-SOK-DIST-DC PIC X(06).                                  
014400       05  FILLER REDEFINES WWDC00-SOK-DIST-DC.                           
014500         07  WWDC00-IDDISTR    PIC 9(4).                                  
014600         07  WWDC00-DC         PIC X(2).                                  
014700*                                                                         
