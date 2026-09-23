000100 01  W90423O1.                                                            
000200*                                 COPYTEXT FÖR MOD W9042301               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE              PIC X(41).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 FILLER               PIC X(9).                                    
000800     03 ARTIKEL-UT.                                                       
000900        05 FILLER            PIC X(9).                                    
001000        05 FILLER            PIC X.                                       
001100        05 FILLER            PIC 9.                                       
001200     03 FILLER               PIC X(25).                                   
001300     03 AREA-OUTPUT.                                                      
001400*                                                                         
001500        05 FILLER            PIC 9(3).                                    
001600        05 FILLER            PIC X(5).                                    
001700        05 VIP-GRP.                                                       
001800*                                                                         
001900           07 FILLER         PIC 9.                                       
002000           07 FILLER         PIC X.                                       
002100        05 FILLER            PIC 9.                                       
002200        05 KDAVT             PIC 9.                                       
002300*                                 AVTALSMÄRKNING                          
002400        05 FILLER            PIC 9.                                       
002500        05 FILLER            PIC X(11).                                   
002600        05 FILLER            PIC X(11).                                   
002700        05 FILLER            PIC Z(6)9.                                   
002800        05 FILLER            PIC Z(6)9.                                   
002900        05 FILLER            PIC X(5).                                    
003000        05 FILLER            PIC X(8).                                    
003100        05 KOLUMN-1-3.                                                    
003200*                                                                         
003300           07 KVLS           OCCURS 3 TIMES                               
003400                             PIC -(7)9.                                   
003500*                                 LAGERSALDO                              
003600           07 FILLER         OCCURS 3 TIMES                               
003700                             PIC Z(7)9.                                   
003800           07 DISP           OCCURS 3 TIMES                               
003900                             PIC -(7)9.                                   
004000*                                 DISPONIBELT LAGER                       
004100           07 FILLER         OCCURS 3 TIMES                               
004200                             PIC Z(7)9.                                   
004300           07 KVAKS-LAGER    OCCURS 3 TIMES                               
004400                             PIC -(7)9.                                   
004500*                                 ANKOMSTSALDO                            
004600           07 FILLER         OCCURS 3 TIMES                               
004700                             PIC Z(7)9.                                   
004800           07 FILLER         OCCURS 3 TIMES                               
004900                             PIC Z(7)9.                                   
005000           07 FILLER         OCCURS 3 TIMES                               
005100                             PIC Z(7)9.                                   
005200           07 FILLER         OCCURS 3 TIMES                               
005300                             PIC Z(7)9.                                   
005400           07 FILLER         OCCURS 3 TIMES                               
005500                             PIC Z(7)9.                                   
005600           07 ARB-SALDO      OCCURS 3 TIMES                               
005700                             PIC -(7)9.                                   
005800*                                 LAGERTILLGÅNG                           
005900           07 FILLER         OCCURS 3 TIMES                               
006000                             PIC Z(7)9.                                   
006100           07 FILLER         PIC Z(7)9.                                   
006200           07 FILLER         PIC Z(6)9.                                   
006300        05 KOLUMN-4-6.                                                    
006400*                                                                         
006500           07 FILLER         PIC Z(6)9.                                   
006600           07 FILLER         PIC Z(6)9.                                   
006700           07 FILLER         OCCURS 3 TIMES                               
006800                             PIC 9(10).                                   
006900           07 FILLER         OCCURS 3 TIMES                               
007000                             PIC 9(10).                                   
007100           07 FILLER         OCCURS 3 TIMES                               
007200                             PIC 9(10).                                   
007300           07 FILLER         OCCURS 3 TIMES                               
007400                             PIC 9(10).                                   
007500           07 FILLER         OCCURS 3 TIMES                               
007600                             PIC 9(10).                                   
007700           07 FILLER         OCCURS 3 TIMES                               
007800                             PIC 9(10).                                   
007900           07 LAGERPLATS.                                                 
008000              09 ADLAGOMR    PIC Z(2)9.                                   
008100*                                 LAGEROMRÅDE                             
008200              09 ADGANG      PIC Z(2)9.                                   
008300*                                 GÅNG                                    
008400              09 ADPLATS     PIC Z(5)9.                                   
008500*                                                     ADPLATS-002         
008600*                                 LAGERPLATS OMR. GÅNG PLATS              
008700           07 FILLER         PIC 9(10).                                   
008800           07 FILLER         PIC Z(7)9.                                   
008900           07 FILLER         PIC Z(7)9.                                   
009000           07 FILLER         PIC Z(7)9.                                   
009100           07 FILLER         OCCURS 3 TIMES                               
009200                             PIC 9(10).                                   
009300           07 FILLER         PIC Z(7)9.                                   
009400     03 FILLER               PIC 9(6).                                    
009500     03 FILLER               PIC Z(6)9.                                   
009600     03 FILLER               PIC X.                                       
009700     03 TEMFSINF             PIC X(55).                                   
009800*                                 INFORMATIONSMEDDELANDE                  
009900*** END OF VILMAII-COPY LENGTH= 802 BYTES                                 
