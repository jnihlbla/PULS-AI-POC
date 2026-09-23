000100 01  W2172020.                                                            
000200     03 IDARTNR              PIC S9(9)           COMP-3.                  
000300     03 BEART                PIC X(25).                                   
000400     03 W2172020-WDK6.                                                    
000500        05 BEFT-CLAG         PIC S9(3)           COMP-3.                  
000600        05 FLLSRDEL          PIC X.                                       
000700        05 FLIART            PIC X.                                       
000800        05 IDARTNR-EMBQ0-CLAG                                             
000900                             PIC S9(9)           COMP-3.                  
001000        05 IDARTNR-EMBQ1-CLAG                                             
001100                             PIC S9(9)           COMP-3.                  
001200        05 IDARTNR-EMBQ2-CLAG                                             
001300                             PIC S9(9)           COMP-3.                  
001400        05 IDBERED           PIC S9(3)           COMP-3.                  
001500        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
001600        05 IDKAT             OCCURS 3 TIMES                               
001700                             PIC X(5).                                    
001800        05 IDPROJ            PIC X(4).                                    
001900        05 KDARTURS-CLAG     PIC X(2).                                    
002000        05 KDERS             PIC S9(3)           COMP-3.                  
002100        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
002200        05 KDFORP.                                                        
002300           07 KDFORPPL       PIC 9.                                       
002400           07 KDFORPGP       PIC 9(2).                                    
002500           07 KDFORPUF       PIC 9.                                       
002600        05 KDPRODSL          PIC S9(3)           COMP-3.                  
002700        05 KDSORT            PIC X(2).                                    
002800        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
002900        05 KVTILLG-TOT       PIC S9(7)           COMP-3.                  
003000        05 TIFINLV           PIC S9(5)           COMP-3.                  
003100        05 TIURPROD          PIC S9(5)           COMP-3.                  
003200        05 VKART-CLAG        PIC S9(7)           COMP-3.                  
003300        05 VLARTNTO-CLAG     PIC S9(8)V9(1)      COMP-3.                  
003400     03 W2172020-WDK7.                                                    
003500        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
003600        05 ADGANG            PIC S9(3)           COMP-3.                  
003700        05 ADPLATS           PIC S9(5)           COMP-3.                  
003800        05 BEFT-SLAG         PIC S9(3)           COMP-3.                  
003900        05 DAPBPLAN          PIC 9(8).                                    
004000        05 DAPUBL            PIC 9(8).                                    
004100        05 DASEASON          PIC 9(8).                                    
004200        05 FLFLYG            PIC X.                                       
004300        05 FLJIT             PIC X.                                       
004400        05 FLREFBEO          PIC X.                                       
004500        05 FLWILSON          PIC X.                                       
004600        05 IDANSK            PIC S9(3)           COMP-3.                  
004700        05 IDARTNR-EMBQ0-SLAG                                             
004800                             PIC S9(9)           COMP-3.                  
004900        05 IDARTNR-EMBQ1-SLAG                                             
005000                             PIC S9(9)           COMP-3.                  
005100        05 IDARTNR-EMBQ2-SLAG                                             
005200                             PIC S9(9)           COMP-3.                  
005300        05 IDDC              PIC X(2).                                    
005400        05 IDDC-REF          PIC X(2).                                    
005500        05 IDINK             PIC X(4).                                    
005600        05 IDLEVNR           PIC X(5).                                    
005700        05 IDLEVNR-SHIP      PIC X(5).                                    
005800        05 IDREFTAB          PIC X.                                       
005900        05 KDARTURS-SLAG     PIC X(2).                                    
006000        05 KDAVT             PIC S9              COMP-3.                  
006100        05 KDFPKPRI          PIC X.                                       
006200        05 KDLEVPLF          PIC X.                                       
006300        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
006400        05 KVAKS-SDC         PIC S9(7)           COMP-3.                  
006500        05 KVEOQ             PIC S9(7)           COMP-3.                  
006600        05 KVLS              PIC S9(7)           COMP-3.                  
006700        05 KVPALL            PIC S9(7)           COMP-3.                  
006800        05 KVPB-PLAN         PIC S9(6)V9(1)      COMP-3.                  
006900        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
007000        05 KVPBREOI          PIC S9(6)V9(1)      COMP-3.                  
007100        05 KVPB-TREND        PIC S9(6)V9(1)      COMP-3.                  
007200        05 KVREFBER          PIC S9(7)           COMP-3.                  
007300        05 KVREFOVL          PIC S9(7)           COMP-3.                  
007400        05 KVREFPKT          PIC S9(7)           COMP-3.                  
007500        05 KVRESS            PIC S9(7)           COMP-3.                  
007600        05 KVROS-BULK        PIC S9(7)           COMP-3.                  
007700        05 KVROS-DAG         PIC S9(7)           COMP-3.                  
007800        05 KVSLAGER          PIC S9(7)           COMP-3.                  
007900        05 KVSPANT           PIC S9(7)           COMP-3.                  
008000        05 KVULOAD           PIC S9(7)           COMP-3.                  
008100        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
008200        05 KVVECKOR-TREND    PIC S9(3)           COMP-3.                  
008300        05 PRAVCOST          PIC S9(7)V9(2)      COMP-3.                  
008400        05 PRMATRL           PIC S9(7)V9(2)      COMP-3.                  
008500        05 RESEASON-PLAN     OCCURS 12 TIMES                              
008600                             PIC S9V9(2)         COMP-3.                  
008700        05 TIDATUM-TREND     PIC S9(7)           COMP-3.                  
008800        05 TIERSDAT-VIPS     PIC S9(5)           COMP-3.                  
008900        05 TILEVDAG          OCCURS 5 TIMES                               
009000                             PIC S9              COMP-3.                  
009100        05 TIMANSEC          PIC S9(7)           COMP-3.                  
009200        05 TIREFPAF          PIC S9(7)           COMP-3.                  
009300        05 TIREFPKT          PIC S9(7)           COMP-3.                  
009400        05 TIREFSTO-LOC      PIC S9(7)           COMP-3.                  
009500        05 VKART-SLAG        PIC S9(7)           COMP-3.                  
009600        05 VLARTNTO-SLAG     PIC S9(8)V9(1)      COMP-3.                  
009700     03 W2172020-WDL7.                                                    
009800        05 KVOI-12-RULL      PIC S9(7)           COMP-3.                  
009900        05 KVOI-YEAR-0       PIC S9(7)           COMP-3.                  
010000        05 KVOI-YEAR-1       PIC S9(7)           COMP-3.                  
010100        05 KVOI-YEAR-2       PIC S9(7)           COMP-3.                  
010200        05 KVOI-YEAR-3       PIC S9(7)           COMP-3.                  
010300        05 KVOI-YEAR-4       PIC S9(7)           COMP-3.                  
010400        05 KVOI-YEAR-5       PIC S9(7)           COMP-3.                  
010500     03 W2172020-SERVICE.                                                 
010600        05 SUINKORD          PIC S9(18)          COMP-3.                  
010700        05 SUAVBRP           PIC S9(5)V9(2)      COMP-3.                  
010800        05 RESERVG-NTO       PIC S9(3)V9(2)      COMP-3.                  
010900*** END OF VILMAII-COPY LENGTH= 380 BYTES                                 
