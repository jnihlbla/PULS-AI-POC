000100 01  W21709.                                                              
000200     03 W21708-RECORD.                                                    
000300        05 IDARTNR           PIC S9(9)           COMP-3.                  
000400        05 IDKAT             OCCURS 3 TIMES                               
000500                             PIC X(5).                                    
000600        05 ADGANG            PIC S9(3)           COMP-3.                  
000700        05 ADINPORT          PIC X(8).                                    
000800        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
000900        05 ADPLATS           PIC S9(5)           COMP-3.                  
001000        05 FILLER            PIC X(4).                                    
001100        05 ADLAGOMR-SVS      PIC S9(3)           COMP-3.                  
001200        05 ADGANG-SVS        PIC S9(3)           COMP-3.                  
001300        05 BEART-ENG         PIC X(25).                                   
001400        05 BEART-SVE         PIC X(25).                                   
001500        05 BEFT              PIC S9(3)           COMP-3.                  
001600        05 DAPBPLAN          PIC 9(8).                                    
001700        05 FLIART            PIC X.                                       
001800        05 FLJIT             PIC X.                                       
001900        05 FLMANQ            PIC X.                                       
002000        05 FLREFILL          PIC X.                                       
002100        05 FLLSRDEL          PIC X.                                       
002200        05 IDANSK            PIC S9(3)           COMP-3.                  
002300        05 IDBERED           PIC S9(3)           COMP-3.                  
002400        05 IDARTNR-EMBQ0     PIC S9(9)           COMP-3.                  
002500        05 IDARTNR-EMBQ1     PIC S9(9)           COMP-3.                  
002600        05 IDARTNR-EMBQ2     PIC S9(9)           COMP-3.                  
002700        05 IDARTNR-EMBQ3     PIC S9(9)           COMP-3.                  
002800        05 IDARTNR-EMBQ4     PIC S9(9)           COMP-3.                  
002900        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
003000        05 IDINK             PIC X(4).                                    
003100        05 IDLEVNR           PIC X(5).                                    
003200        05 IDPROJ            PIC X(4).                                    
003300        05 KDAVT             PIC S9              COMP-3.                  
003400        05 KDERS             PIC S9(3)           COMP-3.                  
003500        05 KDFORP.                                                        
003600           07 KDFORPPL       PIC 9.                                       
003700           07 KDFORPGP       PIC 9(2).                                    
003800           07 KDFORPUF       PIC 9.                                       
003900        05 KDFREKKL          PIC X.                                       
004000        05 KDLEVPLF          PIC X.                                       
004100        05 KDPRISKL          PIC X.                                       
004200        05 KDPRODSL          PIC S9(3)           COMP-3.                  
004300        05 KDSORT            PIC X(2).                                    
004400        05 KDUART            PIC X.                                       
004500        05 KDVVKL            PIC S9              COMP-3.                  
004600        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
004700        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
004800        05 KVAKS-T           PIC S9(7)           COMP-3.                  
004900        05 KVBR-TOT          PIC S9(7)           COMP-3.                  
005000        05 KVLS-CDC          PIC S9(7)           COMP-3.                  
005100        05 KVLS-NDC          PIC S9(7)           COMP-3.                  
005200        05 KVLS-SDC-LDC      PIC S9(7)           COMP-3.                  
005300        05 KVMAD-SEP         PIC S9(6)V9(1)      COMP-3.                  
005400        05 KVMP              PIC S9(7)           COMP-3.                  
005500        05 KVPALL            PIC S9(7)           COMP-3.                  
005600        05 KVPB-PLAN         PIC S9(6)V9(1)      COMP-3.                  
005700        05 KVPB-REF          PIC S9(6)V9(1)      COMP-3.                  
005800        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
005900        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
006000        05 KVPB-TPO          PIC S9(6)V9(1)      COMP-3.                  
006100        05 KVQ               PIC S9(7)           COMP-3.                  
006200        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
006300        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
006400        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
006500        05 KVQPACK-3         PIC S9(5)           COMP-3.                  
006600        05 KVQPACK-4         PIC S9(5)           COMP-3.                  
006700        05 KVRESS-CDC        PIC S9(7)           COMP-3.                  
006800        05 KVRESS-NDC        PIC S9(7)           COMP-3.                  
006900        05 KVRESS-SDC-LDC    PIC S9(7)           COMP-3.                  
007000        05 KVROS             PIC S9(7)           COMP-3.                  
007100        05 KVSLAGER          PIC S9(7)           COMP-3.                  
007200        05 KVSPANT           PIC S9(7)           COMP-3.                  
007300        05 KVSPARR-KVAL-CDC  PIC S9(7)           COMP-3.                  
007400        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
007500        05 PRARTBES          PIC S9(7)V9(2)      COMP-3.                  
007600        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
007700        05 PRDIRLON          PIC S9(4)V9(3)      COMP-3.                  
007800        05 PRDMTRL           PIC S9(6)V9(3)      COMP-3.                  
007900        05 REDIRLEV          PIC S9V9(2)         COMP-3.                  
008000        05 RESLJUST          PIC S9(2)V9(1)      COMP-3.                  
008100        05 TIDISPIN          PIC S9(7)           COMP-3.                  
008200        05 TIFINLV           PIC S9(5)           COMP-3.                  
008300        05 TIPBDAT           PIC S9(5)           COMP-3.                  
008400        05 TIREFSTO          PIC S9(7)           COMP-3.                  
008500        05 TISLJUST          PIC S9(5)           COMP-3.                  
008600        05 TIURPROD          PIC S9(5)           COMP-3.                  
008700        05 IDLEVNR-SHIP      PIC X(5).                                    
008800        05 VKART             PIC S9(7)           COMP-3.                  
008900        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
009000        05 KVEOQ             PIC S9(7)           COMP-3.                  
009100        05 KVULOAD           PIC S9(7)           COMP-3.                  
009200        05 FLNYBER           PIC X.                                       
009300        05 KVVORKO           PIC S9(7)           COMP-3.                  
009400        05 KDARTURS          PIC X(2).                                    
009500        05 KVPB-TREND        PIC S9(6)V9(1)      COMP-3.                  
009600        05 KVVECKOR-TREND    PIC S9(3)           COMP-3.                  
009700        05 TIDATUM-TREND     PIC S9(7)           COMP-3.                  
009800        05 TILEVDAG          OCCURS 5 TIMES                               
009900                             PIC S9              COMP-3.                  
010000        05 KDOTFREK          PIC X.                                       
010100     03 KVINORD              PIC S9(7)           COMP-3.                  
010200     03 KVAVBRAD             PIC S9(7)V9(2)      COMP-3.                  
010300     03 KVOI-12-RULL         PIC S9(7)           COMP-3.                  
010400     03 KVOI-YEAR-0          PIC S9(7)           COMP-3.                  
010500     03 KVOI-YEAR-1          PIC S9(7)           COMP-3.                  
010600     03 KVOI-YEAR-2          PIC S9(7)           COMP-3.                  
010700     03 KVOI-YEAR-3          PIC S9(7)           COMP-3.                  
010800     03 KVOI-YEAR-4          PIC S9(7)           COMP-3.                  
010900     03 KVOI-YEAR-5          PIC S9(7)           COMP-3.                  
011000*** END OF VILMAII-COPY LENGTH= 403 BYTES                                 
