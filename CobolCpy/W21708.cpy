000100 01  W21708.                                                              
000200     03 IDARTNR              PIC S9(9)           COMP-3.                  
000300     03 IDKAT                OCCURS 3 TIMES                               
000400                             PIC X(5).                                    
000500     03 ADGANG               PIC S9(3)           COMP-3.                  
000600     03 ADINPORT             PIC X(8).                                    
000700     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
000800     03 ADPLATS              PIC S9(5)           COMP-3.                  
000900     03 FILLER               PIC X(4).                                    
001000     03 ADLAGOMR-SVS         PIC S9(3)           COMP-3.                  
001100     03 ADGANG-SVS           PIC S9(3)           COMP-3.                  
001200     03 BEART-ENG            PIC X(25).                                   
001300     03 BEART-SVE            PIC X(25).                                   
001400     03 BEFT                 PIC S9(3)           COMP-3.                  
001500     03 DAPBPLAN             PIC 9(8).                                    
001600     03 FLIART               PIC X.                                       
001700     03 FLJIT                PIC X.                                       
001800     03 FLMANQ               PIC X.                                       
001900     03 FLREFILL             PIC X.                                       
002000     03 FLLSRDEL             PIC X.                                       
002100     03 IDANSK               PIC S9(3)           COMP-3.                  
002200     03 IDBERED              PIC S9(3)           COMP-3.                  
002300     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
002400     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
002500     03 IDARTNR-EMBQ2        PIC S9(9)           COMP-3.                  
002600     03 IDARTNR-EMBQ3        PIC S9(9)           COMP-3.                  
002700     03 IDARTNR-EMBQ4        PIC S9(9)           COMP-3.                  
002800     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
002900     03 IDINK                PIC X(4).                                    
003000     03 IDLEVNR              PIC X(5).                                    
003100     03 IDPROJ               PIC X(4).                                    
003200     03 KDAVT                PIC S9              COMP-3.                  
003300     03 KDERS                PIC S9(3)           COMP-3.                  
003400     03 KDFORP.                                                           
003500        05 KDFORPPL          PIC 9.                                       
003600        05 KDFORPGP          PIC 9(2).                                    
003700        05 KDFORPUF          PIC 9.                                       
003800     03 KDFREKKL             PIC X.                                       
003900     03 KDLEVPLF             PIC X.                                       
004000     03 KDPRISKL             PIC X.                                       
004100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
004200     03 KDSORT               PIC X(2).                                    
004300     03 KDUART               PIC X.                                       
004400     03 KDVVKL               PIC S9              COMP-3.                  
004500     03 KVAKS-CDC            PIC S9(7)           COMP-3.                  
004600     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
004700     03 KVAKS-T              PIC S9(7)           COMP-3.                  
004800     03 KVBR-TOT             PIC S9(7)           COMP-3.                  
004900     03 KVLS-CDC             PIC S9(7)           COMP-3.                  
005000     03 KVLS-NDC             PIC S9(7)           COMP-3.                  
005100     03 KVLS-SDC-LDC         PIC S9(7)           COMP-3.                  
005200     03 KVMAD-SEP            PIC S9(6)V9(1)      COMP-3.                  
005300     03 KVMP                 PIC S9(7)           COMP-3.                  
005400     03 KVPALL               PIC S9(7)           COMP-3.                  
005500     03 KVPB-PLAN            PIC S9(6)V9(1)      COMP-3.                  
005600     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
005700     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
005800     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
005900     03 KVPB-TPO             PIC S9(6)V9(1)      COMP-3.                  
006000     03 KVQ                  PIC S9(7)           COMP-3.                  
006100     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
006200     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
006300     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
006400     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
006500     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
006600     03 KVRESS-CDC           PIC S9(7)           COMP-3.                  
006700     03 KVRESS-NDC           PIC S9(7)           COMP-3.                  
006800     03 KVRESS-SDC-LDC       PIC S9(7)           COMP-3.                  
006900     03 KVROS                PIC S9(7)           COMP-3.                  
007000     03 KVSLAGER             PIC S9(7)           COMP-3.                  
007100     03 KVSPANT              PIC S9(7)           COMP-3.                  
007200     03 KVSPARR-KVAL-CDC     PIC S9(7)           COMP-3.                  
007300     03 KVVECKOR-LT          PIC S9(3)           COMP-3.                  
007400     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
007500     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
007600     03 PRDIRLON             PIC S9(4)V9(3)      COMP-3.                  
007700     03 PRDMTRL              PIC S9(6)V9(3)      COMP-3.                  
007800     03 REDIRLEV             PIC S9V9(2)         COMP-3.                  
007900     03 RESLJUST             PIC S9(2)V9(1)      COMP-3.                  
008000     03 TIDISPIN             PIC S9(7)           COMP-3.                  
008100     03 TIFINLV              PIC S9(5)           COMP-3.                  
008200     03 TIPBDAT              PIC S9(5)           COMP-3.                  
008300     03 TIREFSTO             PIC S9(7)           COMP-3.                  
008400     03 TISLJUST             PIC S9(5)           COMP-3.                  
008500     03 TIURPROD             PIC S9(5)           COMP-3.                  
008600     03 IDLEVNR-SHIP         PIC X(5).                                    
008700     03 VKART                PIC S9(7)           COMP-3.                  
008800     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
008900     03 KVEOQ                PIC S9(7)           COMP-3.                  
009000     03 KVULOAD              PIC S9(7)           COMP-3.                  
009100     03 FLNYBER              PIC X.                                       
009200     03 KVVORKO              PIC S9(7)           COMP-3.                  
009300     03 KDARTURS             PIC X(2).                                    
009400     03 KVPB-TREND           PIC S9(6)V9(1)      COMP-3.                  
009500     03 KVVECKOR-TREND       PIC S9(3)           COMP-3.                  
009600     03 TIDATUM-TREND        PIC S9(7)           COMP-3.                  
009700     03 TILEVDAG             OCCURS 5 TIMES                               
009800                             PIC S9              COMP-3.                  
009900     03 KDOTFREK             PIC X.                                       
010000*** END OF VILMAII-COPY LENGTH= 366 BYTES                                 
