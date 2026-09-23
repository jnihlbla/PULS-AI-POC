000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W2214030.                                            
000400 AUTHOR.             IDK, GÖREBORG.                                       
000500 DATE-WRITTEN.       NOV  1978.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*        PROGRAMMET ÄR ETT SUBPROGRAM                                     
001100*        VILKET BERÄKNAR STYRVÄRDEN (PUNKTER) SOM KRÄVS                   
001200*        FÖR ATT UTFÄRDA BESTÄLLNINGAR OCH LEVERANSPLANER.                
001300*        KOMMUNIKATION MED HUVUDPROGRAMMET SKER MED                       
001400*        LINK-AREA                                                        
001500*        BERÄKNADE VECKOBEHOV FÖR ARTIKELN FINNS I                        
001600*        LNK2-AREA                                                        
001700*    SUBPROGRAM:                                                          
001800*            W009VADD    ADD AV VECKOR TILL DATUM                         
001900*            W221LPAD    UPPDATERING AV TABELL KDLPORS-TAB                
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500     SKIP2                                                                
002501*    -COPY WY2000W3                                                       
002510     SKIP3                                                                
002600 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
002700     SKIP3                                                                
002800 01  KONSTANTER.                                                          
002900     03  JA                  PIC X       VALUE 'J'.                       
003000     03  NEJ                 PIC X       VALUE 'N'.                       
003100     SKIP1                                                                
003200     03  QF-LEVNR1           PIC S9(5)   VALUE +1165 COMP-3.              
003300     03  QF-LEVNR2           PIC S9(5)   VALUE +1166 COMP-3.              
003400     03  QF-LEVNR3           PIC S9(5)   VALUE +1620 COMP-3.              
003500     03  ANTAL-KDVVKL-KLASSER                                             
003600                             PIC S9(9)   VALUE +5    COMP-3.              
003700     SKIP3                                                                
003800 01  W-ARBETSAREOR.                                                       
003900     03  TOT-PB              PIC S9(8)V9             COMP-3.              
004000     03  W-KDVVKL-GAM        PIC S9(1)               COMP-3.              
004100     03  W-KDVVKL-NY         PIC S9(1)               COMP-3.              
004200     03  W-KVPB-TOT          PIC S9(6)V9(3)          COMP-3.              
004300     03  W-MIN-SL            PIC S9(8)V9(3)          COMP-3.              
004400     03  W-MAX-SL            PIC S9(8)V9(3)          COMP-3.              
004500     03  W-TIAAVV            PIC S9(5)               COMP-3.              
004600     03  W-KVSLUTKP          PIC S9(9)               COMP-3.              
004700     03  W-KVOVERF           PIC S9(9)               COMP-3.              
004800     03  W-KDLPORS           PIC S9(3)               COMP-3.              
004900     03  WS-KVFRYSTIPLUS1    PIC S9(3)               COMP-3.              
005000     03  WS-TIFINLV-AAVV     PIC S9(5)               COMP-3.              
005100     SKIP1                                                                
005200     03  W-PRVARDE-AAR       PIC S9(9)V9(2)          COMP-3.              
005300     03  W-ARSOMS            PIC S9(9)V9(2)          COMP-3.              
005400     03  W-SUM-BEHOV         PIC S9(8)V9(3)          COMP-3.              
005500     03  W-ANTAL             PIC S9(3)               COMP-3.              
005600     03  W-KVANTAL           PIC S9(6)               COMP-3.              
005700     03  W-REF               PIC S9(6)V9(3)          COMP-3.              
005800     03  W-FAKT-CLAG         OCCURS 2                                     
005900                             PIC S9(6)V9(3)          COMP-3.              
006000     03  W-KVVECKOR-GARD     PIC S9(5)               COMP-3.              
006100     03  W-NETTOTILLGANG     PIC S9(9)               COMP-3.              
006200     03  W-IDEALLAGER        PIC S9(9)               COMP-3.              
006300     03  W-M                 PIC S9(7)               COMP-3.              
006400     SKIP1                                                                
006500     03  W-DATUM-AAP         PIC 9(3).                                    
006600     03  W-DAT-AAP REDEFINES W-DATUM-AAP.                                 
006700         05  W-DAT-AA        PIC 9(2).                                    
006800         05  W-DAT-P         PIC 9(1).                                    
006900     SKIP1                                                                
007000     03  W-DATUM-TOM         PIC 9(4).                                    
007100     03  W-DATUM-FROM        PIC 9(4).                                    
007200     03  W-DATUM-AAVV.                                                    
007300         05  W-DATUM-AA      PIC 9(2).                                    
007400         05  W-DATUM-VV      PIC 9(2).                                    
007500     03  W-DIFF-AA           PIC S9(3)               COMP-3.              
007600     03  W-VECKO-DIFFERENS   PIC S9(3)               COMP-3.              
007700     SKIP3                                                                
007800     03  W-IDLEVNR           PIC X(5).                                    
007900     03  W-IDLEVNR-RED REDEFINES W-IDLEVNR.                               
008000         05  FILLER          PIC X(1).                                    
008100         05  W-IDLEVNR-KOLL  PIC X(1).                                    
008200         05  FILLER          PIC X(3).                                    
008300     SKIP2                                                                
008400     03  FILLER OCCURS 2.                                                 
008500         05  W-KVMAD-TOTTOT  PIC S9(6)V9(1) COMP-3.                       
008600     03  W-KVSLAGER          PIC S9(7)V9(2) COMP-3.                       
008700     03  W-STYCKFORS         PIC S9(7)  VALUE ZERO.                       
008800     EJECT                                                                
008900 01  W-INDEX.                                                             
009000     03  IX                  PIC S9(9)   VALUE +0    COMP SYNC.           
009100     03  IX1                 PIC S9(9)   VALUE +0    COMP SYNC.           
009200     03  IX2                 PIC S9(9)   VALUE +0    COMP SYNC.           
009300     03  IX3                 PIC S9(9)   VALUE +0    COMP SYNC.           
009400     03  IX-M                PIC S9(9)   VALUE +0    COMP SYNC.           
009500     03  IX-START-VECKA      PIC S9(9)   VALUE +0    COMP SYNC.           
009600     03  IX-ANTAL-VECKOR     PIC S9(9)   VALUE +0    COMP SYNC.           
009700     03  IX-VECKA            PIC S9(9)   VALUE +0    COMP SYNC.           
009800     03  TABW200-INDEX       PIC S9(9)   VALUE +0    COMP SYNC.           
009900     SKIP3                                                                
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
010200     03  W221LPAD            PIC X(8)    VALUE 'W221LPAD'.                
010300     03  W221PUNK                PIC X(8)    VALUE 'W221PUNK'.            
010400     EJECT                                                                
010500*    --- LÄNKAREA TILL SUBPROGRAM W221PUNK                                
010600*01 -COPY W221PUNK    -PRE PUNK-                                          
010700     EJECT                                                                
010800*01  -COPY W200W001                                                       
010900     EJECT                                                                
011000*                            ** BYTES-ARTIKEL                   **        
011100 01  FILLER                  PIC X(16)   VALUE 'BYTES-ARTIKEL'.           
011200 01  TEST-IDARTNR            PIC 9(9)    COMP-3.                          
011300*01  FILLER  -COPY WWBYT02   -RED TEST-IDARTNR.                           
011400     EJECT                                                                
011500*                            *************************************        
011600*                            ** VÄRDE-TABELL                    **        
011700*                            *************************************        
011800*01  AREA  -COPY W221W010       -PRE W010-                                
011900     EJECT                                                                
012000*                            *************************************        
012100*                            **                                 **        
012200*                            *************************************        
012300*01  AREA  -COPY W221W011       -PRE W011-.                               
012400     EJECT                                                                
012500*                            *************************************        
012600*                            **                                 **        
012700*                            *************************************        
012800*01  AREA  -COPY W221W012       -PRE W012-.                               
012900     EJECT                                                                
013000*01  -COPY W221W099                                                       
013100     EJECT                                                                
013200 LINKAGE SECTION.                                                         
013300     SKIP3                                                                
013400*                            *************************************        
013500*                            **LINK-AREA                        **        
013600*                            **KOMMUNIKATION MED HUVUDPROGRAM   **        
013700*                            *************************************        
013800*01  AREA  -COPY W221L401   -PRE LINK-.                                   
013900     EJECT                                                                
014000*                            *************************************        
014100*                            ** LNK2-AREA                       **        
014200*                            ** BERÄKNADE VECKO-BEHOV           **        
014300*                            *************************************        
014400*01  AREA  -COPY W222L222   -PRE LNK2-.                                   
014500     EJECT                                                                
014510*01  -COPY W0008  -PRE WDP6-.                                             
014520     05  FILLER                  PIC X.                                   
014530     EJECT                                                                
014540*01  -COPY W0008  -PRE WDF1-.                                             
014550     05  FILLER                  PIC X.                                   
014560     EJECT                                                                
014600 PROCEDURE DIVISION USING LINK-AREA LNK2-AREA WDP6-PCB WDF1-PCB.          
014700     SKIP2                                                                
014800     PERFORM A-INITIERA                                                   
014900     SKIP1                                                                
015000     PERFORM B-BERAKNA-KVPB-TPO                                           
015100     PERFORM C-INITIERA-PUNK-PARM                                         
015200                                                                          
015300     CALL W221PUNK USING PUNK-W221PUNK LNK2-AREA WDP6-PCB                 
015400                                                 WDF1-PCB                 
015500     PERFORM D-SPARA-PUNK-PARM                                            
015600     PERFORM S02-BERAKNA-NETTO-IDEALLAG                                   
015700     PERFORM E-BERAKNA-KVSLUTKP                                           
015800     PERFORM F-BERAKNA-KVOVERF                                            
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INITIERA SECTION.                                                      
016500     SKIP3                                                                
016600     MOVE ZERO TO W-STYCKFORS                                             
016700     .                                                                    
016800     EJECT                                                                
016900 B-BERAKNA-KVPB-TPO SECTION.                                              
017000******************************************************************        
017100*                                                                *        
017200*    KONTROLL OCH BERÄKNING AV KVPB-TPO                          *        
017300*                                                                         
017400*                                                                *        
017500******************************************************************        
017600                                                                          
017700     MOVE +0             TO WS-KVFRYSTIPLUS1                              
017800     MOVE +0             TO WS-TIFINLV-AAVV                               
017900     COMPUTE WS-KVFRYSTIPLUS1 = LINK-KVFRYSTI + 1                         
018000     COMPUTE WS-KVFRYSTIPLUS1 = WS-KVFRYSTIPLUS1 * -1                     
018100     COMPUTE WS-TIFINLV-AAVV  = LINK-TIFINLV / 10                         
018200     CALL W009VADD USING WS-TIFINLV-AAVV WS-KVFRYSTIPLUS1                 
018300                                                                          
018301     MOVE WS-TIFINLV-AAVV   TO TMP1-YYWW                                  
018302     MOVE LINK-TIAAVV-AKT   TO TMP2-YYWW                                  
018310     PERFORM WY2000P3                                                     
018400     IF  TMP1-YYWW > TMP2-YYWW                                            
018500         CONTINUE                                                         
018600     ELSE                                                                 
018700         IF LINK-SUTPO-PB (1) = +0                                        
018800            MOVE +0       TO LINK-KVPB-TPO (1)                            
018900         ELSE                                                             
019000            COMPUTE LINK-KVPB-TPO (1) =                                   
019010                    LINK-SUTPO-PB (1) * 4.33 / 18                         
019100         END-IF                                                           
019200                                                                          
019300         MOVE 'N'          TO LINK-FLMANPB (1)                            
019400                                                                          
019500     END-IF                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 C-INITIERA-PUNK-PARM SECTION.                                            
019900******************************************************************        
020000*                                                                *        
020100*    PARAMETRARNA (LÄNKAREAN) TILL W221PUNK SÄTTS                *        
020200*    (FRÅN LÄNKARIAN FRÅN HUVUDPROGRAMMET)                       *        
020300*                                                                *        
020400******************************************************************        
020500                                                                          
020510     MOVE +1                  TO PUNK-KVANTAL-CLAGER                      
020511     MOVE LINK-KDLPORS-GRP    TO PUNK-KDLPORS-GRP                         
020520     MOVE LINK-KVBR-TOT       TO PUNK-KVBR-TOT                            
020530     MOVE LINK-KVOEKORR       TO PUNK-KVOEKORR                            
020540     MOVE LINK-IDARTNR        TO PUNK-IDARTNR                             
020550     MOVE LINK-KDPRODSL       TO PUNK-KDPRODSL                            
020560     MOVE LINK-TIAAVV-AKT     TO PUNK-TIAAVV-AKT                          
020570     MOVE LINK-TIAAVVD-AKT    TO PUNK-TIAAVVD-AKT                         
020590     MOVE LINK-FLAVRART       TO PUNK-FLAVRART                            
020591     MOVE LINK-FLFSP      (1) TO PUNK-FLFSP          (1)                  
020592     MOVE LINK-FLMANBK        TO PUNK-FLMANBK                             
020593     MOVE LINK-FLMANKP        TO PUNK-FLMANKP                             
020594     MOVE LINK-IDLEVNR        TO PUNK-IDLEVNR                             
020595     MOVE LINK-IDPROD         TO PUNK-IDPROD                              
020596     MOVE LINK-KDERS      (1) TO PUNK-KDERS          (1)                  
020597     MOVE LINK-KDGK           TO PUNK-KDGK                                
020598     MOVE LINK-KDHF           TO PUNK-KDHF                                
020599     MOVE LINK-KDUART         TO PUNK-KDUART                              
020600     MOVE LINK-KVAKS      (1) TO PUNK-KVAKS          (1)                  
020601     MOVE LINK-KVLS       (1) TO PUNK-KVLS           (1)                  
020602     MOVE LINK-KVLS-SDC-OVER  TO PUNK-KVLS-SDC-OVER                       
020603     MOVE LINK-KVOKS-BULK (1) TO PUNK-KVOKS-BULK     (1)                  
020604     MOVE LINK-KVOKS-DAG  (1) TO PUNK-KVOKS-DAG      (1)                  
020605     MOVE LINK-KVOKS-VOR  (1) TO PUNK-KVOKS-VOR      (1)                  
020606     MOVE LINK-KVPALL         TO PUNK-KVPALL                              
020607     MOVE LINK-KVPB-SATS  (1) TO PUNK-KVPB-SATS      (1)                  
020608     MOVE LINK-KVPB-SDC-TOT   TO PUNK-KVPB-SDC-TOT                        
020609     MOVE LINK-KVPB-SDC-EJ-DIR                                            
020610                              TO PUNK-KVPB-SDC-EJ-DIR                     
020611     MOVE LINK-KVPB-TPO   (1) TO PUNK-KVPB-TPO       (1)                  
020612     MOVE LINK-KVPB-PLAN      TO PUNK-KVPB-PLAN                           
020613     MOVE LINK-KVQPACK-1      TO PUNK-KVQPACK-1                           
020614     MOVE LINK-KVPB-VESL  (1) TO PUNK-KVPB-VESL      (1)                  
020615     MOVE LINK-KVRESS     (1) TO PUNK-KVRESS         (1)                  
020616     MOVE LINK-KVROS      (1) TO PUNK-KVROS          (1)                  
020617     MOVE LINK-PRARTSTD       TO PUNK-PRARTSTD                            
020618     MOVE LINK-PRARTBES       TO PUNK-PRARTBES                            
020619     MOVE LINK-REDIRLEV   (1) TO PUNK-REDIRLEV       (1)                  
020620     MOVE LINK-TIFINLV        TO PUNK-TIFINLV                             
020621     MOVE LINK-FLMANQ         TO PUNK-FLMANQ                              
020622     MOVE LINK-FLMPB      (1) TO PUNK-FLMPB          (1)                  
020623     MOVE LINK-KDAVT          TO PUNK-KDAVT                               
020624     MOVE LINK-KDFREKKL       TO PUNK-KDFREKKL                            
020625     MOVE LINK-KDLTK          TO PUNK-KDLTK                               
020626     MOVE LINK-KDPRISKL       TO PUNK-KDPRISKL                            
020627     MOVE LINK-KDVVKL         TO PUNK-KDVVKL                              
020628     MOVE LINK-KVAP           TO PUNK-KVAP                                
020629     MOVE LINK-KVBK           TO PUNK-KVBK                                
020630     MOVE LINK-KVKP           TO PUNK-KVKP                                
020631     MOVE LINK-KVMAD-SEP  (1) TO PUNK-KVMAD-SEP      (1)                  
020632     MOVE LINK-KVMAD-TOT  (1) TO PUNK-KVMAD-TOT      (1)                  
020633     MOVE LINK-KVMP       (1) TO PUNK-KVMP           (1)                  
020634     MOVE LINK-KVPB-SEP   (1) TO PUNK-KVPB-SEP       (1)                  
020635     MOVE LINK-KVQ            TO PUNK-KVQ                                 
020636     MOVE LINK-KVQ-JUST       TO PUNK-KVQ-JUST                            
020637     MOVE LINK-KVVECKOR-FT    TO PUNK-KVVECKOR-FT                         
020638     MOVE LINK-KVVECKOR-BT    TO PUNK-KVVECKOR-BT                         
020639     MOVE LINK-KVSLAGER   (1) TO PUNK-KVSLAGER       (1)                  
020640     MOVE LINK-RESLJUST   (1) TO PUNK-RESLJUST       (1)                  
020641     MOVE LINK-TIQJUST        TO PUNK-TIQJUST                             
020642     MOVE LINK-TISLJUST   (1) TO PUNK-TISLJUST       (1)                  
020643     MOVE LINK-ADLAGOMR       TO PUNK-ADLAGOMR                            
020644     MOVE LINK-FLNYBER        TO PUNK-FLNYBER                             
020645     MOVE LINK-KVULOAD        TO PUNK-KVULOAD                             
020646     MOVE LINK-PRORDSK        TO PUNK-PRORDSK                             
020647     MOVE LINK-VLARTNTO       TO PUNK-VLARTNTO                            
020648     MOVE LINK-BEFT           TO PUNK-BEFT                                
020649     MOVE LINK-KVEOQ          TO PUNK-KVEOQ                               
020650     MOVE LINK-KVSLAGER-OPT   TO PUNK-KVSLAGER-OPT                        
020651     MOVE LINK-KVVECKOR-LVAR  TO PUNK-KVVECKOR-LVAR                       
020660     .                                                                    
020700     EJECT                                                                
020800 D-SPARA-PUNK-PARM SECTION.                                               
020900******************************************************************        
021000*                                                                *        
021100*    PARAMETRARNA (LÄNKAREAN) TILL W221PUNK SPARAS UNDAN         *        
021200*    TILL HUVUDPROGRAMMETS LÄNKAREA                              *        
021300*                                                                *        
021400******************************************************************        
021500                                                                          
021520     MOVE PUNK-KDLPORS-GRP    TO LINK-KDLPORS-GRP                         
021530     MOVE PUNK-KVBR-TOT       TO LINK-KVBR-TOT                            
021540     MOVE PUNK-KVOEKORR       TO LINK-KVOEKORR                            
021550     MOVE PUNK-IDARTNR        TO LINK-IDARTNR                             
021560     MOVE PUNK-KDPRODSL       TO LINK-KDPRODSL                            
021570     MOVE PUNK-TIAAVV-AKT     TO LINK-TIAAVV-AKT                          
021580     MOVE PUNK-TIAAVVD-AKT    TO LINK-TIAAVVD-AKT                         
021591     MOVE PUNK-FLAVRART       TO LINK-FLAVRART                            
021592     MOVE PUNK-FLFSP      (1) TO LINK-FLFSP          (1)                  
021593     MOVE PUNK-FLMANBK        TO LINK-FLMANBK                             
021594     MOVE PUNK-FLMANKP        TO LINK-FLMANKP                             
021595     MOVE PUNK-IDLEVNR        TO LINK-IDLEVNR                             
021596     MOVE PUNK-IDPROD         TO LINK-IDPROD                              
021597     MOVE PUNK-KDERS      (1) TO LINK-KDERS          (1)                  
021598     MOVE PUNK-KDGK           TO LINK-KDGK                                
021599     MOVE PUNK-KDHF           TO LINK-KDHF                                
021600     MOVE PUNK-KDUART         TO LINK-KDUART                              
021601     MOVE PUNK-KVAKS      (1) TO LINK-KVAKS          (1)                  
021602     MOVE PUNK-KVLS       (1) TO LINK-KVLS           (1)                  
021603     MOVE PUNK-KVLS-SDC-OVER  TO LINK-KVLS-SDC-OVER                       
021604     MOVE PUNK-KVOKS-BULK (1) TO LINK-KVOKS-BULK     (1)                  
021605     MOVE PUNK-KVOKS-DAG  (1) TO LINK-KVOKS-DAG      (1)                  
021606     MOVE PUNK-KVOKS-VOR  (1) TO LINK-KVOKS-VOR      (1)                  
021607     MOVE PUNK-KVPALL         TO LINK-KVPALL                              
021608     MOVE PUNK-KVPB-SATS  (1) TO LINK-KVPB-SATS      (1)                  
021609     MOVE PUNK-KVPB-SDC-TOT   TO LINK-KVPB-SDC-TOT                        
021610     MOVE PUNK-KVPB-SDC-EJ-DIR                                            
021611                              TO LINK-KVPB-SDC-EJ-DIR                     
021612     MOVE PUNK-KVPB-TPO   (1) TO LINK-KVPB-TPO       (1)                  
021613     MOVE PUNK-KVPB-PLAN      TO LINK-KVPB-PLAN                           
021614     MOVE PUNK-KVQPACK-1      TO LINK-KVQPACK-1                           
021615     MOVE PUNK-KVPB-VESL  (1) TO LINK-KVPB-VESL      (1)                  
021616     MOVE PUNK-KVRESS     (1) TO LINK-KVRESS         (1)                  
021617     MOVE PUNK-KVROS      (1) TO LINK-KVROS          (1)                  
021618     MOVE PUNK-PRARTSTD       TO LINK-PRARTSTD                            
021619     MOVE PUNK-PRARTBES       TO LINK-PRARTBES                            
021620     MOVE PUNK-REDIRLEV   (1) TO LINK-REDIRLEV       (1)                  
021621     MOVE PUNK-TIFINLV        TO LINK-TIFINLV                             
021622     MOVE PUNK-FLMANQ         TO LINK-FLMANQ                              
021623     MOVE PUNK-FLMPB      (1) TO LINK-FLMPB          (1)                  
021624     MOVE PUNK-KDAVT          TO LINK-KDAVT                               
021625     MOVE PUNK-KDFREKKL       TO LINK-KDFREKKL                            
021626     MOVE PUNK-KDLTK          TO LINK-KDLTK                               
021627     MOVE PUNK-KDPRISKL       TO LINK-KDPRISKL                            
021628     MOVE PUNK-KDVVKL         TO LINK-KDVVKL                              
021629     MOVE PUNK-KVAP           TO LINK-KVAP                                
021630     MOVE PUNK-KVBK           TO LINK-KVBK                                
021631     MOVE PUNK-KVKP           TO LINK-KVKP                                
021632     MOVE PUNK-KVMAD-SEP  (1) TO LINK-KVMAD-SEP      (1)                  
021633     MOVE PUNK-KVMAD-TOT  (1) TO LINK-KVMAD-TOT      (1)                  
021634     MOVE PUNK-KVMP       (1) TO LINK-KVMP           (1)                  
021635     MOVE PUNK-KVPB-SEP   (1) TO LINK-KVPB-SEP       (1)                  
021636     MOVE PUNK-KVQ            TO LINK-KVQ                                 
021637     MOVE PUNK-KVQ-JUST       TO LINK-KVQ-JUST                            
021638     MOVE PUNK-KVVECKOR-FT    TO LINK-KVVECKOR-FT                         
021639     MOVE PUNK-KVVECKOR-BT    TO LINK-KVVECKOR-BT                         
021640     MOVE PUNK-KVSLAGER   (1) TO LINK-KVSLAGER       (1)                  
021641     MOVE PUNK-RESLJUST   (1) TO LINK-RESLJUST       (1)                  
021642     MOVE PUNK-TIQJUST        TO LINK-TIQJUST                             
021643     MOVE PUNK-TISLJUST   (1) TO LINK-TISLJUST       (1)                  
021644     MOVE PUNK-KVULOAD        TO LINK-KVULOAD                             
021645     MOVE PUNK-KVEOQ          TO LINK-KVEOQ                               
021646     MOVE PUNK-KVSLAGER-OPT   TO LINK-KVSLAGER-OPT                        
021650     .                                                                    
021700     EJECT                                                                
021800 E-BERAKNA-KVSLUTKP SECTION.                                              
021900******************************************************************        
022000*                                                                *        
022100*    BERÄKNING AV SLUTKÖPSSALDO                                  *        
022200*                                                                *        
022300******************************************************************        
022400     SKIP1                                                                
022500     IF  LINK-KVSLUTKP > ZERO                                             
022600     SKIP1                                                                
022700         COMPUTE W-KVSLUTKP =                                             
022800                             W-NETTOTILLGANG                              
022900                         -   W-IDEALLAGER                                 
023000     SKIP1                                                                
023100     IF  W-KVSLUTKP < LINK-KVSLUTKP                                       
023200         MOVE W-KVSLUTKP TO LINK-KVSLUTKP                                 
023300     END-IF                                                               
023400     IF  W-KVSLUTKP < ZERO                                                
023500         MOVE ZERO TO LINK-KVSLUTKP                                       
023600     END-IF                                                               
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000 F-BERAKNA-KVOVERF SECTION.                                               
024100******************************************************************        
024200*                                                                *        
024300*    BERÄKNING AV ÖVERFÖRINGSSALDO                               *        
024400*                                                                *        
024500******************************************************************        
024600     SKIP1                                                                
024700     IF  LINK-KVOVERF > ZERO                                              
024800         COMPUTE W-KVOVERF  =                                             
024900                         W-NETTOTILLGANG                                  
025000                     -   LINK-KVSLUTKP                                    
025100                     -   W-IDEALLAGER                                     
025200     SKIP1                                                                
025300         IF  W-KVOVERF < LINK-KVOVERF                                     
025400             MOVE W-KVOVERF TO LINK-KVOVERF                               
025500         END-IF                                                           
025600         IF  W-KVOVERF < ZERO                                             
025700             MOVE ZERO TO LINK-KVOVERF                                    
025800         END-IF                                                           
025900     END-IF                                                               
026000     .                                                                    
026100     EJECT                                                                
026200 S02-BERAKNA-NETTO-IDEALLAG SECTION.                                      
026300     SKIP3                                                                
026400     COMPUTE W-NETTOTILLGANG =                                            
026500                     LINK-KVLS   (1)                                      
026600                 -  (LINK-KVRESS (1)     )                                
026700                 +   LINK-KVAKS  (1)                                      
026800                 -  (LINK-KVROS  (1)     )                                
026900                 -  (LINK-KVOKS-BULK (1) )                                
027000                 -  (LINK-KVOKS-DAG (1)  )                                
027100                 -  (LINK-KVOKS-VOR (1)  )                                
027200                 +   LINK-KVLS-SDC-OVER                                   
027300     SKIP1                                                                
027400     IF  LINK-KDVVKL < 3                                                  
027500         COMPUTE W-IDEALLAGER ROUNDED =                                   
027600                         LINK-KVQ / 2                                     
027700     ELSE                                                                 
027800     COMPUTE W-IDEALLAGER ROUNDED =                                       
027900                         (LINK-KVSLAGER (1)                               
028000                     +    LINK-KVOEKORR)                                  
028100                     /    2                                               
028200         END-IF                                                           
028300     .                                                                    
028310     EJECT                                                                
028400*    -COPY WY2000P3                                                       
