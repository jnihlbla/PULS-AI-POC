000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W221PUNK.                                            
000400 AUTHOR.             LARS THELL (CAP PROGRAMATOR GBG)                     
000500                     STÖRRE DELEN ÄR KNYCKT FRÅN W2214030                 
000600 DATE-WRITTEN.       AUG  1994.                                           
000700     SKIP2                                                                
000800     REMARKS.                                                             
000900*                                                                         
001000*                                                                         
001100*    FUNCTION.                                                            
001200*        PROGRAMMET ÄR ETT SUBPROGRAM                                     
001300*        VILKET BERÄKNAR STYRVÄRDEN (PUNKTER) SOM KRÄVS                   
001400*        FÖR ATT UTFÄRDA BESTÄLLNINGAR OCH LEVERANSPLANER.                
001500*        KOMMUNIKATION MED HUVUDPROGRAMMET SKER MED                       
001600*        LINK-AREA                                                        
001700*        BERÄKNADE VECKOBEHOV FÖR ARTIKELN FINNS I                        
001800*        LNK2-AREA                                                        
001900*    SUBPROGRAM:                                                          
002000*            W221LPAD    UPPDATERING AV TABELL KDLPORS-TAB                
002001*                                                                         
002010* CHANGE LOG:                                                             
002020* 2015-04-22   E'TRACKER 10130993                                         
002030*              REDUCE NUMBER OF DELIVERY SCHEDULES                        
002040*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 DATA DIVISION.                                                           
002400                                                                          
002500 WORKING-STORAGE SECTION.                                                 
002600     SKIP2                                                                
002700*    -COPY WY2000W3                                                       
002800     SKIP3                                                                
002900*    -COPY WY2000W2                                                       
003000     SKIP3                                                                
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
003400     SKIP3                                                                
003500 01  KONSTANTER.                                                          
003600     03  JA                  PIC X       VALUE 'J'.                       
003700     03  NEJ                 PIC X       VALUE 'N'.                       
003800     SKIP1                                                                
003900     03  ANTAL-KDVVKL-KLASSER                                             
004000                             PIC S9(9)   VALUE +5    COMP-3.              
004100     SKIP3                                                                
004200 01  KONSTANTER-EOQ.                                                      
004300     03  PRLAGK              PIC S9(7)V9(2) VALUE    ZERO COMP-3.         
004400     03  RELAGR              PIC S9(3)V9(2) VALUE    ZERO COMP-3.         
004500     03  REOLAGK             PIC S9(3)V9(2) VALUE    ZERO COMP-3.         
004600     03  A0                  PIC S9(1)V9(7) VALUE    ZERO COMP-3.         
004700     03  A1                  PIC S9(1)V9(7) VALUE    ZERO COMP-3.         
004800     03  A2                  PIC S9(1)V9(7) VALUE    ZERO COMP-3.         
004900     03  A3                  PIC S9(1)V9(7) VALUE    ZERO COMP-3.         
005000     03  B0                  PIC S9(1)      VALUE    ZERO COMP-3.         
005100     03  B1                  PIC S9(1)V9(8) VALUE    ZERO COMP-3.         
005200     03  B2                  PIC S9(1)V9(9) VALUE    ZERO COMP-3.         
005300     03  B3                  PIC S9(1)V9(10) VALUE   ZERO COMP-3.         
005400     03  B4                  PIC S9(1)V9(11) VALUE   ZERO COMP-3.         
005500     SKIP3                                                                
005510 01  TEST-KDPRODSL           PIC S9(3) COMP-3.                            
005511     88  GOOD-KDPRODSL                VALUE 11 THRU 19                    
005512                                            21 THRU 29                    
005513                                            31 THRU 39                    
005514                                            51 THRU 59.                   
005520                                                                          
005530                                                                          
005540                                                                          
005600 01  W-ARBETSAREOR.                                                       
005700     03  TOT-PB              PIC S9(8)V9             COMP-3.              
005800     03  W-KDVVKL-GAM        PIC S9(1)               COMP-3.              
005900     03  W-KDVVKL-NY         PIC S9(1)               COMP-3.              
006000     03  W-KVPB-TOT          PIC S9(6)V9(3)          COMP-3.              
006100     03  W-MIN-SL            PIC S9(8)V9(3)          COMP-3.              
006200     03  W-MAX-SL            PIC S9(8)V9(3)          COMP-3.              
006300     03  W-TIAAVV            PIC S9(5)               COMP-3.              
006400     03  W-KVSLUTKP          PIC S9(9)               COMP-3.              
006500     03  W-KVOVERF           PIC S9(9)               COMP-3.              
006700     03  WS-KVFRYSTIPLUS1    PIC S9(3)               COMP-3.              
006800     03  WS-TIFINLV-AAVV     PIC S9(5)               COMP-3.              
006900     SKIP1                                                                
007000     03  W-PRVARDE-AAR       PIC S9(9)V9(2)          COMP-3.              
007100     03  W-ARSOMS            PIC S9(9)V9(2)          COMP-3.              
007200     03  W-SUM-BEHOV         PIC S9(8)V9(3)          COMP-3.              
007300     03  W-ANTAL             PIC S9(3)               COMP-3.              
007400     03  W-KVANTAL           PIC S9(6)               COMP-3.              
007500     03  W-REF               PIC S9(6)V9(3)          COMP-3.              
007600     03  W-FAKT-CLAG         OCCURS 2                                     
007700                             PIC S9(6)V9(3)          COMP-3.              
007800     03  W-KVVECKOR-GARD     PIC S9(5)               COMP-3.              
007900     03  W-NETTOTILLGANG     PIC S9(9)               COMP-3.              
008000     03  W-IDEALLAGER        PIC S9(9)               COMP-3.              
008100     03  W-M                 PIC S9(7)               COMP-3.              
008200     03  W-KONST-KVQ         PIC S9(4)V9(3)          COMP-3.              
008300     03  W-KONST-KVAP        PIC S9(4)V9(3)          COMP-3.              
008400     03  W-KONST-KVMP        PIC S9(4)V9(3)          COMP-3.              
008500     SKIP1                                                                
008600     03  W-DATUM-AAP         PIC 9(3).                                    
008700     03  W-DAT-AAP REDEFINES W-DATUM-AAP.                                 
008800         05  W-DAT-AA        PIC 9(2).                                    
008900         05  W-DAT-P         PIC 9(1).                                    
009000     SKIP3                                                                
009100     03  WS-KDPRODSL         PIC 99.                                      
009200     03  W-KDPRISKL-9        PIC 9.                                       
009300     03  W-ALLA-FREKKL       PIC X(10) VALUE 'ABCDEFGHIJ'.                
009400     SKIP2                                                                
009500     03  FILLER OCCURS 2.                                                 
009600         05  W-KVMAD-TOTTOT  PIC S9(6)V9(1) COMP-3.                       
009700     03  W-KVSLAGER          PIC S9(7)V9(2) COMP-3.                       
009800     03  W-STYCKFORS         PIC S9(7)  VALUE ZERO.                       
009900     03  WS-AVVIKELSE        PIC S9(4)V999 VALUE ZERO  COMP-3.            
010000     SKIP3                                                                
010001*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
010010 01  W-W221LP-CTX            PIC X(08) VALUE 'W221LP01'.                  
010020 01  W-KDLPORS-GRP.                                                       
010030     03 W-KDLPORS OCCURS 4 PIC 9(3).                                      
010040     SKIP3                                                                
010100 01  W-ARBETSAREOR-EOQ.                                                   
010200     03  WS-KVULOAD          PIC S9(7)             COMP-3.                
010300     03  WS-KVSLAGER-OPT     PIC S9(7)             COMP-3.                
010400     03  WS-KVSLAGER         PIC S9(7)             COMP-3.                
010500     03  WS-KVEOQ            PIC S9(7)             COMP-3.                
010600     03  WS-KVQ              PIC S9(7)             COMP-3.                
010700     03  WS-KVQ-DEC          PIC S9(7)V9           COMP-3.                
010800     03  WS-KVVECKOR-LVAR    PIC S9(2)V9           COMP-3.                
010900     03  WS-REVKOST-OMR      PIC S9(2)V9(2)        COMP-3.                
011000     03  WS-REOKOST-OMR      PIC S9(2)V9(2)        COMP-3.                
011100     03  WS-REOKOST-LEV      PIC S9(2)V9(2)        COMP-3.                
011200     03  WS-REOKOST-BEFT     PIC S9(2)V9(2)        COMP-3.                
011300     03  WS-REOLAGK          PIC S9(1)V9(2)        COMP-3.                
011400     03  WS-RETARGET         PIC S9(1)V9(3)        COMP-3.                
011500     03  EOQ                 PIC S9(15)V999        COMP-3.                
011600     03  WS-D                PIC S9(7)V99          COMP-3.                
011700     03  ODD                 PIC S9(7)V9           COMP-3.                
011800     03  E-L                 PIC S9(3)V9(5)        COMP-3.                
011900     03  VAR-D               PIC S9(13)V9(5)       COMP-3.                
012000     03  E-D                 PIC S9(13)V9(5)       COMP-3.                
012100     03  VAR-L               PIC S9(13)V9(5)       COMP-3.                
012200     03  SIGMA               PIC S9(13)V9(5)       COMP-3.                
012300     03  WS-SIGMA            PIC S9(13)V9(5)       COMP-3.                
012400     03  GU-K                PIC S9(7)V9(11)       COMP-3.                
012500     03  WS-K                PIC S9(7)V9(5)        COMP-3.                
012600     03  TALJARE             PIC S9(7)V9(5)        COMP-3.                
012700     03  NAMNARE             PIC S9(7)V9(5)        COMP-3.                
012800     03  Z                   PIC S9(5)V9(6)        COMP-3.                
012900     03  ZZ                  PIC S9(7)V9(6)        COMP-3.                
013000     03  ZZZ                 PIC S9(12)V9(6)       COMP-3.                
013100     03  MIN                 PIC S9(7)             COMP-3.                
013200     03  MAX                 PIC S9(7)             COMP-3.                
013300     03  WS-KVVECKOR-MINSL   PIC S9(2)V9           COMP-3.                
013400     03  WS-KVVECKOR-MAXSL   PIC S9(2)V9           COMP-3.                
013500     03  MULT                PIC S9(7)             COMP-3.                
013600     03  LAGSTA              PIC S9(7)             COMP-3.                
013700     03  HOGSTA              PIC S9(7)             COMP-3.                
013800     03  VOLYM               PIC S9(5)V9(5)        COMP-3.                
013900     03  WS-A                PIC S9(7)  VALUE 5    COMP-3.                
014000     03  WS-B                PIC S9(7)  VALUE 5    COMP-3.                
014100     03  WS-C                PIC S9(7)  VALUE ZERO COMP-3.                
014200     03  ETT                 PIC S9(7)  VALUE ZERO COMP-3.                
014300     03  TVA                 PIC S9(7)  VALUE ZERO COMP-3.                
014400     03  TRE                 PIC S9(7)  VALUE ZERO COMP-3.                
014500     03  FYRA                PIC S9(7)  VALUE ZERO COMP-3.                
014600     EJECT                                                                
014700 01  W-INDEX.                                                             
014800     03  IX                  PIC S9(9)   VALUE +0    COMP SYNC.           
014900     03  IX1                 PIC S9(9)   VALUE +0    COMP SYNC.           
015000     03  IX2                 PIC S9(9)   VALUE +0    COMP SYNC.           
015100     03  IX3                 PIC S9(9)   VALUE +0    COMP SYNC.           
015200     03  IX-M                PIC S9(9)   VALUE +0    COMP SYNC.           
015300     03  IX-START-VECKA      PIC S9(9)   VALUE +0    COMP SYNC.           
015400     03  IX-ANTAL-VECKOR     PIC S9(9)   VALUE +0    COMP SYNC.           
015500     03  IX-VECKA            PIC S9(9)   VALUE +0    COMP SYNC.           
015600 01  W-INDEX-EOQ.                                                         
015700     03  IX-LEV              PIC S9(9)   VALUE +0    COMP SYNC.           
015800     03  IX-OMR              PIC S9(9)   VALUE +0    COMP SYNC.           
015900     03  IX-BEFT             PIC S9(9)   VALUE +0    COMP SYNC.           
016000                                                                          
016100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
016200 01  GENERAL-SUBPROGRAMS.                                                 
016300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016400                                                                          
016500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800     SKIP3                                                                
016900 01  KEYS-TO-DLI.                                                         
017000     03  W-WDP601KY-X.                                                    
017100         05  W-KDPRODSL-X.                                                
017200             07  W-KDPRODSL      PIC S9(3)   VALUE ZERO COMP-3.           
017300         05  W-KDPRISKL-X.                                                
017400             07  W-KDPRISKL      PIC X       VALUE SPACE.                 
017500         05  W-KDFREKKL-X.                                                
017600             07  W-KDFREKKL      PIC X       VALUE SPACE.                 
017700     03  W-IDLEVNR-X.                                                     
017800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
017900     SKIP2                                                                
018000*    --- STATUS-KOD FRÅN IMS                                              
018100 01  STATUS-WS                   PIC XX.                                  
018200     88  SEGMENT-FOUND                       VALUE '  '.                  
018300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018500     SKIP2                                                                
018600 01  GOOD-STATUSCODES.                                                    
018700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018800     SKIP3                                                                
018900 01  SSA1                        PIC X(64).                               
019000 01  SSA2                        PIC X(64).                               
019100     EJECT                                                                
019200*    --- IMS FUNCTION CODES                                               
019300*01  -COPY W0003                                                          
019400     EJECT                                                                
019500                                                                          
019600     SKIP3                                                                
019700 01  DYNAMISKA-SUBPROGRAM.                                                
019800     03  W221LPAD            PIC X(8)    VALUE 'W221LPAD'.                
019900     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
020000     EJECT                                                                
020100*01  -COPY W221W099                                                       
020200     EJECT                                                                
020300*                            ** BYTES-ARTIKEL                   **        
020400 01  FILLER                  PIC X(16)   VALUE 'BYTES-ARTIKEL'.           
020500 01  TEST-IDARTNR            PIC 9(9)    COMP-3.                          
020600*01  FILLER  -COPY WWBYT02   -RED TEST-IDARTNR.                           
020700     EJECT                                                                
020800*                            *************************************        
020900*                            ** VÄRDE-TABELL                    **        
021000*                            *************************************        
021100*01  AREA  -COPY W221W010       -PRE W010-                                
021200     EJECT                                                                
021300*                            *************************************        
021400*                            **   ANV I SÄKERHETSLAGERBERÄKNING **        
021500*                            *************************************        
021600*    -COPY W221W097                                                       
021700     EJECT                                                                
021800     SKIP2                                                                
021900*                            *************************************        
022000*01  AREA  -COPY W221W012       -PRE W012-.                               
022100     EJECT                                                                
022200*                            *************************************        
022300*                            **   EOQ - COPYTEXTER              **        
022400*                            *************************************        
022500*01        -COPY W221W030                                                 
022600     EJECT                                                                
022700*01        -COPY W221W034                                                 
022800     EJECT                                                                
022900*01        -COPY W221W035                                                 
023000     EJECT                                                                
023100*01        -COPY W221W036                                                 
023200     EJECT                                                                
023300*    ---  DLI INPUT-OUTPUT AREA                                           
023400                                                                          
023500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP601'.                      
023600 01  DLI-IO-WDP601.                                                       
023700*    03  -COPY WDP601                                                     
023800     EJECT                                                                
023900                                                                          
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
024100 01  DLI-IO-WDF101.                                                       
024200*    03  -COPY WDF101                                                     
024300     EJECT                                                                
024400 LINKAGE SECTION.                                                         
024500     SKIP3                                                                
024600*                            *************************************        
024700*                            **LINK-AREA                        **        
024800*                            **KOMMUNIKATION MED HUVUDPROGRAM   **        
024900*                            *************************************        
025000*01  AREA  -COPY W221PUNK   -PRE LINK-.                                   
025100     EJECT                                                                
025200*                            *************************************        
025300*                            ** LNK2-AREA                       **        
025400*                            ** BERÄKNADE VECKO-BEHOV           **        
025500*                            *************************************        
025600*01  AREA  -COPY W222L222   -PRE LNK2-.                                   
025700     EJECT                                                                
025800*01  -COPY W0008   -PRE WDP6-                                             
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026100*01  -COPY W0008   -PRE WDF1-                                             
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400 PROCEDURE DIVISION USING LINK-AREA LNK2-AREA WDP6-PCB WDF1-PCB.          
026500     SKIP2                                                                
026610     EVALUATE TRUE                                                        
026620       WHEN LINK-KDPRODSL = 11 OR 21                                      
026630         MOVE 11     TO W-KDPRODSL                                        
026631       WHEN LINK-KDPRODSL = 12 OR 22                                      
026632         MOVE 12     TO W-KDPRODSL                                        
026633       WHEN LINK-KDPRODSL = 13 OR 23                                      
026634         MOVE 13     TO W-KDPRODSL                                        
026635       WHEN LINK-KDPRODSL = 14 OR 24                                      
026636         MOVE 14     TO W-KDPRODSL                                        
026637       WHEN LINK-KDPRODSL = 15 OR 25                                      
026638         MOVE 15     TO W-KDPRODSL                                        
026639       WHEN LINK-KDPRODSL = 16 OR 26                                      
026640         MOVE 16     TO W-KDPRODSL                                        
026641       WHEN LINK-KDPRODSL = 17 OR 27                                      
026642         MOVE 17     TO W-KDPRODSL                                        
026643       WHEN LINK-KDPRODSL = 18 OR 28                                      
026644         MOVE 18     TO W-KDPRODSL                                        
026645       WHEN LINK-KDPRODSL = 19 OR 29                                      
026646         MOVE 19     TO W-KDPRODSL                                        
026647       WHEN LINK-KDPRODSL > 30 AND < 40                                   
026648         MOVE LINK-KDPRODSL TO W-KDPRODSL                                 
026649       WHEN LINK-KDPRODSL > 50 AND < 60                                   
026650         MOVE LINK-KDPRODSL TO W-KDPRODSL                                 
026651       WHEN OTHER                                                         
026652         MOVE 11     TO W-KDPRODSL                                        
026653     END-EVALUATE                                                         
026660                                                                          
030400     PERFORM M-BERAKNA-PRISKLASS                                          
030500     PERFORM N-BERAKNA-FREKVENSKLASS                                      
030600     MOVE LINK-KDPRISKL TO W-KDPRISKL                                     
030700     MOVE LINK-KDFREKKL TO W-KDFREKKL                                     
030900     PERFORM IMS-GET-WDP601                                               
031100                                                                          
031200     PERFORM B-BERAKNA-KDVVKL                                             
031300     MOVE 1 TO IX                                                         
031400     PERFORM C-BERAKNA-KVSLAGER                                           
031500     PERFORM D-BERAKNA-KVQ                                                
031600     IF LINK-FLNYBER = JA AND LINK-PRARTBES > ZERO                        
031700        PERFORM P-KVULOAD                                                 
031800        PERFORM Q-BER-KVEOQ                                               
031900        PERFORM S-BER-KVSLAGER-OPT                                        
032000     END-IF                                                               
032100     PERFORM E-BERAKNA-KVKP                                               
032200     PERFORM F-BERAKNA-KVBK                                               
032300     PERFORM G-BERAKNA-KVAP                                               
032400     MOVE 1 TO IX                                                         
032500     PERFORM H-BERAKNA-KVMP                                               
032600*****                                                                     
032700     IF LINK-KVAP < LINK-KVMP (1)                                         
032800       COMPUTE LINK-KVAP = LINK-KVMP (1)                                  
032900     END-IF                                                               
033000*****                                                                     
033100     PERFORM I-BERAKNA-KVOEKORR                                           
033200     MOVE 1 TO IX                                                         
033300     PERFORM L-KOPIERA-PB-SEP                                             
033400                                                                          
033500     MOVE ZERO TO RETURN-CODE                                             
033600     GOBACK                                                               
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000 B-BERAKNA-KDVVKL SECTION.                                                
034100******************************************************************        
034200*                                                                *        
034300*    BERÄKNING AV VOLYMVÄRDEKLASSER                              *        
034400*                                                                *        
034500******************************************************************        
034600     SKIP1                                                                
034700     COMPUTE W-PRVARDE-AAR = 12                                           
034800                             * (LINK-KVPB-SEP (1)                         
034900                                 +  LINK-KVPB-SATS (1)                    
035000                                 +  LINK-KVPB-TPO (1)                     
035100                                 +  LINK-KVPB-SDC-TOT )                   
035200                             *  LINK-PRARTSTD                             
035300*                                                                         
035400*                                                                         
035500     SKIP1                                                                
035600     MOVE ANTAL-KDVVKL-KLASSER TO IX                                      
035700     PERFORM UNTIL NOT(                                                   
035800        IX > ZERO                                                         
035900     AND     W-PRVARDE-AAR  < W010-PRVARDE-VVKL-NGR (IX))                 
036000         SUBTRACT 1 FROM IX                                               
036100     END-PERFORM                                                          
036200     SKIP1                                                                
036300     MOVE LINK-KDVVKL TO W-KDVVKL-GAM                                     
036400     IF  W-PRVARDE-AAR = ZERO                                             
036500         MOVE ZERO TO LINK-KDVVKL                                         
036600     ELSE                                                                 
036700         MOVE IX TO LINK-KDVVKL                                           
036800     END-IF                                                               
036900     SKIP1                                                                
037000     MOVE 1 TO IX                                                         
037100     PERFORM BA-BERAKNA-KVMAD-PER-CLAGER                                  
037200     .                                                                    
037300     EJECT                                                                
037400 BA-BERAKNA-KVMAD-PER-CLAGER SECTION.                                     
037500******************************************************************        
037600*                                                                *        
037700*                                                                *        
037800*                                                                *        
037900*                                                                *        
038000******************************************************************        
038100     SKIP1                                                                
038200* ---- MAXVÄRDE PÅ MAD-TOT  -----------------------                       
038300     IF LINK-KVMAD-TOT (IX) >                                             
038310             3 * (LINK-KVPB-PLAN + LINK-KVPB-SATS (IX))                   
038400        COMPUTE  LINK-KVMAD-TOT (IX) ROUNDED =                            
038500                 (LINK-KVPB-PLAN + LINK-KVPB-SATS (IX)) ** 0.85           
038600     END-IF                                                               
038700* ----                                                                    
038800     IF  LINK-KVPB-SEP (IX) NOT = LINK-KVPB-VESL (IX)                     
038900                                                                          
039000         MOVE LINK-TIAAVVD-AKT   TO TMP1-YYWWD                            
039100         MOVE LINK-TIFINLV       TO TMP2-YYWWD                            
039200         PERFORM WY2000P2                                                 
039300         IF  TMP1-YYWWD <= TMP2-YYWWD                                     
039400         OR  LINK-KVPB-VESL (IX) = ZERO                                   
039500         OR  LINK-KVPB-SEP (IX) NOT > 1                                   
039600             COMPUTE  LINK-KVMAD-SEP (IX) ROUNDED =                       
039700                                   LINK-KVPB-SEP (IX) ** 0.85             
039800         END-IF                                                           
039900                                                                          
040000         MOVE LINK-TIAAVVD-AKT   TO TMP1-YYWWD                            
040100         MOVE LINK-TIFINLV       TO TMP2-YYWWD                            
040200         PERFORM WY2000P2                                                 
040300         IF  TMP1-YYWWD <= TMP2-YYWWD                                     
040400         OR  (LINK-KVPB-PLAN + LINK-KVPB-SATS (IX)) NOT > 1               
040500             COMPUTE  LINK-KVMAD-TOT (IX) ROUNDED =                       
040510                 (LINK-KVPB-PLAN + LINK-KVPB-SATS (IX)) ** 0.85           
040700         END-IF                                                           
040800     END-IF                                                               
040900                                                                          
041000     .                                                                    
041100     EJECT                                                                
041200 C-BERAKNA-KVSLAGER SECTION.                                              
041300******************************************************************        
041400*                                                                *        
041500*    BERAKNA SÄKERHETSLAGER (BUFFERT) PER C-LAGER                *        
041600*                                                                *        
041700******************************************************************        
041800     SKIP1                                                                
041900     MOVE LINK-KVPB-SEP  (IX) TO W-KVPB-TOT                               
042000     ADD  LINK-KVPB-SATS (IX) TO W-KVPB-TOT                               
042100     ADD  LINK-KVPB-SDC-EJ-DIR                                            
042200                              TO W-KVPB-TOT                               
042300     IF  LINK-KDERS (IX) > 09                                             
042400                                                                          
042500     OR  (LINK-KDERS (IX) > 00                                            
042600      AND LINK-KVBR-TOT = ZERO)                                           
042700                                                                          
042800     OR  W-KVPB-TOT = ZERO                                                
042900                                                                          
043000     OR  LINK-REDIRLEV (IX) = 1.00                                        
043100                                                                          
043200     OR  LINK-KDUART = 'S'                                                
043300                                                                          
043400     OR  (LINK-KDHF > ZERO                                                
043500      AND  LINK-FLAVRART = JA)                                            
043600                                                                          
043700         MOVE ZERO  TO LINK-KVSLAGER (IX)                                 
043800     ELSE                                                                 
043900         MOVE LINK-TISLJUST   (IX)  TO TMP1-YYWW                          
044000         MOVE LINK-TIAAVV-AKT       TO TMP2-YYWW                          
044100         PERFORM WY2000P3                                                 
044200         IF LINK-RESLJUST (IX) = 9.9 AND                                  
044300            TMP1-YYWW  >  TMP2-YYWW                                       
044400            CONTINUE                                                      
044500          ELSE                                                            
044600            PERFORM CA-BERAKNA-KVVECKOR-GARD                              
044700            PERFORM CB-BERAKNA-KVSLAGER                                   
044800         END-IF                                                           
044900     END-IF                                                               
045000     .                                                                    
045100     EJECT                                                                
045200 CA-BERAKNA-KVVECKOR-GARD SECTION.                                        
045300******************************************************************        
045400*                                                                *        
045500*                                                                *        
045600*                                                                *        
045700******************************************************************        
045800     SKIP1                                                                
045900*                                                                         
046000     IF  LINK-KDVVKL > 2                                                  
046100         MOVE LINK-KVVECKOR-FT TO    W-KVVECKOR-GARD                      
046200         IF LINK-KDVVKL = 5                                               
046300            ADD +1 TO W-KVVECKOR-GARD                                     
046400         ELSE                                                             
046500         ADD W010-REKONST-LPKOLL (LINK-KDVVKL)                            
046600                                TO    W-KVVECKOR-GARD ROUNDED             
046700         END-IF                                                           
046800     ELSE                                                                 
046900         MOVE LINK-KVVECKOR-BT TO W-KVVECKOR-GARD                         
047000     END-IF                                                               
047100     SKIP2                                                                
047200     SUBTRACT 0.5 FROM W-KVVECKOR-GARD                                    
047300*                                                                         
047400     IF W-KVVECKOR-GARD < 0                                               
047500        MOVE 0 TO W-KVVECKOR-GARD                                         
047600     END-IF                                                               
047700     .                                                                    
047800     EJECT                                                                
047900 CB-BERAKNA-KVSLAGER SECTION.                                             
048000******************************************************************        
048100*                                                                *        
048200*                                                                *        
048300*                                                                *        
048400******************************************************************        
048500                                                                          
048600*    PERFORM CBA-SAETT-IX-TAB-KONST                                       
048700                                                                          
048800           MOVE LINK-KVMAD-TOT (IX) TO W-KVMAD-TOTTOT (IX)                
048810           MOVE LINK-KDPRODSL   TO TEST-KDPRODSL                          
048900*          IF LINK-KDPRODSL NOT < 11 AND                                  
049000*             LINK-KDPRODSL NOT > 29 AND IX = 1                           
049010           IF GOOD-KDPRODSL AND IX = 1                                    
049100*             DVS PV - CLAGER 1  SKALL HA EGET VÄRDE PÅ KVMAD-TOT         
049200              COMPUTE W-KVMAD-TOTTOT (1) ROUNDED =                        
049300                      (LINK-KVMAD-TOT (1) ** 2) ** 0.5                    
049400           END-IF                                                         
049500                                                                          
049600           COMPUTE LINK-KVSLAGER (IX) ROUNDED =                           
049700*                             TAB-KONSTANT (IX1, IX2, IX3)                
049800                              SLB-KFAKT                                   
049900                          *   W-KVMAD-TOTTOT (IX)                         
050000                          *   (1.00 - LINK-REDIRLEV (IX)) * 0.48          
050100                          *   W-KVVECKOR-GARD ** 0.65                     
050200                                                                          
050300*** MIN SÄKERHETSLAGER OCH MAX-SÄKERHETSLAGER ---------------             
050400           COMPUTE W-MIN-SL ROUNDED =                                     
050500           (1.00 - LINK-REDIRLEV (IX)) * LINK-KVPB-SEP (IX)               
050600           + LINK-KVPB-SATS (IX)                                          
050700           MOVE W-MIN-SL TO W-MAX-SL                                      
050800           COMPUTE W-MIN-SL ROUNDED = W-MIN-SL * 2 / 4.33                 
050900           COMPUTE W-MIN-SL ROUNDED = W-MIN-SL +                          
051000                           (LINK-KVPB-SDC-EJ-DIR * 2 / 4.33)              
051100           COMPUTE W-MAX-SL ROUNDED = W-MAX-SL * 12                       
051200           COMPUTE W-MAX-SL ROUNDED = W-MAX-SL +                          
051300                           (LINK-KVPB-SDC-EJ-DIR * 6)                     
051400* MIN                                                                     
051500           IF W-MIN-SL < 1.0                                              
051600             MOVE 1.0 TO W-MIN-SL                                         
051700           END-IF                                                         
051800           IF  LINK-KVSLAGER (IX) < W-MIN-SL                              
051900               COMPUTE  LINK-KVSLAGER (IX) ROUNDED = W-MIN-SL             
052000           END-IF                                                         
052100                                                                          
052200*                                                                         
052300**** MANUELL JUSTERING AV SÄKERHETSLAGER                                  
052400*                                                                         
052500     MOVE LINK-TISLJUST (IX)  TO TMP1-YYWW                                
052600     MOVE LINK-TIAAVV-AKT     TO TMP2-YYWW                                
052700     PERFORM WY2000P3                                                     
052800     IF  TMP1-YYWW > TMP2-YYWW                                            
052900         MULTIPLY LINK-RESLJUST (IX) BY LINK-KVSLAGER (IX) ROUNDED        
053000     END-IF                                                               
053100                                                                          
053200* MAX                                                                     
053300             IF LINK-KDVVKL > 2                                           
053400                 IF W-MAX-SL < 3                                          
053500                    MOVE 3 TO W-MAX-SL                                    
053600                 END-IF                                                   
053700                 IF LINK-KVSLAGER (IX) > W-MAX-SL                         
053800                    MOVE W-MAX-SL TO LINK-KVSLAGER (IX)                   
053900                 END-IF                                                   
054000             END-IF                                                       
054100                                                                          
054200*               UPPRÄKNING S-LAGER                  ****                  
054300*                                                                         
054400                                                                          
054500     MOVE LINK-TISLJUST (IX)   TO TMP1-YYWW                               
054600     MOVE LINK-TIAAVV-AKT      TO TMP2-YYWW                               
054700     PERFORM WY2000P3                                                     
054800     IF  TMP1-YYWW NOT > TMP2-YYWW                                        
054900         MOVE ZERO TO LINK-RESLJUST (IX)                                  
055000         MOVE ZERO TO LINK-TISLJUST (IX)                                  
055100     END-IF                                                               
055200     .                                                                    
055300     EJECT                                                                
055400 D-BERAKNA-KVQ SECTION.                                                   
055500******************************************************************        
055600*                                                                *        
055700*    BERAKNING AV HEMTAGNINGSKVANTITET                           *        
055800*                                                                *        
055900******************************************************************        
056000     SKIP1                                                                
056100     IF LINK-KDERS (1) > 08                                               
056200        MOVE NEJ TO LINK-FLMANQ                                           
056300     END-IF                                                               
056400     SKIP1                                                                
056500     MOVE LINK-TIQJUST      TO TMP1-YYWW                                  
056600     MOVE LINK-TIAAVV-AKT   TO TMP2-YYWW                                  
056700     PERFORM WY2000P3                                                     
056800     IF  LINK-TIQJUST  > ZERO                                             
056900     AND TMP1-YYWW NOT > TMP2-YYWW                                        
057000         MOVE LINK-KVQ-JUST TO LINK-KVQ                                   
057100         MOVE JA   TO LINK-FLMANQ                                         
057200         MOVE ZERO TO LINK-KVQ-JUST                                       
057300                  LINK-TIQJUST                                            
057400     END-IF                                                               
057500                                                                          
057600     IF   LINK-KDHF > ZERO                                                
057700      AND LINK-FLAVRART = JA                                              
057800         MOVE ZERO TO LINK-KVQ                                            
057900     ELSE                                                                 
058000      IF LINK-FLMANQ = NEJ                                                
058100         IF  LINK-KDVVKL > ZERO AND LINK-KDVVKL < 6                       
058200           IF LINK-KDVVKL = 1 OR 5                                        
058300             IF LINK-KDVVKL = 1                                           
058400                MOVE 12     TO W-KONST-KVQ                                
058500             ELSE                                                         
058600                COMPUTE W-KONST-KVQ ROUNDED = 1 / 4.33                    
058700             END-IF                                                       
058800                                                                          
058900             COMPUTE LINK-KVQ ROUNDED =                                   
059000                           W-KONST-KVQ                                    
059100                     *    ((1.00 - LINK-REDIRLEV (1))                     
059200                     *     LINK-KVPB-SEP (1)                              
059300                     +     LINK-KVPB-SATS (1)                             
059400                     +     LINK-KVPB-TPO (1)                              
059500                     +     LINK-KVPB-SDC-EJ-DIR )                         
059600           ELSE                                                           
059700              IF LINK-KDVVKL = 2 OR 3 OR 4                                
059800                COMPUTE W-ARSOMS = 12                                     
059900                        * (LINK-KVPB-SEP (1)                              
060000                          + LINK-KVPB-SATS (1)                            
060100                          + LINK-KVPB-TPO (1)                             
060200                          + LINK-KVPB-SDC-EJ-DIR )                        
060300                        * LINK-PRARTBES                                   
060400                IF W-ARSOMS < 2000                                        
060500                  COMPUTE LINK-KVQ ROUNDED =                              
060600                    6 *   ((1.00 - LINK-REDIRLEV (1))                     
060700                      *    LINK-KVPB-SEP (1)                              
060800                      +    LINK-KVPB-SATS (1)                             
060900                      +    LINK-KVPB-TPO (1)                              
061000                      +    LINK-KVPB-SDC-EJ-DIR )                         
061100                ELSE                                                      
061200                IF W-ARSOMS > 1999 AND < 5000                             
061300                  COMPUTE LINK-KVQ ROUNDED =                              
061400                   12 *   ((1.00 - LINK-REDIRLEV (1))                     
061500                      *    LINK-KVPB-SEP (1)                              
061600                      +    LINK-KVPB-SATS (1)                             
061700                      +    LINK-KVPB-TPO (1)                              
061800                      +    LINK-KVPB-SDC-EJ-DIR ) / 4.33                  
061900                ELSE                                                      
062000                IF W-ARSOMS > 4999 AND < 20000                            
062100                  COMPUTE LINK-KVQ ROUNDED =                              
062200                    3 *   ((1.00 - LINK-REDIRLEV (1))                     
062300                      *    LINK-KVPB-SEP (1)                              
062400                      +    LINK-KVPB-SATS (1)                             
062500                      +    LINK-KVPB-TPO (1)                              
062600                      +    LINK-KVPB-SDC-EJ-DIR ) / 4.33                  
062700                ELSE                                                      
062800                  IF W-ARSOMS > 19999 AND                                 
062900                     W-ARSOMS < 80000                                     
063000                    COMPUTE LINK-KVQ ROUNDED =                            
063100                    3 * ((1.00 - LINK-REDIRLEV (1))                       
063200                       *  LINK-KVPB-SEP (1)                               
063300                       +  LINK-KVPB-SATS (1)                              
063400                       +  LINK-KVPB-TPO (1)                               
063500                       +  LINK-KVPB-SDC-EJ-DIR ) / 4.33                   
063600                  ELSE                                                    
063700                    IF W-ARSOMS > 79999 AND < 200000                      
063800                     COMPUTE LINK-KVQ ROUNDED =                           
063900                             ((1.00 - LINK-REDIRLEV (1))                  
064000                        *  LINK-KVPB-SEP (1)                              
064100                        +  LINK-KVPB-SATS (1)                             
064200                        +  LINK-KVPB-TPO (1)                              
064300                        +  LINK-KVPB-SDC-EJ-DIR ) / 4.33                  
064400                  ELSE                                                    
064500                    IF W-ARSOMS > 199999                                  
064600                     COMPUTE LINK-KVQ ROUNDED =                           
064700                              ((1.00 - LINK-REDIRLEV (1))                 
064800                        *  LINK-KVPB-SEP (1)                              
064900                        +  LINK-KVPB-SATS (1)                             
065000                        +  LINK-KVPB-TPO (1)                              
065100                        +  LINK-KVPB-SDC-EJ-DIR ) / 4.33                  
065200                    END-IF                                                
065300                  END-IF                                                  
065400                END-IF                                                    
065500               END-IF                                                     
065600               END-IF                                                     
065700               END-IF                                                     
065800             END-IF                                                       
065900           END-IF                                                         
066000           PERFORM DA-AVRUNDA-KVQ                                         
066100         ELSE                                                             
066200             MOVE ZERO TO LINK-KVQ                                        
066300         END-IF                                                           
066400       END-IF                                                             
066500     END-IF                                                               
066600     .                                                                    
066700     EJECT                                                                
066800 DA-AVRUNDA-KVQ SECTION.                                                  
066900     SKIP3                                                                
067000     IF  LINK-KVPALL > ZERO                                               
067100       COMPUTE LINK-KVQ = LINK-KVQ / LINK-KVPALL                          
067200       COMPUTE LINK-KVQ = LINK-KVPALL * LINK-KVQ                          
067300       IF LINK-KVQ = ZERO                                                 
067400          MOVE LINK-KVPALL TO LINK-KVQ                                    
067500       END-IF                                                             
067600     ELSE                                                                 
067700      IF  LINK-KVQPACK-1 > ZERO                                           
067800       COMPUTE LINK-KVQ = LINK-KVQ / LINK-KVQPACK-1                       
067900       COMPUTE LINK-KVQ = LINK-KVQPACK-1 * LINK-KVQ                       
068000       IF LINK-KVQ = ZERO                                                 
068100          MOVE LINK-KVQPACK-1 TO LINK-KVQ                                 
068200       END-IF                                                             
068300         ELSE                                                             
068400             MOVE 1 TO W-M                                                
068500             MOVE 1  TO IX-M                                              
068600             PERFORM UNTIL NOT(                                           
068700                IX-M NOT > W012-MAXINDEX-1                                
068800             AND LINK-KVQ > W012-KVQ-BER-TOM (IX-M))                      
068900                 ADD 1 TO IX-M                                            
069000             END-PERFORM                                                  
069100             MOVE W012-KVANTAL-MULTIPEL (IX-M) TO W-M                     
069200     SKIP1                                                                
069300             DIVIDE  LINK-KVQ BY W-M GIVING W-KVANTAL ROUNDED             
069400             IF  W-KVANTAL = ZERO                                         
069500                MOVE W-M TO LINK-KVQ                                      
069600             ELSE                                                         
069700                MULTIPLY W-M BY W-KVANTAL GIVING LINK-KVQ                 
069800             END-IF                                                       
069900         END-IF                                                           
070000     END-IF                                                               
070100     .                                                                    
070200     EJECT                                                                
070300 E-BERAKNA-KVKP SECTION.                                                  
070400******************************************************************        
070500*                                                                *        
070600*    BERÄKNING AV KÖPPUNKT                                       *        
070700*                                                                *        
070800******************************************************************        
070900     SKIP1                                                                
071000     IF  LINK-FLMANKP = NEJ                                               
071100                                                                          
071200       IF LINK-IDLEVNR  = '1002 '                                         
071300         MOVE ZERO TO LINK-KVKP                                           
071400       ELSE                                                               
071500                 MOVE 1 TO IX-START-VECKA                                 
071600                 MOVE LINK-KVVECKOR-BT TO IX-ANTAL-VECKOR                 
071700                 MOVE LINK-TIAAVV-AKT   TO TMP1-YYWW                      
071800                 MOVE 9431              TO TMP2-YYWW                      
071900                 PERFORM WY2000P3                                         
072000                 IF TMP1-YYWW < TMP2-YYWW                                 
072100                    SUBTRACT 4 FROM IX-ANTAL-VECKOR                       
072200                    IF IX-ANTAL-VECKOR < 1                                
072300                      MOVE 1 TO IX-ANTAL-VECKOR                           
072400                    END-IF                                                
072500                 END-IF                                                   
072600                 PERFORM S03-SUMMERA-VECKOBEHOV                           
072700                 ADD LNK2-KVBEHOV-DESSUTOM TO W-SUM-BEHOV                 
072800     SKIP1                                                                
072900                 COMPUTE LINK-KVKP ROUNDED =                              
073000                                         LINK-KVSLAGER (1)                
073100                                      +  W-SUM-BEHOV                      
073200     SKIP1                                                                
073300*                                  *** FÖR ARTIKLAR MED 1:A INLEV         
073400*                                  *** I FRAMTIDEN ÄR ENDAST SATS         
073500*                                  *** OCH DO-BEHOV HÄMTAT FRÅN           
073600*                                  *** BEHOVSMODULEN                      
073700                 MOVE LINK-TIAAVVD-AKT   TO TMP1-YYWWD                    
073800                 MOVE LINK-TIFINLV       TO TMP2-YYWWD                    
073900                 PERFORM WY2000P2                                         
074000                 IF TMP1-YYWWD < TMP2-YYWWD                               
074100                 COMPUTE LINK-KVKP ROUNDED = LINK-KVKP +                  
074200                 ((LINK-KVPB-SEP (1) ) *                                  
074300                 LINK-KVVECKOR-BT / 4.33)                                 
074400                 END-IF                                                   
074500     SKIP1                                                                
074600**************END-IF                                                      
074700          END-IF                                                          
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 F-BERAKNA-KVBK SECTION.                                                  
075200******************************************************************        
075300*                                                                *        
075400*    BERÄKNING AV BESTÄLLNINGSKVANTITET                          *        
075500*                                                                *        
075600******************************************************************        
075700     SKIP1                                                                
075800     IF  LINK-FLMANBK = NEJ                                               
075900*                                                                         
076000       IF  LINK-IDLEVNR  = '1002 '                                        
076100         MOVE ZERO TO LINK-KVBK                                           
076200       ELSE                                                               
076300     SKIP1                                                                
076400         IF  LINK-KDVVKL > 2                                              
076500         AND LINK-KDVVKL < 6                                              
076600             PERFORM FA-SUMMERA-VECKOBEHOV                                
076700             MOVE W-SUM-BEHOV    TO LINK-KVBK                             
076800     SKIP1                                                                
076900             MOVE ZERO TO W-KVANTAL                                       
077000             IF  LINK-KVQ > ZERO                                          
077100                 DIVIDE  LINK-KVBK BY LINK-KVQ                            
077200                                GIVING W-KVANTAL ROUNDED                  
077300     SKIP1                                                                
077400             IF  W-KVANTAL = ZERO                                         
077500                 MOVE LINK-KVQ TO LINK-KVBK                               
077600             ELSE                                                         
077700                 MULTIPLY W-KVANTAL BY LINK-KVQ                           
077800                                     GIVING LINK-KVBK                     
077900             END-IF                                                       
078000             END-IF                                                       
078100         ELSE                                                             
078200             MOVE LINK-KVQ  TO LINK-KVBK                                  
078300         END-IF                                                           
078400       END-IF                                                             
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800 FA-SUMMERA-VECKOBEHOV SECTION.                                           
078900     SKIP3                                                                
079000     MOVE LINK-KVVECKOR-BT TO IX-START-VECKA                              
079100     ADD  1 TO IX-START-VECKA                                             
079200     SKIP1                                                                
079300     MOVE W010-REKONST-KVBK (LINK-KDVVKL) TO IX-ANTAL-VECKOR              
079400     SKIP1                                                                
079500     PERFORM S03-SUMMERA-VECKOBEHOV                                       
079600     .                                                                    
079700     EJECT                                                                
079800 G-BERAKNA-KVAP SECTION.                                                  
079900******************************************************************        
080000*                                                                *        
080100*    BERÄKNING AV ANNULATIONSPUNKT                               *        
080200*                                                                *        
080300******************************************************************        
080400     SKIP1                                                                
080500     IF  LINK-IDLEVNR NOT = '1002 '                                       
080600                                                                          
080700         IF  LINK-KDVVKL > ZERO                                           
080800         AND LINK-KDVVKL < 6                                              
080900             IF LINK-KDVVKL = 1                                           
081000                MOVE 12     TO W-KONST-KVAP                               
081100             ELSE                                                         
081200                IF LINK-KDVVKL < 4                                        
081300                   MOVE 6   TO W-KONST-KVAP                               
081400                ELSE                                                      
081500                   MOVE 3   TO W-KONST-KVAP                               
081600                END-IF                                                    
081700             END-IF                                                       
081800                                                                          
081900             COMPUTE LINK-KVAP ROUNDED =                                  
082000                              LINK-KVKP + LINK-KVBK                       
082100                         +    W-KONST-KVAP                                
082200                         *   (LINK-KVPB-SEP (1)  +                        
082300                              LINK-KVPB-SATS (1) +                        
082400                              LINK-KVPB-TPO (1)  +                        
082500                              LINK-KVPB-SDC-EJ-DIR )                      
082600         ELSE                                                             
082700             MOVE LINK-KVKP TO LINK-KVAP                                  
082800         END-IF                                                           
082900     ELSE                                                                 
083000         MOVE ZERO TO LINK-KVAP                                           
083100     END-IF                                                               
083200     .                                                                    
083300     EJECT                                                                
083400 H-BERAKNA-KVMP SECTION.                                                  
083500******************************************************************        
083600*                                                                *        
083700*    BERÄKNING AV MAXPUNKT PER C-LAGER                           *        
083800*                                                                *        
083900******************************************************************        
084000     SKIP1                                                                
084100     IF  (LINK-KDHF > ZERO                                                
084200      AND LINK-FLAVRART = JA)                                             
084300     OR  LINK-KDERS (IX) > +10                                            
084400     SKIP1                                                                
084500         MOVE ZERO TO LINK-KVMP (IX)                                      
084600     ELSE                                                                 
084700         IF  LINK-KDVVKL > ZERO                                           
084800         AND LINK-KDVVKL < 6                                              
084900                                                                          
085000             COMPUTE W-FAKT-CLAG (1)  =                                   
085100                              LINK-KVPB-SEP (1)                           
085200                         *   (1 - LINK-REDIRLEV (1))                      
085300                         +    LINK-KVPB-SATS (1)                          
085400                         +    LINK-KVPB-TPO (1)                           
085500                         +    LINK-KVPB-SDC-EJ-DIR                        
085600                                                                          
085700             COMPUTE W-REF = W-FAKT-CLAG (1)                              
085800             IF  W-REF NOT = ZERO                                         
085900                 COMPUTE W-REF =  W-FAKT-CLAG (IX)                        
086000                         /   W-REF                                        
086100             END-IF                                                       
086200                                                                          
086300             IF LINK-KDVVKL = 1                                           
086400                MOVE 96           TO W-KONST-KVMP                         
086500             ELSE                                                         
086600                IF LINK-KDVVKL = 2                                        
086700                   MOVE 36        TO W-KONST-KVMP                         
086800                ELSE                                                      
086900                   IF LINK-KDVVKL = 3                                     
087000                      MOVE 18     TO W-KONST-KVMP                         
087100                   ELSE                                                   
087200                      IF LINK-KDVVKL = 4                                  
087300                         MOVE 12  TO W-KONST-KVMP                         
087400                      ELSE                                                
087500                         MOVE 9   TO W-KONST-KVMP                         
087600                      END-IF                                              
087700                   END-IF                                                 
087800                END-IF                                                    
087900             END-IF                                                       
088000                                                                          
088100             COMPUTE LINK-KVMP (IX) ROUNDED =                             
088200                             LINK-KVSLAGER (IX)                           
088300                         +   W-REF * LINK-KVQ                             
088400                         +   W-KONST-KVMP                                 
088500                         *   W-FAKT-CLAG (IX) / 4.33                      
088600         ELSE                                                             
088700             MOVE ZERO TO LINK-KVMP (IX)                                  
088800         END-IF                                                           
088900     END-IF                                                               
089000     .                                                                    
089100     EJECT                                                                
089200 I-BERAKNA-KVOEKORR SECTION.                                              
089300******************************************************************        
089400*                                                                *        
089500*    BERÄKNING AV KORRIDORGRÄNSER - ÖVRE KORRIDORGRÄNS           *        
089600*                                                                *        
089700******************************************************************        
089800     SKIP1                                                                
089900     IF  LINK-KDHF > ZERO                                                 
090000     AND LINK-FLAVRART = JA                                               
090100     SKIP1                                                                
090200         MOVE ZERO TO LINK-KVOEKORR                                       
090300     ELSE                                                                 
090400         IF  LINK-KDVVKL > 2                                              
090500         AND LINK-KDVVKL < 6                                              
090600             COMPUTE W-FAKT-CLAG (1) =                                    
090700                             (1 - LINK-REDIRLEV (1))                      
090800                         *    LINK-KVPB-SEP (1)                           
090900                         +    LINK-KVPB-SATS (1)                          
091000                         +    LINK-KVPB-TPO (1)                           
091100                         +    LINK-KVPB-SDC-EJ-DIR                        
091200     SKIP1                                                                
091300             COMPUTE LINK-KVOEKORR ROUNDED =                              
091400                             LINK-KVSLAGER (1)                            
091500                         +   LINK-KVQ                                     
091600                         +   3 / 4.33                                     
091700                         *   (W-FAKT-CLAG (1) )                           
091800         ELSE                                                             
091900             MOVE ZERO TO LINK-KVOEKORR                                   
092000         END-IF                                                           
092100     END-IF                                                               
092200     .                                                                    
092300     EJECT                                                                
092400 L-KOPIERA-PB-SEP SECTION.                                                
092500******************************************************************        
092600*                                                                *        
092700*    KOPIERING AV PERIODBEHOV PER C-LAGER                        *        
092800*                                                                         
092900*                                                                *        
093000******************************************************************        
093100     SKIP1                                                                
093200     IF  LINK-KVPB-SEP (IX) NOT = LINK-KVPB-VESL (IX)                     
093300         MOVE  LINK-KVPB-SEP (IX)  TO LINK-KVPB-VESL (IX)                 
093500                                                                          
093510         MOVE LINK-KDLPORS-TAB(01) TO W-KDLPORS(01)                       
093520         MOVE LINK-KDLPORS-TAB(02) TO W-KDLPORS(02)                       
093530         MOVE LINK-KDLPORS-TAB(03) TO W-KDLPORS(03)                       
093540         MOVE 51                   TO W-KDLPORS(04)                       
093550                                                                          
093600         CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                   
093601                                                                          
093610         MOVE W-KDLPORS(01)        TO LINK-KDLPORS-TAB(01)                
093620         MOVE W-KDLPORS(02)        TO LINK-KDLPORS-TAB(02)                
093630         MOVE W-KDLPORS(03)        TO LINK-KDLPORS-TAB(03)                
093700     END-IF                                                               
093800     .                                                                    
093900     EJECT                                                                
094000 M-BERAKNA-PRISKLASS SECTION.                                             
094100******************************************************************        
094200*                                                                *        
094300*    BERÄKNA                                                              
094400*                                                                *        
094500******************************************************************        
094600                                                                          
094700     MOVE +1 TO IX-PKL                                                    
094800     MOVE '9' TO LINK-KDPRISKL                                            
094900     PERFORM UNTIL IX-PKL > MAX-IX-PKL                                    
095000        IF LINK-PRARTSTD      NOT > TAB-PRIS (IX-PKL)                     
095100           MOVE TAB-KDPRISKL (IX-PKL) TO LINK-KDPRISKL                    
095200           MOVE MAX-IX-PKL TO IX-PKL                                      
095300        END-IF                                                            
095400        ADD +1 TO IX-PKL                                                  
095500     END-PERFORM                                                          
095600     .                                                                    
095700     EJECT                                                                
095800 N-BERAKNA-FREKVENSKLASS SECTION.                                         
095900******************************************************************        
096000*                                                                *        
096100*    BERÄKNA                                                              
096200*                                                                *        
096300******************************************************************        
096400                                                                          
096500     COMPUTE W-STYCKFORS ROUNDED =                                        
096600             (LINK-KVPB-SEP (1) +                                         
096700              LINK-KVPB-SATS(1) +                                         
096800              LINK-KVPB-TPO (1) +                                         
096900              LINK-KVPB-SDC-EJ-DIR ) * 12                                 
097000                                                                          
097100     MOVE +1 TO IX-FKL                                                    
097200     MOVE  'J' TO LINK-KDFREKKL                                           
097300     PERFORM UNTIL IX-FKL > MAX-IX-FKL                                    
097400        IF W-STYCKFORS NOT > TAB-ANT (IX-FKL)                             
097500           MOVE TAB-KDFREKKL (IX-FKL) TO LINK-KDFREKKL                    
097600           MOVE MAX-IX-FKL TO IX-FKL                                      
097700        END-IF                                                            
097800        ADD +1 TO IX-FKL                                                  
097900     END-PERFORM                                                          
098000     .                                                                    
098100     EJECT                                                                
098200 P-KVULOAD               SECTION.                                         
098300******************************************************************        
098400*                                                                *        
098500*    OM KVULOAD ÄR NOLL SÄTTS KVULOAD TILL KVPALL                         
098600*                                                                *        
098700******************************************************************        
098800                                                                          
098900     IF LINK-KVULOAD = ZERO                                               
099000        MOVE LINK-KVPALL  TO WS-KVULOAD                                   
099100     ELSE                                                                 
099200        MOVE LINK-KVULOAD TO WS-KVULOAD                                   
099300     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600 Q-BER-KVEOQ             SECTION.                                         
099700******************************************************************        
099800*                                                                *        
099900*    BERÄKNA KVEOQ                                                        
100000*                                                                *        
100100******************************************************************        
100200                                                                          
100300     MOVE    0.15  TO RELAGR                                              
100400     MOVE    ZERO  TO REOLAGK                                             
100500     MOVE 1500.00  TO PRLAGK                                              
100600     MOVE   40.00  TO LINK-PRORDSK                                        
100700     COMPUTE WS-D  = 12 * (LINK-KVPB-PLAN + LINK-KVPB-SATS (1))           
100800                                                                          
100900     COMPUTE VOLYM = LINK-VLARTNTO / 1000000                              
101000                                                                          
101100     PERFORM QA-HAMTA-FAKTORER                                            
101200                                                                          
101300     COMPUTE EOQ = (2 * WS-D * LINK-PRORDSK * WS-REOKOST-LEV *            
101400                    WS-REOKOST-OMR * WS-REOKOST-BEFT) /                   
101500                   (((RELAGR + REOLAGK) * LINK-PRARTBES) +                
101600                    (VOLYM * PRLAGK * WS-REVKOST-OMR))                    
101700                  ON SIZE ERROR                                           
101800                    MOVE ZERO TO EOQ                                      
101900     END-COMPUTE                                                          
102000                                                                          
102100     COMPUTE WS-KVEOQ ROUNDED = EOQ ** 0.5                                
102200                                                                          
102300     MOVE WS-KVEOQ  TO LINK-KVEOQ                                         
102400     .                                                                    
102500     EJECT                                                                
102600 QA-HAMTA-FAKTORER       SECTION.                                         
102700******************************************************************        
102800*                                                                *        
102900*    HÄMTA FRÅN COPYTEXTER  REOKOST -LEV -OMR -BEFT                       
103000*                              W221 W034 W036 W035               *        
103100******************************************************************        
103200                                                                          
103300     MOVE +1 TO IX-LEV                                                    
103400     MOVE 1 TO WS-REOKOST-LEV                                             
103500     PERFORM UNTIL IX-LEV > T34-MAXLEV-IX                                 
103600       IF LINK-IDLEVNR = T34-IDLEVNR (IX-LEV)                             
103700          MOVE T34-REOKOST-LEV (IX-LEV) TO WS-REOKOST-LEV                 
103800       END-IF                                                             
103900       ADD +1 TO IX-LEV                                                   
104000     END-PERFORM                                                          
104100                                                                          
104200     MOVE +1 TO IX-OMR                                                    
104300     MOVE 1 TO WS-REOKOST-OMR                                             
104400     MOVE 1 TO WS-REVKOST-OMR                                             
104500     PERFORM UNTIL IX-OMR > T36-MAXADLAGOMR-IX                            
104600       IF LINK-ADLAGOMR = T36-ADLAGOMR (IX-OMR)                           
104700          MOVE T36-REOKOST-OMR (IX-OMR) TO WS-REOKOST-OMR                 
104800          MOVE T36-REVKOST-OMR (IX-OMR) TO WS-REVKOST-OMR                 
104900       END-IF                                                             
105000       ADD +1 TO IX-OMR                                                   
105100     END-PERFORM                                                          
105200                                                                          
105300     MOVE +1 TO IX-BEFT                                                   
105400     MOVE 1 TO WS-REOKOST-BEFT                                            
105500     PERFORM UNTIL IX-BEFT > T35-MAXBEFT-IX                               
105600       IF LINK-BEFT = T35-BEFT (IX-BEFT)                                  
105700          MOVE T35-REOKOST-BEFT (IX-BEFT) TO WS-REOKOST-BEFT              
105800       END-IF                                                             
105900       ADD +1 TO IX-BEFT                                                  
106000     END-PERFORM                                                          
106100                                                                          
106200     MOVE SLB-REOLAGK TO WS-REOLAGK                                       
106300     .                                                                    
106400     EJECT                                                                
106500 S-BER-KVSLAGER-OPT      SECTION.                                         
106600******************************************************************        
106700*                                                                *        
106800*    BERÄKNA ETT EV NYTT KVQ OCH AVRUNDA                                  
106900*    BERÄKNA KVSLAGER-OPT                                                 
107000*                                                                *        
107100******************************************************************        
107200                                                                          
107300     IF LINK-FLMANQ = NEJ                                                 
107400        MOVE WS-KVEOQ TO WS-KVQ                                           
107500                                                                          
107600        IF WS-KVEOQ > ZERO AND WS-KVEOQ < 1                               
107700           MOVE 1 TO WS-KVQ                                               
107800        END-IF                                                            
107900                                                                          
108000        COMPUTE ODD ROUNDED = LINK-KVPB-PLAN / 4.33 / 5                   
108100                                                                          
108200        IF WS-KVEOQ < ODD                                                 
108300           MOVE ODD TO WS-KVQ-DEC                                         
108400           IF WS-KVQ < 1                                                  
108500              MOVE 1 TO WS-KVQ-DEC                                        
108600           END-IF                                                         
108700           ADD +0.4 TO WS-KVQ-DEC                                         
108800           MOVE WS-KVQ-DEC TO WS-KVQ                                      
108900        END-IF                                                            
109000                                                                          
109100        IF WS-KVQ < LINK-KVPALL                                           
109200           MOVE LINK-KVPALL  TO WS-KVQ                                    
109300        END-IF                                                            
109400                                                                          
109500        IF WS-KVQ < WS-KVULOAD                                            
109600           MOVE WS-KVULOAD   TO WS-KVQ                                    
109700        END-IF                                                            
109800                                                                          
109900        IF WS-KVQ > WS-KVULOAD AND WS-KVULOAD > ZERO                      
110000           PERFORM SA-ROUND-TO-KVULOAD                                    
110100        END-IF                                                            
110200                                                                          
110300        IF LINK-KVPALL = ZERO AND WS-KVULOAD = ZERO                       
110400           MOVE WS-KVQ   TO LINK-KVQ                                      
110500           PERFORM DA-AVRUNDA-KVQ                                         
110600           MOVE LINK-KVQ TO WS-KVQ                                        
110700        END-IF                                                            
110800                                                                          
110900        MOVE WS-KVQ      TO LINK-KVQ                                      
111000     ELSE                                                                 
111100        MOVE LINK-KVQ    TO WS-KVQ                                        
111200     END-IF                                                               
111300                                                                          
111400     PERFORM SB-BER-SIGMA                                                 
111500                                                                          
111600     PERFORM SC-BER-KVSLAGER                                              
111700                                                                          
111800     PERFORM SD-AVR-KVSLAGER                                              
111900     .                                                                    
112000     EJECT                                                                
112100 SA-ROUND-TO-KVULOAD SECTION.                                             
112200******************************************************************        
112300*                                                                *        
112400*    AVRUNDA KVQ TILL NÄRMASTE KVULOAD                           *        
112500*                                                                *        
112600******************************************************************        
112700                                                                          
112800     COMPUTE MULT   = WS-KVQ / WS-KVULOAD                                 
112900     COMPUTE LAGSTA = WS-KVULOAD * MULT                                   
113000     COMPUTE HOGSTA = WS-KVULOAD * (MULT + 1)                             
113100                                                                          
113200     IF (HOGSTA - WS-KVQ) > (WS-KVQ - LAGSTA)                             
113300        MOVE LAGSTA    TO WS-KVQ                                          
113400        IF WS-KVQ < LINK-KVPALL                                           
113500           MOVE HOGSTA TO WS-KVQ                                          
113600        END-IF                                                            
113700     ELSE                                                                 
113800        MOVE HOGSTA    TO WS-KVQ                                          
113900     END-IF                                                               
114000     .                                                                    
114100     EJECT                                                                
114200 SB-BER-SIGMA        SECTION.                                             
114300******************************************************************        
114400*                                                                *        
114500*    BERÄKNA SIGMA (TILL KVSLAGER)                               *        
114600*                                                                *        
114700******************************************************************        
114800                                                                          
114900     COMPUTE E-L   ROUNDED = LINK-KVVECKOR-FT / 4.33                      
115000                                                                          
115100     COMPUTE VAR-D ROUNDED = (1.25 * LINK-KVMAD-TOT(1)) *                 
115200                             (1.25 * LINK-KVMAD-TOT(1))                   
115300                                                                          
115400     COMPUTE E-D   ROUNDED = LINK-KVPB-PLAN * E-L                         
115500                                                                          
115600     PERFORM SBA-HAMTA-LVAR                                               
115700                                                                          
115800     COMPUTE VAR-L ROUNDED = (1.25 * WS-KVVECKOR-LVAR / 4.33) *           
115900                             (1.25 * WS-KVVECKOR-LVAR / 4.33)             
116000                                                                          
116100     COMPUTE SIGMA ROUNDED = (E-L * VAR-D) + (E-D * E-D * VAR-L)          
116200                                                                          
116300     COMPUTE WS-SIGMA ROUNDED = (SIGMA) ** 0.5                            
116400                                                                          
116500     PERFORM SBB-HAMTA-RETARGET                                           
116600                                                                          
116700     IF WS-SIGMA = ZERO                                                   
116800        MOVE 0.4 TO GU-K                                                  
116900     ELSE                                                                 
117000        COMPUTE GU-K  ROUNDED =                                           
117100                  WS-KVQ * (1 - WS-RETARGET) / WS-SIGMA                   
117200     END-IF                                                               
117300                                                                          
117400     .                                                                    
117500     EJECT                                                                
117600 SBA-HAMTA-LVAR          SECTION.                                         
117700******************************************************************        
117800*                                                                *        
117900*    HÄMTA FRÅN WDF1  KVVVECKOR-LVAR                                      
118000*                                                                *        
118100******************************************************************        
118200                                                                          
118300     MOVE ZERO             TO WS-KVVECKOR-LVAR                            
118400     MOVE LINK-IDLEVNR     TO W-IDLEVNR                                   
118500     PERFORM IMS-GET-WDF101                                               
118600     IF SEGMENT-FOUND AND LEV-KVVECKOR-LVAR NUMERIC                       
118700        MOVE LEV-KVVECKOR-LVAR TO WS-KVVECKOR-LVAR                        
118800     END-IF                                                               
118900                                                                          
119000*                    LINK-KVVECKOR-LVAR KAN VARA LOW-VALUE                
119100     IF LINK-KVVECKOR-LVAR NUMERIC AND LINK-KVVECKOR-LVAR > ZERO          
119200        MOVE LINK-KVVECKOR-LVAR TO WS-KVVECKOR-LVAR                       
119300     END-IF                                                               
119400     .                                                                    
119500     EJECT                                                                
119600 SBB-HAMTA-RETARGET      SECTION.                                         
119700     MOVE SLB-RETARGET TO WS-RETARGET                                     
119800     .                                                                    
119900     EJECT                                                                
120000 SC-BER-KVSLAGER     SECTION.                                             
120100******************************************************************        
120200*                                                                *        
120300*    BERÄKNA K-FAKT KVSLAGER                                     *        
120400*                                                                *        
120500******************************************************************        
120600                                                                          
120700     IF GU-K >= 0.399                                                     
120800        MOVE ZERO TO WS-K                                                 
120900     ELSE                                                                 
121000        IF GU-K < 0.00038                                                 
121100           MOVE 3 TO WS-K                                                 
121200        ELSE                                                              
121300           MOVE -5.3925569     TO A0                                      
121400           MOVE  5.6211054     TO A1                                      
121500           MOVE -3.8836830     TO A2                                      
121600           MOVE  1.0897299     TO A3                                      
121700           MOVE  1             TO B0                                      
121800           MOVE -0.72496485    TO B1                                      
121900           MOVE  0.507326622   TO B2                                      
122000           MOVE  0.0669136868  TO B3                                      
122100           MOVE -0.00329129114 TO B4                                      
122200                                                                          
122300           COMPUTE ZZZ     ROUNDED = 25 / ((GU-K) ** 2)                   
122400           COMPUTE ZZ      ROUNDED = FUNCTION LOG (ZZZ)                   
122500           COMPUTE Z       ROUNDED = ZZ ** 0.5                            
122600           COMPUTE TALJARE ROUNDED = (A0 + A1 * Z +                       
122700                                      A2 * (Z ** 2) +                     
122800                                      A3 * (Z ** 3))                      
122900           COMPUTE NAMNARE ROUNDED = (B0 + B1 * Z +                       
123000                                      B2 * (Z ** 2) +                     
123100                                      B3 * (Z ** 3) +                     
123200                                      B4 * (Z ** 4))                      
123300           COMPUTE WS-K    ROUNDED = TALJARE / NAMNARE                    
123400        END-IF                                                            
123500     END-IF                                                               
123600                                                                          
123700     COMPUTE WS-KVSLAGER-OPT ROUNDED = WS-K * WS-SIGMA                    
123800                                                                          
123900     PERFORM SCA-JUST-FAKTOR                                              
124000                                                                          
124100     MOVE WS-KVSLAGER-OPT TO LINK-KVSLAGER-OPT                            
124200     .                                                                    
124300     EJECT                                                                
124400 SCA-JUST-FAKTOR     SECTION.                                             
124500******************************************************************        
124600*                                                                *        
124700*    EV RESLJUST SKALL PÅVERKA KVSLAGER-OPT                      *        
124800*                                                                *        
124900******************************************************************        
125000                                                                          
125100     MOVE LINK-KVPB-SEP  (IX) TO W-KVPB-TOT                               
125200     ADD  LINK-KVPB-SATS (IX) TO W-KVPB-TOT                               
125300     ADD  LINK-KVPB-SDC-EJ-DIR                                            
125400                              TO W-KVPB-TOT                               
125500     IF  LINK-KDERS (IX)    > 09                                          
125600     OR  (LINK-KDERS (IX)   > 00                                          
125700         AND LINK-KVBR-TOT  = ZERO)                                       
125800     OR  W-KVPB-TOT         = ZERO                                        
125900     OR  LINK-REDIRLEV (IX) = 1.00                                        
126000     OR  LINK-KDUART        = 'S'                                         
126100     OR  (LINK-KDHF         > ZERO                                        
126200         AND  LINK-FLAVRART = JA)                                         
126300                                                                          
126400         MOVE ZERO  TO WS-KVSLAGER-OPT                                    
126500     ELSE                                                                 
126600         MOVE LINK-TISLJUST   (IX)  TO TMP1-YYWW                          
126700         MOVE LINK-TIAAVV-AKT       TO TMP2-YYWW                          
126800         PERFORM WY2000P3                                                 
126900         IF LINK-RESLJUST (IX) = 9.9 AND                                  
127000            TMP1-YYWW  >  TMP2-YYWW                                       
127100            MOVE LINK-KVSLAGER (1) TO WS-KVSLAGER-OPT                     
127200         ELSE                                                             
127300           IF LINK-RESLJUST (1) > ZERO AND                                
127400              LINK-RESLJUST (1) < 9.9                                     
127500              COMPUTE WS-KVSLAGER-OPT ROUNDED =                           
127600                      LINK-RESLJUST (1) * WS-KVSLAGER-OPT                 
127700           END-IF                                                         
127800         END-IF                                                           
127900     END-IF                                                               
128000     .                                                                    
128100     EJECT                                                                
128200 SD-AVR-KVSLAGER     SECTION.                                             
128300******************************************************************        
128400*                                                                *        
128500*    AVRUNDA        KVSLAGER                                     *        
128600*                                                                *        
128700******************************************************************        
128800                                                                          
128900     MOVE WS-KVSLAGER-OPT TO WS-KVSLAGER                                  
129000                                                                          
129100     PERFORM  SDA-HAMTA-KVVECKOR-MINSL                                    
129200                                                                          
129300     COMPUTE MIN ROUNDED = WS-KVVECKOR-MINSL *                            
129400        (LINK-KVPB-SEP (1) + LINK-KVPB-SATS (1) +                         
129500         LINK-KVPB-SDC-EJ-DIR) / 4.33                                     
129600                                                                          
129700     IF WS-KVSLAGER-OPT < MIN                                             
129800        MOVE MIN TO WS-KVSLAGER                                           
129900     ELSE                                                                 
130000        MOVE WS-KVSLAGER-OPT TO WS-KVSLAGER                               
130100     END-IF                                                               
130200                                                                          
130300     PERFORM  SDB-HAMTA-KVVECKOR-MAXSL                                    
130400                                                                          
130500     COMPUTE MAX ROUNDED = WS-KVVECKOR-MAXSL *                            
130600        (LINK-KVPB-SEP (1) + LINK-KVPB-SATS (1) +                         
130700         LINK-KVPB-SDC-EJ-DIR) / 4.33                                     
130800                                                                          
130900     IF WS-KVSLAGER > MAX                                                 
131000        MOVE MAX TO WS-KVSLAGER                                           
131100     END-IF                                                               
131200                                                                          
131300     IF  LINK-KDERS (IX)    > 09                                          
131400     OR  (LINK-KDERS (IX)   > 00                                          
131500         AND LINK-KVBR-TOT  = ZERO)                                       
131600     OR  W-KVPB-TOT         = ZERO                                        
131700     OR  LINK-REDIRLEV (IX) = 1.00                                        
131800     OR  LINK-KDUART        = 'S'                                         
131900     OR  (LINK-KDHF         > ZERO                                        
132000         AND  LINK-FLAVRART = JA)                                         
132100         MOVE ZERO  TO WS-KVSLAGER                                        
132200     END-IF                                                               
132300     IF LINK-RESLJUST (1) = 9.9                                           
132400***     MANUELLT SATT KVSLAGER SKALL BEHÅLLA SITT VÄRDE                   
132500        MOVE LINK-KVSLAGER (1) TO WS-KVSLAGER                             
132600     END-IF                                                               
132700                                                                          
132800     MOVE WS-KVSLAGER TO LINK-KVSLAGER (1)                                
132900     .                                                                    
133000     EJECT                                                                
133100 SDA-HAMTA-KVVECKOR-MINSL SECTION.                                        
133200     MOVE SLB-KVVECKOR-MINSL TO WS-KVVECKOR-MINSL                         
133300     .                                                                    
133400     EJECT                                                                
133500 SDB-HAMTA-KVVECKOR-MAXSL SECTION.                                        
133600     MOVE SLB-KVVECKOR-MAXSL TO WS-KVVECKOR-MAXSL                         
133700     .                                                                    
133800     EJECT                                                                
133900 S03-SUMMERA-VECKOBEHOV SECTION.                                          
134000******************************************************************        
134100*                                                                *        
134200*    SUMMERING AV VECKOBEHOV I LNK2-AREA                         *        
134300*    FRÅN IX-START-VECKA I  IX-ANTAL-VECKOR                      *        
134400*                                                                *        
134500******************************************************************        
134600     SKIP1                                                                
134700     MOVE ZERO TO W-SUM-BEHOV                                             
134800     MOVE IX-START-VECKA TO IX-VECKA                                      
134900     ADD  IX-START-VECKA TO IX-ANTAL-VECKOR                               
135000     SKIP1                                                                
135100     PERFORM UNTIL NOT(                                                   
135200        IX-VECKA < IX-ANTAL-VECKOR                                        
135300     AND     IX-VECKA NOT > LNK2-KVVECKOR-BEHOV)                          
135400         ADD LNK2-KVBEHOV-VECKA (IX-VECKA)                                
135500                             TO W-SUM-BEHOV                               
135600         ADD 1 TO IX-VECKA                                                
135700     END-PERFORM                                                          
135800     .                                                                    
135900     EJECT                                                                
136000*    -COPY WY2000P2                                                       
136100     EJECT                                                                
136200*    -COPY WY2000P3                                                       
136300     EJECT                                                                
136400 IMS-GET-WDP601 SECTION.                                                  
136500     STRING 'WDP601  (WDP601KY =' W-WDP601KY-X ')'                        
136600          DELIMITED BY SIZE INTO SSA1                                     
136700     MOVE '    ' TO GOOD-STATUSCODES                                      
136800     CALL CBLTDLI USING GU WDP6-PCB DLI-IO-WDP601 SSA1                    
136900     MOVE WDP6-STATUS-CODE TO STATUS-WS                                   
137000     PERFORM IMS-STATUSCHECK                                              
137100     .                                                                    
137200     SKIP3                                                                
137300 IMS-GET-WDF101 SECTION.                                                  
137400     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
137500          DELIMITED BY SIZE INTO SSA1                                     
137600     MOVE '    ' TO GOOD-STATUSCODES                                      
137700     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
137800     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
137900     PERFORM IMS-STATUSCHECK                                              
138000     .                                                                    
138100     SKIP3                                                                
138200 IMS-STATUSCHECK SECTION.                                                 
138300     SET STATUS-IX TO 1                                                   
138400     SEARCH GOOD-STATUS                                                   
138500       AT END                                                             
138600         MOVE W-KDPRODSL  TO WS-KDPRODSL                                  
138700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
138800         '  WDP6 ' WS-KDPRODSL ' ' W-KDPRISKL ' ' W-KDFREKKL              
138900         '  WDF1 ' W-IDLEVNR                                              
139000         '  SSA  ' SSA1                                                   
139100         DELIMITED BY SIZE INTO ERROR-TEXT                                
139200         CALL FELLOG                                                      
139300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
139400         CONTINUE                                                         
139500     END-SEARCH                                                           
139600     .                                                                    
