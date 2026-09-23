000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    WYRST100.                                                 
000400 AUTHOR.        KARL JOHAN HANSSON.                                       
000500     DATE-WRITTEN.  DEC 1989.                                             
000600*    REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*      PROGRAM  SOM STOPPAR RUTINEN WYR001                                
001000*      OM LAGERVÄRDES-HÖJNINGEN -SÄNKNINGEN ÄR UTANFÖR GIVEN RAM          
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 - OM ANTALET FELPOSTER FÖR STORT                           
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*    --- INFILER:                                                         
002200*           --- FELPOSTER:                                                
002300     SELECT W54020                       ASSIGN TO WYRST1D1.              
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE SECTION.                                                            
002800     SKIP2                                                                
002900 FD  W54020                                                               
003000     RECORDING V                                                          
003100     BLOCK CONTAINS 0.                                                    
003200                                                                          
003210 01  FILLER                  PIC X(998).                                  
003300 01  W54020-POST.                                                         
003400*    03  -COPY  W51060                                                    
003500     SKIP2                                                                
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)      VALUE 'WYRST100'.          
004210 77  WS-MIN-KR-P                 PIC S9(11)    VALUE ZERO COMP-3.         
004300 77  WS-MAX-KR-P                 PIC S9(11)    VALUE ZERO COMP-3.         
004400 77  WS-MIN-KR                   PIC  9(11)    VALUE ZERO.                
004500 77  WS-MAX-KR                   PIC  9(11)    VALUE ZERO.                
004600 77  WS-SUMMA-KR                 PIC  9(11)    VALUE ZERO.                
004700 77  WS-SUMMA-KR-SIGN            PIC X.                                   
004710 77  WS-MIN-KR-SIGN              PIC X.                                   
004720 77  WS-MAX-KR-SIGN              PIC X.                                   
004800 77  WS-SUMMA                    PIC S9(11)V99 VALUE ZERO.                
004900                                                                          
005000 01  W54020-SLUT-SW              PIC X(1)      VALUE 'N'.                 
005100     88  W54020-SLUT                           VALUE 'J'.                 
005400                                                                          
005500 01  DYNAMISKT-SUBPROGRAM.                                                
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005601     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
005610     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
005700                                                                          
005800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005900                                                                          
006000*    --- ARBETS-AREOR TILL  IMS-SEKTIONERNA                               
006100*                                                                         
006200 01  FILLER                      PIC X(16)   VALUE 'IMS-NYCKLAR'.         
006300 01  NYCKLAR-TILL-DLI.                                                    
006900   03  W-WDGXKEY-X.                                                       
006910     05  W-IDHTYP-5107           PIC X(4)    VALUE '5107'.                
006920     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
006930                                                                          
006940   03  W-IDFTG-X.                                                         
006950     05  W-IDFTG                 PIC 9(2)    VALUE 57.                    
007000*    --- STATUS-KOD FRÅN IMS                                              
007100 01  STATUS-WS                   PIC XX.                                  
007200     88  SEGMENT-FINNS                       VALUE '  '.                  
007300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
007400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007500     88  BASEN-SLUT                          VALUE 'GB'.                  
007600     SKIP2                                                                
007700 01  GODK-STATUSKODER.                                                    
007800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007900     SKIP3                                                                
008000 01  SSA1                        PIC X(64).                               
008100 01  SSA2                        PIC X(64).                               
008200     EJECT                                                                
008300*    --- IMS FUNKTIONSKODER                                               
008400*01  -COPY W0003                                                          
008500     EJECT                                                                
008600 01  DLI-IO-WDGX5108.                                                     
008700*    03  -COPY WDGX5108                                                   
008800     EJECT                                                                
008900 LINKAGE SECTION.                                                         
009000*01  -COPY W0008      -PRE WDR2-                                          
009100     05  FILLER                  PIC X.                                   
009200     EJECT                                                                
009300 PROCEDURE DIVISION USING WDR2-PCB.                                       
009400                                                                          
009410 MAIN SECTION.                                                            
009420     ENTRY 'DLITCBL'  USING  WDR2-PCB.                                    
009430                                                                          
009500     OPEN INPUT W54020                                                    
009600                                                                          
009700* GET THE STOCK VALUE CHANGE INTERVALS FROM WDR2(WDGX5108)                
009710     PERFORM IMS-GU-WDGX5108                                              
009720     IF SEGMENT-FINNS                                                     
010000       MOVE 5108-SULSNIV-MIN  TO WS-MIN-KR                                
010100                                 WS-MIN-KR-P                              
010500       MOVE 5108-SULSNIV-MAX  TO WS-MAX-KR                                
010600                                 WS-MAX-KR-P                              
010610     ELSE                                                                 
010620       DISPLAY '---> SULSNIV VALUES NOT FOUND '                           
010700       MOVE +16               TO RKOD-ABEND                               
010910       CALL ABEND          USING RKOD-ABEND                               
011000     END-IF                                                               
011001                                                                          
011010     IF WS-MIN-KR-P < 0                                                   
011020       MOVE '-'               TO WS-MIN-KR-SIGN                           
011030     ELSE                                                                 
011040       MOVE ' '               TO WS-MIN-KR-SIGN                           
011050     END-IF                                                               
011051                                                                          
011060     IF WS-MAX-KR-P < 0                                                   
011070       MOVE '-'               TO WS-MAX-KR-SIGN                           
011080     ELSE                                                                 
011090       MOVE ' '               TO WS-MAX-KR-SIGN                           
011091     END-IF                                                               
011100                                                                          
011200* RÄKNA SAMMAN VÄRDENA PÅ FIL TILL SAP (PEDAL)                            
011300                                                                          
011400     READ W54020 AT END MOVE 'J' TO W54020-SLUT-SW END-READ               
011500     PERFORM UNTIL W54020-SLUT                                            
011600         ADD EKHT-SUBEL     TO WS-SUMMA                                   
011700         READ W54020 AT END MOVE 'J' TO W54020-SLUT-SW END-READ           
011800     END-PERFORM                                                          
011900                                                                          
012000* TESTA OM OVANSTÅENDE SUMMA (LAGERVÄRDESFÖRÄNDRINGEN) LIGGER             
012100* INOM GODKÄNT INTERVALL (PDS:ETS MIN OCH MAXVÄRDEN)                      
012200                                                                          
012300     MOVE WS-SUMMA          TO WS-SUMMA-KR                                
012400     IF WS-SUMMA < 0                                                      
012500       MOVE '-'             TO WS-SUMMA-KR-SIGN                           
012600     ELSE                                                                 
012700       MOVE ' '             TO WS-SUMMA-KR-SIGN                           
012800     END-IF                                                               
012900     DISPLAY ' '                                                          
013000     DISPLAY '                                                KR'         
013100     DISPLAY '---> GODKÄND LÄGSTA LAGERVÄRDESÄNDRING '                    
013200             WS-MIN-KR WS-MIN-KR-SIGN                                     
013300     DISPLAY '---> GODKÄND HÖGSTA LAGERVÄRDESÄNDRING '                    
013400             WS-MAX-KR WS-MAX-KR-SIGN                                     
013500     DISPLAY '--->        AKTUELL LAGERVÄRDESÄNDRING '                    
013600             WS-SUMMA-KR WS-SUMMA-KR-SIGN                                 
013700     IF WS-SUMMA < WS-MIN-KR-P OR  > WS-MAX-KR-P                          
013800       DISPLAY '---> KONTAKTA JOURANSVARIG FÖR ÅTGÄRD '                   
013900       MOVE +16        TO RKOD-ABEND                                      
014000     END-IF                                                               
014100                                                                          
014200     CALL ABEND USING RKOD-ABEND                                          
014300     CLOSE W54020                                                         
014301     MOVE ZERO TO RETURN-CODE                                             
014302     GOBACK                                                               
014303     .                                                                    
014320     EJECT                                                                
014400                                                                          
014500 IMS-GU-WDGX5108 SECTION.                                                 
014510                                                                          
014551     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
014552          DELIMITED BY SIZE INTO SSA1                                     
014553     STRING 'WDGX5108(IDFTG    =' W-IDFTG-X ')'                           
014554          DELIMITED BY SIZE INTO SSA2                                     
014560     MOVE '  '  TO GODK-STATUSKODER                                       
014570     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX5108 SSA1 SSA2             
014590     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
014591     PERFORM IMS-STATUSKONTROLL                                           
014592     .                                                                    
014593     EJECT                                                                
014594 IMS-STATUSKONTROLL SECTION.                                              
014595                                                                          
014596     SET STATUS-IX TO 1                                                   
014597     SEARCH GODK-STATUS                                                   
014598       AT END CALL FELLOG                                                 
014599       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
014600            CONTINUE                                                      
014601     END-SEARCH                                                           
014602     .                                                                    
014603     EJECT                                                                
