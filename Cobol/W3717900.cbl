000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3717900.                                                
000400*AUTHOR.         INGVAR SKJELBRED                                         
000500*DATE-WRITTEN.   99/02/10.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLA CLEARING FAKTUROR I STATUS 4 OCH GODKÄNNANDE                
001100*        ÄLDRE ÄN 12 VECKOR TAS BORT.                                     
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WL3171 (WDR4)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FIL MED RENSADE FAKTUROR                                   
002800     SELECT W37179                     ASSIGN TO W37179D1.                
002810                                                                          
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W37179                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  POST -COPY W37179 -PRE UT-  -L.                                      
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101*    -- CHECKED BY WY2000                                                 
004110     SKIP3                                                                
004200 77  IDPGM                       PIC X(8)    VALUE 'W3717900'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004401 77  OK                          PIC X       VALUE ' '.                   
004410 77  WS-KDBYTBEK                 PIC X       VALUE ' '.                   
004420                                                                          
004430 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
004440 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
004441                                                                          
004500     SKIP2                                                                
004510 01  CHKP-VAR.                                                            
004520 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004530 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004540 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004550 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004551 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004553 03  CHKP-MAX                    PIC S9(3)   VALUE +400.                  
004590     SKIP2                                                                
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900                                                                          
005000 77  W37179-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W37179                       VALUE 'J'.                   
005101                                                                          
005102*                                                                         
005103 01  W-RENSNINGS-DATUM           PIC 9(8)    VALUE ZERO.                  
005104*                                                                         
005200     EJECT                                                                
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006710     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
007200     EJECT                                                                
007300*    - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'WDAGAREA'.            
007800*01  -COPY WDAGAREA                                                       
007900     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W37179       -PRE UT-                                     
008600*                                                                         
008630     SKIP2                                                                
008670                                                                          
008680 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
008690 01  WS-SEKTION              PIC X(30)   VALUE SPACE.                     
008691 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
008692 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
008693 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
008694 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
008695                                                                          
008696 01  NYCKLAR-TILL-DLI.                                                    
008697     03   W-IDHTYP-X.                                                     
008698         05  W-IDHTYP            PIC X(4)   VALUE '3171'.                 
008699         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
008700     03  FILLER                  PIC X(11)  VALUE 'IDARTNR-OBJ'.          
008701     03  W-IDARTNR-OBJ-X.                                                 
008702         05  W-IDARTNR-OBJ       PIC S9(9)  VALUE ZERO COMP-3.            
008703     03  W-IDARTNR-MIN-X.                                                 
008704         05  W-IDARTNR-MIN       PIC S9(7)  VALUE ZERO COMP-3.            
008705     03  FILLER                  PIC X(11)  VALUE 'IDFAKT-3172'.          
008706     03  W-WDGX3172-X.                                                    
008707         05 W-IDFAKT-3172        PIC S9(7) VALUE ZERO COMP-3.             
008708     03  FILLER                  PIC X(11)  VALUE 'IDARTNR    '.          
008709     03  W-IDARTNR-X.                                                     
008710         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
008720     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011500     SKIP3                                                                
011600 01  DLI-IO-AREA1.                                                        
011700     03  IO-AREA1                PIC X(350)  VALUE SPACE.                 
011810     SKIP3                                                                
011820     03  WL317111 REDEFINES IO-AREA1.                                     
011830*        05  -COPY WDGX3172  -PRE 3171-                                   
011840     03  WL317121 REDEFINES IO-AREA1.                                     
011850*        05  -COPY WDGX3174  -PRE 3171-                                   
011860     03  WL317131 REDEFINES IO-AREA1.                                     
011870*        05  -COPY WDGX3176  -PRE 3171-                                   
012100     SKIP3                                                                
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600                                                                          
012700*01  -COPY W0009   -PRE MSG-                                              
012810     EJECT                                                                
012900*01  -COPY W0008  -PRE 3171-                                              
013000     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING MSG-PCB 3171-PCB.                              
013300     ENTRY 'DLITCBL' USING MSG-PCB 3171-PCB.                              
013400                                                                          
013500     SKIP2                                                                
013510*                                                                         
013600     PERFORM A-INIT                                                       
013610*                                                                         
013701     PERFORM IMS-GET-ROOT                                                 
013702     IF SEGMENT-FINNS                                                     
013705        PERFORM IMS-GNP-INVOICE                                           
013706        PERFORM UNTIL SEGMENT-SLUT                                        
013707                   OR SEGMENT-SAKNAS                                      
013708           IF CHKP-ANT > CHKP-MAX                                         
013709              PERFORM S02-TAG-CHECKPOINT                                  
013710              PERFORM IMS-GET-ROOT                                        
013711              IF SEGMENT-FINNS                                            
013712                 PERFORM IMS-GNP-INVOICE                                  
013713              END-IF                                                      
013714           END-IF                                                         
013716           PERFORM C-UPPDATERA                                            
013717           IF SEGMENT-FINNS                                               
013718              PERFORM IMS-GNP-INVOICE                                     
013719           END-IF                                                         
013720        END-PERFORM                                                       
013740     END-IF                                                               
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT SECTION.                                                          
015510     MOVE 'A-INIT'            TO WS-SEKTION                               
015600     SKIP2                                                                
015700                                                                          
015800     OPEN OUTPUT W37179                                                   
015900                                                                          
016000     ACCEPT DAGENS-DATUM       FROM DATE                                  
016100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016110     PERFORM IMS-RESTART                                                  
016120     PERFORM AB-BORTTAGNINGSDATUM                                         
016200     .                                                                    
016210     EJECT                                                                
016300                                                                          
016500 AB-BORTTAGNINGSDATUM SECTION.                                            
016501     MOVE 'AB-BORTTAGNINGSDAT' TO WS-SEKTION                              
016510                                                                          
016511*************************************************************             
016512**** SKAPAR ETT DATUM FÖR BORTRENSINGEN AV RAPPORTER ********             
016513**** FRÅN WDR4 SOM LEGAT MINST 12 VECKOR PÅ REGISTRET********             
016514**** 7 * 12 = 84 DVS 84 DAGAR                        ********             
016515*************************************************************             
016516                                                                          
016517     ACCEPT DAG-TIAAMMDD-TOM FROM DATE                                    
016518     MOVE 84     TO DAG-KVKALDAG                                          
016519     MOVE 003    TO DAG-KDCALL                                            
016520                                                                          
016521     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
016522                                                                          
016523     IF DAG-KDSVAR = OK                                                   
016524       MOVE DAG-TIAAMMDD-FOM TO W-RENSNINGS-DATUM (3:6)                   
016525       MOVE DAG-TISEKEL-FOM  TO W-RENSNINGS-DATUM (1:2)                   
016526       DISPLAY '*** RENSNINGSDATUM FÖR WDR4 ****** '                      
016527       DISPLAY 'RENSNINGSDATUM ' W-RENSNINGS-DATUM                        
016528     ELSE                                                                 
016529       MOVE 'FEL I DATUMKONVERTERING' TO   FELTEXT-STR                    
016530       PERFORM S99-ABEND                                                  
016531     END-IF                                                               
016532     .                                                                    
016533     EJECT                                                                
016534                                                                          
016800                                                                          
019340 C-UPPDATERA SECTION.                                                     
019341     MOVE 'C-UPPDATERA     ' TO WS-SEKTION                                
019350     SKIP2                                                                
019360                                                                          
019361     IF SEGMENT-FINNS                                                     
019362        IF 3171-3172-KDTRSTAT = 4                                         
019363           IF W-RENSNINGS-DATUM  > 3171-3172-DAANKDAG                     
019365              MOVE 3171-3172-IDFAKT TO UT-IDFAKT                          
019367              MOVE 3171-3172-DAANKDAG                                     
019368                                      TO UT-DAANKDAG                      
019370              MOVE 3171-3172-DASNDDAT TO UT-DASNDDAT                      
019373              MOVE 3171-3172-IDDC-REC TO UT-IDDC-REC                      
019376              MOVE 3171-3172-IDDC-SEND TO UT-IDDC-SEND                    
019379              MOVE 3171-3172-KDTRSTAT TO UT-KDTRSTAT                      
019382              MOVE 3171-3172-SUFKTNTO TO UT-SUFKTNTO                      
019385              MOVE 3171-3172-VKORDBTO-FAKT                                
019386                                      TO UT-VKORDBTO-FAKT                 
019388              MOVE 3171-3172-VLORDBTO-FAKT                                
019389                                      TO UT-VLORDBTO-FAKT                 
019394              PERFORM S01-SKRIV-W37179                                    
019395                                                                          
019396              ADD +1 TO  CHKP-ANT                                         
019397              PERFORM IMS-DLET-FAKT-3171                                  
019398           END-IF                                                         
019399        END-IF                                                            
019400     END-IF                                                               
019401     .                                                                    
019402     EJECT                                                                
019410 Z-FINIT SECTION.                                                         
019420     MOVE 'Z-FINIT'            TO WS-SEKTION                              
019500                                                                          
019600                                                                          
019700     CLOSE W37179                                                         
019710                                                                          
019800     SKIP2                                                                
019900     MOVE 'S' TO POSTSUM-OPKOD                                            
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     .                                                                    
020200     EJECT                                                                
021701 S01-SKRIV-W37179 SECTION.                                                
021702     MOVE 'S01-SKRIV-W37179' TO WS-FIL-SEKTION                            
021704     SKIP2                                                                
021705     WRITE UT-POST FROM UT-AREA                                           
021706                                                                          
021707     MOVE 'UT  '   TO POSTSUM-TRANSTYP                                    
021708     MOVE 'W37179' TO POSTSUM-FDNAMN                                      
021709     MOVE 'W37179D1' TO POSTSUM-DDNAMN2                                   
021710     CALL POSTSUM USING POSTSUM-PARM                                      
021711     .                                                                    
021712     EJECT                                                                
021713 S02-TAG-CHECKPOINT SECTION.                                              
021714***************************************************************           
021715**** HÄR NOLLSTÄLLS BUFFER PÅ IMS      ************************           
021716***************************************************************           
021717                                                                          
021720     PERFORM IMS-CHECKPOINT                                               
021722     MOVE ZERO TO  CHKP-ANT                                               
021723                                                                          
021725     .                                                                    
021730     EJECT                                                                
021740 S99-ABEND SECTION.                                                       
021760     SKIP2                                                                
021770     MOVE 'S' TO POSTSUM-OPKOD                                            
021780     CALL POSTSUM USING POSTSUM-PARM                                      
021790     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
021791     .                                                                    
021792     EJECT                                                                
021800* --- IMS SEKTIONER ---                                                   
021900     SKIP3                                                                
022000     EJECT                                                                
022010 IMS-GET-ROOT SECTION.                                                    
022020     MOVE 'IMS-GU-ROOT' TO WS-IMS-SEKTION                                 
022040                                                                          
022050     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
022060          DELIMITED BY SIZE INTO SSA1                                     
022070     MOVE '  ' TO  GODK-STATUSKODER                                       
022080     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-AREA1 SSA1                    
022090     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
022091     PERFORM IMS-STATUSKONTROLL                                           
022092     .                                                                    
022093     EJECT                                                                
022094 IMS-GNP-INVOICE SECTION.                                                 
022095     MOVE 'IMS-GNP-INVOICE'         TO WS-IMS-SEKTION                     
022097                                                                          
022098     MOVE 'WL317111 ' TO SSA1                                             
022099     MOVE '  GEGB' TO  GODK-STATUSKODER                                   
022100     CALL CBLTDLI USING GHNP 3171-PCB DLI-IO-AREA1 SSA1                   
022101     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
022102     PERFORM IMS-STATUSKONTROLL                                           
022103     .                                                                    
022104     EJECT                                                                
022105                                                                          
022122 IMS-DLET-FAKT-3171 SECTION.                                              
022123                                                                          
022124     MOVE 'IMS-DLET-FAKT-3171' TO WS-IMS-SEKTION                          
022126     MOVE '  ' TO GODK-STATUSKODER                                        
022127     CALL CBLTDLI USING DLET 3171-PCB DLI-IO-AREA1                        
022128     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
022129     PERFORM IMS-STATUSKONTROLL                                           
022130     .                                                                    
022131     EJECT                                                                
022140                                                                          
024610 IMS-RESTART SECTION.                                                     
024620     SKIP2                                                                
024630     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024640     MOVE '  ' TO GODK-STATUSKODER                                        
024650     CALL CBLTDLI USING XRST MSG-PCB                                      
024660                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024670                        CHKP-AREA-LENGTH CHKP-AREA                        
024680     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024690     PERFORM IMS-STATUSKONTROLL                                           
024691     .                                                                    
024697 IMS-CHECKPOINT SECTION.                                                  
024699     SKIP2                                                                
024700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024701     MOVE '  XD' TO GODK-STATUSKODER                                      
024702     CALL CBLTDLI USING CHKP MSG-PCB                                      
024703                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024704                        CHKP-AREA-LENGTH CHKP-AREA                        
024705     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024707     PERFORM IMS-STATUSKONTROLL                                           
024708                                                                          
024709     IF IMS-EJ-OK                                                         
024710       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
024711       DISPLAY FELTEXT                                                    
024712       CALL FELLOG                                                        
024713     END-IF                                                               
024716     .                                                                    
024717     EJECT                                                                
024720 IMS-STATUSKONTROLL SECTION.                                              
024800     SKIP2                                                                
024900     SET STATUS-IX TO 1                                                   
025000     SEARCH GODK-STATUS                                                   
025100       AT END                                                             
025200         MOVE 'XXXIMSABENDXXX' TO FELTEXT-STR                             
025300         DISPLAY FELTEXT                                                  
025400         CALL FELLOG                                                      
025500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025600         CONTINUE                                                         
025700     END-SEARCH                                                           
025800     .                                                                    
