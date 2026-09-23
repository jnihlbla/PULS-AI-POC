001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6010700.                                                
001300*AUTHOR.         KATARINA KYMMER.                                         
001400*DATE-WRITTEN.   92/04/21.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        FRÅGEBILD MOT PLACERINGSREGISTRET W6G1-W6PLAA                    
002000*                                                                         
002110*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
002120*                                      WDB6                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W6T107                                              
002500*        MID:         W6I10701                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W6O10701                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003401                                                                          
003410*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W6010700'.            
003510 77  NYCKLAR-OK                  PIC X       VALUE 'J'.                   
003511 77  OTILL-VARDE                 PIC X       VALUE 'N'.                   
003520 77  NYA-NYCKLAR                 PIC X.                                   
003530 77  INDATA-OK                   PIC X       VALUE 'J'.                   
003540 77  PF7-ADINLOMR                 PIC X(4).                               
003541 77  WS-ADINLOMR                 PIC X(4).                                
003550 77  WS-KDINLOMR                 PIC X(3).                                
003560 77  WS-ADINLOMR-PAR             PIC X(4).                                
003570 77  WS-KDINLUPF                 PIC X(4).                                
003580 77  IX                          PIC S9(9)  COMP SYNC.                    
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +665 COMP SYNC.         
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100                                                                          
005610       EJECT                                                              
005700                                                                          
005800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005900     88  ALLT-OK                             VALUE 'J'.                   
006000                                                                          
006010 77  WS-NYCKLAR                 PIC 9.                                    
006020     88  PLAC-AR-NYCKEL                      VALUE 1.                     
006021     88  TYP-AR-NYCKEL                       VALUE 2.                     
006022     88  GRP-AR-NYCKEL                       VALUE 3.                     
006023     88  TYPGRP-AR-NYCKEL                    VALUE 4.                     
006024     88  STATUS-AR-NYCKEL                    VALUE 5.                     
006025     88  TOM-NYCKEL                          VALUE 6.                     
006026                                                                          
006100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006200     88  EGEN-MID                            VALUE '6107'.                
006300     88  GODK-MID                            VALUE '6101' '6102'          
006400                                                   '6103' '6104'          
006500                                                   '6105' '6106'          
006600                                                   '6107' '6108'          
006700                                                   '6109'.                
006800     88  HELP-MID                            VALUE '0551'.                
006900     EJECT                                                                
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007600     EJECT                                                                
007610*01 -COPY WMSGINIT                                                        
007620     SKIP3                                                                
007700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007800*01 -COPY WMEDAREA                                                        
007900     SKIP3                                                                
008000 01  MESSAGE-CODES.                                                       
008300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008400     03  ERR-NOT-ON-REGISTER     PIC X(3)    VALUE '010'.                 
008410     03  INF-FLER-RADER-FINNS    PIC X(3)    VALUE '402'.                 
008420     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '106'.                 
008500     EJECT                                                                
008600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008900     SKIP3                                                                
009000*01  MID -COPY W6I10701                                                   
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009300     SKIP3                                                                
009400*01  -COPY WMSGAREA                                                       
009500     EJECT                                                                
009600     03  MOD REDEFINES MSG-AREA.                                          
009700*      05  -COPY W6O10701                                                 
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010000     SKIP3                                                                
010100*01  -COPY WMFSAREA                                                       
010200     EJECT                                                                
010300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010400*                                                                         
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700     SKIP3                                                                
010800 01  NYCKLAR-TILL-DLI.                                                    
010901     03  W-W6GXKEY-6005-X.                                                
010902         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
010903         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
010904         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
010905     03  W-W6GXKEY-6006-X.                                                
010910         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
010920         05  FILLER              PIC X       VALUE LOW-VALUE.             
010950     03  W-KDINLOMR-X.                                                    
010960         05  W-KDINLOMR          PIC X(3)     VALUE SPACE.                
010970     03  W-ADINLOMR-PARX.                                                 
010980         05  W-ADINLOMR-PAR      PIC X(4)     VALUE SPACE.                
010990     03  W-KDINLUPF-X.                                                    
010991         05  W-KDINLUPF          PIC X(4)     VALUE SPACE.                
010992                                                                          
010993     03  W-IDDC-B6-X.                                                     
010994         05 W-IDDC-B6            PIC X(2).                                
010995                                                                          
011000     SKIP2                                                                
011100*    --- STATUS-KOD FRÅN IMS                                              
011200 01  STATUS-WS                   PIC XX.                                  
011300     88  SEGMENT-FINNS                       VALUE '  '.                  
011400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011510     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011600     SKIP2                                                                
011700 01  GODK-STATUSKODER.                                                    
011800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011900     SKIP3                                                                
012000 01  SSA1                        PIC X(64).                               
012100 01  SSA2                        PIC X(64).                               
012200     EJECT                                                                
012300*    --- IMS FUNKTIONSKODER                                               
012400*01  -COPY W0003                                                          
012600     EJECT                                                                
012700*    ---  DLI INPUT-OUTPUT AREA                                           
012800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012900     SKIP3                                                                
013000 01  DLI-IO-AREA.                                                         
013100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013201     SKIP3                                                                
013202     03  W6PLAA01 REDEFINES IO-AREA.                                      
013203*        05  -COPY W6GX01                                                 
013204     SKIP3                                                                
013205     03  W6PLAA11 REDEFINES IO-AREA.                                      
013210*        05  -COPY W6GX6006                                               
013220                                                                          
013230 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013240 01   DLI-IO-AREA-B601.                                                   
013250*     03  -COPY WDB601                                                    
013260                                                                          
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700                                                                          
013800*01  -COPY W0009   -PRE MSG-                                              
013901     EJECT                                                                
013902*01  -COPY W0008  -PRE USEA-                                              
013910     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014100*01  -COPY W0008  -PRE PLAA-                                              
014101     05  FILLER                  PIC X.                                   
014102     EJECT                                                                
014103*01  -COPY W0008  -PRE WDB6-                                              
014104     05  FILLER                  PIC X.                                   
014105     EJECT                                                                
014106 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB PLAA-PCB WDB6-PCB.            
014110     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB PLAA-PCB WDB6-PCB.            
014200                                                                          
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FINNS                                                     
014600       PERFORM A-INIT                                                     
014610       IF EGEN-MID OR HELP-MID                                            
014700         PERFORM B-KOLLA-NYCKLAR                                          
014800          IF NYCKLAR-OK = JA                                              
015200            PERFORM C-NYCKLAR-OK                                          
015500          END-IF                                                          
015510       ELSE                                                               
015520          MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-IN                         
015530                                  MOD-ADINLOMR-UT                         
015540                                  MOD-KDINLOMR-IN                         
015550                                  MOD-KDINLOMR-UT                         
015560                                  MOD-ADINLOMR-PAR-IN                     
015570                                  MOD-ADINLOMR-PAR-UT                     
015580                                  MOD-KDINLUPF-IN                         
015590                                  MOD-KDINLUPF-UT                         
015591                                  MOD-IDDC-IN                             
015592                                  MOD-IDDC-UT                             
015593       END-IF                                                             
015600       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016510     MOVE JA  TO NYCKLAR-OK                                               
016520     MOVE JA  TO INDATA-OK                                                
016530     MOVE NEJ TO NYA-NYCKLAR                                              
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I10701                 
016900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I10701                  
017300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W6O107N1' TO MFS-IDMOD                                         
018300     MOVE '6107' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF NOT EGEN-MID AND NOT HELP-MID                                     
018700       MOVE SPACE TO MFS-KDTRTYP                                          
018800       MOVE '7' TO MFS-IDPFK                                              
018900     END-IF                                                               
019701     PERFORM AA-INIT-NYCKLAR                                              
019702                                                                          
019703     IF MSGI-IDLAND-SPR = 'GB'                                            
019704       MOVE +2 TO SPRAK-IX                                                
019705       MOVE 'GB ' TO MED-IDSKYLT                                          
019706     ELSE                                                                 
019707       MOVE +1 TO SPRAK-IX                                                
019708       MOVE 'S  ' TO MED-IDSKYLT                                          
019709     END-IF                                                               
019710     .                                                                    
019711     EJECT                                                                
019712*----------------------------------------------------------------*        
019713 AA-INIT-NYCKLAR SECTION.                                                 
019714                                                                          
019715     MOVE ALL '+' TO MSGI-WMSGINIT                                        
019716     MOVE '001'                  TO MSGI-KDCALL                           
019717     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
019720     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
019730     MOVE '6107'                 TO MSGI-IDTRANS                          
019800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-NYCKLAR SECTION.                                                 
020300                                                                          
020400     IF MID-ADINLOMR-IN = ALL '+'                                         
020500        MOVE MID-ADINLOMR-UT TO WS-ADINLOMR                               
020610     ELSE                                                                 
020620        MOVE MID-ADINLOMR-IN TO WS-ADINLOMR                               
020630        MOVE JA TO NYA-NYCKLAR                                            
020640     END-IF                                                               
020650                                                                          
020700                                                                          
020710     IF MID-KDINLOMR-IN = ALL '+'                                         
020720        MOVE MID-KDINLOMR-UT TO WS-KDINLOMR                               
020740     ELSE                                                                 
020750        MOVE MID-KDINLOMR-IN TO WS-KDINLOMR                               
020751        MOVE JA TO NYA-NYCKLAR                                            
020752     END-IF                                                               
020753     IF WS-KDINLOMR = 'SQ '                                               
020754       MOVE 'RTA' TO WS-KDINLOMR                                          
020755     END-IF                                                               
020756     IF WS-KDINLOMR = 'UNL'                                               
020757       MOVE 'LPL' TO WS-KDINLOMR                                          
020758     END-IF                                                               
020759     IF WS-KDINLOMR = 'PG '                                               
020760       MOVE 'FB ' TO WS-KDINLOMR                                          
020761     END-IF                                                               
020762     IF WS-KDINLOMR = 'PGP'                                               
020763       MOVE 'FBP' TO WS-KDINLOMR                                          
020764     END-IF                                                               
020765     IF WS-KDINLOMR = 'P  '                                               
020766       MOVE 'F  ' TO WS-KDINLOMR                                          
020767     END-IF                                                               
020780                                                                          
020790     IF MID-ADINLOMR-PAR-IN = ALL '+'                                     
020791        MOVE MID-ADINLOMR-PAR-UT TO WS-ADINLOMR-PAR                       
020793     ELSE                                                                 
020794        MOVE MID-ADINLOMR-PAR-IN TO WS-ADINLOMR-PAR                       
020795        MOVE JA TO NYA-NYCKLAR                                            
020796     END-IF                                                               
020797                                                                          
020798     IF MID-KDINLUPF-IN = ALL '+'                                         
020799        MOVE MID-KDINLUPF-UT TO WS-KDINLUPF                               
020801     ELSE                                                                 
020803        MOVE MID-KDINLUPF-IN TO WS-KDINLUPF                               
020804        MOVE JA TO NYA-NYCKLAR                                            
020820     END-IF                                                               
020821                                                                          
020822     IF MID-ADINLOMR-IN NOT = ALL '+'                                     
020823        CONTINUE                                                          
020824     ELSE                                                                 
020825       IF WS-KDINLOMR = 'LO' OR 'F' OR 'LPL' OR 'FB' OR                   
020826                        'SYS' OR 'BO' OR 'FBP' OR 'TRG' OR                
020827                        'RTA' OR SPACE OR ALL '+' OR ZERO                 
020828         MOVE JA TO NYCKLAR-OK                                            
020829       ELSE                                                               
020830         MOVE NEJ TO NYCKLAR-OK                                           
020831       END-IF                                                             
020832     END-IF                                                               
020833                                                                          
020834                                                                          
020835     IF WS-ADINLOMR NOT =  ALL '+'                                        
020836       MOVE WS-ADINLOMR TO MOD-ADINLOMR-UT                                
020837     ELSE                                                                 
020838       MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-UT                            
020839     END-IF                                                               
020840                                                                          
020841     IF WS-KDINLOMR NOT =  ALL '+'                                        
020842       MOVE WS-KDINLOMR TO MOD-KDINLOMR-UT                                
020843       IF ENGLISH-TEXT                                                    
020844         IF MOD-KDINLOMR-UT = 'RTA'                                       
020845           MOVE 'SQ ' TO MOD-KDINLOMR-UT                                  
020846         END-IF                                                           
020847         IF MOD-KDINLOMR-UT = 'LPL'                                       
020848           MOVE 'UNL' TO MOD-KDINLOMR-UT                                  
020849         END-IF                                                           
020850         IF MOD-KDINLOMR-UT = 'FBP'                                       
020851           MOVE 'PGP' TO MOD-KDINLOMR-UT                                  
020852         END-IF                                                           
020853         IF MOD-KDINLOMR-UT = 'FB '                                       
020854           MOVE 'PG ' TO MOD-KDINLOMR-UT                                  
020855         END-IF                                                           
020856         IF MOD-KDINLOMR-UT = 'F  '                                       
020857           MOVE 'P  ' TO MOD-KDINLOMR-UT                                  
020858         END-IF                                                           
020859       END-IF                                                             
020860     ELSE                                                                 
020861       MOVE MFS-RENSA-FAELT TO MOD-KDINLOMR-UT                            
020862     END-IF                                                               
020863                                                                          
020864     IF WS-ADINLOMR-PAR NOT =  ALL '+'                                    
020865     MOVE WS-ADINLOMR-PAR TO MOD-ADINLOMR-PAR-UT                          
020866     ELSE                                                                 
020867       MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-PAR-UT                        
020868     END-IF                                                               
020869                                                                          
020870     IF WS-KDINLUPF NOT =  ALL '+'                                        
020871       MOVE WS-KDINLUPF TO MOD-KDINLUPF-UT                                
020874     ELSE                                                                 
020875       MOVE MFS-RENSA-FAELT TO MOD-KDINLUPF-UT                            
020876     END-IF                                                               
020877                                                                          
020878*    -- KONTROLL AV IDDC                                                  
020879                                                                          
020880     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
020881     IF MID-IDDC-IN = ALL '+'                                             
020882       MOVE MSGI-IDDC   TO W-IDDC-B6                                      
020883     ELSE                                                                 
020884       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
020885       MOVE '7'         TO MFS-IDPFK                                      
020886       MOVE SPACE       TO MFS-KDTRTYP                                    
020887       MOVE JA          TO NYA-NYCKLAR                                    
020888     END-IF                                                               
020889     PERFORM IMS-GU-WDB601                                                
020890                                                                          
020891     IF DCS-KDDC = SPACE OR DCS-DDC                                       
020892         MOVE NEJ       TO NYCKLAR-OK                                     
020893     ELSE                                                                 
020894         MOVE DCS-IDDC  TO W-6005-IDDC                                    
020895                           MOD-IDDC-UT                                    
020896     END-IF                                                               
020897                                                                          
020898     PERFORM BA-RENSA-FAELT                                               
020899                                                                          
020900     IF NYCKLAR-OK = NEJ                                                  
020910       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021000       CALL WMEDKONV USING MED-WMEDAREA                                   
021100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021400     END-IF                                                               
021500     .                                                                    
021700     EJECT                                                                
021900 BA-RENSA-FAELT SECTION.                                                  
021906                                                                          
021919     IF WS-ADINLOMR =     ALL '+' AND                                     
021920        WS-KDINLOMR =     ALL '+' AND                                     
021921        WS-ADINLOMR-PAR = ALL '+' AND                                     
021922        WS-KDINLUPF =     ALL '+' OR                                      
021923        WS-ADINLOMR =     SPACE   AND                                     
021924        WS-KDINLOMR =     SPACE   AND                                     
021925        WS-ADINLOMR-PAR = SPACE   AND                                     
021926        WS-KDINLUPF =     SPACE                                           
021927                MOVE 6 TO WS-NYCKLAR                                      
021928         MOVE SPACE TO MOD-KDINLOMR-UT                                    
021929                       MOD-ADINLOMR-PAR-UT                                
021930                       MOD-KDINLUPF-UT                                    
021931                       MOD-ADINLOMR-UT                                    
021932     ELSE                                                                 
021933     IF MID-ADINLOMR-IN NOT = ALL '+'  AND                                
021934        MID-ADINLOMR-IN NOT = SPACE                                       
021935         MOVE SPACE TO MOD-KDINLOMR-UT                                    
021936                       MOD-ADINLOMR-PAR-UT                                
021937                       MOD-KDINLUPF-UT                                    
021938         MOVE 1 TO WS-NYCKLAR                                             
021939     ELSE                                                                 
021940      IF MID-KDINLOMR-IN NOT = ALL '+' AND                                
021941         MID-KDINLOMR-IN NOT = SPACE                                      
021942          IF MID-ADINLOMR-PAR-IN NOT = ALL '+' AND                        
021943             MID-ADINLOMR-PAR-IN NOT = SPACE                              
021944             MOVE SPACE TO MOD-ADINLOMR-UT                                
021945                           MOD-KDINLUPF-UT                                
021946             MOVE 4 TO WS-NYCKLAR                                         
021947          ELSE                                                            
021948           IF MID-ADINLOMR-PAR-IN = SPACE                                 
021949             MOVE 2 TO WS-NYCKLAR                                         
021950             MOVE SPACE TO MOD-ADINLOMR-UT                                
021951                           MOD-ADINLOMR-PAR-UT                            
021952                           MOD-KDINLUPF-UT                                
021953           ELSE                                                           
021954            IF MID-ADINLOMR-PAR-IN = ALL '+' AND                          
021955               MID-ADINLOMR-PAR-UT NOT = ALL '+' AND                      
021956               MID-ADINLOMR-PAR-UT NOT = SPACE                            
021957               MOVE SPACE TO MOD-ADINLOMR-UT                              
021958                             MOD-KDINLUPF-UT                              
021959               MOVE 4 TO WS-NYCKLAR                                       
021960            ELSE                                                          
021961               MOVE SPACE TO MOD-ADINLOMR-UT                              
021962                             MOD-ADINLOMR-PAR-UT                          
021963                             MOD-KDINLUPF-UT                              
021964               MOVE 2 TO WS-NYCKLAR                                       
021966            END-IF                                                        
021967           END-IF                                                         
021968          END-IF                                                          
021969      ELSE                                                                
021970       IF MID-ADINLOMR-PAR-IN NOT = ALL '+' AND                           
021971          MID-ADINLOMR-PAR-IN NOT = SPACE                                 
021972            IF MID-KDINLOMR-IN NOT = ALL '+' AND                          
021973               MID-KDINLOMR-IN NOT = SPACE                                
021974              MOVE SPACE TO MOD-ADINLOMR-UT                               
021975                            MOD-KDINLUPF-UT                               
021976              MOVE 4 TO WS-NYCKLAR                                        
021978            ELSE                                                          
021979           IF MID-KDINLOMR-IN = SPACE                                     
021980             MOVE 3 TO WS-NYCKLAR                                         
021982             MOVE SPACE TO MOD-ADINLOMR-UT                                
021983                           MOD-KDINLOMR-UT                                
021984                           MOD-KDINLUPF-UT                                
021985           ELSE                                                           
021986             IF MID-KDINLOMR-IN = ALL '+' AND                             
021987                MID-KDINLOMR-UT NOT = ALL '+' AND                         
021988                MID-KDINLOMR-UT NOT = SPACE                               
021989                MOVE SPACE TO MOD-ADINLOMR-UT                             
021990                              MOD-KDINLUPF-UT                             
021991                MOVE 4 TO WS-NYCKLAR                                      
021993             ELSE                                                         
021994                MOVE SPACE TO MOD-ADINLOMR-UT                             
021995                              MOD-KDINLOMR-UT                             
021996                              MOD-KDINLUPF-UT                             
021997                MOVE 3 TO WS-NYCKLAR                                      
021999             END-IF                                                       
022000            END-IF                                                        
022001           END-IF                                                         
022002       ELSE                                                               
022003        IF  MID-KDINLUPF-IN NOT = ALL '+' AND                             
022004            MID-KDINLUPF-IN NOT = SPACE                                   
022005             MOVE SPACE TO MOD-ADINLOMR-UT                                
022006                           MOD-ADINLOMR-PAR-UT                            
022007                           MOD-KDINLOMR-UT                                
022008             MOVE 5 TO WS-NYCKLAR                                         
022010        ELSE                                                              
022011            EVALUATE TRUE                                                 
022012              WHEN MID-ADINLOMR-UT NOT = SPACE                            
022013                MOVE 1 TO WS-NYCKLAR                                      
022015              WHEN MID-KDINLOMR-UT NOT = SPACE AND                        
022016                   MID-ADINLOMR-PAR-UT NOT = SPACE                        
022017                   IF MID-KDINLOMR-IN = ALL '+' AND                       
022018                      MID-ADINLOMR-PAR-IN = ALL '+'                       
022019                        MOVE 4 TO WS-NYCKLAR                              
022021                   ELSE                                                   
022022                     IF MID-KDINLOMR-IN = SPACE                           
022023                        MOVE 3 TO WS-NYCKLAR                              
022025                      ELSE                                                
022026                        IF MID-ADINLOMR-PAR-IN = SPACE                    
022027                           MOVE 2 TO WS-NYCKLAR                           
022029                        END-IF                                            
022030                      END-IF                                              
022031                   END-IF                                                 
022032              WHEN MID-KDINLOMR-UT NOT = SPACE                            
022033                MOVE 2 TO WS-NYCKLAR                                      
022035              WHEN MID-ADINLOMR-PAR-UT NOT = SPACE                        
022036                MOVE 3 TO WS-NYCKLAR                                      
022038              WHEN MID-KDINLUPF-UT NOT = SPACE                            
022039                MOVE 5 TO WS-NYCKLAR                                      
022041              WHEN WS-ADINLOMR = ALL '+' AND                              
022042                   WS-KDINLOMR = ALL '+' AND                              
022043                   WS-ADINLOMR-PAR = ALL '+' AND                          
022044                   WS-KDINLUPF = ALL '+'                                  
022045                MOVE 6 TO WS-NYCKLAR                                      
022047              WHEN OTHER                                                  
022048                MOVE NEJ TO NYCKLAR-OK                                    
022049            END-EVALUATE                                                  
022050        END-IF                                                            
022051       END-IF                                                             
022052      END-IF                                                              
022053     END-IF                                                               
022054     END-IF                                                               
022055     EJECT                                                                
022056     .                                                                    
022057 C-NYCKLAR-OK SECTION.                                                    
022060     PERFORM CA-TRANSAR                                                   
022061     IF INDATA-OK = JA                                                    
022064         IF TOM-NYCKEL AND MFS-IDPFK NOT = '8'                            
022065           PERFORM CC-BEHANDLA                                            
022066         ELSE                                                             
022067           PERFORM CB-BEHANDLA                                            
022068         END-IF                                                           
022070     END-IF                                                               
022071                                                                          
022072     EJECT                                                                
022073     .                                                                    
022074 CA-TRANSAR SECTION.                                                      
022075     EVALUATE TRUE                                                        
022076                                                                          
022080                                                                          
022081       WHEN NYA-NYCKLAR = JA                                              
022083         MOVE WS-ADINLOMR     TO W-6006-ADINLOMR                          
022084         MOVE WS-KDINLOMR     TO W-KDINLOMR                               
022085         MOVE WS-ADINLOMR-PAR TO W-ADINLOMR-PAR                           
022086         MOVE WS-KDINLUPF     TO W-KDINLUPF                               
022087         PERFORM CAA-LAES                                                 
022088                                                                          
022089       WHEN MFS-IDPFK = '7' AND NYA-NYCKLAR = NEJ                         
022091         IF TOM-NYCKEL                                                    
022092           CONTINUE                                                       
022093         ELSE                                                             
022094           MOVE WS-ADINLOMR     TO W-6006-ADINLOMR                        
022095           MOVE WS-KDINLOMR     TO W-KDINLOMR                             
022096           MOVE WS-ADINLOMR-PAR TO W-ADINLOMR-PAR                         
022097           MOVE WS-KDINLUPF     TO W-KDINLUPF                             
022098         END-IF                                                           
022099         PERFORM CAA-LAES                                                 
022100                                                                          
022101       WHEN MFS-IDPFK = '8' AND NYA-NYCKLAR = NEJ                         
022103         MOVE MID-ADINLOMR-SPAR      TO W-6006-ADINLOMR                   
022104           PERFORM CAB-LAES-PF8                                           
022105                                                                          
022106       WHEN MFS-IDPFK = SPACE AND NYA-NYCKLAR = NEJ                       
022112           MOVE MID-ADINLOMR(1)   TO W-6006-ADINLOMR                      
022117           MOVE 1 TO WS-NYCKLAR                                           
022118           PERFORM CAA-LAES                                               
022126       END-EVALUATE                                                       
022127     .                                                                    
022128 CAA-LAES SECTION.                                                        
022129      PERFORM IMS-GU-PLAA01                                               
022130      EVALUATE TRUE                                                       
022131                                                                          
022132                                                                          
022133      WHEN TOM-NYCKEL                                                     
022136       IF SEGMENT-SAKNAS                                                  
022137          MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                        
022138          CALL WMEDKONV USING MED-WMEDAREA                                
022139          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
022140          MOVE NEJ TO INDATA-OK                                           
022141       END-IF                                                             
022143                                                                          
022144      WHEN PLAC-AR-NYCKEL                                                 
022145      PERFORM IMS-GU-PLAA11                                               
022146      IF SEGMENT-SAKNAS                                                   
022147         MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                         
022148         CALL WMEDKONV USING MED-WMEDAREA                                 
022149         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022150         MOVE NEJ TO INDATA-OK                                            
022151      END-IF                                                              
022152                                                                          
022153      WHEN TYP-AR-NYCKEL                                                  
022154      PERFORM IMS-GU-KDINLOMR                                             
022155      IF SEGMENT-SAKNAS                                                   
022156         MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                         
022157         CALL WMEDKONV USING MED-WMEDAREA                                 
022158         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022159         MOVE NEJ TO INDATA-OK                                            
022160      END-IF                                                              
022161                                                                          
022162                                                                          
022163      WHEN GRP-AR-NYCKEL                                                  
022164      PERFORM IMS-GU-ADINLOMR-PAR                                         
022165      IF SEGMENT-SAKNAS                                                   
022166         MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                         
022167         CALL WMEDKONV USING MED-WMEDAREA                                 
022168         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022169         MOVE NEJ TO INDATA-OK                                            
022170      END-IF                                                              
022171                                                                          
022172      WHEN STATUS-AR-NYCKEL                                               
022173      PERFORM IMS-GU-KDINLUPF                                             
022174      IF SEGMENT-SAKNAS                                                   
022175         MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                         
022176         CALL WMEDKONV USING MED-WMEDAREA                                 
022177         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022178         MOVE NEJ TO INDATA-OK                                            
022179      END-IF                                                              
022180                                                                          
022181      WHEN TYPGRP-AR-NYCKEL                                               
022182      PERFORM IMS-GU-TYPGRP                                               
022183      IF SEGMENT-SAKNAS                                                   
022184         MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                         
022185         CALL WMEDKONV USING MED-WMEDAREA                                 
022186         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022187         MOVE NEJ TO INDATA-OK                                            
022188      END-IF                                                              
022189      END-EVALUATE                                                        
022190      EJECT                                                               
022191      .                                                                   
022192 CAB-LAES-PF8 SECTION.                                                    
022193     PERFORM IMS-GU-PLAA01                                                
022194     PERFORM IMS-GU-PLAA11                                                
022195     MOVE 6006-ADINLOMR TO W-6006-ADINLOMR                                
022196     MOVE 6006-KDINLOMR TO W-KDINLOMR                                     
022197     MOVE 6006-ADINLOMR-PAR TO W-ADINLOMR-PAR                             
022198     MOVE 6006-KDINLUPF TO W-KDINLUPF                                     
022199     .                                                                    
022200     EJECT                                                                
022201 CB-BEHANDLA SECTION.                                                     
022202     MOVE 1 TO IX                                                         
022203     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
022204                   SEGMENT-SLUT   OR                                      
022205                   IX > 14                                                
022206     MOVE 6006-ADINLOMR     TO MOD-ADINLOMR(IX)                           
022207     MOVE 6006-KDINLOMR     TO MOD-KDINLOMR(IX)                           
022208     IF ENGLISH-TEXT                                                      
022209       IF MOD-KDINLOMR(IX) = 'RTA'                                        
022210         MOVE 'SQ ' TO MOD-KDINLUPF(IX)                                   
022211       END-IF                                                             
022212       IF MOD-KDINLOMR(IX) = 'LPL'                                        
022213         MOVE 'UNL' TO MOD-KDINLUPF(IX)                                   
022214       END-IF                                                             
022215       IF MOD-KDINLOMR(IX) = 'FBP'                                        
022216         MOVE 'PGP' TO MOD-KDINLUPF(IX)                                   
022217       END-IF                                                             
022218       IF MOD-KDINLOMR(IX) = 'FB '                                        
022219         MOVE 'PG ' TO MOD-KDINLUPF(IX)                                   
022220       END-IF                                                             
022221       IF MOD-KDINLOMR(IX) = 'F  '                                        
022222         MOVE 'P  ' TO MOD-KDINLUPF(IX)                                   
022223       END-IF                                                             
022224     END-IF                                                               
022225     MOVE 6006-ADINLOMR-PAR TO MOD-ADINLOMR-PAR(IX)                       
022226     MOVE 6006-KDINLUPF     TO MOD-KDINLUPF(IX)                           
022227     MOVE 6006-ADINLOMR-BO  TO MOD-ADINLOMR-BO(IX)                        
022228     MOVE 6006-ADINLOMR-LPL TO MOD-ADINLOMR-LPL(IX)                       
022229     MOVE 6006-ADPLATS-FOM  TO MOD-ADPLATS-FOM(IX)                        
022230     MOVE 6006-ADPLATS-TOM  TO MOD-ADPLATS-TOM(IX)                        
022231     MOVE 6006-ADGANG-FOM   TO MOD-ADGANG-FOM(IX)                         
022232     MOVE 6006-ADGANG-TOM   TO MOD-ADGANG-TOM(IX)                         
022233                                                                          
022234                                                                          
022235     EVALUATE TRUE                                                        
022236       WHEN TOM-NYCKEL                                                    
022237         PERFORM IMS-GN-PLAA11-OKVAL                                      
022238       WHEN PLAC-AR-NYCKEL                                                
022239         PERFORM IMS-GN-PLAA11                                            
022240       WHEN TYP-AR-NYCKEL                                                 
022241         PERFORM IMS-GN-KDINLOMR                                          
022242       WHEN GRP-AR-NYCKEL                                                 
022243         PERFORM IMS-GN-ADINLOMR-PAR                                      
022244       WHEN STATUS-AR-NYCKEL                                              
022245         PERFORM IMS-GN-KDINLUPF                                          
022246       WHEN TYPGRP-AR-NYCKEL                                              
022247         PERFORM IMS-GN-TYPGRP                                            
022248     END-EVALUATE                                                         
022249                                                                          
022250     ADD 1 TO IX                                                          
022251     END-PERFORM                                                          
022252                                                                          
022253                                                                          
022254     IF SEGMENT-FINNS                                                     
022255       MOVE 6006-ADINLOMR      TO MOD-ADINLOMR-SPAR                       
022256       MOVE 6006-KDINLOMR      TO MOD-KDINLOMR-SPAR                       
022257       MOVE 6006-ADINLOMR-PAR  TO MOD-ADINLOMR-PAR-SPAR                   
022258       MOVE 6006-KDINLUPF      TO MOD-KDINLUPF-SPAR                       
022259       MOVE INF-FLER-RADER-FINNS TO MED-IDMFSINF                          
022260       CALL WMEDKONV USING MED-WMEDAREA                                   
022261       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
022262     ELSE                                                                 
022263       MOVE MOD-ADINLOMR(1) TO MOD-ADINLOMR-SPAR                          
022264       PERFORM UNTIL IX > 14                                              
022265       MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR(IX)                           
022266                               MOD-KDINLOMR(IX)                           
022267                               MOD-ADINLOMR-PAR(IX)                       
022268                               MOD-KDINLUPF(IX)                           
022269                               MOD-ADINLOMR-BO(IX)                        
022270                               MOD-ADINLOMR-LPL(IX)                       
022271                               MOD-ADPLATS-FOM(IX)                        
022272                               MOD-ADPLATS-TOM(IX)                        
022273                               MOD-ADGANG-FOM(IX)                         
022274                               MOD-ADGANG-TOM(IX)                         
022275       ADD 1 TO IX                                                        
022276       END-PERFORM                                                        
022277       IF MFS-IDPFK = '8' AND NYA-NYCKLAR = NEJ                           
022278         MOVE INF-SISTA-SIDAN TO MED-IDMFSINF                             
022279         CALL WMEDKONV USING MED-WMEDAREA                                 
022280         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
022281       END-IF                                                             
022282     END-IF                                                               
022283     EJECT                                                                
022284     .                                                                    
022290 CC-BEHANDLA SECTION.                                                     
022400     MOVE 1 TO IX                                                         
022410     PERFORM IMS-GNP-PLAA11                                               
022500     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
022600                   SEGMENT-SLUT   OR                                      
022700                   IX > 14                                                
022800     MOVE 6006-ADINLOMR     TO MOD-ADINLOMR(IX)                           
022900     MOVE 6006-KDINLOMR     TO MOD-KDINLOMR(IX)                           
022910     IF ENGLISH-TEXT                                                      
022920       IF MOD-KDINLOMR(IX) = 'RTA'                                        
022930         MOVE 'SQ ' TO MOD-KDINLOMR(IX)                                   
022940       END-IF                                                             
022950       IF MOD-KDINLOMR(IX) = 'LPL'                                        
022960         MOVE 'UNL' TO MOD-KDINLOMR(IX)                                   
022970       END-IF                                                             
022980       IF MOD-KDINLOMR(IX) = 'FBP'                                        
022990         MOVE 'PGP' TO MOD-KDINLOMR(IX)                                   
022991       END-IF                                                             
022992       IF MOD-KDINLOMR(IX) = 'FB '                                        
022993         MOVE 'PG ' TO MOD-KDINLOMR(IX)                                   
022994       END-IF                                                             
022995       IF MOD-KDINLOMR(IX) = 'F  '                                        
022996         MOVE 'P  ' TO MOD-KDINLOMR(IX)                                   
022997       END-IF                                                             
022998     END-IF                                                               
023000     MOVE 6006-ADINLOMR-PAR TO MOD-ADINLOMR-PAR(IX)                       
023100     MOVE 6006-KDINLUPF     TO MOD-KDINLUPF(IX)                           
023200     MOVE 6006-ADINLOMR-BO  TO MOD-ADINLOMR-BO(IX)                        
023300     MOVE 6006-ADINLOMR-LPL TO MOD-ADINLOMR-LPL(IX)                       
023400     MOVE 6006-ADPLATS-FOM  TO MOD-ADPLATS-FOM(IX)                        
023500     MOVE 6006-ADPLATS-TOM  TO MOD-ADPLATS-TOM(IX)                        
023600     MOVE 6006-ADGANG-FOM   TO MOD-ADGANG-FOM(IX)                         
023700     MOVE 6006-ADGANG-TOM   TO MOD-ADGANG-TOM(IX)                         
023800                                                                          
023900     PERFORM IMS-GNP-PLAA11                                               
024328                                                                          
024329     ADD 1 TO IX                                                          
024330     END-PERFORM                                                          
024334                                                                          
024335     IF SEGMENT-FINNS                                                     
024336       MOVE 6006-ADINLOMR      TO MOD-ADINLOMR-SPAR                       
024337       MOVE 6006-KDINLOMR      TO MOD-KDINLOMR-SPAR                       
024338       MOVE 6006-ADINLOMR-PAR  TO MOD-ADINLOMR-PAR-SPAR                   
024339       MOVE 6006-KDINLUPF      TO MOD-KDINLUPF-SPAR                       
024340       MOVE INF-FLER-RADER-FINNS TO MED-IDMFSINF                          
024341       CALL WMEDKONV USING MED-WMEDAREA                                   
024342       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
024343     ELSE                                                                 
024344       MOVE MOD-ADINLOMR(1) TO MOD-ADINLOMR-SPAR                          
024345       PERFORM UNTIL IX > 14                                              
024346       MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR(IX)                           
024347                               MOD-KDINLOMR(IX)                           
024348                               MOD-ADINLOMR-PAR(IX)                       
024349                               MOD-KDINLUPF(IX)                           
024350                               MOD-ADINLOMR-BO(IX)                        
024351                               MOD-ADINLOMR-LPL(IX)                       
024352                               MOD-ADPLATS-FOM(IX)                        
024353                               MOD-ADPLATS-TOM(IX)                        
024354                               MOD-ADGANG-FOM(IX)                         
024355                               MOD-ADGANG-TOM(IX)                         
024356       ADD 1 TO IX                                                        
024357       END-PERFORM                                                        
024358       IF MFS-IDPFK = '8' AND NYA-NYCKLAR = NEJ                           
024359         MOVE INF-SISTA-SIDAN TO MED-IDMFSINF                             
024360         CALL WMEDKONV USING MED-WMEDAREA                                 
024361         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
024362       END-IF                                                             
024363     END-IF                                                               
024364     EJECT                                                                
024365     .                                                                    
029100* --- IMS SEKTIONER ---                                                   
029200     SKIP3                                                                
029300 IMS-GET-MSG SECTION.                                                     
029400                                                                          
029500     MOVE '  QC' TO GODK-STATUSKODER                                      
029600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
029700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029800     PERFORM IMS-STATUSKONTROLL                                           
029900     .                                                                    
030000     SKIP3                                                                
030100 IMS-INSERT-MSG SECTION.                                                  
030200                                                                          
030210     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
030220       MOVE '0' TO MFS-KDHUVOMR                                           
030500     END-IF                                                               
030600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
030700     MOVE SPACE TO GODK-STATUSKODER                                       
030800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
030900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031000     PERFORM IMS-STATUSKONTROLL                                           
031100     .                                                                    
031201     EJECT                                                                
031202 IMS-GU-PLAA01   SECTION.                                                 
031203     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
031204          DELIMITED BY SIZE INTO SSA1                                     
031205     MOVE '  GE' TO GODK-STATUSKODER                                      
031206     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1                      
031207     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031208     PERFORM IMS-STATUSKONTROLL                                           
031209     .                                                                    
031210     EJECT                                                                
031211 IMS-GU-ADINLOMR-PAR SECTION.                                             
031212     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
031213          DELIMITED BY SIZE INTO SSA1                                     
031214     STRING 'W6PLAA11(ADINLOMP =' W-ADINLOMR-PARX ')'                     
031215          DELIMITED BY SIZE INTO SSA2                                     
031216     MOVE '  GE' TO GODK-STATUSKODER                                      
031217     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
031218     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031219     PERFORM IMS-STATUSKONTROLL                                           
031220     .                                                                    
031221     EJECT                                                                
031222 IMS-GN-ADINLOMR-PAR SECTION.                                             
031223     STRING 'W6PLAA11(ADINLOMP =' W-ADINLOMR-PARX ')'                     
031224          DELIMITED BY SIZE INTO SSA1                                     
031225     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031226     CALL CBLTDLI USING GN PLAA-PCB DLI-IO-AREA SSA1                      
031227     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031228     PERFORM IMS-STATUSKONTROLL                                           
031229     .                                                                    
031230     EJECT                                                                
031231 IMS-GU-KDINLOMR SECTION.                                                 
031232     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
031233          DELIMITED BY SIZE INTO SSA1                                     
031234     STRING 'W6PLAA11(KDINLOMR =' W-KDINLOMR-X ')'                        
031235          DELIMITED BY SIZE INTO SSA2                                     
031236     MOVE '  GE' TO GODK-STATUSKODER                                      
031237     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
031238     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031239     PERFORM IMS-STATUSKONTROLL                                           
031240     .                                                                    
031241     EJECT                                                                
031242 IMS-GN-KDINLOMR SECTION.                                                 
031243     STRING 'W6PLAA11(KDINLOMR =' W-KDINLOMR-X ')'                        
031244          DELIMITED BY SIZE INTO SSA1                                     
031245     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031246     CALL CBLTDLI USING GN PLAA-PCB DLI-IO-AREA SSA1                      
031247     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031248     PERFORM IMS-STATUSKONTROLL                                           
031249     .                                                                    
031250     EJECT                                                                
031251 IMS-GU-KDINLUPF SECTION.                                                 
031252     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
031253          DELIMITED BY SIZE INTO SSA1                                     
031254     STRING 'W6PLAA11(KDINLUPF =' W-KDINLUPF-X ')'                        
031255          DELIMITED BY SIZE INTO SSA2                                     
031256     MOVE '  GE' TO GODK-STATUSKODER                                      
031257     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
031258     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031259     PERFORM IMS-STATUSKONTROLL                                           
031260     .                                                                    
031261     EJECT                                                                
031262 IMS-GN-KDINLUPF SECTION.                                                 
031263     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
031264          DELIMITED BY SIZE INTO SSA1                                     
031265     STRING 'W6PLAA11(KDINLUPF =' W-KDINLUPF-X ')'                        
031266          DELIMITED BY SIZE INTO SSA2                                     
031267     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031268     CALL CBLTDLI USING GN PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
031269     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031270     PERFORM IMS-STATUSKONTROLL                                           
031271     .                                                                    
031272     EJECT                                                                
031273 IMS-GU-TYPGRP SECTION.                                                   
031274     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
031275          DELIMITED BY SIZE INTO SSA1                                     
031276     STRING 'W6PLAA11(KDINLOMR =' W-KDINLOMR-X                            
031277                    '&ADINLOMP =' W-ADINLOMR-PARX ')'                     
031278          DELIMITED BY SIZE INTO SSA2                                     
031279     MOVE '  GE' TO GODK-STATUSKODER                                      
031280     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
031281     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031282     PERFORM IMS-STATUSKONTROLL                                           
031283     .                                                                    
031284     EJECT                                                                
031285 IMS-GN-TYPGRP SECTION.                                                   
031286     STRING 'W6PLAA11(KDINLOMR =' W-KDINLOMR-X                            
031287                    '&ADINLOMP =' W-ADINLOMR-PARX ')'                     
031288          DELIMITED BY SIZE INTO SSA1                                     
031289     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031290     CALL CBLTDLI USING GN PLAA-PCB DLI-IO-AREA SSA1                      
031291     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031292     PERFORM IMS-STATUSKONTROLL                                           
031293     .                                                                    
031294     EJECT                                                                
031295 IMS-GU-PLAA11   SECTION.                                                 
031296     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
031297          DELIMITED BY SIZE INTO SSA1                                     
031298     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
031299          DELIMITED BY SIZE INTO SSA2                                     
031300     MOVE '  GE' TO GODK-STATUSKODER                                      
031301     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
031302     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031303     PERFORM IMS-STATUSKONTROLL                                           
031304     .                                                                    
031305     EJECT                                                                
031306 IMS-GN-PLAA11-OKVAL  SECTION.                                            
031307     MOVE 'W6PLAA11 ' TO SSA1                                             
031308     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031309     CALL CBLTDLI USING GN PLAA-PCB DLI-IO-AREA SSA1                      
031310     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031311     PERFORM IMS-STATUSKONTROLL                                           
031312     .                                                                    
031313     EJECT                                                                
031314 IMS-GN-PLAA11   SECTION.                                                 
031320     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
031330          DELIMITED BY SIZE INTO SSA1                                     
031340     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031350     CALL CBLTDLI USING GN PLAA-PCB DLI-IO-AREA SSA1                      
031360     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031370     PERFORM IMS-STATUSKONTROLL                                           
031380     .                                                                    
031390     EJECT                                                                
031391 IMS-GNP-PLAA11   SECTION.                                                
031392     MOVE 'W6PLAA11 ' TO SSA1                                             
031394     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031395     CALL CBLTDLI USING GNP PLAA-PCB DLI-IO-AREA SSA1                     
031396     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031397     PERFORM IMS-STATUSKONTROLL                                           
031398     .                                                                    
031399     EJECT                                                                
031400 IMS-GU-WDB601    SECTION.                                                
031401     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
031402          DELIMITED BY SIZE INTO SSA1                                     
031403     MOVE '  GE' TO GODK-STATUSKODER                                      
031404     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
031405     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031406     PERFORM IMS-STATUSKONTROLL                                           
031407     IF SEGMENT-SAKNAS                                                    
031408         MOVE SPACE TO DCS-KDDC                                           
031409     END-IF                                                               
031410     .                                                                    
031420 IMS-STATUSKONTROLL SECTION.                                              
031500                                                                          
031600     SET STATUS-IX TO 1                                                   
031700     SEARCH GODK-STATUS                                                   
031800       AT END                                                             
031900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032000         DELIMITED BY SIZE INTO FELTEXT                                   
032100         CALL FELLOG                                                      
032200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032300         CONTINUE                                                         
032400     END-SEARCH                                                           
032500     .                                                                    
