001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W4065300.                                                
001500 AUTHOR.         BO HAMMARIN.                                             
001600 DATE-WRITTEN.   97/02/25.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*                                                                         
002010*        FRÅGE-MPP SOM VISAR                                              
002020*        - DIVERSE ANTAL PER TRANSPORTLÖSNING                             
002030*        - GRAND-TOTAL PÅ RAD 14 NÄR SISTA SIDAN VISAS                    
002041*        - ENDAST ENGELSKT FORMAT                                         
002042                                                                          
002043*        INMATNINGSREGLER                                                 
002044*        1 IDDC,     OM IFYLLT                                            
002045*                    -FRÅN SKÄRM                                          
002046*                    OM EJ IFYLLT                                         
002047*                    -FRÅN USERDB                                         
002048*                                                                         
002049*        2 TIRFSDAT, OM IFYLLT                                            
002050*                    -FRÅN SKÄRM                                          
002051*                    OM EJ IFYLLT                                         
002052*                    -FRÅN USERDB (LOKALT DAGENS-DATUM)                   
002053*                                                                         
002054*        3 IDTRPLOS-FR, OM IFYLLT                                         
002055*                       -FRÅN SKÄRM                                       
002056*                       OM EJ IFYLLT                                      
002057*                       -VÄRDE= '000'                                     
002058*                                                                         
002059*        4 IDTRPLOS-TO, OM IFYLLT                                         
002060*                       -FRÅN SKÄRM                                       
002061*                       OM EJ IFYLLT OCH IDTRPLOS-FR IFYLLT               
002062*                       -FRÅN IDPRC-FR                                    
002063*                       OM EJ IFYLLT OCH IDTRPLOS-FR EJ IFYLLT            
002064*                       -KODAT VÄRDE= '999'                               
002065*                                                                         
002066*        LÄSNING GÖRS AV                                                  
002070*        - WDQ3E/WDQ3E1                                                   
002071*        - WDE6/WDE601                                                    
002072*        - WDB6/WDB601                                                    
002080*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T653                                              
002600*        MID:         W4I65301                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O65301                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W4065300'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  MOD-INDX                    PIC S9(4)   VALUE +0   COMP SYNC.        
004403 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
004404 77  IX1                         PIC S9(4)   VALUE +0   COMP SYNC.        
004410 77  MAX-MOD-INDX                PIC S9(4)   VALUE +12  COMP SYNC.        
004411*77  MAX-MOD-INDX                PIC S9(4)   VALUE +5   COMP SYNC.        
004420                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005004                                                                          
005010 77  SPAR-IDPFK                  PIC X.                                   
005011     88  SPAR-ENTER                          VALUE ' '.                   
005012     88  SPAR-FIRST                          VALUE '7'.                   
005013     88  SPAR-NEXT                           VALUE '8'.                   
005014                                                                          
005020 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '4653'.                
005600     88  GODK-MID                            VALUE '4653' '4654'.         
006100     88  HELP-MID                            VALUE '0551'.                
006120 77  WS-DARFSDAT                 PIC 9(8).                                
006130 77  WS-JFR-IDTRPLOS             PIC X(3).                                
006131 77  WS-IDTRPLOS-FR              PIC X(3).                                
006140 77  WS-IDTRPLOS-TO              PIC X(3).                                
006200     SKIP2                                                                
006201 77  WS-REPROCENT                PIC 9(3)V9(2).                           
006210     EJECT                                                                
006307*    --- RED-AREA FÖR IDTRP                                               
006308 01  FILLER                      PIC X(16)   VALUE 'RED-IDTRP'.           
006309 01  WS-IDTRP.                                                            
006311     05  WS-IDTRP-1              PIC X(3).                                
006312     05  WS-IDTRP-2              PIC X(2).                                
006315*    --- AREA FÖR WORK-TABELL TILL MOD                                    
006316 01  FILLER                      PIC X(16)   VALUE 'TABELL  '.            
006317 01  WS-MOD-TABELL.                                                       
006318     03  MOD-TABELL OCCURS 12.                                            
006320         05  TAB-IDTRPLOS            PIC X(3).                            
006321         05  TAB-KVRADER             PIC 9(5)   COMP-3.                   
006322         05  TAB-KVRADER-UTSKR       PIC 9(5)   COMP-3.                   
006323         05  TAB-REPROCENT-UTSKR     PIC 9(3)   COMP-3.                   
006324         05  TAB-KVRADER-PACK        PIC 9(5)   COMP-3.                   
006325         05  TAB-REPROCENT-PACK      PIC 9(3)   COMP-3.                   
006326         05  TAB-KVKOLLI             PIC 9(5)   COMP-3.                   
006327         05  TAB-KVKOLLI-LAST        PIC 9(5)   COMP-3.                   
006328         05  TAB-REPROCENT-LAST      PIC 9(3)   COMP-3.                   
006329         05  TAB-KVKOLLI-FAKT        PIC 9(5)   COMP-3.                   
006330         05  TAB-REPROCENT-FAKT      PIC 9(3)   COMP-3.                   
006331     EJECT                                                                
006332 01  FILLER                      PIC X(16)   VALUE 'TOTAL   '.            
006333 01  WS-TOTALER.                                                          
006334     03  TOT-KVRADER             PIC 9(5) VALUE ZERO COMP-3.              
006335     03  TOT-KVRADER-UTSKR       PIC 9(5) VALUE ZERO COMP-3.              
006336     03  TOT-REPROCENT-UTSKR     PIC 9(3) VALUE ZERO COMP-3.              
006337     03  TOT-KVRADER-PACK        PIC 9(5) VALUE ZERO COMP-3.              
006338     03  TOT-REPROCENT-PACK      PIC 9(3) VALUE ZERO COMP-3.              
006339     03  TOT-KVKOLLI             PIC 9(5) VALUE ZERO COMP-3.              
006340     03  TOT-KVKOLLI-LAST        PIC 9(5) VALUE ZERO COMP-3.              
006341     03  TOT-REPROCENT-LAST      PIC 9(3) VALUE ZERO COMP-3.              
006342     03  TOT-KVKOLLI-FAKT        PIC 9(5) VALUE ZERO COMP-3.              
006343     03  TOT-REPROCENT-FAKT      PIC 9(3) VALUE ZERO COMP-3.              
006344     EJECT                                                                
006350*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006902     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007620     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '106'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '029'.                 
007803     EJECT                                                                
007804*01  -COPY WDATAREA                                                       
007809     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008503*                                                                         
008504 01  FILLER                     PIC X(16)   VALUE 'SPAR-AREA'.            
008505 01  SPAR-AREA.                                                           
008506     03  SPAR-IDTRANS           PIC X(4)    VALUE '4653'.                 
008507     03  SPAR-IDDC-ENTER        PIC X(2).                                 
008508     03  SPAR-IDDC-NEXT         PIC X(2).                                 
008509     03  SPAR-DARFSDAT-ENTER    PIC 9(8).                                 
008510     03  SPAR-DARFSDAT-NEXT     PIC 9(8).                                 
008511     03  SPAR-IDTRP-ENTER       PIC X(5).                                 
008512     03  SPAR-IDTRP-NEXT        PIC X(5).                                 
008513     03  SPAR-IDORDER-ENTER     PIC S9(7)        COMP-3.                  
008514     03  SPAR-IDORDER-NEXT      PIC S9(7)        COMP-3.                  
008515     03  SPAR-IDPRODNR-ENTER    PIC S9(7)        COMP-3.                  
008516     03  SPAR-IDPRODNR-NEXT     PIC S9(7)        COMP-3.                  
008517     03  SPAR-IDPLKLST-ENTER    PIC S9(3)        COMP-3.                  
008520     03  SPAR-IDPLKLST-NEXT     PIC S9(3)        COMP-3.                  
008530     03  SPAR-KVORDRAD          PIC S9(5)  COMP-3 VALUE ZERO.             
008540     03  SPAR-KVORDRAD-UTSKR    PIC S9(5)  COMP-3 VALUE ZERO.             
008550     03  SPAR-REPROCENT-UTSKR   PIC S9(3)  COMP-3 VALUE ZERO.             
008560     03  SPAR-KVORDRAD-PACK     PIC S9(5)  COMP-3 VALUE ZERO.             
008570     03  SPAR-REPROCENT-PACK    PIC S9(3)  COMP-3 VALUE ZERO.             
008580     03  SPAR-KVKOLLI           PIC S9(5)  COMP-3 VALUE ZERO.             
008590     03  SPAR-KVKOLLI-LAST      PIC S9(5)  COMP-3 VALUE ZERO.             
008600     03  SPAR-REPROCENT-LAST    PIC S9(3)  COMP-3 VALUE ZERO.             
008610     03  SPAR-KVKOLLI-FAKT      PIC S9(5)  COMP-3 VALUE ZERO.             
008620     03  SPAR-REPROCENT-FAKT    PIC S9(3)  COMP-3 VALUE ZERO.             
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W4I65301                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W4O65301                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011028     03  W-WDQ3E1KY-MIN-X.                                                
011029         05  W-IDDC-MIN          PIC X(2).                                
011030         05  W-DARFSDAT-MIN      PIC 9(8).                                
011031         05  W-IDTRP-MIN         PIC X(5).                                
011032         05  W-IDORDER-MIN       PIC S9(7)  COMP-3 VALUE ZERO.            
011033         05  W-IDPRODNR-MIN      PIC S9(7)  COMP-3 VALUE ZERO.            
011034         05  W-IDPLKLST-MIN      PIC S9(3)  COMP-3 VALUE ZERO.            
011035     03  W-WDQ3E1KY-MAX-X.                                                
011036         05  W-IDDC-MAX          PIC X(2).                                
011037         05  W-DARFSDAT-MAX      PIC 9(8).                                
011038         05  W-IDTRP-MAX         PIC X(5).                                
011040         05  W-IDORDER-MAX       PIC S9(7)  COMP-3 VALUE +9999999.        
011050         05  W-IDPRODNR-MAX      PIC S9(7)  COMP-3 VALUE +9999999.        
011060         05  W-IDPLKLST-MAX      PIC S9(3)  COMP-3 VALUE +999.            
011061                                                                          
011070     03  W-IDPRODNR-X.                                                    
011080         05  W-IDPRODNR          PIC S9(07) COMP-3.                       
011090                                                                          
011091     03  W-IDDC-B6-X.                                                     
011092         05 W-IDDC-B6            PIC X(2).                                
011093                                                                          
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011610     88  BASEN-SLUT                          VALUE 'GB'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(128).                              
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-Q3E1'.                        
013002 01  DLI-IO-Q3E1.                                                         
013010*    03  -COPY WDQ3E1                                                     
013300     EJECT                                                                
013310 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
013320 01  DLI-IO-E601.                                                         
013330*    03  -COPY WDE601                                                     
013331                                                                          
013332 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013333 01   DLI-IO-AREA-B601.                                                   
013334*     03  -COPY WDB601                                                    
013335                                                                          
013340     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE WDP7-                                             
013700     05  FILLER                  PIC X.                                   
013801     EJECT                                                                
013802*01  -COPY W0008  -PRE WDQ3E-                                             
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000*01  -COPY W0008  -PRE WDE6-                                              
014001     05  FILLER                  PIC X.                                   
014002     EJECT                                                                
014003*01  -COPY W0008  -PRE WDB6-                                              
014004     05  FILLER                  PIC X.                                   
014005     EJECT                                                                
014006 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDQ3E-PCB WDE6-PCB            
014007                           WDB6-PCB.                                      
014008 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDQ3E-PCB WDE6-PCB            
014020                           WDB6-PCB.                                      
014266                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014310                                                                          
014400     IF SEGMENT-FINNS                                                     
014500        PERFORM A-INIT                                                    
014610        PERFORM B-KOLLA-NYCKLAR                                           
014700        IF NYCKLAR-OK                                                     
014901           IF MFS-FIRST                                                   
014902              PERFORM C-FOERSTA-SIDA                                      
014903           ELSE                                                           
014905              IF MFS-NEXT                                                 
014906                 PERFORM D-NAESTA-SIDA                                    
014907              ELSE                                                        
014909                 PERFORM E-SAMMA-SIDA                                     
014910              END-IF                                                      
014911           END-IF                                                         
015200           PERFORM F-LAES-VISA-INFO                                       
015300        END-IF                                                            
015600        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O65301 + 4                     
015700        PERFORM IMS-INSERT-MSG                                            
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I65301                 
016900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I65301                 
017300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
017400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
017800     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
017810                                             SPAR-IDPFK                   
017900     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
018000                                                                          
018100     MOVE LOW-VALUE                       TO MSG-AREA                     
018200     MOVE 'W4O653N1'                      TO MFS-IDMOD                    
018300     MOVE '4653'                          TO MOD-IDTRANS                  
018400     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
018410                                             MOD-TEMFSINF                 
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE                         TO MFS-KDTRTYP                  
019000       MOVE '7'                           TO MFS-IDPFK                    
019100     END-IF                                                               
019200                                                                          
019300     MOVE 'GB'                            TO MED-IDSKYLT                  
019410     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019601                                                                          
019610     MOVE JA                      TO NYCKLAR-SW                           
019700                                                                          
019800     MOVE ALL '+'                 TO MSGI-WMSGINIT                        
019900     MOVE '001'                   TO MSGI-KDCALL                          
020000     MOVE MSG-LTERM-NAME          TO MSGI-IDLTERM-USER                    
020100     MOVE MSG-SIGNON-USERID       TO MSGI-IDUSER                          
020200     MOVE '4653'                  TO MSGI-IDTRANS                         
020210                                                                          
020700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
020710                                                                          
020800     IF GODK-MID                                                          
020801       MOVE MSGI-SPAR-AREA        TO SPAR-AREA                            
020802     ELSE                                                                 
020803       MOVE SPACE                 TO W-IDDC-MIN                           
020804                                     W-IDTRP-MIN                          
020808       MOVE ZERO                  TO W-DARFSDAT-MIN                       
020812     END-IF                                                               
020813                                                                          
021000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
021001                             MOD-TIRFSDAT-IN                              
021002                             MOD-IDTRPLOS-FR-IN                           
021003                             MOD-IDTRPLOS-TO-IN                           
021004                                                                          
021022     PERFORM BA-KOLLA-IDDC                                                
021023     PERFORM BB-KOLLA-TIRFSDAT                                            
021024     PERFORM BC-KOLLA-IDTRPLOS-FR                                         
021025     PERFORM BD-KOLLA-IDTRPLOS-TO                                         
021026     PERFORM BE-KOLLA-SAMBAND-IDTRPLOS                                    
021030                                                                          
021103     IF GODK-MID OR NYCKLAR-OK                                            
021105       MOVE DCS-IDDC        TO MOD-IDDC-UT                                
021106       MOVE WS-DARFSDAT (3:6) TO MOD-TIRFSDAT-UT                          
021107       MOVE WS-IDTRPLOS-FR  TO MOD-IDTRPLOS-FR-UT                         
021108       MOVE WS-IDTRPLOS-TO  TO MOD-IDTRPLOS-TO-UT                         
021115     ELSE                                                                 
021116       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
021117                               MOD-TIRFSDAT-UT                            
021118                               MOD-IDTRPLOS-FR-UT                         
021119                               MOD-IDTRPLOS-TO-UT                         
021120     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021410       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     END-IF                                                               
021901                                                                          
021903     IF NYCKLAR-OK                                                        
021904       MOVE DCS-IDDC             TO SPAR-IDDC-ENTER                       
021905       MOVE WS-DARFSDAT          TO SPAR-DARFSDAT-ENTER                   
021906       MOVE WS-IDTRPLOS-FR       TO WS-IDTRP-1                            
021907       MOVE '  '                 TO WS-IDTRP-2                            
021908       MOVE WS-IDTRP             TO SPAR-IDTRP-ENTER                      
021909       MOVE ZERO                 TO SPAR-IDORDER-ENTER                    
021910                                    SPAR-IDPRODNR-ENTER                   
021911                                    SPAR-IDORDER-ENTER                    
021912                                    SPAR-IDPLKLST-ENTER                   
021915     END-IF                                                               
022100     .                                                                    
022101     SKIP2                                                                
022102 BA-KOLLA-IDDC     SECTION.                                               
022103                                                                          
022120     IF MID-IDDC-IN NOT = ALL '+'                                         
022121       MOVE MID-IDDC-IN      TO MSGI-IDDC                                 
022122     ELSE                                                                 
022123       IF SPAR-FIRST OR SPAR-NEXT                                         
022125         MOVE MID-IDDC-UT   TO MSGI-IDDC                                  
022127       END-IF                                                             
022128     END-IF                                                               
022129                                                                          
022130     IF MID-IDDC-IN NOT = ALL '+' AND EGEN-MID                            
022131       MOVE    '7'           TO MFS-IDPFK                                 
022132       MOVE    SPACE         TO MFS-KDTRTYP                               
022133     END-IF                                                               
022134                                                                          
022136     MOVE MSGI-IDDC        TO W-IDDC-B6                                   
022137     PERFORM IMS-GU-WDB601                                                
022138     IF DCS-KDDC = SPACE OR DCS-CDC OR DCS-CDC-TR OR DCS-DDC              
022139       MOVE NEJ            TO NYCKLAR-SW                                  
022140       MOVE SPACE          TO DCS-IDDC                                    
022141     ELSE                                                                 
022142        MOVE MSGI-IDDC     TO W-IDDC-MIN                                  
022143                              W-IDDC-MAX                                  
022144                              MID-IDDC-IN                                 
022147     END-IF                                                               
022153     .                                                                    
022154     EJECT                                                                
022155 BB-KOLLA-TIRFSDAT SECTION.                                               
022156                                                                          
022157     IF MID-TIRFSDAT-IN = ALL '+'                                         
022158       IF SPAR-FIRST OR SPAR-NEXT                                         
022159         MOVE MID-TIRFSDAT-UT  TO WS-DARFSDAT                             
022160       ELSE                                                               
022161         MOVE MSGI-TILOKDAT    TO WS-DARFSDAT                             
022162       END-IF                                                             
022163     ELSE                                                                 
022164       MOVE MID-TIRFSDAT-IN    TO WS-DARFSDAT                             
022165       MOVE '7'                TO MFS-IDPFK                               
022166       MOVE SPACE              TO MFS-KDTRTYP                             
022167     END-IF                                                               
022168                                                                          
022169     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
022170     MOVE WS-DARFSDAT (3:6)    TO DAT-I-TIDATUM                           
022171                                                                          
022172     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
022173                   DAT-O-TIDATUM DAT-KDSVAR                               
022174                                                                          
022175     IF DAT-KDSVAR-OK                                                     
022176       MOVE DAT-TISEKEL        TO WS-DARFSDAT (1:2)                       
022177       MOVE WS-DARFSDAT        TO W-DARFSDAT-MIN                          
022180                                  W-DARFSDAT-MAX                          
022181     ELSE                                                                 
022182       MOVE NEJ                TO NYCKLAR-SW                              
022185     END-IF                                                               
022186     .                                                                    
022187     EJECT                                                                
022188 BC-KOLLA-IDTRPLOS-FR SECTION.                                            
022189                                                                          
022190     IF MID-IDTRPLOS-FR-IN = ALL '+'                                      
022191       IF SPAR-FIRST OR SPAR-NEXT                                         
022192         MOVE MID-IDTRPLOS-FR-UT TO WS-IDTRPLOS-FR                        
022193       ELSE                                                               
022194         MOVE '000'              TO WS-IDTRPLOS-FR                        
022196       END-IF                                                             
022197     ELSE                                                                 
022198       MOVE MID-IDTRPLOS-FR-IN   TO WS-IDTRPLOS-FR                        
022199       MOVE '7'                  TO MFS-IDPFK                             
022200       MOVE SPACE                TO MFS-KDTRTYP                           
022201     END-IF                                                               
022202                                                                          
022205     INSPECT WS-IDTRPLOS-FR REPLACING LEADING SPACE BY ZERO               
022206                                                                          
022207     IF WS-IDTRPLOS-FR NOT NUMERIC                                        
022208       MOVE NEJ                  TO NYCKLAR-SW                            
022211     END-IF                                                               
022212     .                                                                    
022213     EJECT                                                                
022214 BD-KOLLA-IDTRPLOS-TO SECTION.                                            
022215                                                                          
022216     IF MID-IDTRPLOS-TO-IN = ALL '+'                                      
022217       IF SPAR-FIRST OR SPAR-NEXT                                         
022218         MOVE MID-IDTRPLOS-TO-UT TO WS-IDTRPLOS-TO                        
022219       ELSE                                                               
022220         MOVE '999'              TO WS-IDTRPLOS-TO                        
022221       END-IF                                                             
022222     ELSE                                                                 
022223       MOVE MID-IDTRPLOS-TO-IN   TO WS-IDTRPLOS-TO                        
022224       MOVE '7'                  TO MFS-IDPFK                             
022225       MOVE SPACE                TO MFS-KDTRTYP                           
022226     END-IF                                                               
022227                                                                          
022228     INSPECT WS-IDTRPLOS-TO REPLACING LEADING SPACE BY ZERO               
022229                                                                          
022230     IF WS-IDTRPLOS-TO NOT NUMERIC                                        
022231       MOVE NEJ                  TO NYCKLAR-SW                            
022234     END-IF                                                               
022235     .                                                                    
022236     EJECT                                                                
022237 BE-KOLLA-SAMBAND-IDTRPLOS SECTION.                                       
022238                                                                          
022239     IF WS-IDTRPLOS-FR > WS-IDTRPLOS-TO AND                               
022240       WS-IDTRPLOS-TO NOT = '   '                                         
022241       MOVE NEJ               TO NYCKLAR-SW                               
022244     ELSE                                                                 
022245       IF WS-IDTRPLOS-TO = '   '                                          
022246         MOVE WS-IDTRPLOS-FR  TO WS-IDTRPLOS-TO                           
022247       END-IF                                                             
022248     END-IF                                                               
022249                                                                          
022250     IF NYCKLAR-OK                                                        
022251       MOVE WS-IDTRPLOS-FR    TO WS-IDTRP-1                               
022252       MOVE '00'              TO WS-IDTRP-2                               
022253       MOVE WS-IDTRP          TO W-IDTRP-MIN                              
022254       MOVE WS-IDTRPLOS-TO    TO WS-IDTRP-1                               
022255       MOVE '99'              TO WS-IDTRP-2                               
022256       MOVE WS-IDTRP          TO W-IDTRP-MAX                              
022257     END-IF                                                               
022258     .                                                                    
022259     EJECT                                                                
022260 C-FOERSTA-SIDA SECTION.                                                  
022261                                                                          
022262     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022263     CALL WMEDKONV USING MED-WMEDAREA                                     
022264     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
022265     .                                                                    
022266     EJECT                                                                
022267 D-NAESTA-SIDA SECTION.                                                   
022268                                                                          
022269*    IF SPAR-IDTRANS = '4653'                                             
022270     IF W-IDTRANS = '4653'                                                
022272       MOVE SPAR-IDDC-NEXT       TO W-IDDC-MIN                            
022276                                                                          
022277       IF SPAR-DARFSDAT-NEXT NUMERIC                                      
022278         MOVE SPAR-DARFSDAT-NEXT TO W-DARFSDAT-MIN                        
022279       ELSE                                                               
022280         MOVE ZERO               TO W-DARFSDAT-MIN                        
022281       END-IF                                                             
022282                                                                          
022283       IF SPAR-IDTRP-NEXT    NUMERIC                                      
022284         MOVE SPAR-IDTRP-NEXT    TO W-IDTRP-MIN                           
022285       ELSE                                                               
022286         MOVE ZERO               TO W-IDTRP-MIN                           
022287       END-IF                                                             
022288                                                                          
022289       IF SPAR-IDORDER-NEXT  NUMERIC                                      
022290         MOVE SPAR-IDORDER-NEXT  TO W-IDORDER-MIN                         
022291       ELSE                                                               
022292         MOVE ZERO               TO W-IDORDER-MIN                         
022293       END-IF                                                             
022294                                                                          
022295       IF SPAR-IDPRODNR-NEXT NUMERIC                                      
022296         MOVE SPAR-IDPRODNR-NEXT TO W-IDPRODNR-MIN                        
022297       ELSE                                                               
022298         MOVE ZERO               TO W-IDPRODNR-MIN                        
022299       END-IF                                                             
022300                                                                          
022301       IF SPAR-IDPLKLST-NEXT NUMERIC                                      
022302         MOVE SPAR-IDPLKLST-NEXT TO W-IDPLKLST-MIN                        
022303       ELSE                                                               
022304         MOVE ZERO               TO W-IDPLKLST-MIN                        
022305       END-IF                                                             
022306     ELSE                                                                 
022307       PERFORM MFS-RENSA-FAELT-IN                                         
022308     END-IF                                                               
022309     .                                                                    
022310     EJECT                                                                
022311 E-SAMMA-SIDA SECTION.                                                    
022312                                                                          
022313*    IF SPAR-IDTRANS = '4653' OR '0551'                                   
022314     IF W-IDTRANS = '4653' OR '0551'                                      
022315       MOVE SPAR-IDDC-ENTER     TO W-IDDC-MIN                             
022316       MOVE SPAR-DARFSDAT-ENTER TO W-DARFSDAT-MIN                         
022317       MOVE SPAR-IDTRP-ENTER    TO W-IDTRP-MIN                            
022318       MOVE SPAR-IDORDER-ENTER  TO W-IDORDER-MIN                          
022319       MOVE SPAR-IDPRODNR-ENTER TO W-IDPRODNR-MIN                         
022320       MOVE SPAR-IDPLKLST-ENTER TO W-IDPLKLST-MIN                         
022321       IF MID-IDDC-IN = ALL '+'                                           
022322         PERFORM MFS-RENSA-FAELT-IN                                       
022323       END-IF                                                             
022324     ELSE                                                                 
022325       PERFORM MFS-RENSA-FAELT-IN                                         
022326     END-IF                                                               
022327     .                                                                    
022330     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022554     MOVE +0                   TO MOD-INDX                                
022555     PERFORM IMS-GN-WDQ3E1                                                
022556     IF SEGMENT-FINNS                                                     
022557       MOVE +1                 TO MOD-INDX                                
022558       MOVE +0                 TO TAB-KVRADER       (MOD-INDX)            
022559                                  TAB-KVRADER-UTSKR (MOD-INDX)            
022560                                  TAB-KVRADER-PACK  (MOD-INDX)            
022561                                  TAB-KVKOLLI       (MOD-INDX)            
022562                                  TAB-KVKOLLI-LAST  (MOD-INDX)            
022563                                  TAB-KVKOLLI-FAKT  (MOD-INDX)            
022564       MOVE SEQE-IDDC     TO SPAR-IDDC-NEXT                               
022565       MOVE SEQE-DARFSDAT TO SPAR-DARFSDAT-NEXT                           
022566       MOVE SEQE-IDTRP    TO SPAR-IDTRP-NEXT                              
022567       MOVE SEQE-IDORDER  TO SPAR-IDORDER-NEXT                            
022568       MOVE SEQE-IDPRODNR TO SPAR-IDPRODNR-NEXT                           
022569       MOVE SEQE-IDPLKLST TO SPAR-IDPLKLST-NEXT                           
022570     ELSE                                                                 
022571       MOVE W-IDDC-MIN         TO SPAR-IDDC-NEXT                          
022572       MOVE W-DARFSDAT-MIN     TO SPAR-DARFSDAT-NEXT                      
022573       MOVE W-IDTRP-MIN        TO SPAR-IDTRP-NEXT                         
022574       MOVE W-IDORDER-MIN      TO SPAR-IDORDER-NEXT                       
022575       MOVE W-IDPRODNR-MIN     TO SPAR-IDPRODNR-NEXT                      
022576       MOVE W-IDPLKLST-MIN     TO SPAR-IDPLKLST-NEXT                      
022577     END-IF                                                               
022578                                                                          
022661     IF MFS-ENTER OR MFS-FIRST                                            
022662       PERFORM FA-BYGG-SUMMERA-INOM-SIDAN                                 
022663       PERFORM FB-SUMMERA-RESTEN                                          
022664     ELSE                                                                 
022665       PERFORM FD-BYGG-INOM-SIDAN                                         
022666     END-IF                                                               
022680     PERFORM FC-VISA-DETRADER-TOTRAD                                      
023501                                                                          
023502     MOVE '002'                 TO MSGI-KDCALL                            
023503     MOVE '4653'                TO MSGI-IDTRANS                           
023504                                   SPAR-IDTRANS                           
023505                                                                          
023519     MOVE SPAR-AREA             TO MSGI-SPAR-AREA                         
023520     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023600     .                                                                    
023700     EJECT                                                                
023800 FA-BYGG-SUMMERA-INOM-SIDAN SECTION.                                      
023900                                                                          
023901     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
023902                   BASEN-SLUT     OR                                      
023903                   MOD-INDX > MAX-MOD-INDX                                
023904       MOVE SEQE-IDTRPLOS        TO TAB-IDTRPLOS (MOD-INDX)               
023905                                    WS-JFR-IDTRPLOS                       
023906       ADD SEQE-KVRADER          TO TAB-KVRADER (MOD-INDX)                
023907                                    TOT-KVRADER                           
023908       IF SEQE-KDODELSTA = 'U'                                            
023909         ADD SEQE-KVRADER        TO TAB-KVRADER-UTSKR (MOD-INDX)          
023910                                    TOT-KVRADER-UTSKR                     
023911       ELSE                                                               
023912         IF SEQE-KDODELSTA = 'P'                                          
023913           ADD SEQE-KVRADER TO TAB-KVRADER-PACK (MOD-INDX)                
023914                                    TOT-KVRADER-PACK                      
023915                                    TAB-KVRADER-UTSKR (MOD-INDX)          
023916                                    TOT-KVRADER-UTSKR                     
023917         END-IF                                                           
023918       END-IF                                                             
023919                                                                          
023920       IF SEQE-KDODELSTA = 'P' OR 'U'                                     
023921         MOVE SEQE-IDPRODNR TO W-IDPRODNR                                 
023922         PERFORM IMS-GU-WDE601                                            
023923         IF SEGMENT-FINNS                                                 
023924           ADD VORD-KVKOLLI      TO TAB-KVKOLLI (MOD-INDX)                
023926                                    TOT-KVKOLLI                           
023927           ADD VORD-KVKOLLI-FL   TO TAB-KVKOLLI-LAST (MOD-INDX)           
023929                                    TOT-KVKOLLI-LAST                      
023930           ADD VORD-KVKOLLI-FAKT TO TAB-KVKOLLI-FAKT (MOD-INDX)           
023932                                    TOT-KVKOLLI-FAKT                      
023933         END-IF                                                           
023934       END-IF                                                             
023935                                                                          
023936       PERFORM IMS-GN-WDQ3E1                                              
023937       IF (SEGMENT-FINNS                             AND                  
023938           WS-JFR-IDTRPLOS NOT = SEQE-IDTRPLOS) OR                        
023939          (NOT SEGMENT-FINNS)                                             
023940         IF TAB-KVRADER (MOD-INDX) > 0                                    
023941           COMPUTE WS-REPROCENT ROUNDED           =                       
023942                   TAB-KVRADER-UTSKR (MOD-INDX) /                         
023943                  (TAB-KVRADER (MOD-INDX) / 100)                          
023944           MOVE WS-REPROCENT TO TAB-REPROCENT-UTSKR (MOD-INDX)            
023945           COMPUTE WS-REPROCENT                   =                       
023946                   TAB-KVRADER-PACK  (MOD-INDX) /                         
023947                  (TAB-KVRADER (MOD-INDX) / 100)                          
023948           MOVE WS-REPROCENT TO TAB-REPROCENT-PACK  (MOD-INDX)            
023949         END-IF                                                           
023950         IF TAB-KVKOLLI (MOD-INDX) > 0                                    
023951           COMPUTE WS-REPROCENT ROUNDED           =                       
023952                   TAB-KVKOLLI-LAST (MOD-INDX) /                          
023953                  (TAB-KVKOLLI (MOD-INDX) / 100)                          
023954           MOVE WS-REPROCENT TO TAB-REPROCENT-LAST (MOD-INDX)             
023955           COMPUTE WS-REPROCENT                   =                       
023956                   TAB-KVKOLLI-FAKT  (MOD-INDX) /                         
023957                  (TAB-KVKOLLI (MOD-INDX) / 100)                          
023958           MOVE WS-REPROCENT TO TAB-REPROCENT-FAKT  (MOD-INDX)            
023959         END-IF                                                           
023960         ADD  +1             TO MOD-INDX                                  
023961         IF MOD-INDX < 13                                                 
023963           MOVE +0           TO TAB-KVRADER       (MOD-INDX)              
023964                                TAB-KVRADER-UTSKR (MOD-INDX)              
023965                                TAB-KVRADER-PACK  (MOD-INDX)              
023966                                TAB-KVKOLLI       (MOD-INDX)              
023967                                TAB-KVKOLLI-LAST  (MOD-INDX)              
023968                                TAB-KVKOLLI-FAKT  (MOD-INDX)              
023969         END-IF                                                           
023970       END-IF                                                             
023971     END-PERFORM                                                          
023972                                                                          
023973     .                                                                    
023974     EJECT                                                                
023975 FB-SUMMERA-RESTEN SECTION.                                               
023980                                                                          
024000     IF MOD-INDX = 13 AND                                                 
024100       SEGMENT-FINNS                                                      
024200       MOVE SEQE-IDDC              TO SPAR-IDDC-NEXT                      
024300       MOVE SEQE-DARFSDAT          TO SPAR-DARFSDAT-NEXT                  
024400       MOVE SEQE-IDTRP             TO SPAR-IDTRP-NEXT                     
024500       MOVE SEQE-IDORDER           TO SPAR-IDORDER-NEXT                   
024600       MOVE SEQE-IDPRODNR          TO SPAR-IDPRODNR-NEXT                  
024700       MOVE SEQE-IDPLKLST          TO SPAR-IDPLKLST-NEXT                  
024740       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
024750                     BASEN-SLUT                                           
024760         ADD SEQE-KVRADER          TO TOT-KVRADER                         
024770         IF SEQE-KDODELSTA = 'U'                                          
024780           ADD SEQE-KVRADER        TO TOT-KVRADER-UTSKR                   
024790         ELSE                                                             
024791           IF SEQE-KDODELSTA = 'P'                                        
024792             ADD SEQE-KVRADER TO TOT-KVRADER-PACK                         
024793                                      TOT-KVRADER-UTSKR                   
024794           END-IF                                                         
024795         END-IF                                                           
024796                                                                          
024797         IF SEQE-KDODELSTA = 'P' OR 'U'                                   
024798           MOVE SEQE-IDPRODNR TO W-IDPRODNR                               
024799           PERFORM IMS-GU-WDE601                                          
024800           IF SEGMENT-FINNS                                               
024801             ADD VORD-KVKOLLI      TO TOT-KVKOLLI                         
024803             ADD VORD-KVKOLLI-FL   TO TOT-KVKOLLI-LAST                    
024805             ADD VORD-KVKOLLI-FAKT TO TOT-KVKOLLI-FAKT                    
024807           END-IF                                                         
024808         END-IF                                                           
024809         PERFORM IMS-GN-WDQ3E1                                            
024810       END-PERFORM                                                        
024811     END-IF                                                               
024812                                                                          
024813     MOVE TOT-KVRADER           TO SPAR-KVORDRAD                          
024814     MOVE TOT-KVRADER-UTSKR     TO SPAR-KVORDRAD-UTSKR                    
024815     MOVE TOT-REPROCENT-UTSKR   TO SPAR-REPROCENT-UTSKR                   
024816     MOVE TOT-KVRADER-PACK      TO SPAR-KVORDRAD-PACK                     
024817     MOVE TOT-REPROCENT-PACK    TO SPAR-REPROCENT-PACK                    
024818     MOVE TOT-KVKOLLI           TO SPAR-KVKOLLI                           
024819     MOVE TOT-KVKOLLI-LAST      TO SPAR-KVKOLLI-LAST                      
024820     MOVE TOT-REPROCENT-LAST    TO SPAR-REPROCENT-LAST                    
024821     MOVE TOT-KVKOLLI-FAKT      TO SPAR-KVKOLLI-FAKT                      
024822     MOVE TOT-REPROCENT-FAKT    TO SPAR-REPROCENT-FAKT                    
024823     .                                                                    
024824     EJECT                                                                
024825 FC-VISA-DETRADER-TOTRAD SECTION.                                         
024826                                                                          
024827     IF MOD-INDX = 13                                                     
024829       MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                        
024830       CALL WMEDKONV USING MED-WMEDAREA                                   
024831       MOVE MED-MFSINF             TO MOD-TEMFSINF                        
024832     END-IF                                                               
024833     IF MOD-INDX > 0                                                      
024834       COMPUTE MAX-MOD-INDX = MOD-INDX - 1                                
024835       MOVE +1               TO MOD-INDX                                  
024836       PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                              
024837         MOVE '*'            TO MOD-FILLERX1 (MOD-INDX)                   
024838         MOVE TAB-IDTRPLOS (MOD-INDX)                                     
024839                             TO MOD-IDTRPLOS (MOD-INDX)                   
024840         MOVE TAB-KVRADER (MOD-INDX)                                      
024841                             TO MOD-KVORDRAD (MOD-INDX)                   
024842         MOVE TAB-KVRADER-UTSKR (MOD-INDX)                                
024843                             TO MOD-KVORDRAD-UTSKR (MOD-INDX)             
024844         MOVE TAB-REPROCENT-UTSKR (MOD-INDX)                              
024845                             TO MOD-REPROCENT-UTSKR (MOD-INDX)            
024846         MOVE TAB-KVRADER-PACK (MOD-INDX)                                 
024847                             TO MOD-KVORDRAD-PACK (MOD-INDX)              
024848         MOVE TAB-REPROCENT-PACK (MOD-INDX)                               
024849                             TO MOD-REPROCENT-PACK (MOD-INDX)             
024850         MOVE TAB-KVKOLLI (MOD-INDX)                                      
024851                             TO MOD-KVKOLLI (MOD-INDX)                    
024852         MOVE TAB-KVKOLLI-LAST (MOD-INDX)                                 
024853                             TO MOD-KVKOLLI-LAST (MOD-INDX)               
024854         MOVE TAB-REPROCENT-LAST (MOD-INDX)                               
024855                             TO MOD-REPROCENT-LAST (MOD-INDX)             
024856         MOVE TAB-KVKOLLI-FAKT (MOD-INDX)                                 
024857                             TO MOD-KVKOLLI-FAKT (MOD-INDX)               
024858         MOVE TAB-REPROCENT-FAKT (MOD-INDX)                               
024859                             TO MOD-REPROCENT-FAKT (MOD-INDX)             
024860         ADD +1 TO MOD-INDX                                               
024861       END-PERFORM                                                        
024862     END-IF                                                               
024863                                                                          
024864     IF MOD-INDX < 13 AND                                                 
024866        MFS-NEXT                                                          
024867       MOVE INF-LAST-PAGE-SHOWN TO MED-IDMFSFEL                           
024868       CALL WMEDKONV USING MED-WMEDAREA                                   
024869       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
024870     END-IF                                                               
024871                                                                          
024872     IF MOD-INDX = 0                                                      
024873       MOVE ERR-RECORD-MISSING  TO MED-IDMFSFEL                           
024874       CALL WMEDKONV USING MED-WMEDAREA                                   
024875       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
024876       MOVE SPACE               TO MOD-TEMFSINF                           
024877     END-IF                                                               
024878                                                                          
024879     IF MOD-INDX > 0 AND < 13                                             
024881       IF SPAR-KVORDRAD > 0                                               
024882         COMPUTE WS-REPROCENT ROUNDED           =                         
024883                 SPAR-KVORDRAD-UTSKR /                                    
024884                (SPAR-KVORDRAD / 100)                                     
024885         MOVE WS-REPROCENT       TO TOT-REPROCENT-UTSKR                   
024886         COMPUTE WS-REPROCENT                   =                         
024887                 SPAR-KVORDRAD-PACK /                                     
024888                (SPAR-KVORDRAD / 100)                                     
024889         MOVE WS-REPROCENT       TO TOT-REPROCENT-PACK                    
024890       END-IF                                                             
024891       IF SPAR-KVKOLLI > 0                                                
024892         COMPUTE WS-REPROCENT ROUNDED           =                         
024893                 SPAR-KVKOLLI-LAST /                                      
024894                (SPAR-KVKOLLI / 100)                                      
024895         MOVE WS-REPROCENT       TO TOT-REPROCENT-LAST                    
024896         COMPUTE WS-REPROCENT                   =                         
024897                 SPAR-KVKOLLI-FAKT /                                      
024898                (SPAR-KVKOLLI / 100)                                      
024899         MOVE WS-REPROCENT       TO TOT-REPROCENT-FAKT                    
024900       END-IF                                                             
024901       MOVE 'TOTALT'             TO MOD-FILLERX5-TOT                      
024902       MOVE '*'                  TO MOD-FILLERX1-TOT                      
024903       MOVE SPAR-KVORDRAD        TO MOD-KVORDRAD-TOT                      
024904       MOVE SPAR-KVORDRAD-UTSKR  TO MOD-KVORDRAD-UTSKR-TOT                
024905       MOVE TOT-REPROCENT-UTSKR  TO MOD-REPROCENT-UTSKR-TOT               
024906       MOVE SPAR-KVORDRAD-PACK   TO MOD-KVORDRAD-PACK-TOT                 
024907       MOVE TOT-REPROCENT-PACK   TO MOD-REPROCENT-PACK-TOT                
024908       MOVE SPAR-KVKOLLI         TO MOD-KVKOLLI-TOT                       
024909       MOVE SPAR-KVKOLLI-LAST    TO MOD-KVKOLLI-LAST-TOT                  
024910       MOVE TOT-REPROCENT-LAST   TO MOD-REPROCENT-LAST-TOT                
024911       MOVE SPAR-KVKOLLI-FAKT    TO MOD-KVKOLLI-FAKT-TOT                  
024912       MOVE TOT-REPROCENT-FAKT   TO MOD-REPROCENT-FAKT-TOT                
024913     ELSE                                                                 
024914       MOVE MFS-RENSA-FAELT      TO MOD-FILLERX5-TOT                      
024915                                    MOD-FILLERX1-TOT                      
024916                                    MOD-KVORDRAD-TOT                      
024917                                    MOD-KVORDRAD-UTSKR-TOT                
024918                                    MOD-REPROCENT-UTSKR-TOT               
024919                                    MOD-KVORDRAD-PACK-TOT                 
024920                                    MOD-REPROCENT-PACK-TOT                
024921                                    MOD-KVKOLLI-TOT                       
024922                                    MOD-KVKOLLI-LAST-TOT                  
024923                                    MOD-REPROCENT-LAST-TOT                
024924                                    MOD-KVKOLLI-FAKT-TOT                  
024925                                    MOD-REPROCENT-FAKT-TOT                
024926     END-IF                                                               
024927     .                                                                    
024928     EJECT                                                                
024929 FD-BYGG-INOM-SIDAN SECTION.                                              
024930                                                                          
024931     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
024932                   BASEN-SLUT     OR                                      
024933                   MOD-INDX > MAX-MOD-INDX                                
024934       MOVE SEQE-IDTRPLOS        TO TAB-IDTRPLOS (MOD-INDX)               
024935                                    WS-JFR-IDTRPLOS                       
024936       ADD SEQE-KVRADER          TO TAB-KVRADER (MOD-INDX)                
024937       IF SEQE-KDODELSTA = 'U'                                            
024938         ADD SEQE-KVRADER        TO TAB-KVRADER-UTSKR (MOD-INDX)          
024939       ELSE                                                               
024940         IF SEQE-KDODELSTA = 'P'                                          
024941           ADD SEQE-KVRADER TO TAB-KVRADER-PACK (MOD-INDX)                
024942                                    TAB-KVRADER-UTSKR (MOD-INDX)          
024943         END-IF                                                           
024944       END-IF                                                             
024945                                                                          
024946       IF SEQE-KDODELSTA = 'P' OR 'U'                                     
024947         MOVE SEQE-IDPRODNR TO W-IDPRODNR                                 
024948         PERFORM IMS-GU-WDE601                                            
024949         IF SEGMENT-FINNS                                                 
024950           ADD VORD-KVKOLLI      TO TAB-KVKOLLI (MOD-INDX)                
024952           ADD VORD-KVKOLLI-FL   TO TAB-KVKOLLI-LAST (MOD-INDX)           
024954           ADD VORD-KVKOLLI-FAKT TO TAB-KVKOLLI-FAKT (MOD-INDX)           
024956         END-IF                                                           
024957       END-IF                                                             
024958                                                                          
024959       PERFORM IMS-GN-WDQ3E1                                              
024960       IF (SEGMENT-FINNS                             AND                  
024961           WS-JFR-IDTRPLOS NOT = SEQE-IDTRPLOS) OR                        
024962          (NOT SEGMENT-FINNS)                                             
024963         IF TAB-KVRADER (MOD-INDX) > 0                                    
024964           COMPUTE WS-REPROCENT ROUNDED           =                       
024965                   TAB-KVRADER-UTSKR (MOD-INDX) /                         
024966                  (TAB-KVRADER (MOD-INDX) / 100)                          
024967           MOVE WS-REPROCENT TO TAB-REPROCENT-UTSKR (MOD-INDX)            
024968           COMPUTE WS-REPROCENT                   =                       
024969                   TAB-KVRADER-PACK  (MOD-INDX) /                         
024970                  (TAB-KVRADER (MOD-INDX) / 100)                          
024971           MOVE WS-REPROCENT TO TAB-REPROCENT-PACK  (MOD-INDX)            
024972         END-IF                                                           
024973         IF TAB-KVKOLLI (MOD-INDX) > 0                                    
024974           COMPUTE WS-REPROCENT ROUNDED           =                       
024975                   TAB-KVKOLLI-LAST (MOD-INDX) /                          
024976                  (TAB-KVKOLLI (MOD-INDX) / 100)                          
024977           MOVE WS-REPROCENT TO TAB-REPROCENT-LAST (MOD-INDX)             
024978           COMPUTE WS-REPROCENT                   =                       
024979                   TAB-KVKOLLI-FAKT  (MOD-INDX) /                         
024980                  (TAB-KVKOLLI (MOD-INDX) / 100)                          
024981           MOVE WS-REPROCENT TO TAB-REPROCENT-FAKT  (MOD-INDX)            
024982         END-IF                                                           
024983         ADD  +1             TO MOD-INDX                                  
024984         IF MOD-INDX < 13                                                 
024986           MOVE +0           TO TAB-KVRADER       (MOD-INDX)              
024987                                TAB-KVRADER-UTSKR (MOD-INDX)              
024988                                TAB-KVRADER-PACK  (MOD-INDX)              
024989                                TAB-KVKOLLI       (MOD-INDX)              
024990                                TAB-KVKOLLI-LAST  (MOD-INDX)              
024991                                TAB-KVKOLLI-FAKT  (MOD-INDX)              
024992         END-IF                                                           
024993       END-IF                                                             
024994     END-PERFORM                                                          
024995     .                                                                    
024996     EJECT                                                                
024997 MFS-RENSA-FAELT-UT SECTION.                                              
024998                                                                          
025000     MOVE 1 TO IX1                                                        
025100     PERFORM UNTIL IX1 > MAX-MOD-INDX                                     
025200        MOVE MFS-RENSA-FAELT TO MOD-FILLERX1        (IX1)                 
025210                                MOD-IDTRPLOS        (IX1)                 
025300                                MOD-KVORDRAD        (IX1)                 
025310                                MOD-KVORDRAD-UTSKR  (IX1)                 
025311                                MOD-REPROCENT-UTSKR (IX1)                 
025312                                MOD-KVORDRAD-PACK   (IX1)                 
025313                                MOD-REPROCENT-PACK  (IX1)                 
025314                                MOD-KVKOLLI         (IX1)                 
025315                                MOD-KVKOLLI-LAST    (IX1)                 
025316                                MOD-REPROCENT-LAST  (IX1)                 
025317                                MOD-KVKOLLI-FAKT    (IX1)                 
025318                                MOD-REPROCENT-FAKT  (IX1)                 
025370        ADD 1 TO IX1                                                      
025380     END-PERFORM                                                          
025381                                                                          
025390     MOVE MFS-RENSA-FAELT    TO MOD-FILLERX5-TOT                          
025391                                MOD-FILLERX1-TOT                          
025392                                MOD-KVORDRAD-TOT                          
025393                                MOD-KVORDRAD-UTSKR-TOT                    
025394                                MOD-REPROCENT-UTSKR-TOT                   
025395                                MOD-KVORDRAD-PACK-TOT                     
025396                                MOD-REPROCENT-PACK-TOT                    
025397                                MOD-KVKOLLI-TOT                           
025398                                MOD-KVKOLLI-LAST-TOT                      
025399                                MOD-REPROCENT-LAST-TOT                    
025400                                MOD-KVKOLLI-FAKT-TOT                      
025410                                MOD-REPROCENT-FAKT-TOT                    
025500     .                                                                    
025501     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
026000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
026100                             MOD-IDDC-UT                                  
026110                             MOD-TIRFSDAT-IN                              
026120                             MOD-TIRFSDAT-UT                              
026130                             MOD-IDTRPLOS-FR-IN                           
026140                             MOD-IDTRPLOS-FR-UT                           
026150                             MOD-IDTRPLOS-TO-IN                           
026160                             MOD-IDTRPLOS-TO-UT                           
026200     .                                                                    
026300     EJECT                                                                
029400*---- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030600     IF MSGI-IDLAND-SPR = 'GB'                                            
030700       MOVE 'N' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GN-WDQ3E1   SECTION.                                                 
031503                                                                          
031504     STRING 'WDQ3E1  (WDQ3E1KY>=' W-WDQ3E1KY-MIN-X                        
031505                    '&WDQ3E1KY<=' W-WDQ3E1KY-MAX-X ')'                    
031510          DELIMITED BY SIZE INTO SSA1                                     
031511     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031512     CALL CBLTDLI USING GN WDQ3E-PCB DLI-IO-Q3E1 SSA1                     
031513     MOVE WDQ3E-STATUS-CODE TO STATUS-WS                                  
031514     PERFORM IMS-STATUSKONTROLL                                           
031515     .                                                                    
031516 IMS-GU-WDE601    SECTION.                                                
031517                                                                          
031518     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
031520          DELIMITED BY SIZE INTO SSA1                                     
031530     MOVE '  GE' TO GODK-STATUSKODER                                      
031540     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
031550     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
031560     PERFORM IMS-STATUSKONTROLL                                           
031570     .                                                                    
031580 IMS-GU-WDB601    SECTION.                                                
031590     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
031600          DELIMITED BY SIZE INTO SSA1                                     
031610     MOVE '  GE' TO GODK-STATUSKODER                                      
031620     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
031630     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031640     PERFORM IMS-STATUSKONTROLL                                           
031650     IF SEGMENT-SAKNAS                                                    
031660         MOVE SPACE TO DCS-KDDC                                           
031670     END-IF                                                               
031680     .                                                                    
031700 IMS-STATUSKONTROLL SECTION.                                              
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
