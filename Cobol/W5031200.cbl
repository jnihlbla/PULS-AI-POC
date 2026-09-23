001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W5031200.                                                
001600 AUTHOR.         ASPFJÄLL MARKUS.                                         
001700 DATE-WRITTEN.   05/06/29.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        LÄSER WDH7 OCH VISAR POSTER                                      
002200*                                                                         
002301*        PROGRAMMET LÄSER      WDP3                                       
002310*        PROGRAMMET LÄSER      WDH7                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W5T312                                              
002700*        MID:         W5I31201                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W5O31201                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W5031200'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004601*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004602 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004610 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005100                                                                          
005200 77  WS-POST                     PIC X       VALUE 'J'.                   
005300     88  POST-FINNS                          VALUE 'J'.                   
005400     88  POST-SAKNAS                         VALUE 'N'.                   
005500                                                                          
005510 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005520     88  NYCKLAR-OK                          VALUE 'J'.                   
005530     88  NYCKLAR-FEL                         VALUE 'N'.                   
005540                                                                          
005550 77  INDATA-SW                  PIC X       VALUE 'J'.                    
005560     88  INDATA-OK                          VALUE 'J'.                    
005570     88  INDATA-FEL                         VALUE 'N'.                    
005580                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '5312'.                
005800     88  GODK-MID                            VALUE '5311' '5312'.         
006200                                                                          
006300     88  HELP-MID                            VALUE '0551'.                
006310 01  WS-ANTALDAGAR               PIC 9(3).                                
006400     EJECT                                                                
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
007110     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007510*01  -COPY WDAGAREA                                                       
007520     SKIP3                                                                
007530*01  -COPY WORKAREA                                                       
007540     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007801     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007802     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
007810     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008710 01  PROG-TO-PROG-SW.                                                     
008720*    03  -COPY WMSGSOP                                                    
008730     EJECT                                                                
008800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008810*    --- AREOR FÖR DATUM                                                  
008820 01  WS-DATUM                      PIC 9(9).                              
008830 01  WS-DATUM1-9 REDEFINES WS-DATUM.                                      
008840     03 WS-DATUM1-3                PIC 9(3).                              
008850     03 WS-DATUM4-9                PIC 9(6).                              
008860                                                                          
008870 01  WS-DATUM1                     PIC 9(9).                              
008880 01  WS-DATUM2                     PIC 9(9).                              
008890                                                                          
008891 01  WS-DATUM-FOM                  PIC 9(6).                              
008892 01  WS-DATUM-TOM                  PIC 9(6).                              
008893                                                                          
008894                                                                          
008900*                                                                         
009240                                                                          
009250 01  SPAR-AREA.                                                           
009260     03  SPAR-IDTRANS               PIC X(4)    VALUE '5312'.             
009270     03  SPAR-IDDC                  PIC X(2).                             
009280     03  SPAR-IDARTNR-ENTER         PIC S9(9) COMP-3.                     
009290     03  SPAR-IDARTNR-NEXT          PIC S9(9) COMP-3.                     
009291     03  SPAR-KDINVKAT-ENTER        PIC S9(3) COMP-3.                     
009292     03  SPAR-KDINVKAT-NEXT         PIC S9(3) COMP-3.                     
009293     03  SPAR-SEQF-KDINVKAT         PIC S9(3) COMP-3.                     
009294     03  SPAR-DATUM-ENTER           PIC  9(6).                            
009295     03  SPAR-DATUM-NEXT            PIC  9(6).                            
009296     03  SPAR-IDUSER-ENTER          PIC X(8).                             
009297     03  SPAR-IDUSER-NEXT           PIC X(8).                             
009298     03  SPAR-TISEGKEY-ENTER        PIC S9(9) COMP-3.                     
009299     03  SPAR-TISEGKEY-NEXT         PIC S9(9) COMP-3.                     
009300     03  SPAR-DAREGDAT-9KOMPL-ENTER PIC 9(8).                             
009301     03  SPAR-DAREGDAT-9KOMPL-NEXT  PIC 9(8).                             
009302     03  SPAR-SEQF-DAREGDAT-9KOMPL  PIC 9(8).                             
009303     03  SPAR-SEQD-DAREGDAT-9KOMPL  PIC 9(8).                             
009304     03  SPAR-IDARTNR-SEQ-ENTER    PIC S9(9) COMP-3.                      
009305     03  SPAR-IDARTNR-SEQ-NEXT     PIC S9(9) COMP-3.                      
009306     03  SPAR-KDSEGKEY-ENTER       PIC X.                                 
009307     03  SPAR-KDSEGKEY-NEXT        PIC X.                                 
009308     03  SPAR-IDARTNR-MID          PIC 9(9).                              
009309     03  SPAR-KDINVKAT-MID         PIC 9(2).                              
009310     03  SPAR-DATUM-MID            PIC 9(8).                              
009311     03  SPAR-IDUSER-MID             PIC X(8).                            
009312     03  SPAR-SOK-TYP             PIC X(10)   VALUE SPACE.                
009313     03  SPAR-KDJUSTYP-ENTER      PIC 9       VALUE ZERO.                 
009314     03  SPAR-KDJUSTYP-NEXT       PIC 9       VALUE ZERO.                 
009320     EJECT                                                                
009400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009700     SKIP3                                                                
009800*01  MID -COPY W5I31201                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010100     SKIP3                                                                
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400     03  MOD REDEFINES MSG-AREA.                                          
010500*      05  -COPY W5O31201                                                 
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010800     SKIP3                                                                
010900*01  -COPY WMFSAREA                                                       
011000     EJECT                                                                
011100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011601*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011602     03  W-IDARTNR-MIN-X.                                                 
011603         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO  COMP-3.            
011604                                                                          
011605     03  W-TISEGKEY-X.                                                    
011607         05  W-TISEGKEY   PIC S9(9) VALUE +999999999 COMP-3.              
011609                                                                          
011613     03  W-KDJUSTYP-X.                                                    
011614         05  W-KDJUSTYP          PIC 9(1).                                
011615                                                                          
011616     03  W-KDARBTYP-X.                                                    
011617         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
011618                                                                          
011619     03  W-IDPERSON-X.                                                    
011620         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
011621                                                                          
011622     03  W-IDARTNR-X.                                                     
011623         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011624                                                                          
011625     03  W-IDDC-X.                                                        
011626         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011627                                                                          
011630     03    W-WDH711KY-X.                                                  
011640       05    W-TISEGKEY-UNIK       PIC S9(9)   VALUE ZERO COMP-3.         
011641       05    W-IDDC-UNIK           PIC X(2)    VALUE SPACE.               
011660                                                                          
011690     03    W-WDH71-KEY-MIN-X.                                             
011696       05    W-TISEGKEY-MIN        PIC S9(9) VALUE ZERO COMP-3.           
011697       05    W-IDDC-MIN            PIC X(2)  VALUE SPACE.                 
011698                                                                          
011700     03    W-WDH71-KEY-MAX-X.                                             
011705       05    W-TISEGKEY-MAX    PIC S9(9) VALUE +999999999 COMP-3.         
011706       05    W-IDDC-MAX        PIC X(2)  VALUE SPACE.                     
011710                                                                          
011711     03  WS-IDMAIL.                                                       
011712         05  IDMAIL              PIC X(58) VALUE SPACE.                   
011713                                                                          
011726      03  WS-URVAL1.                                                      
011727          05  URV-BAS             PIC X(4) VALUE 'WDH1'.                  
011728          05  URV-IDDC            PIC X(2) VALUE SPACE.                   
011729          05  URV-SOK-TYP         PIC X(10) VALUE SPACE.                  
011730          05  URV-IDARTNR         PIC S9(9) VALUE ZERO COMP-3.            
011731          05  URV-KDJUSTYP        PIC S9(3) VALUE ZERO COMP-3.            
011732          05  URV-DATUM           PIC 9(6) VALUE ZERO.                    
011733          05  URV-IDUSER          PIC X(8) VALUE SPACE.                   
011734                                                                          
011735     SKIP2                                                                
011736**** --- BILD RUBRIKER ----                                               
011740                                                                          
011791 01  FILLER                      PIC X(16)   VALUE 'CREATED-RAD1'.        
011792 01  CREATED-RAD1.                                                        
011794     03  FILLER                  PIC X(1)    VALUE SPACE.                 
011795     03  CR1-TYPE                PIC X(2)    VALUE SPACE.                 
011796     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011797     03  FILLER                  PIC X(9)    VALUE                        
011798         'CREATED  '.                                                     
011799     03  CR1-DATE                PIC X(8)    VALUE SPACE.                 
011800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
011801     03  CR1-USER                PIC X(8)    VALUE SPACE.                 
011802                                                                          
011803 01  FILLER                      PIC X(16)   VALUE 'PRINT-RAD1'.          
011804 01  PRINT-RAD1.                                                          
011805     03  FILLER                  PIC X(1)   VALUE SPACE.                  
011806     03  RAD1-TYPE               PIC X(2)    VALUE SPACE.                 
011807     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011808     03  FILLER                  PIC X(9)    VALUE                        
011809         'PRINTED1 '.                                                     
011810     03  RAD1-DATE               PIC X(8)    VALUE SPACE.                 
011811     03  FILLER                  PIC X       VALUE SPACE.                 
011812     03  RAD1-USER               PIC X(8)    VALUE SPACE.                 
011813     03  FILLER                  PIC X(9)    VALUE                        
011814         'CR   PR1 '.                                                     
011815     03  RAD1-WS-DATE            PIC Z(2)9    VALUE ZERO.                 
011816     03  FILLER                  PIC X(14)   VALUE                        
011817         '  CR  PR1     '.                                                
011818     03  RAD1-WS-DATETOT         PIC Z(2)9   VALUE ZERO.                  
011819                                                                          
011820 01  FILLER                      PIC X(16)   VALUE 'PRINT-RAD2'.          
011821 01  PRINT-RAD2.                                                          
011822     03  FILLER                  PIC X(1)   VALUE SPACE.                  
011823     03  RAD2-TYPE               PIC X(2)    VALUE SPACE.                 
011824     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011825     03  FILLER                  PIC X(9)    VALUE                        
011826         'PRINTED2 '.                                                     
011827     03  RAD2-DATE               PIC X(8)    VALUE SPACE.                 
011828     03  FILLER                  PIC X       VALUE SPACE.                 
011829     03  RAD2-USER               PIC X(8)    VALUE SPACE.                 
011830     03  FILLER                  PIC X(9)   VALUE                         
011831         'PR1  PR2 '.                                                     
011832     03  RAD2-WS-DATE            PIC Z(2)9   VALUE ZERO.                  
011833     03  FILLER                  PIC X(14)   VALUE                        
011834         '  PR1 PR2     '.                                                
011835     03  RAD2-WS-DATETOT         PIC Z(2)9   VALUE ZERO.                  
011836     03  FILLERXTRA              PIC X(5)    VALUE SPACE.                 
011837**   03  RAD2-ANTAL              PIC 9(9)    VALUE ZERO.                  
011838                                                                          
011839 01  FILLER                      PIC X(16)   VALUE 'PRINT-RAD3'.          
011840 01  PRINT-RAD3.                                                          
011841     03  FILLER                  PIC X(1)   VALUE SPACE.                  
011842     03  RAD3-TYPE               PIC X(2)    VALUE SPACE.                 
011843     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011844     03  FILLER                  PIC X(9)    VALUE                        
011845         'PRINTED3 '.                                                     
011846     03  RAD3-DATE               PIC X(8)    VALUE SPACE.                 
011847     03  FILLER                  PIC X       VALUE SPACE.                 
011848     03  RAD3-USER               PIC X(8)    VALUE SPACE.                 
011849     03  FILLER                  PIC X(9)   VALUE                         
011850         'PR2  PR3 '.                                                     
011851     03  RAD3-WS-DATE            PIC Z(2)9   VALUE ZERO.                  
011852     03  FILLER                  PIC X(14)   VALUE                        
011853         '  PR1 PR3     '.                                                
011854     03  RAD3-WS-DATETOT         PIC Z(2)9   VALUE ZERO.                  
011855                                                                          
011856 01  FILLER                      PIC X(16)   VALUE 'PRINT-RAD4'.          
011857 01  PRINT-RAD4.                                                          
011858     03  FILLER                  PIC X(1)   VALUE SPACE.                  
011859     03  RAD4-TYPE               PIC X(2)    VALUE SPACE.                 
011860     03  FILLER                  PIC X(2)    VALUE SPACE.                 
011861     03  FILLER                  PIC X(9)    VALUE                        
011862         'CLOSED   '.                                                     
011863     03  RAD4-DATE               PIC X(8)    VALUE SPACE.                 
011864     03  FILLER                  PIC X       VALUE SPACE.                 
011865     03  RAD4-USER               PIC X(8)    VALUE SPACE.                 
011866     03  FILLER                  PIC X(9)    VALUE                        
011867         'PR3  CL  '.                                                     
011868     03  RAD4-WS-DATE            PIC Z(2)9   VALUE ZERO.                  
011869     03  FILLER                  PIC X(14)   VALUE                        
011870         '  PR1 CL      '.                                                
011871     03  RAD4-WS-DATETOT         PIC Z(2)9   VALUE ZERO.                  
011872                                                                          
011880*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
013602 01  DLI-IO-WDP301.                                                       
013603*    03  -COPY WDP301                                                     
013604     EJECT                                                                
013605 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
013606 01  DLI-IO-WDP311.                                                       
013607*    03  -COPY WDP311                                                     
013608 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH701'.                      
013609 01  DLI-IO-WDH701.                                                       
013610*    03  -COPY WDH701                                                     
013611     EJECT                                                                
013612 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH711'.                      
013613 01  DLI-IO-WDH711.                                                       
013620*    03  -COPY WDH711                                                     
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014110*01  -COPY W0009   -PRE ALT-                                              
014200*01  -COPY W0008   -PRE WDP7-                                             
014300     05  FILLER                  PIC X.                                   
014401                                                                          
014402*01  -COPY W0008  -PRE WDP3-                                              
014403     05  FILLER                  PIC X.                                   
014404                                                                          
014405*01  -COPY W0008  -PRE WDH7-                                              
014410     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014601 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB WDP7-PCB WDP3-PCB             
014602                           WDH7-PCB.                                      
014603 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB WDP7-PCB WDP3-PCB             
014620                           WDH7-PCB.                                      
014700                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FINNS                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-KOLLA-NYCKLAR                                            
015300       IF NYCKLAR-OK                                                      
015501           IF MFS-FIRST                                                   
015502             PERFORM C-FOERSTA-SIDA                                       
015503           ELSE                                                           
015504             IF MFS-NEXT                                                  
015505               PERFORM D-NAESTA-SIDA                                      
015506             ELSE                                                         
015507               IF MFS-UPDATE                                              
015508                 PERFORM G-UPDATE                                         
015509                 IF INDATA-FEL                                            
015510                   PERFORM E-SAMMA-SIDA                                   
015511                 END-IF                                                   
015512               ELSE                                                       
015513                 PERFORM E-SAMMA-SIDA                                     
015514               END-IF                                                     
015515             END-IF                                                       
015520           END-IF                                                         
015800         PERFORM F-LAES-VISA-INFO                                         
015900       END-IF                                                             
016000*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
016100*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O31201 + 4                      
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     IF MSG-DUBBLA-TRANSKODER                                             
017400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I31201                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I31201                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018600                                                                          
018700     MOVE LOW-VALUE TO MSG-AREA                                           
018800     MOVE 'W5O31201' TO MFS-IDMOD                                         
018900     MOVE '5312' TO MOD-IDTRANS                                           
019000     MOVE ALL SPACE       TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF EGEN-MID OR HELP-MID                                              
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-NYCKLAR SECTION.                                                 
020300                                                                          
020400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020500     MOVE '001'             TO MSGI-KDCALL                                
020600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020800     MOVE '5312'            TO MSGI-IDTRANS                               
020900     IF GODK-MID                                                          
021010         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
021100     END-IF                                                               
021200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021300     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
021400                                                                          
021500*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
021600     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
021700                                                                          
021800     MOVE JA TO NYCKLAR-SW                                                
021900                                                                          
022001                                                                          
022002*    -- KONTROLL AV IDDC                                                  
022003     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
022004                                                                          
022005*    IF MID-IDDC-IN NOT = ALL '+'                                         
022006*      MOVE '7'         TO MFS-IDPFK                                      
022007*      MOVE SPACE       TO MFS-KDTRTYP                                    
022008*    END-IF                                                               
022009                                                                          
022018                                                                          
022019     IF GODK-MID                                                          
022020       IF MID-IDDC-IN = ALL '+' OR '00' OR  '  '                          
022021         MOVE SPAR-IDDC TO MID-IDDC-IN                                    
022022       ELSE                                                               
022023         IF MID-IDDC-IN NOT = SPAR-IDDC                                   
022024           MOVE '7'         TO MFS-IDPFK                                  
022025           MOVE SPACE       TO MFS-KDTRTYP                                
022026         END-IF                                                           
022027       END-IF                                                             
022028     ELSE                                                                 
022029       MOVE MSGI-IDDC       TO SPAR-IDDC                                  
022030                               MOD-IDDC-UT                                
022031       MOVE NEJ TO NYCKLAR-SW                                             
022032     END-IF                                                               
022033                                                                          
022034     IF MID-IDDC-IN = '00' OR '  '                                        
022035       MOVE MSGI-IDDC  TO MID-IDDC-IN                                     
022036     END-IF                                                               
022037                                                                          
022038     INSPECT MID-IDDC-IN REPLACING LEADING SPACE BY ZERO                  
022039     IF MID-IDDC-IN NOT = ALL '+'                                         
022040       MOVE MID-IDDC-IN    TO W-IDDC                                      
022041                              MOD-IDDC-IN                                 
022042                              MOD-IDDC-UT                                 
022043                              W-IDDC-UNIK                                 
022044                              URV-IDDC                                    
022045                             SPAR-IDDC                                    
022046     ELSE                                                                 
022047       MOVE MSGI-IDDC      TO MOD-IDDC-UT                                 
022048                              MOD-IDDC-IN                                 
022049                              W-IDDC                                      
022050                              W-IDDC-UNIK                                 
022051                              URV-IDDC                                    
022052                             SPAR-IDDC                                    
022060     END-IF                                                               
022101                                                                          
022102*    -- KONTROLL AV IDARTNR                                               
022103     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
022104                                                                          
022105     IF MID-IDARTNR-IN NOT = ALL '+'                                      
022106       MOVE '7'         TO MFS-IDPFK                                      
022107       MOVE SPACE       TO MFS-KDTRTYP                                    
022108                                                                          
022109       INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO             
022110       IF MID-IDARTNR-IN NUMERIC                                          
022111         MOVE MID-IDARTNR-IN TO W-IDARTNR                                 
022112                                URV-IDARTNR                               
022113                                MOD-IDARTNR-UT                            
022114         IF MID-KDJUSTYP-IN = ALL '+' OR                                  
022115            MID-KDJUSTYP-IN = ZERO                                        
022116           MOVE ZERO TO SPAR-KDJUSTYP-ENTER                               
022117           MOVE MFS-RENSA-FAELT TO MOD-KDJUSTYP-UT                        
022118         END-IF                                                           
022119       ELSE                                                               
022120         MOVE NEJ TO NYCKLAR-SW                                           
022121       END-IF                                                             
022122                                                                          
022123     ELSE                                                                 
022124*      MOVE NEJ TO NYCKLAR-SW                                             
022125       IF EGEN-MID                                                        
022126         IF SPAR-IDARTNR-ENTER  NUMERIC AND                               
022127            SPAR-IDARTNR-ENTER > ZERO                                     
022128           MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR                           
022129                                      MOD-IDARTNR-UT                      
022130         END-IF                                                           
022131       END-IF                                                             
022132                                                                          
022133     END-IF                                                               
022134                                                                          
022135*    -- KONTROLL AV KDJUSTYP-IN                                           
022136     MOVE MFS-RENSA-FAELT TO MOD-KDJUSTYP-IN                              
022137                                                                          
022138     IF MID-KDJUSTYP-IN NOT = ALL '+' AND                                 
022139        MID-KDJUSTYP-IN >= ZERO                                           
022140       MOVE '7'         TO MFS-IDPFK                                      
022141       MOVE SPACE       TO MFS-KDTRTYP                                    
022142                                                                          
022143       INSPECT MID-KDJUSTYP-IN  REPLACING LEADING SPACE BY ZERO           
022144       IF MID-KDJUSTYP-IN  NUMERIC                                        
022145         MOVE MID-KDJUSTYP-IN  TO MOD-KDJUSTYP-UT                         
022146                                  W-KDJUSTYP                              
022147                                  SPAR-KDJUSTYP-ENTER                     
022148                                  URV-KDJUSTYP                            
022149        IF MID-KDJUSTYP-IN = ZERO                                         
022150          MOVE MFS-RENSA-FAELT TO MOD-KDJUSTYP-UT                         
022151        END-IF                                                            
022152       ELSE                                                               
022153         MOVE NEJ TO NYCKLAR-SW                                           
022154       END-IF                                                             
022155                                                                          
022156                                                                          
022157     END-IF                                                               
022160                                                                          
022167                                                                          
022168     IF GODK-MID OR NYCKLAR-OK                                            
022169       CONTINUE                                                           
022170     ELSE                                                                 
022171       MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-UT                          
022180     END-IF                                                               
022200                                                                          
022300     IF NYCKLAR-FEL                                                       
022400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022500       CALL WMEDKONV USING MED-WMEDAREA                                   
022600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022700       PERFORM MFS-RENSA-FAELT-IN                                         
022800       PERFORM MFS-RENSA-FAELT-UT                                         
022900     END-IF                                                               
023000     .                                                                    
023101     EJECT                                                                
023102 C-FOERSTA-SIDA SECTION.                                                  
023103                                                                          
023104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
023105     CALL WMEDKONV USING MED-WMEDAREA                                     
023106     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
023107                                                                          
023108     PERFORM MFS-RENSA-FAELT-IN                                           
023109     .                                                                    
023110     EJECT                                                                
023111 D-NAESTA-SIDA SECTION.                                                   
023112                                                                          
023113     IF SPAR-IDTRANS = '5312'                                             
023114       MOVE SPAR-IDARTNR-NEXT  TO W-IDARTNR                               
023115                                  W-IDARTNR-MIN                           
023116                                  MOD-IDARTNR-UT                          
023117       MOVE SPAR-TISEGKEY-NEXT TO W-TISEGKEY-UNIK                         
023118                                                                          
023119       IF SPAR-KDJUSTYP-NEXT > 0                                          
023120         MOVE SPAR-KDJUSTYP-NEXT TO MOD-KDJUSTYP-UT                       
023121       END-IF                                                             
023122     ELSE                                                                 
023123       PERFORM MFS-RENSA-FAELT-IN                                         
023124     END-IF                                                               
023125     .                                                                    
023126     EJECT                                                                
023127 E-SAMMA-SIDA SECTION.                                                    
023128                                                                          
023129     IF SPAR-IDTRANS = '5312' OR '0551'                                   
023130       MOVE SPAR-IDARTNR-ENTER  TO W-IDARTNR                              
023131                                   MOD-IDARTNR-UT                         
023132       MOVE SPAR-TISEGKEY-ENTER TO W-TISEGKEY-UNIK                        
023133                                                                          
023134       IF SPAR-KDJUSTYP-ENTER > 0                                         
023135         MOVE SPAR-KDJUSTYP-NEXT TO MOD-KDJUSTYP-UT                       
023136       END-IF                                                             
023137                                                                          
023138       IF MID-IDARTNR-IN  = ALL '+' AND                                   
023139          MID-KDJUSTYP-IN = ALL '+' AND                                   
023141          MID-KDARBTYP-IN = ALL '+' AND                                   
023142          MID-IDPERSON-IN = ALL '+'                                       
023143         PERFORM MFS-RENSA-FAELT-IN                                       
023144       ELSE                                                               
023145         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
023146         CALL WMEDKONV USING MED-WMEDAREA                                 
023147         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
023148         PERFORM EA-MID-INDATA-TILL-MOD                                   
023149       END-IF                                                             
023150     ELSE                                                                 
023151       PERFORM MFS-RENSA-FAELT-IN                                         
023152     END-IF                                                               
023153     .                                                                    
023154     EJECT                                                                
023155 EA-MID-INDATA-TILL-MOD SECTION.                                          
023156     IF MID-IDARTNR-IN = ALL '+'                                          
023157       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                           
023158     ELSE                                                                 
023159       MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                           
023160     END-IF                                                               
023161                                                                          
023162     IF MID-KDJUSTYP-IN = ALL '+'                                         
023163       MOVE MFS-RENSA-FAELT   TO MOD-KDJUSTYP-UT                          
023164     ELSE                                                                 
023165       MOVE MID-KDJUSTYP-IN     TO MOD-KDJUSTYP-UT                        
023166     END-IF                                                               
023167                                                                          
023168                                                                          
023169     IF MID-IDDC-IN = ALL '+'                                             
023170       MOVE MFS-RENSA-FAELT   TO MOD-IDDC-UT                              
023171     ELSE                                                                 
023172       MOVE MID-IDDC-IN       TO MOD-IDDC-UT                              
023173     END-IF                                                               
023174     IF MID-KDARBTYP-IN = ALL '+'                                         
023175       MOVE MFS-RENSA-FAELT   TO MOD-KDARBTYP-UT                          
023176     ELSE                                                                 
023177       MOVE MID-KDARBTYP-IN   TO MOD-KDARBTYP-UT                          
023178       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDARBTYP-ATTR                   
023179     END-IF                                                               
023180     IF MID-IDPERSON-IN = ALL '+'                                         
023181       MOVE MFS-RENSA-FAELT   TO MOD-IDPERSON-UT                          
023182     ELSE                                                                 
023183       MOVE MID-IDPERSON-IN   TO MOD-IDPERSON-UT                          
023184       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDPERSON-ATTR                   
023185     END-IF                                                               
023186* * * * * FÖR VARJE MID-FÄLT                                              
023187* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
023188* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
023189* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
023190     .                                                                    
023200     EJECT                                                                
023400 F-LAES-VISA-INFO SECTION.                                                
023500                                                                          
023600     PERFORM FA-LAES-GRUNDDATA                                            
023700                                                                          
023800     IF SEGMENT-SAKNAS                                                    
023900* FLYTTA LÄMPLIGT FELMEDDELANDE                                           
024000        CALL WMEDKONV USING MED-WMEDAREA                                  
024100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024200        PERFORM MFS-RENSA-FAELT-UT                                        
024300     ELSE                                                                 
024401*      -- POSITIONERA FÖR LÄSNING AV DATA TILL ÖVERSTA RADEN              
024402*      -- (EJ NÖDVÄNDIGT OM -MIN NYCKLAR ANVÄNDS DIREKT I SSA)            
024403                                                                          
024404       MOVE W-IDARTNR            TO SPAR-IDARTNR-ENTER                    
024405       MOVE +99999999            TO SPAR-TISEGKEY-ENTER                   
024406                                                                          
024407       PERFORM FB-LAES-RADDATA                                            
024408                                                                          
024409       IF SEGMENT-FINNS                                                   
024410       MOVE +1 TO INDX                                                    
024411       MOVE INVH-TISEGKEY         TO SPAR-TISEGKEY-ENTER                  
024412                                                                          
024413         PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX                  
024414                                                                          
024415           MOVE INVH-TISEGKEY        TO SPAR-TISEGKEY-ENTER               
024416*          MOVE INVA-IDARTNR         TO CR1-IDARTNR                       
024417           MOVE INVH-KDJUSTYP        TO CR1-TYPE                          
024418                                                                          
024419           MOVE INVH-DAREGDAT-CRE    TO WS-DATUM                          
024420           MOVE INVH-IDUSER-CRE      TO CR1-USER                          
024421           MOVE WS-DATUM4-9          TO CR1-DATE                          
024422           MOVE CREATED-RAD1         TO MOD-RAD-INFO(INDX)                
024423                                                                          
024424           ADD  +1 TO INDX                                                
024425           IF INDX < MAX-INDX                                             
024426             IF INVH-DAREGDAT-PR1 NUMERIC                                 
024427               MOVE INVH-KDJUSTYP        TO RAD1-TYPE                     
024428               MOVE INVH-DAREGDAT-PR1    TO WS-DATUM                      
024429               MOVE WS-DATUM4-9          TO RAD1-DATE                     
024430               MOVE INVH-IDUSER-PR1      TO RAD1-USER                     
024431               MOVE INVH-DAREGDAT-PR1(3:6) TO WS-DATUM-TOM                
024432               MOVE INVH-DAREGDAT-CRE(3:6) TO WS-DATUM-FOM                
024433               IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                   
024434                 PERFORM S01-DATUM                                        
024435                 MOVE WS-ANTALDAGAR      TO RAD1-WS-DATE                  
024436                                            RAD1-WS-DATETOT               
024437               ELSE                                                       
024438                 MOVE ZERO               TO RAD1-WS-DATE                  
024439                                            RAD1-WS-DATETOT               
024440               END-IF                                                     
024443                                                                          
024444               MOVE PRINT-RAD1           TO MOD-RAD-INFO(INDX)            
024445               ADD  +1 TO INDX                                            
024446             END-IF                                                       
024447           END-IF                                                         
024448           IF INDX < MAX-INDX                                             
024449             IF INVH-DAREGDAT-PR2 NUMERIC                                 
024450               MOVE INVH-KDJUSTYP          TO RAD2-TYPE                   
024451               MOVE INVH-DAREGDAT-PR2      TO WS-DATUM                    
024452               MOVE WS-DATUM4-9            TO RAD2-DATE                   
024453               MOVE INVH-IDUSER-PR2        TO RAD2-USER                   
024454****           MOVE INVH-KVJUSTKV          TO RAD2-ANTAL                  
024455               MOVE INVH-DAREGDAT-PR2(3:6) TO WS-DATUM-TOM                
024456               MOVE INVH-DAREGDAT-PR1(3:6) TO WS-DATUM-FOM                
024457                                                                          
024458               IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                   
024459                 PERFORM S01-DATUM                                        
024460                 MOVE WS-ANTALDAGAR        TO RAD2-WS-DATE                
024462               ELSE                                                       
024463                 MOVE ZERO                 TO RAD2-WS-DATE                
024465               END-IF                                                     
024469               MOVE INVH-DAREGDAT-PR1(3:6) TO WS-DATUM-FOM                
024470               MOVE INVH-DAREGDAT-PR2(3:6) TO WS-DATUM-TOM                
024471               IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                   
024472                 PERFORM S01-DATUM                                        
024473                 MOVE WS-ANTALDAGAR        TO RAD2-WS-DATETOT             
024474               ELSE                                                       
024475                 MOVE ZERO                 TO RAD2-WS-DATETOT             
024476               END-IF                                                     
024477                                                                          
024478               MOVE PRINT-RAD2           TO MOD-RAD-INFO(INDX)            
024479               ADD  +1 TO INDX                                            
024480             END-IF                                                       
024481           END-IF                                                         
024482           IF INDX < MAX-INDX                                             
024483             IF INVH-DAREGDAT-PR3 NUMERIC                                 
024484               MOVE INVH-KDJUSTYP          TO RAD3-TYPE                   
024485               MOVE INVH-DAREGDAT-PR3      TO WS-DATUM                    
024486               MOVE WS-DATUM4-9            TO RAD3-DATE                   
024487               MOVE INVH-IDUSER-PR3        TO RAD3-USER                   
024488               MOVE INVH-DAREGDAT-PR3(3:6) TO WS-DATUM-TOM                
024489               MOVE INVH-DAREGDAT-PR2(3:6) TO WS-DATUM-FOM                
024490               IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                   
024491                 PERFORM S01-DATUM                                        
024492                 MOVE WS-ANTALDAGAR        TO RAD3-WS-DATE                
024493               ELSE                                                       
024494                 MOVE ZERO                 TO RAD3-WS-DATE                
024495               END-IF                                                     
024496               MOVE INVH-DAREGDAT-PR1(3:6) TO WS-DATUM-FOM                
024497               MOVE INVH-DAREGDAT-PR3(3:6) TO WS-DATUM-TOM                
024498               IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                   
024499                 PERFORM S01-DATUM                                        
024500                 MOVE WS-ANTALDAGAR        TO RAD3-WS-DATETOT             
024501               ELSE                                                       
024502                 MOVE ZERO                 TO RAD3-WS-DATETOT             
024503               END-IF                                                     
024508                                                                          
024509               MOVE PRINT-RAD3             TO MOD-RAD-INFO(INDX)          
024510               ADD  +1 TO INDX                                            
024511             END-IF                                                       
024512           END-IF                                                         
024513           IF INDX < MAX-INDX                                             
024514             IF INVH-DAREGDAT-CLO NUMERIC                                 
024515               MOVE INVH-KDJUSTYP          TO RAD4-TYPE                   
024516               MOVE INVH-DAREGDAT-CLO      TO WS-DATUM                    
024517               MOVE WS-DATUM4-9            TO RAD4-DATE                   
024518               MOVE INVH-IDUSER-CLO        TO RAD4-USER                   
024519               MOVE INVH-DAREGDAT-CLO(3:6) TO WS-DATUM-TOM                
024520               MOVE INVH-DAREGDAT-PR3(3:6) TO WS-DATUM-FOM                
024521               IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                   
024522                 PERFORM S01-DATUM                                        
024523                 MOVE WS-ANTALDAGAR        TO RAD4-WS-DATE                
024524               ELSE                                                       
024525                 MOVE ZERO                 TO RAD4-WS-DATE                
024526               END-IF                                                     
024527               MOVE INVH-DAREGDAT-PR1(3:6) TO WS-DATUM-FOM                
024528               MOVE INVH-DAREGDAT-CLO(3:6) TO WS-DATUM-TOM                
024529               IF WS-DATUM-FOM > 0 AND WS-DATUM-TOM > 0                   
024530                 PERFORM S01-DATUM                                        
024531                 MOVE WS-ANTALDAGAR        TO RAD4-WS-DATETOT             
024532               ELSE                                                       
024533                 MOVE ZERO                 TO RAD4-WS-DATETOT             
024534               END-IF                                                     
024538                                                                          
024539               MOVE PRINT-RAD4           TO MOD-RAD-INFO(INDX)            
024540               ADD  +1 TO INDX                                            
024541               PERFORM FB-LAES-RADDATA                                    
024542             END-IF                                                       
024543           END-IF                                                         
024544                                                                          
024545         END-PERFORM                                                      
024546       END-IF                                                             
024547       IF SEGMENT-FINNS                                                   
024548         MOVE INVA-IDARTNR         TO SPAR-IDARTNR-NEXT                   
024549         MOVE SPAR-KDJUSTYP-ENTER  TO SPAR-KDJUSTYP-NEXT                  
024550         MOVE INVH-TISEGKEY        TO SPAR-TISEGKEY-NEXT                  
024551         IF MOD-TEMFSINF = ALL SPACE                                      
024552           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
024553           CALL WMEDKONV USING MED-WMEDAREA                               
024554           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
024555         END-IF                                                           
024556                                                                          
024557                                                                          
024558       ELSE                                                               
024559         MOVE INVA-IDARTNR        TO SPAR-IDARTNR-NEXT                    
024560         MOVE SPAR-TISEGKEY-ENTER TO SPAR-TISEGKEY-NEXT                   
024561         MOVE SPAR-KDJUSTYP-ENTER TO SPAR-KDJUSTYP-NEXT                   
024562                                                                          
024563         IF  MOD-TEMFSINF = ALL SPACE                                     
024564           MOVE INF-LAST-PAGE        TO MED-IDMFSINF                      
024565           CALL WMEDKONV USING MED-WMEDAREA                               
024566           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
024567         END-IF                                                           
024568       END-IF                                                             
024569                                                                          
024570       MOVE '002'      TO MSGI-KDCALL                                     
024571       MOVE '5312'     TO SPAR-IDTRANS                                    
024572       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
024573       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
024580     END-IF                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 FA-LAES-GRUNDDATA SECTION.                                               
024900     PERFORM IMS-GHU-WDH701                                               
025310                                                                          
025400     .                                                                    
025501     EJECT                                                                
025502 FB-LAES-RADDATA SECTION.                                                 
025503     MOVE NEJ TO WS-POST                                                  
025504     IF MFS-NEXT                                                          
025505       PERFORM IMS-GNP-WDH711-UNIK                                        
025506       MOVE '7' TO MFS-IDPFK                                              
025507     ELSE                                                                 
025508       PERFORM IMS-GNP-WDH711                                             
025509     END-IF                                                               
025510                                                                          
025511     PERFORM UNTIL POST-FINNS OR SEGMENT-SAKNAS                           
025537*** FALL 1 IDARTNR IFYLLT                                                 
025538       IF MOD-IDARTNR-UT NOT = ALL '+'                                    
025539         IF SEGMENT-FINNS                                                 
025540           IF SPAR-KDJUSTYP-ENTER > 0                                     
025541             IF SPAR-KDJUSTYP-ENTER = INVH-KDJUSTYP                       
025542               MOVE JA TO WS-POST                                         
025543             END-IF                                                       
025544           ELSE                                                           
025545             MOVE JA TO WS-POST                                           
025546           END-IF                                                         
025547         END-IF                                                           
025548       END-IF                                                             
025549****                                                                      
025550       IF POST-SAKNAS                                                     
025551         PERFORM IMS-GNP-WDH711                                           
025552       END-IF                                                             
025553     END-PERFORM                                                          
025554                                                                          
025560     .                                                                    
025700     EJECT                                                                
025701 G-UPDATE SECTION.                                                        
025702     IF MID-KDARBTYP-IN NOT = ALL '+'                                     
025703       MOVE MID-KDARBTYP-IN   TO MOD-KDARBTYP-UT                          
025704                                 W-KDARBTYP                               
025705     ELSE                                                                 
025706       MOVE NEJ               TO INDATA-SW                                
025707       MOVE MFS-RENSA-FAELT   TO MOD-KDARBTYP-UT                          
025708     END-IF                                                               
025709     IF MID-IDPERSON-IN NOT = ALL '+'                                     
025710       MOVE MID-IDPERSON-IN   TO MOD-IDPERSON-UT                          
025711                                 W-IDPERSON                               
025712     ELSE                                                                 
025713       MOVE NEJ               TO INDATA-SW                                
025714       MOVE MFS-RENSA-FAELT   TO MOD-IDPERSON-UT                          
025715     END-IF                                                               
025716                                                                          
025717     IF INDATA-OK                                                         
025718       PERFORM IMS-GET-WDP301                                             
025719       IF SEGMENT-FINNS                                                   
025720         MOVE MFS-ALFA-FAELT-RAETT        TO MOD-KDARBTYP-ATTR            
025721         PERFORM IMS-GET-WDP311                                           
025722         IF SEGMENT-FINNS                                                 
025723           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDPERSON-ATTR            
025724           MOVE PERS-IDMAIL TO IDMAIL                                     
025725                                                                          
025726           IF SPAR-IDARTNR-ENTER > 0                                      
025727             MOVE SPAR-IDARTNR-ENTER     TO URV-IDARTNR                   
025728           END-IF                                                         
025729                                                                          
025730           IF SPAR-KDJUSTYP-ENTER > 0                                     
025731             MOVE SPAR-KDJUSTYP-ENTER   TO URV-KDJUSTYP                   
025732           END-IF                                                         
025733           MOVE W-IDDC                 TO URV-IDDC                        
025735           MOVE SPAR-IDUSER-ENTER      TO URV-IDUSER                      
025736           MOVE 'WDH7'                 TO URV-BAS                         
025740                                                                          
025741********   STARTA SOP ***********                                         
025742                                                                          
025743           MOVE '5312'   TO MSGSOP-IDTRANS                                
025744           MOVE '1'      TO MSGSOP-KDMFSFOR                               
025745           MOVE 'O'      TO MSGSOP-KDSOPFUNK                              
025746           MOVE 'W513S1' TO MSGSOP-IDPROCESS                              
025747                                                                          
025748           STRING 'IDUSER(' MSG-SIGNON-USERID ') URVAL1('                 
025749                            WS-URVAL1 ') MAIL('                           
025750                            WS-IDMAIL ')'                                 
025751           DELIMITED BY SIZE INTO MSGSOP-TESYMBV                          
025752           MOVE 'SENDING MAIL'            TO MOD-TEMFSINF                 
025753           PERFORM IMS-INSERT-ALTMSG                                      
025754           MOVE MFS-RENSA-FAELT           TO MOD-KDARBTYP-UT              
025755           MOVE MFS-RENSA-FAELT           TO MOD-IDPERSON-UT              
025756           MOVE ALL '+'                   TO MID-KDARBTYP-IN              
025757                                             MID-IDPERSON-IN              
025758         ELSE                                                             
025760           MOVE NEJ TO INDATA-SW                                          
025761           MOVE MID-IDPERSON-IN           TO MOD-IDPERSON-UT              
025762           MOVE ERR-WRONG-KEY             TO MED-IDMFSINF                 
025763           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDPERSON-ATTR            
025764           MOVE MFS-ALFA-FAELT-RAETT      TO MOD-KDARBTYP-ATTR            
025768         END-IF                                                           
025769       ELSE                                                               
025772         MOVE NEJ TO INDATA-SW                                            
025773         MOVE MID-KDARBTYP-IN             TO MOD-KDARBTYP-UT              
025774         MOVE ERR-WRONG-KEY               TO MED-IDMFSINF                 
025775         MOVE MFS-ALFA-FAELT-FEL          TO MOD-KDARBTYP-ATTR            
025776         MOVE MFS-ALFA-FAELT-RAETT        TO MOD-IDPERSON-ATTR            
025778       END-IF                                                             
025779     ELSE                                                                 
025780       MOVE ERR-WRONG-KEY                 TO MED-IDMFSINF                 
025781                                                                          
025782     END-IF                                                               
025783                                                                          
025784     .                                                                    
025785     EJECT                                                                
025786 S01-DATUM SECTION.                                                       
025790     MOVE ZERO               TO WS-ANTALDAGAR                             
025791     MOVE WS-DATUM-FOM       TO WORK-TIAAMMDD-FOM                         
025792     MOVE WS-DATUM-TOM       TO WORK-TIAAMMDD-TOM                         
025793     MOVE W-IDDC             TO WORK-IDDC                                 
025794     MOVE 001                TO WORK-KDCALL                               
025797                                                                          
025798     CALL WORKDAY  USING  WORK-KDCALL  WORK-DATE-AREA                     
025799                          WORK-KDSVAR                                     
025800     IF WORK-KDSVAR = SPACE                                               
025801       MOVE WORK-KVWORKD     TO WS-ANTALDAGAR                             
025802     END-IF                                                               
025803                                                                          
025804     .                                                                    
025805     EJECT                                                                
025810 MFS-RENSA-FAELT-UT SECTION.                                              
025900                                                                          
026000*    --- ALLA UTDATA-FÄLT                                                 
026110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
026200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                               
026300                             MOD-KDJUSTYP-UT                              
026320                             MOD-IDDC-UT                                  
026321     MOVE +1 TO INDX                                                      
026322     PERFORM UNTIL INDX > 14                                              
026330      MOVE MFS-RENSA-FAELT TO MOD-RAD-INFO (INDX)                         
026340      ADD +1 TO INDX                                                      
026350     END-PERFORM                                                          
026400     .                                                                    
026501     SKIP3                                                                
026502 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
026503                                                                          
026504*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
026505     MOVE MFS-RENSA-FAELT TO MOD-RAD-INFO (INDX)                          
026506                                                                          
026507                                                                          
026510     .                                                                    
026600     SKIP3                                                                
026700 MFS-RENSA-FAELT-IN SECTION.                                              
026800                                                                          
026900*    --- ALLA INDATA-FÄLT                                                 
027000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
027100                             MOD-KDJUSTYP-IN                              
027210     MOVE +1 TO INDX                                                      
027220     PERFORM UNTIL INDX > 14                                              
027230      MOVE MFS-RENSA-FAELT TO MOD-RAD-INFO (INDX)                         
027240      ADD +1 TO INDX                                                      
027250     END-PERFORM                                                          
027260     .                                                                    
027300     EJECT                                                                
027400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027500                                                                          
027600*    --- ALLA UTDATA-FÄLT                                                 
027710*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
027800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
027900                                 MOD-KDJUSTYP-UT                          
028001     MOVE +1 TO INDX                                                      
028002     PERFORM UNTIL INDX > MAX-INDX                                        
028003       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
028004       ADD +1 TO INDX                                                     
028005     END-PERFORM                                                          
028006     .                                                                    
028007     SKIP2                                                                
028008 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
028009                                                                          
028010*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
028011     MOVE MFS-ROER-EJ-FAELT TO MOD-RAD-INFO(INDX)                         
028012                                                                          
028013                                                                          
028020     .                                                                    
028200     SKIP3                                                                
028300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028400                                                                          
028500*    --- ALLA INDATA-FÄLT                                                 
028600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
028700                                 MOD-KDJUSTYP-IN                          
028800     .                                                                    
028900     EJECT                                                                
029000 MFS-FORM-ATTR SECTION.                                                   
029100                                                                          
029200*    --- ALLA INDATA-FÄLT                                                 
029300     MOVE MFS-FORMATETS-ATTR TO MOD-KDARBTYP-ATTR                         
029400                                MOD-IDPERSON-ATTR                         
029500     .                                                                    
029600     SKIP2                                                                
029700 MFS-LAES-IN-IGEN SECTION.                                                
029800                                                                          
029900*    --- ALLA INDATA-FÄLT                                                 
030000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDARBTYP-ATTR                      
030100                                   MOD-IDPERSON-ATTR                      
030200     .                                                                    
030300     EJECT                                                                
030400* --- IMS SEKTIONER ---                                                   
030500     SKIP3                                                                
030600 IMS-GET-MSG SECTION.                                                     
030700                                                                          
030800     MOVE '  QC' TO GODK-STATUSKODER                                      
030900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300     SKIP3                                                                
031400 IMS-INSERT-MSG SECTION.                                                  
031500                                                                          
031600                                                                          
031700     MOVE 'N' TO MFS-KDHUVOMR                                             
031800                                                                          
031900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032000     MOVE SPACE TO GODK-STATUSKODER                                       
032100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032300     PERFORM IMS-STATUSKONTROLL                                           
032400     .                                                                    
032501     EJECT                                                                
032502 IMS-INSERT-ALTMSG SECTION.                                               
032503                                                                          
032504     MOVE SPACE TO GODK-STATUSKODER                                       
032505     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
032506     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
032507     PERFORM IMS-STATUSKONTROLL                                           
032508     .                                                                    
032509     EJECT                                                                
032514                                                                          
032515 IMS-GET-WDP301 SECTION.                                                  
032516                                                                          
032517     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
032518          DELIMITED BY SIZE INTO SSA1                                     
032519     MOVE '  GE' TO GODK-STATUSKODER                                      
032520     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP301 SSA1                    
032521     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
032522     PERFORM IMS-STATUSKONTROLL                                           
032523     .                                                                    
032524     EJECT                                                                
032525 IMS-GET-WDP311 SECTION.                                                  
032526                                                                          
032527     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
032528          DELIMITED BY SIZE INTO SSA1                                     
032529     MOVE '  GE' TO GODK-STATUSKODER                                      
032530     CALL CBLTDLI USING GNP WDP3-PCB DLI-IO-WDP311 SSA1                   
032531     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
032532     PERFORM IMS-STATUSKONTROLL                                           
032533     .                                                                    
032534     EJECT                                                                
032535 IMS-GHU-WDH701      SECTION.                                             
032536     STRING 'WDH701  (IDARTNR  =' W-IDARTNR-X ')'                         
032537            DELIMITED BY SIZE INTO SSA1                                   
032538     MOVE '  GE' TO GODK-STATUSKODER                                      
032539     CALL  CBLTDLI  USING GHU WDH7-PCB DLI-IO-WDH701 SSA1                 
032540     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
032541     PERFORM IMS-STATUSKONTROLL                                           
032542     .                                                                    
032543     SKIP2                                                                
032544 IMS-GNP-WDH711 SECTION.                                                  
032545     STRING 'WDH711  (TISEGKEY<=' W-TISEGKEY-X                            
032546                    '&IDDC     =' W-IDDC-X ')'                            
032547            DELIMITED BY SIZE INTO SSA1                                   
032548     MOVE '  GE' TO GODK-STATUSKODER                                      
032549     CALL  CBLTDLI  USING GHNP WDH7-PCB DLI-IO-WDH711 SSA1                
032550     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
032551     PERFORM IMS-STATUSKONTROLL                                           
032552     .                                                                    
032560     SKIP2                                                                
032605                                                                          
032606 IMS-GNP-WDH711-UNIK SECTION.                                             
032607***  STRING 'WDH701  (IDARTNR  =' W-IDARTNR-X ')'                         
032608***         DELIMITED BY SIZE INTO SSA1                                   
032609     STRING 'WDH711  (WDH711KY =' W-WDH711KY-X ')'                        
032610                                                                          
032611            DELIMITED BY SIZE INTO SSA1                                   
032612     MOVE '  GE' TO GODK-STATUSKODER                                      
032620     CALL  CBLTDLI  USING GNP WDH7-PCB DLI-IO-WDH711 SSA1                 
032630     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
032640     PERFORM IMS-STATUSKONTROLL                                           
032650     .                                                                    
032660     SKIP2                                                                
032670                                                                          
032700 IMS-STATUSKONTROLL SECTION.                                              
032800                                                                          
032900     SET STATUS-IX TO 1                                                   
033000     SEARCH GODK-STATUS                                                   
033100       AT END                                                             
033200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033300         DELIMITED BY SIZE INTO FELTEXT                                   
033400         CALL FELLOG                                                      
033500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033600         CONTINUE                                                         
033700     END-SEARCH                                                           
033800     .                                                                    
