001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6023100.                                                
001300*AUTHOR.         ANN WESTBERG.                                            
001400*DATE-WRITTEN.   92/07/08.                                                
001500                                                                          
001600*    REMARKS.   BERÖR EJ SDC                                              
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        PROVPLAN                                                         
001910*                                                                         
002000*        UPPDATERING AV:                                                  
002100*        -SKIPLOT (SKALL 4 8 16 ELLER 32 FÖR REDUCERAT UTTAG)             
002101*        -AVISERAT ANTAL FR.O.M  (INTERVALL KONTROLL)                     
002102*        -AVISERAT ANTAL T.O.M       -"-                                  
002103*        -ANTAL ART. SOM SKALL KONTROLLERAS                               
002104*                                                                         
002110*        PROGRAMMET UPPDATERAR W6PROA (W6G1)                              
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W6T231                                              
002500*        MID:         W6I23101                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W6O23101                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003401                                                                          
003410*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W6023100'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004301*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004302 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004310 77  MAX-INDX                    PIC S9(4)  VALUE +5    COMP SYNC.        
004320 77  TAB-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
004330 77  TAB-MAX-INDX                PIC S9(4)  VALUE +5    COMP SYNC.        
004400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004500*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
004600*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
004700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +249 COMP SYNC.         
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005010 77  WS-IDPROVPL                 PIC 9      VALUE ZERO.                   
005020 77  WS-KDPROVPL                 PIC X      VALUE SPACE.                  
005030 77  WS-KVAVIS-FOM               PIC 9(7)   VALUE ZERO.                   
005040 77  WS-KVAVIS-TOM               PIC 9(7)   VALUE ZERO.                   
005050 77  WS-KVPROVPL                 PIC 9(6)   VALUE ZERO.                   
005062 77  HELTAL                      PIC 9(2)   VALUE ZERO.                   
005063 77  DECIMAL                     PIC 9(2)   VALUE ZERO.                   
005070 77  WS-INDX                     PIC 9(2)   VALUE ZERO.                   
005100                                                                          
005201 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005202     88  INDATA-OK                           VALUE 'J'.                   
005210     88  INDATA-FEL                          VALUE 'N'.                   
005300                                                                          
005310 77  KLAR-SW                     PIC X       VALUE 'J'.                   
005320     88  KLAR                                VALUE 'J'.                   
005330     88  EJ-KLAR                             VALUE 'N'.                   
005340                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005610                                                                          
005620 77  SORT-SW                     PIC X       VALUE 'J'.                   
005630     88  SORT-JA                             VALUE 'J'.                   
005640     88  SORT-NEJ                            VALUE 'N'.                   
005690                                                                          
005710 77  ENDAST-SKIPLOT-SW           PIC X       VALUE 'J'.                   
005720     88  ENDAST-SKIPLOT                      VALUE 'J'.                   
005730     88  EJ-ENDAST-SKIPLOT                   VALUE 'N'.                   
005740                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '6231'.                
006000     88  GODK-MID                            VALUE '6231' '6232'          
006100                                                   '6233' '6234'          
006200                                                   '6235' '6236'          
006300                                                   '6237' '6238'          
006400                                                   '6239'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007801     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007802     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007803     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007804     03  ERR-NO-UPDATE           PIC X(3)    VALUE '007'.                 
007810     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007901     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007910     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     03  ERR-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
008200     EJECT                                                                
008300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008600     SKIP3                                                                
008700*01  MID -COPY W6I23101                                                   
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009000     SKIP3                                                                
009100*01  -COPY WMSGAREA                                                       
009200     EJECT                                                                
009300     03  MOD REDEFINES MSG-AREA.                                          
009400*      05  -COPY W6O23101                                                 
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009700     SKIP3                                                                
009800*01  -COPY WMFSAREA                                                       
009900     EJECT                                                                
010000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010100*                                                                         
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010601     03  W-W6GXKEY-6101-X.                                                
010602         05  FILLER              PIC X(4)     VALUE '6101'.               
010603         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
010604     03  W-W6GXKEY-6102-X.                                                
010610         05  W-IDPROVPL          PIC 9       VALUE  ZERO.                 
010620         05  W-KDPROVPL          PIC X       VALUE SPACE.                 
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012300     EJECT                                                                
012400*    ---  DLI INPUT-OUTPUT AREA                                           
012500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012600     SKIP3                                                                
012700 01  DLI-IO-AREA.                                                         
012800     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012901     SKIP3                                                                
012902     03  W6PROA01 REDEFINES IO-AREA.                                      
012903*        05  -COPY W6GX01  -PRE PROA-                                     
012904     SKIP3                                                                
012905     03  W6PROA11 REDEFINES IO-AREA.                                      
012910*        05  -COPY W6GX6102  -PRE PROA-                                   
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0009   -PRE MSG-                                              
013601     EJECT                                                                
013602*01  -COPY W0008  -PRE PROA-                                              
013610     05  FILLER                  PIC X.                                   
013700     EJECT                                                                
013801 PROCEDURE DIVISION  USING MSG-PCB PROA-PCB.                              
013810     ENTRY 'DLITCBL' USING MSG-PCB PROA-PCB.                              
013900                                                                          
014100     PERFORM IMS-GET-MSG                                                  
014200     IF SEGMENT-FINNS                                                     
014300       PERFORM A-INIT                                                     
014400       PERFORM B-KOLLA-NYCKLAR                                            
014500       IF NYCKLAR-OK                                                      
014601         IF MFS-UPDATE                                                    
014602           PERFORM G-KOLLA-INPUT                                          
014603           IF INDATA-OK                                                   
014604             PERFORM H-UPPDATERA                                          
014605             PERFORM F-LAES-VISA-INFO                                     
014606           END-IF                                                         
014610         ELSE                                                             
014701           IF MFS-FIRST                                                   
014702             PERFORM C-FOERSTA-SIDA                                       
014703           ELSE                                                           
014704             IF MFS-NEXT                                                  
014705               PERFORM D-NAESTA-SIDA                                      
014706             ELSE                                                         
014707               PERFORM E-SAMMA-SIDA                                       
014708             END-IF                                                       
014710           END-IF                                                         
014900           PERFORM F-LAES-VISA-INFO                                       
015010         END-IF                                                           
015100       END-IF                                                             
015200       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
015300       PERFORM IMS-INSERT-MSG                                             
015400     END-IF                                                               
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200                                                                          
016300     IF MSG-DUBBLA-TRANSKODER                                             
016400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I23101                 
016500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
016600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016700     ELSE                                                                 
016800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I23101                  
016900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
017000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017100     END-IF                                                               
017200                                                                          
017300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
017500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017600                                                                          
017700     MOVE LOW-VALUE TO MSG-AREA                                           
017800     MOVE 'W6O231N1' TO MFS-IDMOD                                         
017900     MOVE '6231' TO MOD-IDTRANS                                           
018000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018100                                                                          
018200     IF EGEN-MID OR HELP-MID                                              
018300       CONTINUE                                                           
018400     ELSE                                                                 
018500       MOVE SPACE TO MFS-KDTRTYP                                          
018600       MOVE '7' TO MFS-IDPFK                                              
018700     END-IF                                                               
018800                                                                          
018900     IF ENGLISH-TEXT                                                      
019000       MOVE +2 TO SPRAK-IX                                                
019100       MOVE 'GB ' TO MED-IDSKYLT                                          
019200     ELSE                                                                 
019300       MOVE +1 TO SPRAK-IX                                                
019400       MOVE 'S  ' TO MED-IDSKYLT                                          
019500     END-IF                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 B-KOLLA-NYCKLAR SECTION.                                                 
020100                                                                          
020200     MOVE JA TO NYCKLAR-SW                                                
020303     MOVE MFS-RENSA-FAELT TO MOD-IDPROVPL-IN                              
020304                             MOD-KDPROVPL-IN                              
020305                                                                          
020306     IF MID-IDPROVPL-IN = ALL '+'                                         
020307       MOVE MID-IDPROVPL-UT TO WS-IDPROVPL                                
020308     ELSE                                                                 
020309       MOVE MID-IDPROVPL-IN TO WS-IDPROVPL                                
020310       MOVE '7'         TO MFS-IDPFK                                      
020311       MOVE SPACE       TO MFS-KDTRTYP                                    
020312     END-IF                                                               
020320     MOVE WS-IDPROVPL TO W-IDPROVPL                                       
020330                                                                          
020402     IF MID-KDPROVPL-IN = '+'                                             
020403       MOVE MID-KDPROVPL-UT TO WS-KDPROVPL                                
020405     ELSE                                                                 
020406       MOVE MID-KDPROVPL-IN TO WS-KDPROVPL                                
020407       MOVE '7'         TO MFS-IDPFK                                      
020408       MOVE SPACE       TO MFS-KDTRTYP                                    
020409     END-IF                                                               
020410     MOVE WS-KDPROVPL TO W-KDPROVPL                                       
020411                                                                          
020414                                                                          
020420     IF WS-IDPROVPL NUMERIC                                               
020421       IF WS-KDPROVPL = 'N' OR 'R'                                        
020422         MOVE JA TO NYCKLAR-SW                                            
020423       ELSE                                                               
020424         MOVE NEJ TO NYCKLAR-SW                                           
020425       END-IF                                                             
020426     ELSE                                                                 
020427       MOVE NEJ TO NYCKLAR-SW                                             
020428     END-IF                                                               
020429                                                                          
020438     IF GODK-MID OR NYCKLAR-OK                                            
020439       MOVE WS-IDPROVPL TO MOD-IDPROVPL-UT                                
020440       MOVE WS-KDPROVPL TO MOD-KDPROVPL-UT                                
020441     ELSE                                                                 
020442       MOVE MFS-RENSA-FAELT TO MOD-IDPROVPL-UT                            
020443                               MOD-KDPROVPL-UT                            
020450     END-IF                                                               
020500                                                                          
020600     IF NYCKLAR-FEL                                                       
020700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
020800       CALL WMEDKONV USING MED-WMEDAREA                                   
020900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021000       PERFORM MFS-RENSA-FAELT-IN                                         
021100       PERFORM MFS-RENSA-FAELT-UT                                         
021200     END-IF                                                               
021300     .                                                                    
021401     EJECT                                                                
021402 C-FOERSTA-SIDA SECTION.                                                  
021407                                                                          
021415     PERFORM MFS-RENSA-FAELT-IN                                           
021416     .                                                                    
021417     EJECT                                                                
021418 D-NAESTA-SIDA SECTION.                                                   
021419                                                                          
021426     PERFORM MFS-RENSA-FAELT-IN                                           
021427     .                                                                    
021428     EJECT                                                                
021429 E-SAMMA-SIDA SECTION.                                                    
021430                                                                          
021431     IF EGEN-MID OR HELP-MID                                              
021438       IF MID-INPUT = ALL '+'                                             
021439         PERFORM MFS-RENSA-FAELT-IN                                       
021440       ELSE                                                               
021445         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
021446         CALL WMEDKONV USING MED-WMEDAREA                                 
021447         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
021448         PERFORM EA-MID-INDATA-TILL-MOD                                   
021450       END-IF                                                             
021451     ELSE                                                                 
021452       PERFORM MFS-RENSA-FAELT-IN                                         
021453     END-IF                                                               
021454     .                                                                    
021455     EJECT                                                                
021456 EA-MID-INDATA-TILL-MOD SECTION.                                          
021457                                                                          
021475     IF MID-KVSKPLOT-UPD NOT = ALL '+'                                    
021476       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVSKPLOT-ATTR                    
021477       MOVE MID-KVSKPLOT-UPD TO MOD-KVSKPLOT-UPD                          
021478     ELSE                                                                 
021479       MOVE MFS-RENSA-FAELT       TO MOD-KVSKPLOT-UPD                     
021481     END-IF                                                               
021482                                                                          
021483     IF MID-KVAVIS-FOM-UPD NOT = ALL '+'                                  
021484       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAVIS-FOM-ATTR                  
021485       MOVE MID-KVAVIS-FOM-UPD    TO MOD-KVAVIS-FOM-UPD                   
021486     ELSE                                                                 
021487       MOVE MFS-RENSA-FAELT       TO MOD-KVAVIS-FOM-UPD                   
021488     END-IF                                                               
021489                                                                          
021490     IF MID-KVAVIS-TOM-UPD NOT = ALL '+'                                  
021491       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAVIS-TOM-ATTR                  
021492       MOVE MID-KVAVIS-TOM-UPD   TO MOD-KVAVIS-TOM-UPD                    
021493     ELSE                                                                 
021494       MOVE MFS-RENSA-FAELT       TO MOD-KVAVIS-TOM-UPD                   
021495     END-IF                                                               
021496                                                                          
021497     IF MID-KVPROVPL-UPD NOT = ALL '+'                                    
021498       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPROVPL-ATTR                    
021499       MOVE MID-KVPROVPL-UPD      TO MOD-KVPROVPL-UPD                     
021500     ELSE                                                                 
021501       MOVE MFS-RENSA-FAELT       TO MOD-KVPROVPL-UPD                     
021502     END-IF                                                               
021503                                                                          
021504     IF MID-KDCMD-UPD NOT = ALL '+'                                       
021505       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR                       
021506       MOVE MID-KDCMD-UPD         TO MOD-KDCMD-UPD                        
021507     ELSE                                                                 
021508       MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-UPD                        
021509     END-IF                                                               
021510     .                                                                    
021600     EJECT                                                                
021700 F-LAES-VISA-INFO SECTION.                                                
021800                                                                          
021910     PERFORM IMS-GET-PROA-6101                                            
022000                                                                          
022100     IF SEGMENT-SAKNAS                                                    
022200       MOVE ERR-URVAL-SAKNAS   TO MED-IDMFSINF                            
022300       CALL WMEDKONV USING MED-WMEDAREA                                   
022310       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
022600     ELSE                                                                 
022701       MOVE +1 TO INDX                                                    
022702       PERFORM IMS-GET-PROA-6102                                          
022703       IF SEGMENT-SAKNAS                                                  
022704          MOVE ERR-URVAL-SAKNAS   TO MED-IDMFSINF                         
022705          CALL WMEDKONV USING MED-WMEDAREA                                
022706          MOVE MED-MFSINF TO MOD-TEMFSFEL                                 
022708       END-IF                                                             
022709       IF WS-KDPROVPL = 'N'                                               
022710         IF ENGLISH-TEXT                                                  
022711           MOVE 'Nr of controls  ' TO MOD-TEXT-UT                         
022712         ELSE                                                             
022713           MOVE 'Antal kontroller' TO MOD-TEXT-UT                         
022714         END-IF                                                           
022720       ELSE                                                               
022721         IF ENGLISH-TEXT                                                  
022722           MOVE 'Skiplotfrequence' TO MOD-TEXT-UT                         
022723         ELSE                                                             
022725           MOVE 'Skiplot-frekvens' TO MOD-TEXT-UT                         
022726         END-IF                                                           
022728       END-IF                                                             
022729       PERFORM UNTIL INDX  > MAX-INDX                                     
022730         IF SEGMENT-FINNS                                                 
022739           IF PROA-6102-KVAVIS-FOM (INDX) NOT = ALL ZERO                  
022743               MOVE PROA-6102-KVAVIS-FOM  (INDX)                          
022744                                    TO MOD-KVAVIS-FOM (INDX)              
022745               MOVE PROA-6102-KVAVIS-TOM  (INDX)                          
022746                                    TO MOD-KVAVIS-TOM (INDX)              
022747               MOVE PROA-6102-KVPROVPL (INDX)                             
022748                                    TO MOD-KVPROVPL (INDX)                
022758           END-IF                                                         
022764           MOVE PROA-6102-KVSKPLOT      TO MOD-KVSKPLOT                   
022766           ADD +1 TO INDX                                                 
022782         ELSE                                                             
022783           MOVE MFS-RENSA-FAELT TO MOD-KVAVIS-FOM (INDX)                  
022787                                   MOD-KVAVIS-TOM (INDX)                  
022788                                   MOD-KVPROVPL (INDX)                    
022789           ADD +1 TO INDX                                                 
022790         END-IF                                                           
022791       END-PERFORM                                                        
022810     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023902 G-KOLLA-INPUT SECTION.                                                   
023903                                                                          
023915     MOVE JA  TO INDATA-SW                                                
023916                                                                          
023917     IF MID-INPUT = ALL '+'                                               
023918       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
023919       CALL WMEDKONV USING MED-WMEDAREA                                   
023920       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023921       MOVE NEJ TO INDATA-SW                                              
023923     ELSE                                                                 
023924       PERFORM IMS-GET-PROA-6101                                          
023925       IF SEGMENT-FINNS                                                   
023926         PERFORM IMS-GET-PROA-6102                                        
023927         IF SEGMENT-FINNS                                                 
023948           PERFORM GA-KONTR-SKIPLOT                                       
023949           PERFORM GB-KONTR-INFAELT                                       
023953           IF INDATA-OK AND EJ-ENDAST-SKIPLOT                             
023954             IF MID-KDCMD-UPD = ALL '+'                                   
023956               PERFORM GC-KONTR-ANTAL                                     
023957               IF INDATA-OK AND SORT-JA                                   
023958                 PERFORM GD-KONTR-INTERVALL                               
023959                 IF INDATA-OK                                             
023960                   IF INDX < +6                                           
023961                     PERFORM GE-SORTERA-TABELL                            
023962                   ELSE                                                   
023963                     MOVE NEJ TO INDATA-SW                                
023964                     PERFORM MFS-ROER-EJ-FAELT-UT                         
023965                     PERFORM MFS-RENSA-FAELT-IN                           
023967                     MOVE ERR-NO-UPDATE TO MED-IDMFSFEL                   
023968                     CALL WMEDKONV USING MED-WMEDAREA                     
023969                     MOVE MED-MFSFEL TO MOD-TEMFSFEL                      
023972                   END-IF                                                 
023978                 END-IF                                                   
023979               END-IF                                                     
023980             ELSE                                                         
023981               PERFORM GF-HITTA-I-TAB-DLET                                
023982             END-IF                                                       
023984           END-IF                                                         
023985         ELSE                                                             
023986           PERFORM GA-KONTR-SKIPLOT                                       
023987           PERFORM GB-KONTR-INFAELT                                       
024000         END-IF                                                           
024003       END-IF                                                             
024004     END-IF                                                               
024013     .                                                                    
024014     EJECT                                                                
024015 GA-KONTR-SKIPLOT SECTION.                                                
024016                                                                          
024017     MOVE JA TO INDATA-SW                                                 
024020                                                                          
024021     IF MID-KVSKPLOT-UPD NOT = ALL '+'                                    
024022       IF MID-KVSKPLOT-UPD NUMERIC                                        
024023         IF WS-KDPROVPL = 'R'                                             
024024           IF MID-KVSKPLOT-UPD = 4 OR 8 OR 16 OR 32                       
024025             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKPLOT-ATTR                
024026             MOVE MID-KVSKPLOT-UPD    TO PROA-6102-KVSKPLOT               
024052           ELSE                                                           
024055             MOVE MFS-NUM-FAELT-FEL TO MOD-KVSKPLOT-ATTR                  
024056             MOVE NEJ TO INDATA-SW                                        
024059           END-IF                                                         
024060         ELSE                                                             
024061           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKPLOT-ATTR                  
024062           MOVE MID-KVSKPLOT-UPD  TO PROA-6102-KVSKPLOT                   
024067         END-IF                                                           
024068       ELSE                                                               
024069         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVSKPLOT-ATTR                  
024070         MOVE NEJ TO INDATA-SW                                            
024071       END-IF                                                             
024072     ELSE                                                                 
024073       IF SEGMENT-FINNS                                                   
024074         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVSKPLOT-ATTR                    
024079       ELSE                                                               
024080         MOVE MFS-NUM-FAELT-FEL       TO MOD-KVSKPLOT-ATTR                
024081         MOVE NEJ TO INDATA-SW                                            
024082       END-IF                                                             
024083     END-IF                                                               
024101     .                                                                    
024102     EJECT                                                                
024115 GB-KONTR-INFAELT SECTION.                                                
024118                                                                          
024119     IF MID-KVAVIS-FOM-UPD NOT = ALL '+'                                  
024120       IF MID-KVAVIS-FOM-UPD NOT NUMERIC                                  
024121         MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-FOM-ATTR                    
024122         MOVE NEJ TO INDATA-SW                                            
024123       ELSE                                                               
024124          IF MID-KVAVIS-FOM-UPD = ZERO                                    
024125            MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-FOM-ATTR                 
024126            MOVE NEJ TO INDATA-SW                                         
024127          ELSE                                                            
024128            MOVE MFS-NUM-FAELT-RAETT TO MOD-KVAVIS-FOM-ATTR               
024129          END-IF                                                          
024130       END-IF                                                             
024131     END-IF                                                               
024132                                                                          
024133     IF MID-KVAVIS-TOM-UPD NOT = ALL '+'                                  
024134       IF MID-KVAVIS-TOM-UPD NOT NUMERIC                                  
024135         MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-TOM-ATTR                    
024136         MOVE NEJ TO INDATA-SW                                            
024137       ELSE                                                               
024138          IF MID-KVAVIS-TOM-UPD = ZERO                                    
024139            MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-TOM-ATTR                 
024140            MOVE NEJ TO INDATA-SW                                         
024141          ELSE                                                            
024142            MOVE MFS-NUM-FAELT-RAETT TO MOD-KVAVIS-TOM-ATTR               
024143          END-IF                                                          
024144       END-IF                                                             
024145     END-IF                                                               
024146                                                                          
024147     IF MID-KVPROVPL-UPD NOT = ALL '+'                                    
024148       IF MID-KVPROVPL-UPD NOT NUMERIC                                    
024149         MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROVPL-ATTR                      
024150         MOVE NEJ TO INDATA-SW                                            
024151       ELSE                                                               
024152         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPROVPL-ATTR                    
024153       END-IF                                                             
024154     END-IF                                                               
024155                                                                          
024156     IF MID-KDCMD-UPD NOT = ALL '+'                                       
024157       IF MID-KDCMD-UPD  = 'D' OR 'B'                                     
024158         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR                      
024159       ELSE                                                               
024160         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR                        
024161         MOVE NEJ TO INDATA-SW                                            
024162       END-IF                                                             
024163     END-IF                                                               
024164                                                                          
024165     IF INDATA-OK                                                         
024166       IF MID-KVSKPLOT-UPD NOT = ALL '+' AND                              
024167          MID-KVAVIS-FOM-UPD   = ALL '+' AND                              
024168          MID-KVAVIS-TOM-UPD   = ALL '+' AND                              
024169          MID-KVPROVPL-UPD     = ALL '+' AND                              
024170          MID-KDCMD-UPD        = ALL '+'                                  
024171         MOVE JA                    TO ENDAST-SKIPLOT-SW                  
024172       ELSE                                                               
024173         MOVE NEJ                   TO ENDAST-SKIPLOT-SW                  
024174         IF MID-KVAVIS-FOM-UPD = ALL '+'                                  
024175           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVAVIS-FOM-ATTR                
024176           MOVE NEJ                 TO INDATA-SW                          
024177         ELSE                                                             
024178           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVAVIS-FOM-ATTR                
024179         END-IF                                                           
024180         IF MID-KDCMD-UPD = ALL '+'                                       
024181           IF MID-KVPROVPL-UPD = ALL '+'                                  
024182             MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROVPL-ATTR                  
024183             MOVE NEJ               TO INDATA-SW                          
024184           ELSE                                                           
024185             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPROVPL-ATTR                
024186           END-IF                                                         
024187         END-IF                                                           
024188       END-IF                                                             
024189     END-IF                                                               
024190                                                                          
024191     IF INDATA-FEL                                                        
024192       MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                         
024193       CALL WMEDKONV USING MED-WMEDAREA                                   
024194       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024195       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024196       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024197     END-IF                                                               
024198     .                                                                    
024199     EJECT                                                                
024200 GC-KONTR-ANTAL SECTION.                                                  
024201                                                                          
024202     MOVE +1 TO INDX                                                      
024203     PERFORM UNTIL INDX > 5                                               
024204       IF MID-KVAVIS-TOM-UPD = ALL '+'                                    
024205         IF MID-KVAVIS-FOM-UPD = PROA-6102-KVAVIS-FOM (INDX)              
024206           IF MID-KVPROVPL-UPD NOT = PROA-6102-KVPROVPL(INDX)             
024207             MOVE MID-KVPROVPL-UPD    TO PROA-6102-KVPROVPL(INDX)         
024208             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVAVIS-FOM-ATTR              
024209                                         MOD-KVPROVPL-ATTR                
024210             MOVE JA  TO INDATA-SW                                        
024211             MOVE +6  TO INDX                                             
024212           ELSE                                                           
024213             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVPROVPL-ATTR                
024214             MOVE NEJ TO INDATA-SW                                        
024215           END-IF                                                         
024216         ELSE                                                             
024217           MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-FOM-ATTR                  
024218           MOVE NEJ TO INDATA-SW                                          
024219         END-IF                                                           
024220         MOVE NEJ TO SORT-SW                                              
024221       ELSE                                                               
024222         IF MID-KVAVIS-FOM-UPD = PROA-6102-KVAVIS-FOM (INDX)              
024223           IF MID-KVAVIS-TOM-UPD = PROA-6102-KVAVIS-TOM (INDX)            
024224             IF MID-KVPROVPL-UPD NOT = PROA-6102-KVPROVPL(INDX)           
024226               MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVAVIS-FOM-ATTR           
024227                                            MOD-KVAVIS-TOM-ATTR           
024228                                            MOD-KVPROVPL-ATTR             
024229               MOVE MID-KVPROVPL-UPD TO PROA-6102-KVPROVPL(INDX)          
024231               MOVE JA  TO INDATA-SW                                      
024233             ELSE                                                         
024234               MOVE MFS-NUM-FAELT-FEL   TO MOD-KVPROVPL-ATTR              
024235               MOVE NEJ TO INDATA-SW                                      
024238             END-IF                                                       
024239             MOVE +6  TO INDX                                             
024240             MOVE NEJ TO SORT-SW                                          
024241           END-IF                                                         
024244         END-IF                                                           
024245       END-IF                                                             
024246       ADD +1 TO INDX                                                     
024247     END-PERFORM                                                          
024248                                                                          
024249     IF INDATA-FEL                                                        
024251       MOVE ERR-NO-UPDATE TO MED-IDMFSFEL                                 
024252       CALL WMEDKONV USING MED-WMEDAREA                                   
024253       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024254       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024255       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024256     END-IF                                                               
024257     .                                                                    
024258     EJECT                                                                
024259 GD-KONTR-INTERVALL SECTION.                                              
024260                                                                          
024261     MOVE +1 TO INDX                                                      
024262     PERFORM UNTIL INDX > +5                                              
024263       IF ((PROA-6102-KVAVIS-FOM (INDX) < MID-KVAVIS-FOM-UPD OR           
024264           PROA-6102-KVAVIS-FOM (INDX) = MID-KVAVIS-FOM-UPD) AND          
024265          (PROA-6102-KVAVIS-TOM (INDX) > MID-KVAVIS-FOM-UPD  OR           
024266           PROA-6102-KVAVIS-TOM (INDX) = MID-KVAVIS-FOM-UPD))             
024267         MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-FOM-ATTR                    
024268         MOVE NEJ TO INDATA-SW                                            
024269       END-IF                                                             
024270                                                                          
024271       IF ((PROA-6102-KVAVIS-FOM (INDX) < MID-KVAVIS-TOM-UPD OR           
024272           PROA-6102-KVAVIS-FOM (INDX) = MID-KVAVIS-TOM-UPD) AND          
024273          (PROA-6102-KVAVIS-TOM (INDX) > MID-KVAVIS-TOM-UPD  OR           
024274           PROA-6102-KVAVIS-TOM (INDX) = MID-KVAVIS-TOM-UPD))             
024275         MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-TOM-ATTR                    
024276         MOVE NEJ TO INDATA-SW                                            
024277       END-IF                                                             
024278                                                                          
024279       IF (MID-KVAVIS-FOM-UPD < PROA-6102-KVAVIS-FOM (1)) AND             
024280          (MID-KVAVIS-TOM-UPD > PROA-6102-KVAVIS-FOM (1))                 
024281         MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-TOM-ATTR                    
024282         MOVE NEJ TO INDATA-SW                                            
024283       END-IF                                                             
024284                                                                          
024285       IF MID-KVAVIS-FOM-UPD > MID-KVAVIS-TOM-UPD                         
024286         MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-FOM-ATTR                    
024287         MOVE NEJ TO INDATA-SW                                            
024288       END-IF                                                             
024289                                                                          
024290       IF MID-KVAVIS-FOM-UPD = MID-KVAVIS-TOM-UPD                         
024291         MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-FOM-ATTR                    
024292         MOVE MFS-NUM-FAELT-FEL TO MOD-KVAVIS-TOM-ATTR                    
024293         MOVE NEJ TO INDATA-SW                                            
024294       END-IF                                                             
024295       ADD +1 TO INDX                                                     
024296     END-PERFORM                                                          
024297                                                                          
024298     IF INDATA-FEL                                                        
024299       MOVE ERR-NO-UPDATE TO MED-IDMFSFEL                                 
024300       CALL WMEDKONV USING MED-WMEDAREA                                   
024301       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024302       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024303       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024304     END-IF                                                               
024305                                                                          
024306     MOVE +1 TO INDX                                                      
024307     MOVE NEJ TO KLAR-SW                                                  
024308     PERFORM UNTIL (INDX > TAB-MAX-INDX) OR (KLAR)                        
024309       IF PROA-6102-KVAVIS-TOM (INDX) = ALL ZERO                          
024310         MOVE JA TO KLAR-SW                                               
024311       ELSE                                                               
024312         ADD +1 TO INDX                                                   
024313       END-IF                                                             
024314     END-PERFORM                                                          
024315     .                                                                    
024316     EJECT                                                                
024317 GE-SORTERA-TABELL SECTION.                                               
024318                                                                          
024319     IF MID-KVAVIS-FOM-UPD < PROA-6102-KVAVIS-FOM (1)  OR                 
024320       (PROA-6102-KVAVIS-FOM (1) = ALL ZERO)                              
024321       PERFORM GEA-FLYTTA-4-TILL-5                                        
024322       PERFORM GEB-FLYTTA-3-TILL-4                                        
024323       PERFORM GEC-FLYTTA-2-TILL-3                                        
024324       PERFORM GED-FLYTTA-1-TILL-2                                        
024325       MOVE +1 TO INDX                                                    
024326       PERFORM GEE-FLYTTA-MID-TILL-TAB                                    
024327     ELSE                                                                 
024328       IF (MID-KVAVIS-FOM-UPD < PROA-6102-KVAVIS-FOM (2))  OR             
024329          (PROA-6102-KVAVIS-FOM (2) = ALL ZERO)                           
024330         PERFORM GEA-FLYTTA-4-TILL-5                                      
024331         PERFORM GEB-FLYTTA-3-TILL-4                                      
024332         PERFORM GEC-FLYTTA-2-TILL-3                                      
024333         MOVE +2 TO INDX                                                  
024334         PERFORM GEE-FLYTTA-MID-TILL-TAB                                  
024335       ELSE                                                               
024336         IF (MID-KVAVIS-FOM-UPD < PROA-6102-KVAVIS-FOM (3)) OR            
024337            (PROA-6102-KVAVIS-FOM (3) = ALL ZERO)                         
024338           PERFORM GEA-FLYTTA-4-TILL-5                                    
024339           PERFORM GEB-FLYTTA-3-TILL-4                                    
024340           MOVE +3 TO INDX                                                
024341           PERFORM GEE-FLYTTA-MID-TILL-TAB                                
024342         ELSE                                                             
024343           IF (MID-KVAVIS-FOM-UPD < PROA-6102-KVAVIS-FOM (4))             
024344              OR                                                          
024345              (PROA-6102-KVAVIS-FOM (4) = ALL ZERO)                       
024346             PERFORM GEA-FLYTTA-4-TILL-5                                  
024347             MOVE +4 TO INDX                                              
024348             PERFORM GEE-FLYTTA-MID-TILL-TAB                              
024349           ELSE                                                           
024350             MOVE +5 TO INDX                                              
024351             PERFORM GEE-FLYTTA-MID-TILL-TAB                              
024352           END-IF                                                         
024353         END-IF                                                           
024354       END-IF                                                             
024355     END-IF                                                               
024356     .                                                                    
024357     EJECT                                                                
024358 GEA-FLYTTA-4-TILL-5 SECTION.                                             
024359                                                                          
024360     MOVE PROA-6102-KVAVIS-FOM(4) TO PROA-6102-KVAVIS-FOM(5)              
024361     MOVE PROA-6102-KVAVIS-TOM(4) TO PROA-6102-KVAVIS-TOM(5)              
024362     MOVE PROA-6102-KVPROVPL  (4) TO PROA-6102-KVPROVPL  (5)              
024363     .                                                                    
024364     EJECT                                                                
024365 GEB-FLYTTA-3-TILL-4 SECTION.                                             
024366                                                                          
024367     MOVE PROA-6102-KVAVIS-FOM(3) TO PROA-6102-KVAVIS-FOM(4)              
024368     MOVE PROA-6102-KVAVIS-TOM(3) TO PROA-6102-KVAVIS-TOM(4)              
024369     MOVE PROA-6102-KVPROVPL  (3) TO PROA-6102-KVPROVPL  (4)              
024370     .                                                                    
024371     EJECT                                                                
024372 GEC-FLYTTA-2-TILL-3 SECTION.                                             
024373                                                                          
024374     MOVE PROA-6102-KVAVIS-FOM(2) TO PROA-6102-KVAVIS-FOM(3)              
024375     MOVE PROA-6102-KVAVIS-TOM(2) TO PROA-6102-KVAVIS-TOM(3)              
024376     MOVE PROA-6102-KVPROVPL  (2) TO PROA-6102-KVPROVPL  (3)              
024377     .                                                                    
024378     EJECT                                                                
024379 GED-FLYTTA-1-TILL-2 SECTION.                                             
024380                                                                          
024381     MOVE PROA-6102-KVAVIS-FOM(1) TO PROA-6102-KVAVIS-FOM(2)              
024382     MOVE PROA-6102-KVAVIS-TOM(1) TO PROA-6102-KVAVIS-TOM(2)              
024383     MOVE PROA-6102-KVPROVPL  (1) TO PROA-6102-KVPROVPL  (2)              
024384     .                                                                    
024385     EJECT                                                                
024386 GEE-FLYTTA-MID-TILL-TAB SECTION.                                         
024387                                                                          
024388     MOVE MID-KVAVIS-FOM-UPD TO PROA-6102-KVAVIS-FOM(INDX)                
024389     MOVE MID-KVAVIS-TOM-UPD TO PROA-6102-KVAVIS-TOM(INDX)                
024390     MOVE MID-KVPROVPL-UPD   TO PROA-6102-KVPROVPL(INDX)                  
024391     .                                                                    
024392     EJECT                                                                
024393 GF-HITTA-I-TAB-DLET SECTION.                                             
024394                                                                          
024395     MOVE +1 TO INDX                                                      
024396     MOVE NEJ TO KLAR-SW                                                  
024397     PERFORM UNTIL (INDX > TAB-MAX-INDX) OR (KLAR)                        
024398       IF MID-KVAVIS-FOM-UPD = PROA-6102-KVAVIS-FOM (INDX)                
024399         MOVE JA TO KLAR-SW                                               
024400       ELSE                                                               
024401         ADD +1 TO INDX                                                   
024402       END-IF                                                             
024403     END-PERFORM                                                          
024404                                                                          
024405     IF INDX > +5                                                         
024406       MOVE ERR-URVAL-SAKNAS   TO MED-IDMFSINF                            
024407       CALL WMEDKONV USING MED-WMEDAREA                                   
024408       MOVE MED-MFSINF         TO MOD-TEMFSFEL                            
024409       MOVE NEJ                TO INDATA-SW                               
024410       MOVE MFS-NUM-FAELT-FEL  TO MOD-KVAVIS-FOM-ATTR                     
024411       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024412       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024413     END-IF                                                               
024414     .                                                                    
024415     EJECT                                                                
024416 H-UPPDATERA SECTION.                                                     
024417                                                                          
024418     IF SEGMENT-FINNS                                                     
024419       IF MID-KDCMD-UPD = 'D' OR 'B'                                      
024420         IF INDX = +1  AND                                                
024421         PROA-6102-KVAVIS-FOM (+2) = ALL ZERO AND                         
024422         PROA-6102-KVAVIS-TOM (+2) = ALL ZERO AND                         
024423         PROA-6102-KVPROVPL   (+2) = ALL ZERO                             
024424           PERFORM IMS-DLET-PROA-6102                                     
024425         ELSE                                                             
024426           SET PROA-6102-AVIS-IX TO INDX                                  
024427           MOVE ALL ZERO TO                                               
024428                      PROA-6102-KVAVIS-FOM(PROA-6102-AVIS-IX)             
024429                      PROA-6102-KVAVIS-TOM(PROA-6102-AVIS-IX)             
024430                      PROA-6102-KVPROVPL  (PROA-6102-AVIS-IX)             
024431           PERFORM HB-RAETTA-TILL-TABELL                                  
024432           PERFORM IMS-REPL-PROA-6102                                     
024433         END-IF                                                           
024434       ELSE                                                               
024435         PERFORM IMS-REPL-PROA-6102                                       
024436       END-IF                                                             
024437     ELSE                                                                 
024438       MOVE WS-IDPROVPL        TO PROA-6102-IDPROVPL                      
024439       MOVE WS-KDPROVPL        TO PROA-6102-KDPROVPL                      
024440       MOVE MID-KVSKPLOT-UPD   TO PROA-6102-KVSKPLOT                      
024441       MOVE MID-KVAVIS-FOM-UPD TO PROA-6102-KVAVIS-FOM(+1)                
024442       MOVE MID-KVAVIS-TOM-UPD TO PROA-6102-KVAVIS-TOM(+1)                
024443       MOVE MID-KVPROVPL-UPD   TO PROA-6102-KVPROVPL  (+1)                
024444       PERFORM HC-INITIERA-TABELL                                         
024445       PERFORM IMS-ISRT-PROA-6102                                         
024446     END-IF                                                               
024447                                                                          
024448     MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                            
024449     CALL WMEDKONV USING MED-WMEDAREA                                     
024450     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
024451     PERFORM MFS-RENSA-FAELT-IN                                           
024452     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
024453     .                                                                    
024454     EJECT                                                                
024455 HB-RAETTA-TILL-TABELL SECTION.                                           
024456                                                                          
024457     IF INDX = +1                                                         
024458       MOVE PROA-6102-KVAVIS-FOM (2) TO PROA-6102-KVAVIS-FOM (1)          
024459       MOVE PROA-6102-KVAVIS-TOM (2) TO PROA-6102-KVAVIS-TOM (1)          
024460       MOVE PROA-6102-KVPROVPL   (2) TO PROA-6102-KVPROVPL   (1)          
024461     END-IF                                                               
024462                                                                          
024463     IF INDX = +1 OR +2                                                   
024464       MOVE PROA-6102-KVAVIS-FOM (3) TO PROA-6102-KVAVIS-FOM (2)          
024465       MOVE PROA-6102-KVAVIS-TOM (3) TO PROA-6102-KVAVIS-TOM (2)          
024466       MOVE PROA-6102-KVPROVPL   (3) TO PROA-6102-KVPROVPL   (2)          
024467     END-IF                                                               
024468                                                                          
024469     IF INDX = +1 OR +2 OR +3                                             
024470       MOVE PROA-6102-KVAVIS-FOM (4) TO PROA-6102-KVAVIS-FOM (3)          
024471       MOVE PROA-6102-KVAVIS-TOM (4) TO PROA-6102-KVAVIS-TOM (3)          
024472       MOVE PROA-6102-KVPROVPL   (4) TO PROA-6102-KVPROVPL   (3)          
024473     END-IF                                                               
024474                                                                          
024475     IF INDX = +1 OR +2 OR +3 OR +4                                       
024476       MOVE PROA-6102-KVAVIS-FOM (5) TO PROA-6102-KVAVIS-FOM (4)          
024477       MOVE PROA-6102-KVAVIS-TOM (5) TO PROA-6102-KVAVIS-TOM (4)          
024478       MOVE PROA-6102-KVPROVPL   (5) TO PROA-6102-KVPROVPL   (4)          
024479     END-IF                                                               
024480                                                                          
024481     IF INDX = +1 OR +2 OR +3 OR +4                                       
024482       MOVE ALL ZERO                 TO PROA-6102-KVAVIS-FOM (5)          
024483       MOVE ALL ZERO                 TO PROA-6102-KVAVIS-TOM (5)          
024484       MOVE ALL ZERO                 TO PROA-6102-KVPROVPL   (5)          
024485     END-IF                                                               
024486     .                                                                    
024487     EJECT                                                                
024488 HC-INITIERA-TABELL SECTION.                                              
024489                                                                          
024490     IF ENDAST-SKIPLOT                                                    
024491       MOVE +1 TO INDX                                                    
024492       SET PROA-6102-AVIS-IX TO +1                                        
024493     ELSE                                                                 
024494       MOVE +2 TO INDX                                                    
024495       SET PROA-6102-AVIS-IX TO +2                                        
024496     END-IF                                                               
024497     PERFORM UNTIL INDX > MAX-INDX                                        
024498       MOVE ALL ZERO         TO PROA-6102-KVAVIS-FOM                      
024499                               (PROA-6102-AVIS-IX)                        
024500                                PROA-6102-KVAVIS-TOM                      
024501                               (PROA-6102-AVIS-IX)                        
024502                                PROA-6102-KVPROVPL                        
024503                               (PROA-6102-AVIS-IX)                        
024504       ADD +1 TO INDX                                                     
024505       SET PROA-6102-AVIS-IX TO INDX                                      
024506     END-PERFORM                                                          
024507     .                                                                    
024508     EJECT                                                                
024509 MFS-RENSA-FAELT-UT SECTION.                                              
024510                                                                          
024520     MOVE MFS-RENSA-FAELT TO MOD-IDPROVPL-UT                              
024600                             MOD-KDPROVPL-UT                              
024800     .                                                                    
024901     SKIP2                                                                
024902 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
024903                                                                          
024904     MOVE +1 TO INDX                                                      
024905     PERFORM UNTIL INDX > MAX-INDX                                        
024906       MOVE MFS-RENSA-FAELT TO MOD-KVAVIS-FOM (INDX)                      
024909                               MOD-KVAVIS-TOM (INDX)                      
024912                               MOD-KVPROVPL   (INDX)                      
024914       ADD +1 TO INDX                                                     
024915     END-PERFORM                                                          
024916     .                                                                    
024917     SKIP2                                                                
025100 MFS-RENSA-FAELT-IN SECTION.                                              
025200                                                                          
025400     MOVE MFS-RENSA-FAELT TO MOD-KVSKPLOT-UPD                             
025500                             MOD-KVAVIS-FOM-UPD                           
025530                             MOD-KVAVIS-TOM-UPD                           
025540                             MOD-KVPROVPL-UPD                             
025550                             MOD-KDCMD-UPD                                
025600     .                                                                    
025700     EJECT                                                                
025800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
025900                                                                          
026200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROVPL-UT                            
026300                               MOD-KDPROVPL-UT                            
026501     MOVE +1 TO INDX                                                      
026502     PERFORM UNTIL INDX > MAX-INDX                                        
026503       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
026504       ADD +1 TO INDX                                                     
026505     END-PERFORM                                                          
026506     .                                                                    
026507     SKIP2                                                                
026508 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
026509                                                                          
026511     MOVE MFS-ROER-EJ-FAELT TO MOD-KVAVIS-FOM (INDX)                      
026515                               MOD-KVAVIS-TOM (INDX)                      
026516                               MOD-KVPROVPL   (INDX)                      
026600     .                                                                    
026700     SKIP2                                                                
026800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
026900                                                                          
027100     MOVE MFS-ROER-EJ-FAELT TO MOD-KVSKPLOT-UPD                           
027200                               MOD-KVAVIS-FOM-UPD                         
027230                               MOD-KVAVIS-TOM-UPD                         
027240                               MOD-KVPROVPL-UPD                           
027250                               MOD-KDCMD-UPD                              
027300     .                                                                    
027400     EJECT                                                                
028900* --- IMS SEKTIONER ---                                                   
029000     SKIP3                                                                
029100 IMS-GET-MSG SECTION.                                                     
029200                                                                          
029300     MOVE '  QC' TO GODK-STATUSKODER                                      
029400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
029500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029600     PERFORM IMS-STATUSKONTROLL                                           
029700     .                                                                    
029800     SKIP3                                                                
029900 IMS-INSERT-MSG SECTION.                                                  
030000                                                                          
030100     IF NOT ENGLISH-TEXT                                                  
030200       MOVE '0' TO MFS-KDHUVOMR                                           
030300     END-IF                                                               
030400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
030500     MOVE SPACE TO GODK-STATUSKODER                                       
030600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
030700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031001     EJECT                                                                
031002 IMS-GET-PROA-6101 SECTION.                                               
031003     STRING 'W6PROA01(W6GXKEY  =' W-W6GXKEY-6101-X ')'                    
031004          DELIMITED BY SIZE INTO SSA1                                     
031005     MOVE '  GE' TO GODK-STATUSKODER                                      
031006     CALL CBLTDLI USING GU PROA-PCB DLI-IO-AREA SSA1                      
031007     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
031008     PERFORM IMS-STATUSKONTROLL                                           
031009     .                                                                    
031010     EJECT                                                                
031011 IMS-GET-PROA-6102 SECTION.                                               
031012     STRING 'W6PROA11(W6GXKEY  =' W-W6GXKEY-6102-X ')'                    
031013          DELIMITED BY SIZE INTO SSA1                                     
031014     MOVE '  GE' TO GODK-STATUSKODER                                      
031015     CALL CBLTDLI USING GHNP PROA-PCB DLI-IO-AREA SSA1                    
031016     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
031017     PERFORM IMS-STATUSKONTROLL                                           
031018     .                                                                    
031019     SKIP3                                                                
031029 IMS-ISRT-PROA-6102 SECTION.                                              
031030                                                                          
031031     STRING 'W6PROA01(W6GXKEY  =' W-W6GXKEY-6101-X ')'                    
031032          DELIMITED BY SIZE INTO SSA1                                     
031033     MOVE 'W6PROA11 ' TO SSA2                                             
031034     MOVE '  II' TO GODK-STATUSKODER                                      
031035     CALL CBLTDLI USING ISRT PROA-PCB DLI-IO-AREA SSA1 SSA2               
031036     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
031037     PERFORM IMS-STATUSKONTROLL                                           
031038     .                                                                    
031039     SKIP3                                                                
031040 IMS-REPL-PROA-6102 SECTION.                                              
031041                                                                          
031042     MOVE '  ' TO GODK-STATUSKODER                                        
031043     CALL CBLTDLI USING REPL PROA-PCB DLI-IO-AREA                         
031044     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
031045     PERFORM IMS-STATUSKONTROLL                                           
031046     .                                                                    
031047     SKIP3                                                                
031048 IMS-DLET-PROA-6102 SECTION.                                              
031049                                                                          
031050     MOVE '  ' TO GODK-STATUSKODER                                        
031051     CALL CBLTDLI USING DLET PROA-PCB DLI-IO-AREA                         
031052     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
031053     PERFORM IMS-STATUSKONTROLL                                           
031060     .                                                                    
031100     EJECT                                                                
031200 IMS-STATUSKONTROLL SECTION.                                              
031300                                                                          
031400     SET STATUS-IX TO 1                                                   
031500     SEARCH GODK-STATUS                                                   
031600       AT END                                                             
031700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031800         DELIMITED BY SIZE INTO FELTEXT                                   
031900         CALL FELLOG                                                      
032000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032100         CONTINUE                                                         
032200     END-SEARCH                                                           
032300     .                                                                    
