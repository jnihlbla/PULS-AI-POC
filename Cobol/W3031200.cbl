001000 ID DIVISION.                                                             
001200 PROGRAM-ID.     W3031200.                                                
001300 AUTHOR.         THOMAS LARSSON.                                          
001400 DATE-WRITTEN.   93/11/02.                                                
001410 DATE-COMPILED.                                                           
001500                                                                          
001800*    FUNKTION:                                                            
001900*        UPPDATERAR ORDERKLASS PÅ PRISOMRÅDE                              
002000*                                                                         
002101*        PROGRAMMET UPPDATERAR WDC2                                       
002110*        PROGRAMMET LÄSER      WDB1                                       
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W3T312                                              
002500*        MID:         W3I31201                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W3O31201                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003401                                                                          
003410*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W3031200'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004301*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004302 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004310 77  MAX-INDX                    PIC S9(4)  VALUE +8    COMP SYNC.        
004400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004500 01  W-IDPARTNR                  PIC X(9)   VALUE SPACE.                  
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005010 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
005011 01  FILLER REDEFINES WS-IDPROMR.                                         
005012     03 WS-MARKBOLAG             PIC X(1).                                
005013     03 FILLER                   PIC X(2).                                
005014                                                                          
005020 77  WS-IDMARKBO                 PIC X(1)    VALUE SPACE.                 
005100                                                                          
005201 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005202     88  INDATA-OK                           VALUE 'J'.                   
005210     88  INDATA-FEL                          VALUE 'N'.                   
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '3312'.                
006000     88  GODK-MID                            VALUE '3312'                 
006100                                                   '3313' '3314'          
006200                                                   '3315' '3316'          
006300                                                   '3317' '3318'          
006400                                                   '3319'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006640                                                                          
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
007810     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007901     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007910     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     03  ERR-PRICEAREA-MISSING   PIC X(3)    VALUE '236'.                 
008200     EJECT                                                                
008300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008600     SKIP3                                                                
008700*01  MID -COPY W3I31201                                                   
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009000     SKIP3                                                                
009100*01  -COPY WMSGAREA                                                       
009200     EJECT                                                                
009300     03  MOD REDEFINES MSG-AREA.                                          
009400*      05  -COPY W3O31201                                                 
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009700     SKIP3                                                                
009800*01  -COPY WMFSAREA                                                       
009900     EJECT                                                                
010000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010100*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010601     03  W-IDPROMR-X.                                                     
010602         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
010700                                                                          
010710     03  W-WDB1ASEQ-X.                                                    
010711         05  W-IDPROMR-SEQ       PIC X(3)    VALUE SPACE.                 
010712                                                                          
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011210     88  BAS-SLUT                            VALUE 'GB'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(128).                              
011800 01  SSA2                        PIC X(128).                              
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012300     EJECT                                                                
012400*    ---  DLI INPUT-OUTPUT AREA                                           
012500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012600                                                                          
012700 01  DLI-IO-AREA.                                                         
012902   03  WDC201.                                                            
012903*    05  -COPY WDC201  -PRE WDC2-                                         
012904                                                                          
012920 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-B101'.         
012930                                                                          
012940 01  DLI-IO-B101.                                                         
013000   03  WDB101.                                                            
013100*    05  -COPY WDB101  -PRE WDB1A-                                        
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0009   -PRE MSG-                                              
013601     EJECT                                                                
013602*01  -COPY W0008  -PRE WDC2-                                              
013603     05  FILLER                  PIC X.                                   
013604     EJECT                                                                
013800*01  -COPY W0008  -PRE WDB1A-                                             
013801     05  FILLER                  PIC X.                                   
013802     EJECT                                                                
013803 PROCEDURE DIVISION  USING MSG-PCB WDC2-PCB WDB1A-PCB.                    
013804 MAIN SECTION.                                                            
013810     ENTRY 'DLITCBL' USING MSG-PCB WDC2-PCB WDB1A-PCB.                    
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
014605           END-IF                                                         
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
014910         END-IF                                                           
015000         PERFORM F-LAES-VISA-INFO                                         
015100       END-IF                                                             
015200       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O31201 + 4                      
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
016400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I31201                 
016500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
016600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016700     ELSE                                                                 
016800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I31201                  
016900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017100     END-IF                                                               
017200                                                                          
017300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017600                                                                          
017700     MOVE LOW-VALUE TO MSG-AREA                                           
017800     MOVE 'W3O312N1' TO MFS-IDMOD                                         
017900     MOVE '3312' TO MOD-IDTRANS                                           
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
020301                                                                          
020360*    -- KONTROLL AV IDPROMR                                               
020370     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-IN                               
020380                                                                          
020390     IF MID-IDPROMR-IN = ALL '+'                                          
020400       MOVE MID-IDPROMR-UT TO WS-IDPROMR                                  
020401     ELSE                                                                 
020402       MOVE MID-IDPROMR-IN TO WS-IDPROMR                                  
020403       MOVE '7'         TO MFS-IDPFK                                      
020404       MOVE SPACE       TO MFS-KDTRTYP                                    
020405     END-IF                                                               
020406                                                                          
020407     IF WS-IDPROMR NOT = SPACE                                            
020409         MOVE WS-IDPROMR TO W-IDPROMR                                     
020410                            W-IDPROMR-SEQ                                 
020415     ELSE                                                                 
020416       MOVE NEJ TO NYCKLAR-SW                                             
020417     END-IF                                                               
020418                                                                          
020419     IF NYCKLAR-OK                                                        
020420       IF EGEN-MID OR GODK-MID                                            
020422         MOVE WS-IDPROMR    TO MOD-IDPROMR-UT                             
020423       ELSE                                                               
020425         MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                           
020426       END-IF                                                             
020427     ELSE                                                                 
020429       MOVE WS-IDPROMR      TO MOD-IDPROMR-UT                             
020430     END-IF                                                               
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
021403                                                                          
021404     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
021405     CALL WMEDKONV USING MED-WMEDAREA                                     
021406     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
021407                                                                          
021408*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
021414     .                                                                    
021415     EJECT                                                                
021416 D-NAESTA-SIDA SECTION.                                                   
021417                                                                          
021418     MOVE MID-SPAR-IDPROMR  TO W-IDPROMR-SEQ                              
021420                               W-IDPROMR                                  
021424     MOVE MID-IDPARTNR-NEXT TO W-IDPARTNR                                 
021427     .                                                                    
021428     EJECT                                                                
021429 E-SAMMA-SIDA SECTION.                                                    
021430                                                                          
021431     IF EGEN-MID OR HELP-MID                                              
021432       MOVE MID-SPAR-IDPROMR TO W-IDPROMR-SEQ                             
021437       MOVE MID-IDPARTNR-ENTER TO W-IDPARTNR                              
021438       IF MID-INPUT = ALL '+'                                             
021439         PERFORM MFS-RENSA-FAELT-IN                                       
021440       ELSE                                                               
021441         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
021442         CALL WMEDKONV USING MED-WMEDAREA                                 
021443         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
021444         PERFORM EA-MID-INDATA-TILL-MOD                                   
021445       END-IF                                                             
021446     ELSE                                                                 
021447       PERFORM MFS-RENSA-FAELT-IN                                         
021448     END-IF                                                               
021449     .                                                                    
021450     EJECT                                                                
021451 EA-MID-INDATA-TILL-MOD SECTION.                                          
021452                                                                          
021457     IF MID-KDORDKL-DOG-IN NOT = ALL '+'                                  
021458       MOVE MID-KDORDKL-DOG-IN    TO MOD-KDORDKL-DOG-IN                   
021459       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDORDKL-DOG-ATTR                 
021460     ELSE                                                                 
021461       MOVE MFS-RENSA-FAELT       TO MOD-KDORDKL-DOG-IN                   
021462     END-IF                                                               
021470     .                                                                    
021500     EJECT                                                                
021700 F-LAES-VISA-INFO SECTION.                                                
021800                                                                          
021900     PERFORM IMS-GHU-WDC2                                                 
022000                                                                          
022100     IF SEGMENT-SAKNAS                                                    
022110       MOVE ERR-PRICEAREA-MISSING TO MED-IDMFSFEL                         
022300       CALL WMEDKONV USING MED-WMEDAREA                                   
022400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022500       PERFORM MFS-RENSA-FAELT-UT                                         
022600     ELSE                                                                 
022700       MOVE WDC2-PRO-KDORDKL-DOG TO MOD-KDORDKL-DOG-UT                    
022701       MOVE WDC2-PRO-IDPROMR     TO MOD-SPAR-IDPROMR                      
022702       MOVE +1 TO INDX                                                    
022703                                                                          
022704       IF W-IDPARTNR NOT = SPACE                                          
022705         PERFORM FA-LAES-FRAM                                             
022706       ELSE                                                               
022707         PERFORM IMS-GU-WDB101                                            
022708       END-IF                                                             
022709                                                                          
022710       IF SEGMENT-FINNS                                                   
022711         IF WDB1A-BET-IDPARTNR >= W-IDPARTNR                              
022712           MOVE WDB1A-BET-IDPARTNR    TO MOD-IDPARTNR-ENTER               
022713         END-IF                                                           
022714       ELSE                                                               
022715         MOVE SPACE                 TO MOD-IDPARTNR-ENTER                 
022716       END-IF                                                             
022717                                                                          
022718       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
022719         IF SEGMENT-FINNS                                                 
022720           IF WDB1A-BET-IDPARTNR >= W-IDPARTNR                            
022721             MOVE WDB1A-BET-IDPARTNR TO MOD-IDPARTNR (INDX)               
022722             MOVE WDB1A-BET-IDLANDX2 TO MOD-IDLANDX2 (INDX)               
022723           END-IF                                                         
022724         ELSE                                                             
022725           MOVE MFS-RENSA-FAELT     TO MOD-IDPARTNR (INDX)                
022726           MOVE MFS-RENSA-FAELT     TO MOD-IDLANDX2 (INDX)                
022727         END-IF                                                           
022728         ADD +1 TO INDX                                                   
022729         PERFORM IMS-GN-WDB101                                            
022731       END-PERFORM                                                        
022732                                                                          
022733       MOVE 'SEK'  TO MOD-KDVALIS1                                        
022734                      MOD-KDVALIS2                                        
022735                      MOD-KDVALIS3                                        
022736                      MOD-KDVALIS5                                        
022737                      MOD-KDVALIS6                                        
022742                                                                          
022743       IF SEGMENT-FINNS                                                   
022744         MOVE WDB1A-BET-IDPARTNR   TO MOD-IDPARTNR-NEXT                   
022745         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
022746         CALL WMEDKONV USING MED-WMEDAREA                                 
022747         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
022748       ELSE                                                               
022749         MOVE HIGH-VALUE            TO MOD-IDPARTNR-NEXT                  
022750       END-IF                                                             
022760                                                                          
022800     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 FA-LAES-FRAM SECTION.                                                    
023110     PERFORM IMS-GU-WDB101                                                
023120     PERFORM UNTIL SEGMENT-SAKNAS OR (W-IDPARTNR =                        
023130                     WDB1A-BET-IDPARTNR)                                  
023140       PERFORM IMS-GN-WDB101                                              
023150     END-PERFORM                                                          
023160                                                                          
023200     .                                                                    
023300     EJECT                                                                
023902 G-KOLLA-INPUT SECTION.                                                   
023903                                                                          
023904     MOVE JA  TO INDATA-SW                                                
023905     IF MID-INPUT = ALL '+'                                               
023906       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
023907       CALL WMEDKONV USING MED-WMEDAREA                                   
023908       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023909       PERFORM MFS-ROER-EJ-FAELT-IN                                       
023910       PERFORM MFS-ROER-EJ-FAELT-UT                                       
023911       MOVE NEJ TO INDATA-SW                                              
023912     ELSE                                                                 
023913                                                                          
023925       IF MID-KDORDKL-DOG-IN NOT = ALL '+'                                
023926         IF MID-KDORDKL-DOG-IN NOT NUMERIC                                
023927           MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-DOG-ATTR                 
023928           MOVE NEJ TO INDATA-SW                                          
023929         ELSE                                                             
023930           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDORDKL-DOG-ATTR               
023931         END-IF                                                           
023951       END-IF                                                             
023952                                                                          
023953       IF INDATA-FEL                                                      
023954         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
023955         CALL WMEDKONV USING MED-WMEDAREA                                 
023956         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
023957         PERFORM MFS-ROER-EJ-FAELT-UT                                     
023960         PERFORM MFS-ROER-EJ-FAELT-IN                                     
023961       END-IF                                                             
023962     END-IF                                                               
023963     .                                                                    
023964     EJECT                                                                
023965 H-UPPDATERA SECTION.                                                     
023966                                                                          
023967     PERFORM IMS-GHU-WDC2                                                 
023968     IF SEGMENT-FINNS                                                     
023969       IF MID-KDORDKL-DOG-IN NOT = ALL '+'                                
023970         MOVE MID-KDORDKL-DOG-IN TO WDC2-PRO-KDORDKL-DOG                  
023971         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDORDKL-DOG-ATTR               
023972         PERFORM IMS-REPL-WDC2                                            
023973       ELSE                                                               
023974         MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDKL-DOG-IN                     
023975       END-IF                                                             
023977                                                                          
023985       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
023986       CALL WMEDKONV USING MED-WMEDAREA                                   
023987       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
023988       PERFORM MFS-FORM-ATTR                                              
023989       PERFORM MFS-RENSA-FAELT-IN                                         
023990* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
023991     END-IF                                                               
023992     .                                                                    
024000     EJECT                                                                
024100 MFS-RENSA-FAELT-UT SECTION.                                              
024200                                                                          
024300*    --- ALLA UTDATA-FÄLT                                                 
024410*    --- INKL. BLÄDDRINGSNYCKLAR                                          
024705     MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR-ENTER                           
024706                             MOD-IDPARTNR-NEXT                            
024707                             MOD-SPAR-IDPROMR                             
024708     MOVE +1 TO INDX                                                      
024709     PERFORM UNTIL INDX > MAX-INDX                                        
024710       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
024711       ADD +1 TO INDX                                                     
024720     END-PERFORM                                                          
024800     .                                                                    
024901     SKIP2                                                                
024902 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
024903                                                                          
024904*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
024907     MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR (INDX)                          
024908     MOVE MFS-RENSA-FAELT TO MOD-IDLANDX2 (INDX)                          
024910     .                                                                    
025000     SKIP2                                                                
025100 MFS-RENSA-FAELT-IN SECTION.                                              
025200                                                                          
025300*    --- ALLA INDATA-FÄLT                                                 
025400     MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-DOG-IN                           
025600     .                                                                    
025700     EJECT                                                                
025800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
025900                                                                          
026000*    --- ALLA UTDATA-FÄLT                                                 
026110*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
026405     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPARTNR-ENTER                         
026406                               MOD-IDPARTNR-NEXT                          
026407                               MOD-SPAR-IDPROMR                           
026501     MOVE +1 TO INDX                                                      
026502     PERFORM UNTIL INDX > MAX-INDX                                        
026503       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
026504       ADD +1 TO INDX                                                     
026505     END-PERFORM                                                          
026506     .                                                                    
026507     SKIP2                                                                
026508 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
026509                                                                          
026510*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
026512     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPARTNR (INDX)                        
026513     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLANDX2 (INDX)                        
026600     .                                                                    
026700     SKIP2                                                                
026800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
026900                                                                          
027000*    --- ALLA INDATA-FÄLT                                                 
027100     MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDKL-DOG-IN                         
027300     .                                                                    
027400     EJECT                                                                
027500 MFS-FORM-ATTR SECTION.                                                   
027600                                                                          
027700*    --- ALLA INDATA-FÄLT                                                 
027800     MOVE MFS-FORMATETS-ATTR TO MOD-KDORDKL-DOG-ATTR                      
028000     .                                                                    
028100     SKIP2                                                                
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
031002 IMS-GHU-WDC2 SECTION.                                                    
031003     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
031004          DELIMITED BY SIZE INTO SSA1                                     
031005     MOVE '  GE' TO GODK-STATUSKODER                                      
031006     CALL CBLTDLI USING GHU WDC2-PCB DLI-IO-AREA SSA1                     
031007     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
031008     PERFORM IMS-STATUSKONTROLL                                           
031009     .                                                                    
031010     SKIP3                                                                
031011 IMS-REPL-WDC2 SECTION.                                                   
031012                                                                          
031013     MOVE '  ' TO GODK-STATUSKODER                                        
031014     CALL CBLTDLI USING REPL WDC2-PCB DLI-IO-AREA                         
031015     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
031016     PERFORM IMS-STATUSKONTROLL                                           
031017     .                                                                    
031018     EJECT                                                                
031110 IMS-GU-WDB101 SECTION.                                                   
031111     STRING 'WDB101  (WDB1ASEQ =' W-WDB1ASEQ-X ')'                        
031113          DELIMITED BY SIZE INTO SSA1                                     
031116                                                                          
031117     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031150     CALL CBLTDLI USING GU WDB1A-PCB DLI-IO-B101 SSA1                     
031160     MOVE WDB1A-STATUS-CODE TO STATUS-WS                                  
031170     PERFORM IMS-STATUSKONTROLL                                           
031180     .                                                                    
031190     EJECT                                                                
031191 IMS-GN-WDB101 SECTION.                                                   
031192     STRING 'WDB101  (WDB1ASEQ =' W-WDB1ASEQ-X ')'                        
031194          DELIMITED BY SIZE INTO SSA1                                     
031195                                                                          
031196     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031197     CALL CBLTDLI USING GN WDB1A-PCB DLI-IO-B101  SSA1                    
031198     MOVE WDB1A-STATUS-CODE TO STATUS-WS                                  
031199     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031201     EJECT                                                                
031210 IMS-STATUSKONTROLL SECTION.                                              
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
