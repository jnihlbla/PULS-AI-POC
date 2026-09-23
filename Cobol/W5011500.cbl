000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5011500.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   94/08/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SPIS FRÅGA / GODKÄNNA PRISFÖRSLAG.                               
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLPRIF (WDC6A)                             
001200*        PROGRAMMET LÄSER      WLXXEH (WDR4)                              
001300*        PROGRAMMET UPPDATERAR WLPRIE (WDC6)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W5T115                                              
001700*        MID:         W5I11501                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W5O11501                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W5011500'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  WS-SPIS-STDPRIS             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
003500                                                                          
003600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004000*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
004100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1010 COMP SYNC.        
004200                                                                          
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
004500 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004600 77  WS-KDPRBEH                  PIC X(1)    VALUE SPACE.                 
004700 77  WS-REAENDR                  PIC X(6)    VALUE SPACE.                 
004710 77  WS-REAENDR-NUM              PIC 9(4)V9(1) VALUE ZERO.                
004800                                                                          
004810 01  WS-STYR-LAS.                                                         
004820     03  WS-LEVNR                PIC X(1)    VALUE SPACE.                 
004830     03  WS-ARTNR                PIC X(1)    VALUE SPACE.                 
004840     03  WS-BEHKOD               PIC X(1)    VALUE SPACE.                 
004850     03  WS-AENDR                PIC X(1)    VALUE SPACE.                 
004860                                                                          
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  KOLL-SW                     PIC X       VALUE 'J'.                   
005800     88  RAD-FINNS                           VALUE 'J'.                   
005900     88  RAD-FINNS-EJ                        VALUE 'N'.                   
006000                                                                          
006001 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006002     88  ALLT-OK                             VALUE 'J'.                   
006004                                                                          
006010 77  BYT-SW                      PIC X       VALUE 'J'.                   
006020     88  BYT-BILD                            VALUE 'J'.                   
006030     88  BYT-EJ-BILD                         VALUE 'N'.                   
006040                                                                          
006100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006200     88  EGEN-MID                            VALUE '5115'.                
006210     88  SPIS-MID                            VALUE '5114' '5115'.         
006300     88  GODK-MID                            VALUE '5111' '5112'          
006400                                                   '5113' '5114'          
006500                                                   '5115' '5116'          
006600                                                   '5117' '5118'          
006700                                                   '5119'.                
006800     88  HELP-MID                            VALUE '0551'.                
006900     EJECT                                                                
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007410     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007420     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007500     SKIP2                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007700*01 -COPY WMSGINIT                                                        
007710     SKIP2                                                                
007720*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007730*01 -COPY WMEDAREA                                                        
007800     SKIP2                                                                
007900 01  MESSAGE-CODES.                                                       
008000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008410     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008610     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
008700     EJECT                                                                
008710*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
008720*01 -COPY WDECAREA                                                        
008730     SKIP2                                                                
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W5I11501                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W5O11501                                                 
010000     EJECT                                                                
010010 01  W-PROG-TO-PROG-SW.                                                   
010020     03  M-SW-LL                 PIC S9(4)   VALUE +240 COMP SYNC.        
010030     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
010040     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W5T114  '.            
010050     03  M-SW-IDTRANS            PIC X(4)    VALUE '5115'.                
010060     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '1'.                   
010070                                                                          
010080*    03  MID  -COPY W5I11401 -PRE 5114-                                   
010090     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200     SKIP3                                                                
010300*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700     SKIP2                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-WDC6A1KY-X.                                                    
011200         05  W-IDLEVNR-A1        PIC X(5)    VALUE SPACE.                 
011300         05  W-IDARTNR-A1        PIC X(9)    VALUE SPACE.                 
011400                                                                          
011500     03  W-WDC6A1KY-MIN-X.                                                
011600         05  W-IDLEVNR-MIN       PIC X(5)    VALUE SPACE.                 
011700         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
011800                                                                          
011900     03  W-WDC6A1KY-MAX-X.                                                
012000         05  W-IDLEVNR-MAX       PIC X(5)    VALUE SPACE.                 
012100         05  W-IDARTNR-MAX       PIC S9(9)   VALUE +999999999             
012110                                             COMP-3.                      
012200                                                                          
012600     03  W-IDARTNR-X.                                                     
012700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012800                                                                          
012900     03  W-KDPRBEH-X.                                                     
013000         05  W-KDPRBEH           PIC X(1)    VALUE SPACE.                 
013100                                                                          
013200     03  W-REAENDR-X.                                                     
013300         05  W-REAENDR        PIC S9(4)V9(1) VALUE ZERO COMP-3.           
013400                                                                          
013500     03  W-WDGX5119-X.                                                    
013600         05  W-IDHTYP            PIC X(4)    VALUE '5119'.                
013700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
013800                                                                          
013900     SKIP2                                                                
014000*    --- STATUS-KOD FRÅN IMS                                              
014100 01  STATUS-WS                   PIC XX.                                  
014200     88  SEGMENT-FINNS                       VALUE '  '.                  
014300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014410     88  BAS-SLUT                            VALUE 'GB'.                  
014500     SKIP2                                                                
014600 01  GODK-STATUSKODER.                                                    
014700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800     SKIP3                                                                
014900 01  SSA1                        PIC X(99).                               
015000 01  SSA2                        PIC X(64).                               
015100     EJECT                                                                
015200*    --- IMS FUNKTIONSKODER                                               
015300*01  -COPY W0003                                                          
015400     EJECT                                                                
015500*    ---  DLI INPUT-OUTPUT AREA                                           
015600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015700     SKIP3                                                                
015800 01  DLI-IO-AREA.                                                         
015900     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
016000     SKIP3                                                                
016100     03  WLPRIF01 REDEFINES IO-AREA.                                      
016200*        05  -COPY WDC6A1  -PRE PRIF-                                     
016300     EJECT                                                                
016400 01  DLI-IO-AREA2.                                                        
016500     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
016600     SKIP3                                                                
016700     03  WLPRIE01 REDEFINES IO-AREA2.                                     
016800*        05  -COPY WDC601  -PRE PRIE-                                     
016900     EJECT                                                                
017000 01  DLI-IO-AREA3.                                                        
017100     03  IO-AREA3                PIC X(100)  VALUE SPACE.                 
017200     SKIP3                                                                
017300     03  WLXXEH01 REDEFINES IO-AREA3.                                     
017400*        05  -COPY WDGX5120  -PRE XXEH-                                   
017500     EJECT                                                                
017600 LINKAGE SECTION.                                                         
017700                                                                          
017800*01  -COPY W0009   -PRE MSG-                                              
017900     EJECT                                                                
017910*01  -COPY W0009   -PRE ALT-                                              
017920     EJECT                                                                
018000*01  -COPY W0008  -PRE USEA-                                              
018100     05  FILLER                  PIC X.                                   
018110     EJECT                                                                
018120*01  -COPY W0008  -PRE PRIF-                                              
018130     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300*01  -COPY W0008  -PRE PRIE-                                              
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018900*01  -COPY W0008  -PRE XXEH-                                              
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
019210                                           PRIF-PCB PRIE-PCB              
019300                           XXEH-PCB.                                      
019400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
019401                                           PRIF-PCB PRIE-PCB              
019410                           XXEH-PCB.                                      
019600                                                                          
019700     PERFORM IMS-GET-MSG                                                  
019800     IF SEGMENT-FINNS                                                     
019900       PERFORM A-INIT                                                     
020000       PERFORM B-KOLLA-NYCKLAR                                            
020100       IF NYCKLAR-OK                                                      
020110         PERFORM Q-KOLL-OM-BYT-INMATAT                                    
020200         IF MFS-UPDATE                                                    
020300           PERFORM G-KOLLA-INPUT                                          
020400           IF INDATA-OK                                                   
020500             PERFORM H-UPPDATERA                                          
020600           END-IF                                                         
020700         ELSE                                                             
020800           IF MFS-FIRST                                                   
020900             PERFORM C-FOERSTA-SIDA                                       
021000           ELSE                                                           
021100             IF MFS-NEXT                                                  
021200               PERFORM D-NAESTA-SIDA                                      
021300             ELSE                                                         
021400               PERFORM E-SAMMA-SIDA                                       
021500             END-IF                                                       
021600           END-IF                                                         
021700         END-IF                                                           
021710         IF ALLT-OK                                                       
021800           PERFORM F-LAES-VISA-INFO                                       
021810         END-IF                                                           
021900       END-IF                                                             
021910       IF BYT-EJ-BILD                                                     
022000         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
022100         PERFORM IMS-INSERT-MSG                                           
022110       END-IF                                                             
022200     END-IF                                                               
022300                                                                          
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     IF MSG-DUBBLA-TRANSKODER                                             
023100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I11501                 
023200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
023300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023400     ELSE                                                                 
023500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I11501                  
023600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
023700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023800     END-IF                                                               
023900                                                                          
024000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
024200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024300                                                                          
024400     MOVE LOW-VALUE TO MSG-AREA                                           
024500     MOVE 'W5O115N1' TO MFS-IDMOD                                         
024600     MOVE '5115' TO MOD-IDTRANS                                           
024700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024800                                                                          
024900*    --- OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4               
025000*    --- OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17              
025100                                                                          
025200     IF EGEN-MID OR HELP-MID                                              
025300       CONTINUE                                                           
025400     ELSE                                                                 
025500       MOVE SPACE TO MFS-KDTRTYP                                          
025600       MOVE '7' TO MFS-IDPFK                                              
025700     END-IF                                                               
025800                                                                          
025900     IF MSGI-IDLAND-SPR = 'GB'                                            
026000       MOVE +2 TO SPRAK-IX                                                
026100       MOVE 'GB ' TO MED-IDSKYLT                                          
026200     ELSE                                                                 
026300       MOVE +1 TO SPRAK-IX                                                
026400       MOVE 'S  ' TO MED-IDSKYLT                                          
026500     END-IF                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 B-KOLLA-NYCKLAR SECTION.                                                 
026900                                                                          
027000     MOVE JA TO NYCKLAR-SW                                                
027010     MOVE NEJ TO BYT-SW                                                   
027100                                                                          
027420     MOVE ALL '+' TO MSGI-WMSGINIT                                        
027430     MOVE '001'             TO MSGI-KDCALL                                
027440     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027450                                                                          
027460     IF MFS-IDTRANS = '5115'                                              
027470         MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                            
027480         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
027490     ELSE                                                                 
027491       IF MID-IDARTNR-IN NUMERIC                                          
027493       AND MID-IDARTNR-IN > ZERO                                          
027494         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
027496       END-IF                                                             
027497       MOVE SPACE              TO MID-IDLEVNR-IN                          
027498       MOVE ZERO               TO MID-REAENDR-IN                          
027499       MOVE SPACE              TO MID-KDPRBEH-IN                          
027500     END-IF                                                               
027501                                                                          
027502     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027503                                                                          
027504*    -- KONTROLL AV IDLEVNR                                               
027505     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
027506                                                                          
027507     IF MID-IDLEVNR-IN = ALL '+'                                          
027508       MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                  
027510     ELSE                                                                 
027511       MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                  
027512       MOVE '7'         TO MFS-IDPFK                                      
027513       MOVE SPACE       TO MFS-KDTRTYP                                    
027514     END-IF                                                               
027515                                                                          
028400       MOVE WS-IDLEVNR TO W-IDLEVNR-A1                                    
028600                          W-IDLEVNR-MIN                                   
028610       IF WS-IDLEVNR NOT = SPACE                                          
028700         MOVE WS-IDLEVNR TO W-IDLEVNR-MAX                                 
028800       ELSE                                                               
028900         MOVE '99999'  TO W-IDLEVNR-MAX                                   
028910       END-IF                                                             
029100                                                                          
029200*    -- KONTROLL AV IDARTNR                                               
029300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
029400                                                                          
029410     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
029420     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
029430                                                                          
029500     IF MID-IDARTNR-IN = ALL '+'                                          
029600       CONTINUE                                                           
029800     ELSE                                                                 
030000       MOVE '7'         TO MFS-IDPFK                                      
030100       MOVE SPACE       TO MFS-KDTRTYP                                    
030200     END-IF                                                               
030210                                                                          
030300     IF WS-IDARTNR NUMERIC                                                
030400       MOVE WS-IDARTNR TO W-IDARTNR                                       
030500                          W-IDARTNR-A1                                    
030510                          W-IDARTNR-MIN                                   
030600     ELSE                                                                 
030700       MOVE NEJ TO NYCKLAR-SW                                             
030800     END-IF                                                               
030900                                                                          
030910     IF WS-IDARTNR = ZERO AND                                             
030920        WS-IDLEVNR = SPACE                                                
030921                                                                          
030922        MOVE NEJ TO NYCKLAR-SW                                            
030930     END-IF                                                               
030940                                                                          
031000*    -- KONTROLL AV KDPRBEH                                               
031100     MOVE MFS-RENSA-FAELT TO MOD-KDPRBEH-IN                               
031200                                                                          
031300     IF MID-KDPRBEH-IN = ALL '+'                                          
031400       MOVE MID-KDPRBEH-UT TO WS-KDPRBEH                                  
031600     ELSE                                                                 
031700       MOVE MID-KDPRBEH-IN TO WS-KDPRBEH                                  
031800       MOVE '7'         TO MFS-IDPFK                                      
031900       MOVE SPACE       TO MFS-KDTRTYP                                    
032000     END-IF                                                               
032100                                                                          
032200     MOVE WS-KDPRBEH TO W-KDPRBEH                                         
032300                                                                          
032400*    -- KONTROLL AV REAENDR                                               
032500     MOVE MFS-RENSA-FAELT TO MOD-REAENDR-IN                               
032600                                                                          
032700     IF MID-REAENDR-IN = ALL '+'                                          
032800       MOVE MID-REAENDR-UT TO WS-REAENDR                                  
033000     ELSE                                                                 
033100       MOVE MID-REAENDR-IN TO WS-REAENDR                                  
033200       MOVE '7'         TO MFS-IDPFK                                      
033300       MOVE SPACE       TO MFS-KDTRTYP                                    
033400     END-IF                                                               
033500                                                                          
033700     MOVE WS-REAENDR TO DEC-IDFRIDATA                                     
033800     MOVE 4          TO DEC-KVHELTAL                                      
033900     MOVE 1          TO DEC-KVDECIMAL                                     
033910                                                                          
033920     CALL WDECEDIT USING DEC-WDECAREA                                     
033930                                                                          
033940     IF DEC-KDSVAR-OK                                                     
033950       MOVE DEC-IDEDITDATA TO WS-REAENDR-NUM                              
033960       MOVE WS-REAENDR-NUM TO W-REAENDR                                   
034000     ELSE                                                                 
034010       MOVE NEJ TO NYCKLAR-SW                                             
034020     END-IF                                                               
034030                                                                          
034200     MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                    
034400     MOVE WS-IDARTNR         TO MOD-IDARTNR-UT                            
034500     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
034600     MOVE WS-KDPRBEH         TO MOD-KDPRBEH-UT                            
034700     MOVE WS-REAENDR-NUM     TO MOD-REAENDR-UT                            
034710                                                                          
034800     IF WS-IDLEVNR = SPACE OR ALL '+'                                     
034810       MOVE SPACE TO WS-LEVNR                                             
034820     ELSE                                                                 
034831       MOVE JA TO WS-LEVNR                                                
034840     END-IF                                                               
034850                                                                          
034860     IF WS-IDARTNR = ZERO OR ALL '+'                                      
034870       MOVE SPACE TO WS-ARTNR                                             
034880     ELSE                                                                 
034891       MOVE JA TO WS-ARTNR                                                
034892     END-IF                                                               
034893                                                                          
034894     IF WS-KDPRBEH = SPACE OR ALL '+'                                     
034895       MOVE SPACE TO WS-BEHKOD                                            
034896     ELSE                                                                 
034898       MOVE JA TO WS-BEHKOD                                               
034899     END-IF                                                               
034900                                                                          
034901     IF WS-REAENDR-NUM = ZERO                                             
034902       MOVE SPACE TO WS-AENDR                                             
034903*      IF SPIS-MID                                                        
034904*        IF W-IDTRANS = '5114' OR MFS-NEXT                                
034905*          MOVE JA TO WS-AENDR                                            
034906*        ELSE                                                             
034908*        END-IF                                                           
034909*      ELSE                                                               
034910*        MOVE SPACE TO WS-AENDR                                           
034911*      END-IF                                                             
034912     ELSE                                                                 
034913       MOVE JA TO WS-AENDR                                                
034914     END-IF                                                               
035500                                                                          
035600     IF NYCKLAR-FEL                                                       
035700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
035800       CALL WMEDKONV USING MED-WMEDAREA                                   
035900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
036000       PERFORM MFS-RENSA-FAELT-IN                                         
036100       PERFORM MFS-RENSA-FAELT-UT                                         
036200     END-IF                                                               
036210                                                                          
036310     .                                                                    
036400     EJECT                                                                
036500 C-FOERSTA-SIDA SECTION.                                                  
036600                                                                          
036700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
036800     CALL WMEDKONV USING MED-WMEDAREA                                     
036900*    MOVE MED-MFSINF TO MOD-TEMFSINF                                      
037000                                                                          
037100*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
037910     MOVE JA TO ALLT-SW                                                   
037920                                                                          
038000     PERFORM MFS-RENSA-FAELT-IN                                           
038100     .                                                                    
038200     EJECT                                                                
038300 D-NAESTA-SIDA SECTION.                                                   
038400                                                                          
038600     MOVE MID-IDLEVNR-NEXT TO W-IDLEVNR-A1                                
038800                              W-IDLEVNR-MIN                               
038900                              W-IDLEVNR-MAX                               
038930                                                                          
039220     MOVE MID-IDARTNR-NEXT TO W-IDARTNR-MIN                               
039221                              W-IDARTNR                                   
039222                              W-IDARTNR-A1                                
039900                                                                          
040300*    MOVE MID-KDPRBEH-NEXT TO W-KDPRBEH                                   
040400                                                                          
040500*    MOVE MID-REAENDR-NEXT TO DEC-IDFRIDATA                               
040600*    MOVE 4                TO DEC-KVHELTAL                                
040700*    MOVE 1                TO DEC-KVDECIMAL                               
040800                                                                          
040810*    CALL WDECEDIT USING DEC-WDECAREA                                     
040820                                                                          
040830*    IF DEC-KDSVAR-OK                                                     
040840*      MOVE DEC-IDEDITDATA TO W-REAENDR                                   
040850*    ELSE                                                                 
040860*      MOVE 'FEL I REAENDR ' TO MOD-TEMFSINF                              
040870*    END-IF                                                               
040880                                                                          
040890     MOVE JA TO ALLT-SW                                                   
040891                                                                          
040900     PERFORM MFS-RENSA-FAELT-IN                                           
041000     .                                                                    
041100     EJECT                                                                
041200 E-SAMMA-SIDA SECTION.                                                    
041300                                                                          
041310     MOVE +1 TO INDX                                                      
041320     MOVE JA  TO KOLL-SW                                                  
041330     PERFORM UNTIL INDX > MAX-INDX                                        
041350       IF MID-KDPRBEH-IN-UT (INDX) = ALL '+'                              
041351          MOVE JA TO KOLL-SW                                              
041352          ADD +1 TO INDX                                                  
041370       ELSE                                                               
041380         MOVE NEJ TO KOLL-SW                                              
041382         MOVE +9999 TO INDX                                               
041390       END-IF                                                             
041392     END-PERFORM                                                          
041393                                                                          
041395     IF RAD-FINNS                                                         
041396       IF BYT-EJ-BILD                                                     
041397         IF EGEN-MID                                                      
041600           MOVE MID-IDLEVNR-ENTER TO W-IDLEVNR-A1                         
041800                                     W-IDLEVNR-MIN                        
041900                                     W-IDLEVNR-MAX                        
041910           MOVE MID-IDARTNR-ENTER TO W-IDARTNR                            
041920                                     W-IDARTNR-MIN                        
041930                                     W-IDARTNR-A1                         
041931           MOVE MID-KDPRBEH-ENTER TO W-KDPRBEH                            
041932                                                                          
041933           IF MID-REAENDR-UT NOT = ZERO                                   
041934             MOVE MID-REAENDR-UT  TO DEC-IDFRIDATA                        
041935           ELSE                                                           
041941             MOVE MID-REAENDR-ENTER TO DEC-IDFRIDATA                      
041942           END-IF                                                         
041943           MOVE 4                 TO DEC-KVHELTAL                         
041944           MOVE 1                 TO DEC-KVDECIMAL                        
041945                                                                          
041946           CALL WDECEDIT USING DEC-WDECAREA                               
041947                                                                          
041948           IF DEC-KDSVAR-OK                                               
041949             MOVE DEC-IDEDITDATA TO W-REAENDR                             
041950           ELSE                                                           
041951             MOVE 'FEL I REAENDR ' TO MOD-TEMFSINF                        
041952           END-IF                                                         
041953                                                                          
041960         END-IF                                                           
041961                                                                          
041970         MOVE JA TO ALLT-SW                                               
041980         PERFORM MFS-RENSA-FAELT-IN                                       
041990       END-IF                                                             
042000     ELSE                                                                 
042100       IF EGEN-MID OR HELP-MID                                            
042110         MOVE NEJ TO ALLT-SW                                              
045300         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
045400         CALL WMEDKONV USING MED-WMEDAREA                                 
045500         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
045510         PERFORM MFS-ROER-EJ-FAELT-IN                                     
045520         PERFORM MFS-ROER-EJ-FAELT-UT                                     
045600         PERFORM EA-MID-INDATA-TILL-MOD                                   
045610       ELSE                                                               
045620         MOVE JA TO ALLT-SW                                               
045630         PERFORM EA-MID-INDATA-TILL-MOD                                   
045700       END-IF                                                             
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 EA-MID-INDATA-TILL-MOD SECTION.                                          
046400                                                                          
046500* * * * * FÖR VARJE MID-FÄLT                                              
046600* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
046700* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
046800* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
046900     SKIP2                                                                
047000     MOVE +1 TO INDX                                                      
047100     PERFORM UNTIL INDX > MAX-INDX                                        
047200       IF MID-SELECT-URVAL(INDX) NOT = ALL '+'                            
047300         MOVE MID-SELECT-URVAL (INDX) TO MOD-SELECT-URVAL   (INDX)        
047400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SELECT-URVAL-ATTR(INDX)        
047500       ELSE                                                               
047600         MOVE MFS-RENSA-FAELT TO MOD-SELECT-URVAL (INDX)                  
047700       END-IF                                                             
047800                                                                          
047900       IF MID-KDPRBEH-IN-UT(INDX) NOT = ALL '+'                           
048000         MOVE MID-KDPRBEH-IN-UT (INDX) TO MOD-KDPRBEH-IN-UT (INDX)        
048100         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
048200              MOD-KDPRBEH-IN-UT-ATTR (INDX)                               
048300       ELSE                                                               
048400         MOVE MFS-RENSA-FAELT TO MOD-KDPRBEH-IN-UT (INDX)                 
048500       END-IF                                                             
048600       ADD +1 TO INDX                                                     
048700     END-PERFORM                                                          
048800     .                                                                    
048900     EJECT                                                                
048910 F-LAES-VISA-INFO SECTION.                                                
048920    SKIP2                                                                 
048930     PERFORM IMS-GU-WDGX11                                                
048940     IF SEGMENT-FINNS                                                     
048950       MOVE XXEH-5120-TIUPPDAT  TO MOD-TIUPPDAT                           
048960     ELSE                                                                 
048970       MOVE ZERO                TO MOD-TIUPPDAT                           
048980     END-IF                                                               
048990                                                                          
048995     EVALUATE WS-STYR-LAS                                                 
048996                                                                          
048997       WHEN 'J   '  PERFORM IMS-GN-WDC6A1                                 
048998                    PERFORM FA-LAES-VISA-SOEK                             
049000                                                                          
049001       WHEN ' J  '  PERFORM IMS-GHU-WDC601                                
049002                    PERFORM FB-LAES-VISA-UNIK                             
049004                                                                          
049005       WHEN 'JJ  '  PERFORM IMS-GN-WDC6A1-REAENDR                         
049006                    PERFORM FA-LAES-VISA-SOEK                             
049008                                                                          
049009       WHEN 'JJJ '  PERFORM IMS-GN-WDC6A1-KDPRBEH                         
049010                    PERFORM FA-LAES-VISA-SOEK                             
049011                                                                          
049012       WHEN 'J J '  PERFORM IMS-GN-WDC6A1-KDPRBEH                         
049013                    PERFORM FA-LAES-VISA-SOEK                             
049014                                                                          
049015       WHEN 'JJ J'  PERFORM IMS-GN-WDC6A1-REAENDR                         
049016                    PERFORM FA-LAES-VISA-SOEK                             
049017                                                                          
049018       WHEN 'J  J'  PERFORM IMS-GN-WDC6A1-REAENDR                         
049019                    PERFORM FA-LAES-VISA-SOEK                             
049020                                                                          
049021       WHEN 'J JJ'  PERFORM IMS-GN-WDC6A1-KDPRBEH-REAENDR                 
049022                    PERFORM FA-LAES-VISA-SOEK                             
049023                                                                          
049024       WHEN 'JJJJ'  PERFORM IMS-GN-WDC6A1-KDPRBEH-REAENDR                 
049025                    PERFORM FA-LAES-VISA-SOEK                             
049026                                                                          
049027     END-EVALUATE                                                         
049028                                                                          
049029     .                                                                    
049030     EJECT                                                                
049040 FA-LAES-VISA-SOEK SECTION.                                               
049050                                                                          
049060     IF SEGMENT-SAKNAS OR BAS-SLUT                                        
049070        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
049080        CALL WMEDKONV USING MED-WMEDAREA                                  
049090        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
049100        PERFORM MFS-RENSA-FAELT-UT                                        
049200     ELSE                                                                 
049300       MOVE +1 TO INDX                                                    
049400       IF SEGMENT-FINNS                                                   
049500         MOVE PRIF-SEQA-IDLEVNR TO MOD-IDLEVNR-ENTER                      
049600         MOVE PRIF-SEQA-IDARTNR TO MOD-IDARTNR-ENTER                      
049700         MOVE PRIF-SEQA-KDPRBEH TO MOD-KDPRBEH-ENTER                      
049800         MOVE PRIF-SEQA-REAENDR TO MOD-REAENDR-ENTER                      
049900       ELSE                                                               
050000         MOVE SPACE            TO MOD-IDLEVNR-ENTER                       
050100         MOVE ZERO             TO MOD-IDARTNR-ENTER                       
050200         MOVE SPACE            TO MOD-KDPRBEH-ENTER                       
050300         MOVE ZERO             TO MOD-REAENDR-ENTER                       
050400       END-IF                                                             
050500                                                                          
050600       PERFORM UNTIL INDX > MAX-INDX                                      
050700         IF SEGMENT-FINNS                                                 
050800           MOVE PRIF-SEQA-IDARTNR    TO W-IDARTNR                         
050900           PERFORM IMS-GHU-WDC601                                         
051000           MOVE MFS-OEPPNA-ALFA-FAELT TO                                  
051100                                    MOD-SELECT-URVAL      (INDX)          
051200           MOVE MFS-RENSA-FAELT TO   MOD-SELECT-URVAL     (INDX)          
051300           MOVE PRIE-ART-IDARTNR     TO MOD-IDARTNR       (INDX)          
051400           MOVE PRIE-ART-PRARTBEL-PR TO MOD-PRARTBEL-PR   (INDX)          
051500           MOVE PRIE-ART-KDVALISO    TO MOD-KDVALISO      (INDX)          
051600           MOVE PRIE-ART-PRINK-AKT   TO MOD-PRINK-AKT     (INDX)          
051700           MOVE PRIE-ART-PRINK-KOM   TO MOD-PRINK-KOM     (INDX)          
051800                                                                          
051900           COMPUTE WS-SPIS-STDPRIS = PRIE-ART-PRINK-KOM +                 
052000                   PRIE-ART-PRDIRLON-KOM +                                
052100                   PRIE-ART-PRDMTRL-KOM  +                                
052200                   PRIE-ART-PROVRPAL-KOM                                  
052300                                                                          
052400           MOVE WS-SPIS-STDPRIS      TO MOD-SPIS-PRARTSTD (INDX)          
052500           MOVE PRIE-ART-REAENDR     TO MOD-REAENDR-INK   (INDX)          
052600           MOVE PRIE-ART-KDPRBEH     TO MOD-KDPRBEH-IN-UT (INDX)          
052700                                                                          
052800           EVALUATE WS-STYR-LAS                                           
052900                                                                          
053000             WHEN 'J   ' PERFORM IMS-GN-WDC6A1                            
053100                                                                          
053110             WHEN 'JJ  ' PERFORM IMS-GN-WDC6A1-REAENDR                    
053120                                                                          
053200             WHEN 'JJJ ' PERFORM IMS-GN-WDC6A1-KDPRBEH                    
053300                                                                          
053400             WHEN 'J J ' PERFORM IMS-GN-WDC6A1-KDPRBEH                    
053500                                                                          
053600             WHEN 'JJ J' PERFORM IMS-GN-WDC6A1-REAENDR                    
053700                                                                          
053800             WHEN 'J  J' PERFORM IMS-GN-WDC6A1-REAENDR                    
053900                                                                          
054000             WHEN 'J JJ' PERFORM IMS-GN-WDC6A1-KDPRBEH-REAENDR            
054100                                                                          
054110             WHEN 'JJJJ' PERFORM IMS-GN-WDC6A1-KDPRBEH-REAENDR            
054120                                                                          
054200           END-EVALUATE                                                   
054300         ELSE                                                             
054400           MOVE MFS-RENSA-FAELT      TO MOD-SELECT-URVAL  (INDX)          
054500                                        MOD-IDARTNR       (INDX)          
054600                                        MOD-PRARTBEL-PR   (INDX)          
054700                                        MOD-KDVALISO      (INDX)          
054800                                        MOD-PRINK-AKT     (INDX)          
054900                                        MOD-PRINK-KOM     (INDX)          
055000                                        MOD-SPIS-PRARTSTD (INDX)          
055100                                        MOD-REAENDR-INK   (INDX)          
055200                                        MOD-KDPRBEH-IN-UT (INDX)          
055300           MOVE MFS-STAENG-FAELT TO  MOD-SELECT-URVAL-ATTR (INDX)         
055400                                     MOD-KDPRBEH-IN-UT-ATTR(INDX)         
055500         END-IF                                                           
055600         ADD 1 TO INDX                                                    
055700       END-PERFORM                                                        
055800                                                                          
055900       IF SEGMENT-FINNS                                                   
056000         MOVE PRIE-ART-IDLEVNR TO MOD-IDLEVNR-NEXT                        
056100         MOVE PRIE-ART-IDARTNR TO MOD-IDARTNR-NEXT                        
056110         MOVE PRIE-ART-KDPRBEH TO MOD-KDPRBEH-NEXT                        
056120         MOVE PRIE-ART-REAENDR TO MOD-REAENDR-NEXT                        
056130         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
056140         CALL WMEDKONV USING MED-WMEDAREA                                 
056150         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
056160       ELSE                                                               
056170         MOVE PRIE-ART-IDLEVNR TO MOD-IDLEVNR-NEXT                        
056180         MOVE ZERO             TO MOD-IDARTNR-NEXT                        
056190         MOVE PRIE-ART-KDPRBEH TO MOD-KDPRBEH-NEXT                        
056191         MOVE PRIE-ART-REAENDR TO MOD-REAENDR-NEXT                        
056192         MOVE INF-LAST-PAGE    TO MED-IDMFSINF                            
056193         CALL WMEDKONV USING MED-WMEDAREA                                 
056194         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
056195       END-IF                                                             
056196                                                                          
056197     END-IF                                                               
056198     .                                                                    
056199     EJECT                                                                
056200 FB-LAES-VISA-UNIK SECTION.                                               
056300                                                                          
056400     IF SEGMENT-SAKNAS                                                    
056410        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
056600        CALL WMEDKONV USING MED-WMEDAREA                                  
056700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
056800        PERFORM MFS-RENSA-FAELT-UT                                        
056900     ELSE                                                                 
057000       MOVE +1 TO INDX                                                    
057100       IF SEGMENT-FINNS                                                   
057200         MOVE PRIE-ART-IDLEVNR TO MOD-IDLEVNR-ENTER                       
057300         MOVE PRIE-ART-IDARTNR TO MOD-IDARTNR-ENTER                       
057400         MOVE PRIE-ART-KDPRBEH TO MOD-KDPRBEH-ENTER                       
057500         MOVE PRIE-ART-REAENDR TO MOD-REAENDR-ENTER                       
057601                                                                          
057602****************************************************************          
057603******** EFTERSOM DET ÄR EN UNIK LÄSNING FLYTTAS INFO ENDAST              
057604******** EN GÅNG .                                                        
057605****************************************************************          
057610         MOVE MFS-OEPPNA-ALFA-FAELT TO                                    
057611                                  MOD-SELECT-URVAL-ATTR (INDX)            
057620         MOVE PRIE-ART-IDARTNR       TO MOD-IDARTNR       (INDX)          
057630         MOVE PRIE-ART-PRARTBEL-PR TO MOD-PRARTBEL-PR     (INDX)          
057640         MOVE PRIE-ART-KDVALISO      TO MOD-KDVALISO      (INDX)          
057650         MOVE PRIE-ART-PRINK-AKT     TO MOD-PRINK-AKT     (INDX)          
057660         MOVE PRIE-ART-PRINK-KOM     TO MOD-PRINK-KOM     (INDX)          
057670                                                                          
057680         COMPUTE WS-SPIS-STDPRIS = PRIE-ART-PRINK-KOM +                   
057690                 PRIE-ART-PRDIRLON-KOM +                                  
057691                 PRIE-ART-PRDMTRL-KOM    +                                
057692                 PRIE-ART-PROVRPAL-KOM                                    
057693                                                                          
057694         MOVE WS-SPIS-STDPRIS        TO MOD-SPIS-PRARTSTD (INDX)          
057695         MOVE PRIE-ART-REAENDR       TO MOD-REAENDR-INK   (INDX)          
057696         MOVE PRIE-ART-KDPRBEH       TO MOD-KDPRBEH-IN-UT (INDX)          
057697         ADD +1 TO INDX                                                   
057700       ELSE                                                               
057800         MOVE SPACE            TO MOD-IDLEVNR-ENTER                       
057810         MOVE ZERO             TO MOD-IDARTNR-ENTER                       
057820         MOVE SPACE            TO MOD-KDPRBEH-ENTER                       
057830         MOVE ZERO             TO MOD-REAENDR-ENTER                       
057840       END-IF                                                             
057850                                                                          
057860       PERFORM UNTIL INDX > MAX-INDX                                      
057908         MOVE MFS-RENSA-FAELT        TO MOD-SELECT-URVAL  (INDX)          
057909                                      MOD-IDARTNR         (INDX)          
057910                                      MOD-PRARTBEL-PR     (INDX)          
057911                                      MOD-KDVALISO        (INDX)          
057912                                      MOD-PRINK-AKT       (INDX)          
057913                                      MOD-PRINK-KOM       (INDX)          
057914                                      MOD-SPIS-PRARTSTD (INDX)            
057915                                      MOD-REAENDR-INK     (INDX)          
057916                                      MOD-KDPRBEH-IN-UT (INDX)            
057917         MOVE MFS-STAENG-FAELT TO MOD-SELECT-URVAL-ATTR (INDX)            
057919                                   MOD-KDPRBEH-IN-UT-ATTR (INDX)          
057920         ADD 1 TO INDX                                                    
057921       END-PERFORM                                                        
057922                                                                          
057930       MOVE PRIE-ART-IDLEVNR TO MOD-IDLEVNR-NEXT                          
057931       MOVE PRIE-ART-IDARTNR TO MOD-IDARTNR-NEXT                          
057932       MOVE SPACE              TO MOD-KDPRBEH-NEXT                        
057933       MOVE ZERO               TO MOD-REAENDR-NEXT                        
057941                                                                          
057942     END-IF                                                               
057943     .                                                                    
057950     EJECT                                                                
058033 G-KOLLA-INPUT SECTION.                                                   
058040                                                                          
058100     MOVE JA  TO INDATA-SW                                                
058200     MOVE +1 TO INDX                                                      
058300     PERFORM UNTIL INDX > MAX-INDX                                        
058500       IF MID-KDPRBEH-IN-UT (INDX) = ALL '+'                              
058600          MOVE NEJ TO KOLL-SW                                             
058700       ELSE                                                               
058800          MOVE JA TO KOLL-SW                                              
058900          ADD +9999 TO INDX                                               
059000       END-IF                                                             
059100       ADD +1 TO INDX                                                     
059200     END-PERFORM                                                          
059300                                                                          
059400     IF RAD-FINNS-EJ                                                      
059500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
059600       CALL WMEDKONV USING MED-WMEDAREA                                   
059700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
059800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
059900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
060000       MOVE NEJ TO INDATA-SW                                              
060100     ELSE                                                                 
060200       MOVE +1 TO INDX                                                    
060300       PERFORM UNTIL INDX > MAX-INDX                                      
061900         IF MID-KDPRBEH-IN-UT (INDX) NOT = ALL '+'                        
062000           IF MID-KDPRBEH-IN-UT (INDX) = 'J' OR 'N'                       
062100             MOVE MFS-ALFA-FAELT-RAETT TO                                 
062200                  MOD-KDPRBEH-IN-UT-ATTR (INDX)                           
062400           ELSE                                                           
062500             MOVE MFS-ALFA-FAELT-FEL TO                                   
062600                  MOD-KDPRBEH-IN-UT-ATTR (INDX)                           
062610             MOVE NEJ TO INDATA-SW                                        
062620             MOVE 'OTILLÅTEN BEHANDLINGSKOD' TO MOD-TEMFSINF              
062700           END-IF                                                         
062701         ELSE                                                             
062710           MOVE MFS-ALFA-FAELT-RAETT TO                                   
062720                MOD-KDPRBEH-IN-UT-ATTR (INDX)                             
062800         END-IF                                                           
062900                                                                          
063000         ADD +1 TO INDX                                                   
063100       END-PERFORM                                                        
063200                                                                          
063300       IF INDATA-FEL                                                      
063310         MOVE NEJ TO ALLT-SW                                              
063400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
063500         CALL WMEDKONV USING MED-WMEDAREA                                 
063600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
063700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
063800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
063900       END-IF                                                             
064000     END-IF                                                               
064100     .                                                                    
064200     EJECT                                                                
064300 H-UPPDATERA SECTION.                                                     
064400     SKIP2                                                                
064500     MOVE +1 TO INDX                                                      
064600     PERFORM UNTIL INDX > MAX-INDX                                        
064700       IF MID-KDPRBEH-IN-UT (INDX) NOT = ALL '+'                          
064800         MOVE MID-IDARTNR (INDX) TO W-IDARTNR                             
064900                                                                          
065000         PERFORM IMS-GHU-WDC601                                           
065100         IF SEGMENT-FINNS                                                 
065200           MOVE MID-KDPRBEH-IN-UT(INDX) TO PRIE-ART-KDPRBEH               
065300           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
065400                MOD-KDPRBEH-IN-UT-ATTR (INDX)                             
065500         END-IF                                                           
065600         PERFORM IMS-REPL-WDC6                                            
065700       ELSE                                                               
065800         MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRBEH-IN-UT-ATTR(INDX)           
065900       END-IF                                                             
066000       ADD +1 TO INDX                                                     
066100     END-PERFORM                                                          
066200                                                                          
066300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
066400     CALL WMEDKONV USING MED-WMEDAREA                                     
066500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
066600     PERFORM MFS-FORM-ATTR                                                
066700     PERFORM MFS-RENSA-FAELT-IN                                           
066800     .                                                                    
066900     EJECT                                                                
066910 Q-KOLL-OM-BYT-INMATAT SECTION.                                           
066920     SKIP2                                                                
066930     IF EGEN-MID                                                          
066940       MOVE +1 TO INDX                                                    
066950       PERFORM UNTIL INDX > MAX-INDX                                      
066960         IF MID-SELECT-URVAL (INDX) NOT = ALL '+'                         
066970           IF MID-SELECT-URVAL (INDX) = 'S'                               
066980             IF NOT MFS-UPDATE                                            
066990               IF MFS-ENTER                                               
066991                 MOVE NEJ TO ALLT-SW                                      
066992                 MOVE JA TO BYT-SW                                        
066993                 PERFORM QA-BYT-BILD                                      
066994                 MOVE +9999 TO INDX                                       
066995               ELSE                                                       
066996                 MOVE MFS-ALFA-FAELT-FEL TO                               
066997                      MOD-SELECT-URVAL-ATTR (INDX)                        
066998                 MOVE +9999 TO INDX                                       
066999               END-IF                                                     
067000             ELSE                                                         
067001               MOVE MFS-ALFA-FAELT-FEL TO                                 
067002                    MOD-SELECT-URVAL-ATTR (INDX)                          
067003               MOVE +9999 TO INDX                                         
067004             END-IF                                                       
067005           ELSE                                                           
067006             MOVE MFS-ALFA-FAELT-FEL TO                                   
067007                  MOD-SELECT-URVAL-ATTR (INDX)                            
067008             MOVE +9999 TO INDX                                           
067009           END-IF                                                         
067010         END-IF                                                           
067011         ADD +1 TO INDX                                                   
067012       END-PERFORM                                                        
067013     END-IF                                                               
067014     .                                                                    
067015     EJECT                                                                
067016 QA-BYT-BILD SECTION.                                                     
067017     SKIP2                                                                
067018     IF ENGLISH-TEXT                                                      
067019       MOVE '2' TO M-SW-KDMFSTYP                                          
067020     ELSE                                                                 
067021       MOVE '1' TO M-SW-KDMFSTYP                                          
067022     END-IF                                                               
067023                                                                          
067024     INSPECT MID-IDARTNR (INDX) REPLACING LEADING SPACE BY ZERO           
067026                                                                          
067027     MOVE LOW-VALUE TO 5114-MID-W5I11401                                  
067028     MOVE MID-IDARTNR (INDX) TO 5114-MID-IDARTNR-IN                       
067029     MOVE MID-IDLEVNR-ENTER  TO 5114-MID-IDLEVNR-IN                       
067030     MOVE MID-KDPRBEH-UT     TO 5114-MID-KDPRBEH-IN                       
067032     MOVE MID-REAENDR-UT     TO 5114-MID-REAENDR-IN                       
067034     PERFORM IMS-INSERT-ALT-MSG                                           
067035     .                                                                    
067036     EJECT                                                                
067040 MFS-RENSA-FAELT-UT SECTION.                                              
067100                                                                          
067200*    --- ALLA UTDATA-FÄLT                                                 
067300*    --- INKL. BLÄDDRINGSNYCKLAR                                          
067400     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-ENTER                            
067500                             MOD-IDLEVNR-NEXT                             
067600                             MOD-IDARTNR-ENTER                            
067700                             MOD-IDARTNR-NEXT                             
067800                             MOD-KDPRBEH-ENTER                            
067900                             MOD-KDPRBEH-NEXT                             
068000                             MOD-REAENDR-ENTER                            
068100                             MOD-REAENDR-NEXT                             
068300                             MOD-TIUPPDAT                                 
068400     MOVE +1 TO INDX                                                      
068500     PERFORM UNTIL INDX > MAX-INDX                                        
068600       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
068700       ADD +1 TO INDX                                                     
068800     END-PERFORM                                                          
068900     .                                                                    
069000     SKIP3                                                                
069100 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
069200                                                                          
069300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
069400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR       (INDX)                     
069600                             MOD-PRARTBEL-PR   (INDX)                     
069700                             MOD-KDVALISO      (INDX)                     
069800                             MOD-PRINK-AKT     (INDX)                     
069900                             MOD-PRINK-KOM     (INDX)                     
070000                             MOD-SPIS-PRARTSTD (INDX)                     
070100                             MOD-REAENDR-INK   (INDX)                     
070200                             MOD-KDPRBEH-IN-UT (INDX)                     
070300     .                                                                    
070400     SKIP3                                                                
070500 MFS-RENSA-FAELT-IN SECTION.                                              
070600                                                                          
070700*    --- ALLA INDATA-FÄLT                                                 
070800     SKIP2                                                                
070900     MOVE +1 TO INDX                                                      
071000     PERFORM UNTIL INDX > MAX-INDX                                        
071100       MOVE MFS-RENSA-FAELT TO MOD-SELECT-URVAL  (INDX)                   
071200                               MOD-KDPRBEH-IN-UT (INDX)                   
071300       ADD +1 TO INDX                                                     
071400     END-PERFORM                                                          
071500     .                                                                    
071600     EJECT                                                                
071700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
071800                                                                          
071900*    --- ALLA UTDATA-FÄLT                                                 
072000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
072100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-ENTER                          
072200                               MOD-IDLEVNR-NEXT                           
072300                               MOD-IDARTNR-ENTER                          
072400                               MOD-IDARTNR-NEXT                           
072500                               MOD-KDPRBEH-ENTER                          
072600                               MOD-KDPRBEH-NEXT                           
072700                               MOD-REAENDR-ENTER                          
072800                               MOD-REAENDR-NEXT                           
073000                               MOD-TIUPPDAT                               
073100     MOVE +1 TO INDX                                                      
073200     PERFORM UNTIL INDX > MAX-INDX                                        
073300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
073400       ADD +1 TO INDX                                                     
073500     END-PERFORM                                                          
073600     .                                                                    
073700     EJECT                                                                
073800     SKIP2                                                                
073900 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
074000                                                                          
074100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
074200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR       (INDX)                   
074400                               MOD-PRARTBEL-PR   (INDX)                   
074500                               MOD-KDVALISO      (INDX)                   
074600                               MOD-PRINK-AKT     (INDX)                   
074700                               MOD-PRINK-KOM     (INDX)                   
074800                               MOD-SPIS-PRARTSTD (INDX)                   
074900                               MOD-REAENDR-INK   (INDX)                   
075000                               MOD-KDPRBEH-IN-UT (INDX)                   
075100     .                                                                    
075200     SKIP3                                                                
075300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
075400                                                                          
075500*    --- ALLA INDATA-FÄLT                                                 
075600     MOVE +1 TO INDX                                                      
075700     PERFORM UNTIL INDX > MAX-INDX                                        
075800       MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT-URVAL  (INDX)                 
075900                                 MOD-KDPRBEH-IN-UT (INDX)                 
076000       ADD +1 TO INDX                                                     
076100     END-PERFORM                                                          
076200     .                                                                    
076300     SKIP2                                                                
076400 MFS-FORM-ATTR SECTION.                                                   
076500                                                                          
076600*    --- ALLA INDATA-FÄLT                                                 
076700     MOVE +1 TO INDX                                                      
076800     PERFORM UNTIL INDX > MAX-INDX                                        
076900       MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-URVAL-ATTR (INDX)            
077000                                  MOD-KDPRBEH-IN-UT-ATTR (INDX)           
077100       ADD +1 TO INDX                                                     
077200     END-PERFORM                                                          
077300     .                                                                    
077400     EJECT                                                                
077500* --- IMS SEKTIONER ---                                                   
077600     SKIP3                                                                
077700 IMS-GET-MSG SECTION.                                                     
077800                                                                          
077900     MOVE '  QC' TO GODK-STATUSKODER                                      
078000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
078100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
078200     PERFORM IMS-STATUSKONTROLL                                           
078300     .                                                                    
078400     SKIP3                                                                
078500 IMS-INSERT-MSG SECTION.                                                  
078600                                                                          
078710     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
078800       MOVE '0' TO MFS-KDHUVOMR                                           
078900     END-IF                                                               
079000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
079100     MOVE SPACE TO GODK-STATUSKODER                                       
079200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
079300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
079400     PERFORM IMS-STATUSKONTROLL                                           
079500     .                                                                    
079600     SKIP2                                                                
079610 IMS-INSERT-ALT-MSG SECTION.                                              
079620                                                                          
079670     MOVE SPACE TO GODK-STATUSKODER                                       
079680     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
079690     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
079691     PERFORM IMS-STATUSKONTROLL                                           
079692     .                                                                    
079693     EJECT                                                                
079700 IMS-GN-WDC6A1 SECTION.                                                   
079800                                                                          
079900     STRING 'WLPRIF01(WDC6A1KY>=' W-WDC6A1KY-MIN-X                        
079910                    '&WDC6A1KY<=' W-WDC6A1KY-MAX-X ')'                    
080000          DELIMITED BY SIZE INTO SSA1                                     
080100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
080200     CALL CBLTDLI USING GN PRIF-PCB DLI-IO-AREA SSA1                      
080300     MOVE PRIF-STATUS-CODE TO STATUS-WS                                   
080400     PERFORM IMS-STATUSKONTROLL                                           
080500     .                                                                    
080600     SKIP2                                                                
080700 IMS-GN-WDC6A1-KDPRBEH SECTION.                                           
080800                                                                          
080900     STRING 'WLPRIF01(WDC6A1KY>=' W-WDC6A1KY-MIN-X                        
081000                    '&WDC6A1KY<=' W-WDC6A1KY-MAX-X                        
081010                    '&KDPRBEH  =' W-KDPRBEH-X ')'                         
081100          DELIMITED BY SIZE INTO SSA1                                     
081200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
081300     CALL CBLTDLI USING GN PRIF-PCB DLI-IO-AREA SSA1                      
081400     MOVE PRIF-STATUS-CODE TO STATUS-WS                                   
081500     PERFORM IMS-STATUSKONTROLL                                           
081600     .                                                                    
081700     SKIP2                                                                
081710 IMS-GN-WDC6A1-REAENDR SECTION.                                           
081720                                                                          
081730     STRING 'WLPRIF01(WDC6A1KY>=' W-WDC6A1KY-MIN-X                        
081740                    '&WDC6A1KY<=' W-WDC6A1KY-MAX-X                        
081741                    '&REAENDR >=' W-REAENDR-X ')'                         
081750          DELIMITED BY SIZE INTO SSA1                                     
081760     MOVE '  GBGE' TO GODK-STATUSKODER                                    
081770     CALL CBLTDLI USING GN PRIF-PCB DLI-IO-AREA SSA1                      
081780     MOVE PRIF-STATUS-CODE TO STATUS-WS                                   
081790     PERFORM IMS-STATUSKONTROLL                                           
081791     .                                                                    
081792     EJECT                                                                
081793 IMS-GN-WDC6A1-KDPRBEH-REAENDR SECTION.                                   
081794                                                                          
081795     STRING 'WLPRIF01(WDC6A1KY>=' W-WDC6A1KY-MIN-X                        
081796                    '&WDC6A1KY<=' W-WDC6A1KY-MAX-X                        
081797                    '&KDPRBEH  =' W-KDPRBEH-X                             
081798                    '&REAENDR >=' W-REAENDR-X ')'                         
081799          DELIMITED BY SIZE INTO SSA1                                     
081800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
081801     CALL CBLTDLI USING GN PRIF-PCB DLI-IO-AREA SSA1                      
081802     MOVE PRIF-STATUS-CODE TO STATUS-WS                                   
081803     PERFORM IMS-STATUSKONTROLL                                           
081804     .                                                                    
081805     SKIP2                                                                
081810 IMS-GHU-WDC601 SECTION.                                                  
081900                                                                          
082000     STRING 'WLPRIE01(IDARTNR  =' W-IDARTNR-X ')'                         
082100          DELIMITED BY SIZE INTO SSA1                                     
082200     MOVE '  GE' TO GODK-STATUSKODER                                      
082300     CALL CBLTDLI USING GHU PRIE-PCB DLI-IO-AREA2 SSA1                    
082400     MOVE PRIE-STATUS-CODE TO STATUS-WS                                   
082500     PERFORM IMS-STATUSKONTROLL                                           
082600     .                                                                    
082700     SKIP3                                                                
083800 IMS-REPL-WDC6 SECTION.                                                   
083900                                                                          
084000     MOVE '  ' TO GODK-STATUSKODER                                        
084100     CALL CBLTDLI USING REPL PRIE-PCB DLI-IO-AREA2                        
084200     MOVE PRIE-STATUS-CODE TO STATUS-WS                                   
084300     PERFORM IMS-STATUSKONTROLL                                           
084400     .                                                                    
084500     EJECT                                                                
084600 IMS-GU-WDGX11 SECTION.                                                   
084700                                                                          
084800     STRING 'WLXXEH01(WDGXKEY  =' W-WDGX5119-X ')'                        
084900          DELIMITED BY SIZE INTO SSA1                                     
085000     MOVE 'WLXXEH11 '         TO SSA2                                     
085100     MOVE '  GE' TO GODK-STATUSKODER                                      
085200     CALL CBLTDLI USING GU XXEH-PCB DLI-IO-AREA3 SSA1 SSA2                
085300     MOVE XXEH-STATUS-CODE TO STATUS-WS                                   
085400     PERFORM IMS-STATUSKONTROLL                                           
085500     .                                                                    
085600     EJECT                                                                
085700 IMS-STATUSKONTROLL SECTION.                                              
085800                                                                          
085900     SET STATUS-IX TO 1                                                   
086000     SEARCH GODK-STATUS                                                   
086100       AT END                                                             
086200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
086300         DELIMITED BY SIZE INTO FELTEXT                                   
086400         CALL FELLOG                                                      
086500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
086600         CONTINUE                                                         
086700     END-SEARCH                                                           
086800     .                                                                    
