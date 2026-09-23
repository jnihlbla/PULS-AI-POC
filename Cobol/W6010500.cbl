000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010500.                                                
000400*AUTHOR.         PER BERGH.                                               
000500*DATE-WRITTEN.   92/02/07.                                                
000600                                                                          
000700***  REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        DEFINIERA STYRPARAMETRAR.                                        
001100*        PROGRAMMET LÄGGER UPP, VISAR OCH TAR BORT STYRPARAMETRAR.        
001200*        LÄSER PLACERINGSREGISTRET                                        
001300*        W6PLAA (W6G101 -130) CTEXT W6GX01 -W6GX6006                      
001400*        LÄSER OCH UPPDATERAR STYRPARAMETEWREGISTRET                      
001500*        W6HANB (W6G1)        CTEXT W6GX01 -W6GX6032                      
001510*                                          -W6GX6034                      
001520*                                          -W6GX6036                      
001530*                                          -W6GX6038                      
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W6T105                                              
001900*        MID:         W6I10501                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W6O10501                                            
002300***                                                                       
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002801                                                                          
002810*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W6010500'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- RÄKNARE                                                          
003800 77  RAEKNARE                    PIC S9(3)  VALUE +1    COMP SYNC.        
003900                                                                          
004000*    --- INDEX FÖR BLÄDDRINGSRADER OCH KONTROLLER                         
004100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004110 77  TAB-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004120 77  MAXTAB-IX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  RAD-IX                      PIC S9(4)  VALUE +1    COMP SYNC.        
004300 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +961  COMP SYNC.        
004512 77  WS-BEFT-NUM2                PIC 9(2)   VALUE ZERO.                   
005400                                                                          
006500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006600 77  WS-ADINLOMR                 PIC X(4)    VALUE SPACE.                 
006610 77  IDDC-WS                     PIC X(2)    VALUE SPACE.                 
006700     EJECT                                                                
006710*      --- VALID IDDC CODES                                               
006720*                                                                         
006730*01    -COPY WWDCKONS                                                     
006740       EJECT                                                              
006800*    --- SWITCHAR                                                         
006900                                                                          
007000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007100     88  INDATA-OK                           VALUE 'J'.                   
007200     88  INDATA-FEL                          VALUE 'N'.                   
007300                                                                          
007400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007500     88  NYCKLAR-OK                          VALUE 'J'.                   
007600     88  NYCKLAR-FEL                         VALUE 'N'.                   
007700                                                                          
007800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007900     88  ALLT-OK                             VALUE 'J'.                   
008000                                                                          
008100 77  START-SW                    PIC X       VALUE 'N'.                   
008200     88  START-OK                            VALUE 'J'.                   
008300                                                                          
008400 77  SID-SW                      PIC X       VALUE 'N'.                   
008500     88  FLER-SIDOR-FINNS                    VALUE 'J'.                   
008600     88  FLER-SIDOR-FINNS-EJ                 VALUE 'N'.                   
008700                                                                          
008800 77  URVAL-SW                    PIC X       VALUE 'J'.                   
008900     88  URVAL-SAKNAS                        VALUE 'N'.                   
009000     88  URVAL-FINNS                         VALUE 'J'.                   
009100                                                                          
009200 77  TAB-SW                      PIC X       VALUE 'N'.                   
009300     88  TAB-TRAEFF                          VALUE 'J'.                   
009400                                                                          
009500 77  ARTNR-TAB-SW                PIC X       VALUE 'N'.                   
009600     88  ARTNR-TAB-KLAR                      VALUE 'J'.                   
009700                                                                          
009800 77  FKNGRP-TAB-SW               PIC X       VALUE 'N'.                   
009900     88  FKNGRP-TAB-KLAR                     VALUE 'J'.                   
010000                                                                          
010100 77  LEVNR-TAB-SW                PIC X       VALUE 'N'.                   
010200     88  LEVNR-TAB-KLAR                      VALUE 'J'.                   
010300                                                                          
010400 77  FT-TAB-SW                   PIC X       VALUE 'N'.                   
010500     88  FT-TAB-KLAR                         VALUE 'J'.                   
010600                                                                          
010700 77  ARTNR-INPUT-SW              PIC X       VALUE 'J'.                   
010800     88  ARTNR-INPUT-SAKNAS                  VALUE 'N'.                   
010900                                                                          
011000 77  FKNGRP-INPUT-SW             PIC X       VALUE 'J'.                   
011100     88  FKNGRP-INPUT-SAKNAS                 VALUE 'N'.                   
011200                                                                          
011300 77  LEVNR-INPUT-SW              PIC X       VALUE 'J'.                   
011400     88  LEVNR-INPUT-SAKNAS                  VALUE 'N'.                   
011500                                                                          
011600 77  CMDKOD-SW                   PIC X       VALUE 'N'.                   
011700     88  CMDKOD-FINNS                        VALUE 'J'.                   
011800     88  CMDKOD-SAKNAS                       VALUE 'N'.                   
011900                                                                          
012000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012100     88  EGEN-MID                            VALUE '6105'.                
012200     88  HELP-MID                            VALUE '0551'.                
012300     88  GODK-MID                            VALUE '6105'.                
012700     EJECT                                                                
012800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012900 01  GENERELLA-SUBPROGRAM.                                                
013000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013210     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013300     SKIP3                                                                
013310*01 -COPY WMSGINIT                                                        
013320     SKIP3                                                                
013400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013500*01 -COPY WMEDAREA                                                        
013600     SKIP3                                                                
013700 01  MESSAGE-CODES.                                                       
013800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014100     03  ERR-NOT-ON-REGISTER     PIC X(3)    VALUE '010'.                 
014200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014300     03  ERR-MANY-FUNCTIONS      PIC X(3)    VALUE '097'.                 
014400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014600     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
014700     03  ERR-LAST-PAGE-SHOWED    PIC X(3)    VALUE '115'.                 
014800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014900     03  ERR-WRONG-CHOISE        PIC X(3)    VALUE '416'.                 
015000     03  ERR-WRONG-INTERVAL      PIC X(3)    VALUE '738'.                 
015100     EJECT                                                                
015200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015300*                                                                         
015400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015500     SKIP3                                                                
015600*01  MID -COPY W6I10501                                                   
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015900     SKIP3                                                                
016000*01  -COPY WMSGAREA                                                       
016100     EJECT                                                                
016200     03  MOD REDEFINES MSG-AREA.                                          
016300*      05  -COPY W6O10501                                                 
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016600     SKIP3                                                                
016700*01  -COPY WMFSAREA                                                       
016800     EJECT                                                                
016900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017000*                                                                         
017100     SKIP2                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-6005KEY-X.                                                     
017600         05  W-6005-IDHTYP       PIC X(04)       VALUE '6005'.            
017610         05  W-6005-IDDC         PIC X(02)       VALUE SPACE.             
017700         05  W-6005-LOW-VALUE    PIC X(24).                               
017800     03  W-ADINLOMR-X.                                                    
017900         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
018000     03  W-6031KEY-X.                                                     
018100         05  W-6031-IDHTYP       PIC X(04)       VALUE '6031'.            
018110         05  W-6031-IDDC         PIC X(02)       VALUE SPACE.             
018200         05  W-6031-LOW-VALUE    PIC X(24).                               
018300     03  W-KDSEGKEY-X.                                                    
018400         05  W-KDSEGKEY          PIC X(01)    VALUE '1'.                  
018410     03  W-IDARTNR-FOM-X.                                                 
018420         05  W-IDARTNR-FOM       PIC S9(9)    VALUE +0 COMP-3.            
018430     03  W-IDARTNR-TOM-X.                                                 
018440         05  W-IDARTNR-TOM       PIC S9(9)    VALUE +0 COMP-3.            
018450     03  W-IDFKNGRP-FOM-X.                                                
018460         05  W-IDFKNGRP-FOM      PIC S9(5)    VALUE +0 COMP-3.            
018470     03  W-IDFKNGRP-TOM-X.                                                
018480         05  W-IDFKNGRP-TOM      PIC S9(5)    VALUE +0 COMP-3.            
018490     03  W-IDLEVNR-X.                                                     
018491         05  W-IDLEVNR           PIC X(5)     VALUE SPACE.                
018494     03  W-BEFT-FOM-X.                                                    
018495         05  W-BEFT-FOM          PIC S9(3)    VALUE +0 COMP-3.            
018496     03  W-BEFT-TOM-X.                                                    
018497         05  W-BEFT-TOM          PIC S9(3)    VALUE +0 COMP-3.            
018498                                                                          
018499     03  W-IDDC-B6-X.                                                     
018500         05 W-IDDC-B6            PIC X(2).                                
018600                                                                          
020186     SKIP2                                                                
020187*    --- ARBETSFÄLT                                                       
020188 01  W-KDINLOMR                  PIC X(03)    VALUE SPACE.                
020189                                                                          
020192 01  SPAR-TABELL.                                                         
020193     03  SPAR-IDARTNR-FOM   OCCURS 12 PIC  9(8)     VALUE ZEROS.          
020194     03  SPAR-IDARTNR-TOM   OCCURS 12 PIC  9(8)     VALUE ZEROS.          
020198     03  SPAR-IDFKNGRP-FOM  OCCURS 12 PIC  9(4)     VALUE ZEROS.          
020199     03  SPAR-IDFKNGRP-TOM  OCCURS 12 PIC  9(4)     VALUE ZEROS.          
020200     03  SPAR-BEFT-FOM      OCCURS 12 PIC  9(2)     VALUE ZEROS.          
020201     03  SPAR-BEFT-TOM      OCCURS 12 PIC  9(2)     VALUE ZEROS.          
020210     03  SPAR-IDLEVNR       OCCURS 12 PIC  X(5)     VALUE SPACE.          
020220     03  SPAR-ADINLOMR      OCCURS 12 PIC  X(4)     VALUE SPACE.          
020500                                                                          
021000                                                                          
021500     SKIP2                                                                
021600*    --- STATUS-KOD FRÅN IMS                                              
021700 01  STATUS-WS                   PIC XX.                                  
021800     88  SEGMENT-FINNS                       VALUE '  '.                  
021900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022100     SKIP2                                                                
022200 01  GODK-STATUSKODER.                                                    
022300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022400     SKIP3                                                                
022500 01  SSA1                        PIC X(64).                               
022600 01  SSA2                        PIC X(64).                               
022700     EJECT                                                                
022800*    --- IMS FUNKTIONSKODER                                               
022900*01  -COPY W0003                                                          
023000     EJECT                                                                
024100*    ---  DLI INPUT-OUTPUT AREA                                           
024200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
024300     SKIP3                                                                
024400 01  DLI-IO-AREA1.                                                        
024500     03  IO-AREA1                PIC X(100) VALUE SPACE.                  
024600     SKIP3                                                                
024700     03  W6PLAA01 REDEFINES IO-AREA1.                                     
024800*        05  -COPY W6GX01                                                 
024900     SKIP3                                                                
025000     03  W6PLAA11 REDEFINES IO-AREA1.                                     
025100*        05  -COPY W6GX6006                                               
025200     EJECT                                                                
025300     03  W6HANB01 REDEFINES IO-AREA1.                                     
025400*        05  -COPY W6GX01                                                 
025500     EJECT                                                                
025600 01  DLI-IO-AREA2.                                                        
025700     03  IO-AREA2                PIC X(100) VALUE SPACE.                  
025800     SKIP3                                                                
026010     03  W6HANB11 REDEFINES IO-AREA2.                                     
026020*        05  -COPY W6GX6032                                               
026030     03  W6HANB12 REDEFINES IO-AREA2.                                     
026040*        05  -COPY W6GX6034                                               
026050     03  W6HANB13 REDEFINES IO-AREA2.                                     
026060*        05  -COPY W6GX6036                                               
026070     03  W6HANB14 REDEFINES IO-AREA2.                                     
026080*        05  -COPY W6GX6038                                               
026090                                                                          
026091 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026092 01   DLI-IO-AREA-B601.                                                   
026093*     03  -COPY WDB601                                                    
026094                                                                          
026100     EJECT                                                                
026200 LINKAGE SECTION.                                                         
026300                                                                          
026400*01  -COPY W0009   -PRE MSG-                                              
026500     EJECT                                                                
026600*01  -COPY W0008  -PRE USEA-                                              
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026810*01  -COPY W0008  -PRE PLAA-                                              
026820     05  FILLER                  PIC X.                                   
026830     EJECT                                                                
026900*01  -COPY W0008  -PRE HANB-                                              
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027110*01  -COPY W0008  -PRE WDB6-                                              
027120     05  FILLER                  PIC X.                                   
027130     EJECT                                                                
027200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB PLAA-PCB HANB-PCB             
027210                           WDB6-PCB.                                      
027300     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB PLAA-PCB HANB-PCB             
027310                           WDB6-PCB.                                      
027400                                                                          
027500     PERFORM IMS-GET-MSG                                                  
027600     IF SEGMENT-FINNS                                                     
027700       PERFORM A-INIT                                                     
027800       PERFORM B-KOLLA-NYCKLAR                                            
027900       IF NYCKLAR-OK                                                      
028000         IF MFS-UPDATE                                                    
028100           PERFORM G-KOLLA-INPUT                                          
028200           IF INDATA-OK                                                   
028300             PERFORM H-UPPDATERA                                          
028400           END-IF                                                         
028500         ELSE                                                             
028600           IF MFS-FIRST                                                   
028700             PERFORM C-FOERSTA-SIDA                                       
028800           ELSE                                                           
028900             IF MFS-NEXT                                                  
029000               PERFORM D-NAESTA-SIDA                                      
029100             ELSE                                                         
029200               PERFORM E-SAMMA-SIDA                                       
029300             END-IF                                                       
029400           END-IF                                                         
029500         END-IF                                                           
029600         IF ALLT-OK                                                       
029700           PERFORM F-LAES-VISA-INFO                                       
029800         END-IF                                                           
029900       END-IF                                                             
030000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
030100       PERFORM IMS-INSERT-MSG                                             
030200     END-IF                                                               
030300                                                                          
030400     MOVE ZERO TO RETURN-CODE                                             
030500     GOBACK                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 A-INIT SECTION.                                                          
031000                                                                          
031100     IF MSG-DUBBLA-TRANSKODER                                             
031200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I10501                 
031300       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
031400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031500     ELSE                                                                 
031600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I10501                  
031700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
031800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031900     END-IF                                                               
032000                                                                          
032100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032200     MOVE MSG-IDPFK TO MFS-IDPFK                                          
032300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032400                                                                          
032500     MOVE LOW-VALUE TO MSG-AREA                                           
032600     MOVE 'W6O105N1' TO MFS-IDMOD                                         
032700     MOVE '6105' TO MOD-IDTRANS                                           
032800     MOVE     SPACE       TO MOD-TEMFSFEL MOD-TEMFSINF                    
032900                                                                          
033000     IF EGEN-MID OR HELP-MID                                              
033100       CONTINUE                                                           
033200     ELSE                                                                 
033300       MOVE SPACE TO MFS-KDTRTYP                                          
033400       MOVE '7' TO MFS-IDPFK                                              
033500     END-IF                                                               
034400                                                                          
034500     MOVE LOW-VALUE        TO W-6005-LOW-VALUE                            
034600                              W-6031-LOW-VALUE                            
034601     PERFORM AA-INIT-NYCKLAR                                              
034602                                                                          
034603     IF MSGI-IDLAND-SPR = 'GB'                                            
034604       MOVE +2 TO SPRAK-IX                                                
034605       MOVE 'GB ' TO MED-IDSKYLT                                          
034606     ELSE                                                                 
034607       MOVE +1 TO SPRAK-IX                                                
034608       MOVE 'S  ' TO MED-IDSKYLT                                          
034609     END-IF                                                               
034610     .                                                                    
034611     EJECT                                                                
034612*----------------------------------------------------------------*        
034613 AA-INIT-NYCKLAR SECTION.                                                 
034614                                                                          
034615     MOVE ALL '+' TO MSGI-WMSGINIT                                        
034616     MOVE '001'                  TO MSGI-KDCALL                           
034617     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
034618     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
034619     MOVE '6105'                 TO MSGI-IDTRANS                          
034620     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034700     .                                                                    
034800     EJECT                                                                
034900 B-KOLLA-NYCKLAR SECTION.                                                 
035100                                                                          
035200     MOVE JA TO NYCKLAR-SW                                                
035300     MOVE JA TO URVAL-SW                                                  
035400                                                                          
035500*    -- KONTROLL AV ADINLOMR                                              
035600                                                                          
035700     IF MID-ADINLOMR-IN = ALL '+'                                         
035800       MOVE MID-ADINLOMR-UT TO WS-ADINLOMR                                
035900     ELSE                                                                 
036000       MOVE MID-ADINLOMR-IN TO WS-ADINLOMR                                
036100       MOVE '7'         TO MFS-IDPFK                                      
036200       MOVE SPACE       TO MFS-KDTRTYP                                    
036300     END-IF                                                               
036400     MOVE WS-ADINLOMR TO W-ADINLOMR                                       
036500                                                                          
036510*    -- KONTROLL AV IDDC                                                  
036520                                                                          
036521     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
036530     IF MID-IDDC-IN = ALL '+'                                             
036540       MOVE MSGI-IDDC   TO IDDC-WS                                        
036550     ELSE                                                                 
036560       MOVE MID-IDDC-IN TO IDDC-WS                                        
036570       MOVE '7'         TO MFS-IDPFK                                      
036580       MOVE SPACE       TO MFS-KDTRTYP                                    
036590     END-IF                                                               
036591                                                                          
036592     MOVE IDDC-WS     TO W-IDDC-B6                                        
036593     PERFORM IMS-GU-WDB601                                                
036594     IF DCS-KDDC = SPACE OR DCS-DDC                                       
036595         MOVE NEJ       TO NYCKLAR-SW                                     
036596     ELSE                                                                 
036597         MOVE IDDC-WS   TO W-6005-IDDC                                    
036598                           W-6031-IDDC                                    
036600     END-IF                                                               
036601                                                                          
036610     PERFORM BA-KOLL-ADINLOMR                                             
036700     IF GODK-MID OR NYCKLAR-OK                                            
036800       IF WS-ADINLOMR = ALL '+'                                           
036900         MOVE SPACE TO MOD-ADINLOMR-UT                                    
037000       ELSE                                                               
037100         MOVE WS-ADINLOMR TO MOD-ADINLOMR-UT                              
037200         INSPECT MOD-ADINLOMR-UT REPLACING LEADING ZERO BY SPACE          
037210         MOVE IDDC-WS     TO MOD-IDDC-UT                                  
037220         INSPECT MOD-IDDC-UT     REPLACING LEADING ZERO BY SPACE          
037300       END-IF                                                             
037400     ELSE                                                                 
037500       MOVE SPACE TO MOD-ADINLOMR-UT                                      
037510                     MOD-IDDC-UT                                          
037600     END-IF                                                               
037700                                                                          
037800     IF NYCKLAR-FEL                                                       
037810       IF GODK-MID                                                        
037811         PERFORM S09-ERR-WRONG-KEY                                        
037820       ELSE                                                               
037821         PERFORM S11-RENSA-FAELT-UT                                       
037822         PERFORM S12-RENSA-FAELT-IN                                       
037830       END-IF                                                             
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 BA-KOLL-ADINLOMR SECTION.                                                
038500                                                                          
038600     EVALUATE W-ADINLOMR                                                  
038700        WHEN ALL SPACE                                                    
038800             MOVE NEJ TO URVAL-SW                                         
038900        WHEN '0000'                                                       
039000             MOVE NEJ TO NYCKLAR-SW                                       
039100        WHEN ALL '+'                                                      
039200             MOVE NEJ TO NYCKLAR-SW                                       
039300             MOVE SPACE TO W-ADINLOMR                                     
039310        WHEN 'FB  '                                                       
039320             MOVE 'FBP' TO W-KDINLOMR                                     
039400        WHEN OTHER                                                        
039500         PERFORM IMS-GET-PLAA11                                           
039600         IF SEGMENT-FINNS                                                 
039700           IF  6006-KDINLOMR = 'FB ' OR 'F  ' OR 'FBP'                    
039800             MOVE 6006-KDINLOMR TO W-KDINLOMR                             
039900           ELSE                                                           
040000             MOVE NEJ TO NYCKLAR-SW                                       
040100           END-IF                                                         
040200         ELSE                                                             
040300           MOVE NEJ TO NYCKLAR-SW                                         
040400         END-IF                                                           
040500     END-EVALUATE                                                         
040600     .                                                                    
040700     EJECT                                                                
040800 C-FOERSTA-SIDA SECTION.                                                  
041000                                                                          
041100     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
041200     CALL WMEDKONV USING MED-WMEDAREA                                     
041300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
041400                                                                          
041500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
041600     MOVE ZERO  TO W-IDARTNR-FOM                                          
041700                   W-IDFKNGRP-FOM                                         
041900                   W-BEFT-FOM                                             
042000     MOVE SPACE TO W-IDLEVNR                                              
042800     MOVE JA TO ALLT-SW                                                   
042900     .                                                                    
043000     EJECT                                                                
043100 D-NAESTA-SIDA SECTION.                                                   
043300                                                                          
043400     IF MID-IDARTNR-NEXT  = ZERO             AND                          
043500        MID-IDFKNGRP-NEXT = ZERO             AND                          
043600        MID-IDLEVNR-NEXT  = SPACE            AND                          
043700        MID-BEFT-NEXT     = ZERO                                          
043800        MOVE ERR-LAST-PAGE-SHOWED TO MED-IDMFSFEL                         
043900        CALL WMEDKONV USING MED-WMEDAREA                                  
044000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
044100        PERFORM S11-RENSA-FAELT-UT                                        
044200        MOVE NEJ TO ALLT-SW                                               
044300     ELSE                                                                 
044400        MOVE MID-IDARTNR-NEXT       TO W-IDARTNR-FOM                      
044600        MOVE MID-IDFKNGRP-NEXT      TO W-IDFKNGRP-FOM                     
044800        MOVE MID-IDLEVNR-NEXT       TO W-IDLEVNR                          
045000        MOVE MID-BEFT-NEXT          TO W-BEFT-FOM                         
045200        MOVE JA                     TO ALLT-SW                            
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 E-SAMMA-SIDA SECTION.                                                    
045800                                                                          
045900     MOVE NEJ TO CMDKOD-SW                                                
046000     MOVE +1   TO RAD-IX                                                  
046100     PERFORM UNTIL RAD-IX > MAX-INDX OR CMDKOD-FINNS                      
046200      IF MID-KDCMD-RAD(RAD-IX) NOT = ALL '+'                              
046300       MOVE JA TO CMDKOD-SW                                               
046400      END-IF                                                              
046500      ADD +1 TO RAD-IX                                                    
046600     END-PERFORM                                                          
046700     IF MID-INPUT = ALL '+' AND CMDKOD-SAKNAS                             
046800       PERFORM EA-FLYTTA-TABELL-INDEX                                     
046900     ELSE                                                                 
047000       IF EGEN-MID OR HELP-MID                                            
047100         PERFORM EA-FLYTTA-TABELL-INDEX                                   
047200         PERFORM EB-FLYTTA-MID-TILL-MOD                                   
047300         PERFORM S08-INF-PRESS-PF11                                       
047400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-INPUT-ATTR            
047500       ELSE                                                               
047600         PERFORM EA-FLYTTA-TABELL-INDEX                                   
047700       END-IF                                                             
047800     END-IF                                                               
047900     .                                                                    
048000     EJECT                                                                
048100 EA-FLYTTA-TABELL-INDEX SECTION.                                          
048300                                                                          
048400     MOVE MID-IDARTNR-ENTER   TO W-IDARTNR-FOM                            
048500     MOVE MID-IDFKNGRP-ENTER  TO W-IDFKNGRP-FOM                           
048600     MOVE MID-IDLEVNR-ENTER   TO W-IDLEVNR                                
048610     MOVE MID-BEFT-ENTER      TO W-BEFT-FOM                               
049600     .                                                                    
049700     EJECT                                                                
049800 EB-FLYTTA-MID-TILL-MOD SECTION.                                          
050000                                                                          
050100     MOVE +1 TO RAD-IX                                                    
050200     PERFORM UNTIL RAD-IX > MAX-INDX                                      
050300       IF MID-KDCMD-RAD(RAD-IX) = ALL '+'                                 
050400         MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-RAD(RAD-IX)              
050500       ELSE                                                               
050600         MOVE MID-KDCMD-RAD(RAD-IX) TO  MOD-KDCMD-RAD(RAD-IX)             
050700       END-IF                                                             
050800       ADD +1 TO RAD-IX                                                   
050900     END-PERFORM                                                          
051000                                                                          
051100     IF MID-ADINLOMR-INPUT          = ALL '+'                             
051200       MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR-INPUT                 
051300     ELSE                                                                 
051400       MOVE MID-ADINLOMR-INPUT     TO  MOD-ADINLOMR-INPUT                 
051500     END-IF                                                               
051600                                                                          
051700     IF MID-IDARTNR-FOM-INPUT       = ALL '+'                             
051800       MOVE MFS-RENSA-FAELT        TO  MOD-IDARTNR-FOM-INPUT              
051900     ELSE                                                                 
052000       MOVE MID-IDARTNR-FOM-INPUT  TO  MOD-IDARTNR-FOM-INPUT              
052100     END-IF                                                               
052200                                                                          
052300     IF MID-IDARTNR-TOM-INPUT       = ALL '+'                             
052400       MOVE MFS-RENSA-FAELT        TO  MOD-IDARTNR-TOM-INPUT              
052500     ELSE                                                                 
052600       MOVE MID-IDARTNR-TOM-INPUT  TO  MOD-IDARTNR-TOM-INPUT              
052700     END-IF                                                               
052800                                                                          
052900     IF MID-IDFKNGRP-FOM-INPUT      = ALL '+'                             
053000       MOVE MFS-RENSA-FAELT        TO  MOD-IDFKNGRP-FOM-INPUT             
053100     ELSE                                                                 
053200       MOVE MID-IDFKNGRP-FOM-INPUT TO  MOD-IDFKNGRP-FOM-INPUT             
053300     END-IF                                                               
053400                                                                          
053500     IF MID-IDFKNGRP-TOM-INPUT      = ALL '+'                             
053600       MOVE MFS-RENSA-FAELT        TO  MOD-IDFKNGRP-TOM-INPUT             
053700     ELSE                                                                 
053800       MOVE MID-IDFKNGRP-TOM-INPUT TO  MOD-IDFKNGRP-TOM-INPUT             
053900     END-IF                                                               
054000                                                                          
054100     IF MID-IDLEVNR-INPUT             = ALL '+'                           
054200       MOVE MFS-RENSA-FAELT        TO  MOD-IDLEVNR-INPUT                  
054300     ELSE                                                                 
054400       MOVE MID-IDLEVNR-INPUT      TO  MOD-IDLEVNR-INPUT                  
054500     END-IF                                                               
054600                                                                          
055300     IF MID-BEFT-FOM-INPUT          = ALL '+'                             
055400       MOVE MFS-RENSA-FAELT        TO  MOD-BEFT-FOM-INPUT                 
055500     ELSE                                                                 
055600       MOVE MID-BEFT-FOM-INPUT     TO  MOD-BEFT-FOM-INPUT                 
055700     END-IF                                                               
055800                                                                          
055900     IF MID-BEFT-TOM-INPUT          = ALL '+'                             
056000       MOVE MFS-RENSA-FAELT        TO  MOD-BEFT-TOM-INPUT                 
056100     ELSE                                                                 
056200       MOVE MID-BEFT-TOM-INPUT     TO  MOD-BEFT-TOM-INPUT                 
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 F-LAES-VISA-INFO SECTION.                                                
056800                                                                          
057600     PERFORM S10-RENSA-RADER                                              
057610     PERFORM IMS-GU-HANB01                                                
057611     IF SEGMENT-FINNS                                                     
057700       MOVE NEJ TO START-SW                                               
057800       IF URVAL-SAKNAS  OR  W-KDINLOMR = 'FBP'                            
057900         IF W-IDARTNR-FOM  = ZERO               AND                       
058000            W-IDFKNGRP-FOM = ZERO               AND                       
058100            W-IDLEVNR      = SPACE              AND                       
058200            W-BEFT-FOM     = ZERO                                         
058300            MOVE JA TO START-SW                                           
058400         END-IF                                                           
058500       ELSE                                                               
058510*NDC-ÄNDRING                                                              
058600         IF W-KDINLOMR = 'FB' OR 'PG'                                     
058700           IF W-IDARTNR-FOM  = ZERO          AND                          
058800              W-IDFKNGRP-FOM = ZERO          AND                          
058900              W-IDLEVNR      = SPACE                                      
059000              MOVE JA TO START-SW                                         
059100           END-IF                                                         
059200         ELSE                                                             
059300           IF W-KDINLOMR = 'F'                                            
059400             IF W-BEFT-FOM = ZERO                                         
059500               MOVE JA TO START-SW                                        
059600             END-IF                                                       
059700           END-IF                                                         
059800         END-IF                                                           
059900       END-IF                                                             
060000                                                                          
060100       MOVE +1 TO RAD-IX                                                  
060200       IF NOT MFS-UPDATE                                                  
060300         MOVE ZEROS TO   MOD-IDARTNR-ENTER                                
060400                         MOD-IDFKNGRP-ENTER                               
060600                         MOD-BEFT-ENTER                                   
060610         MOVE SPACES TO  MOD-IDLEVNR-ENTER                                
060700       END-IF                                                             
060800       MOVE ZEROS TO     MOD-IDARTNR-NEXT                                 
060900                         MOD-IDFKNGRP-NEXT                                
061100                         MOD-BEFT-NEXT                                    
061110       MOVE SPACES TO    MOD-IDLEVNR-NEXT                                 
061200       IF URVAL-SAKNAS OR (W-KDINLOMR = 'FB' OR 'FBP')                    
061300         IF START-OK  OR W-IDARTNR-FOM > ZERO                             
061400           MOVE JA TO START-SW                                            
061500           PERFORM FA-VISA-ARTNR-TAB                                      
061600         END-IF                                                           
061700         IF START-OK  OR W-IDFKNGRP-FOM > ZERO                            
061800           MOVE JA TO START-SW                                            
061900           PERFORM FB-VISA-FKNGRP-TAB                                     
062000         END-IF                                                           
062100         IF START-OK  OR W-IDLEVNR      > SPACE                           
062200           MOVE JA TO START-SW                                            
062300           PERFORM FC-VISA-LEVNR-TAB                                      
062400         END-IF                                                           
062500       END-IF                                                             
062600       IF URVAL-SAKNAS OR (W-KDINLOMR = 'F' OR 'FBP')                     
062700         PERFORM FD-VISA-FT-TAB                                           
062800       END-IF                                                             
063000       IF (MFS-ENTER OR HELP-MID) AND NOT MFS-UPDATE                      
063100           CONTINUE                                                       
063200        ELSE                                                              
063300           PERFORM S12-RENSA-FAELT-IN                                     
063400       END-IF                                                             
063410     ELSE                                                                 
063420        PERFORM S05-ERR-NOT-ON-REGISTER                                   
063421        PERFORM S11-RENSA-FAELT-UT                                        
063422        PERFORM S12-RENSA-FAELT-IN                                        
063430     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 FA-VISA-ARTNR-TAB SECTION.                                               
063900                                                                          
064000     IF RAD-IX NOT > MAX-INDX                                             
064310       IF URVAL-SAKNAS                                                    
064400         PERFORM IMS-GNP-HANB11-OKVAL                                     
064410       ELSE                                                               
064420         PERFORM IMS-GNP-HANB11-OMR                                       
064430       END-IF                                                             
064440                                                                          
064500       PERFORM UNTIL RAD-IX > MAX-INDX                                    
064600                  OR SEGMENT-SAKNAS                                       
064610        IF URVAL-SAKNAS                                                   
064620          PERFORM FAA-FLYTTA-ARTNR-TILL-MOD                               
064630          IF RAD-IX = +1                                                  
064640            MOVE 6032-IDARTNR-FOM TO MOD-IDARTNR-ENTER                    
064650          END-IF                                                          
064660          ADD +1 TO RAD-IX                                                
064661          PERFORM IMS-GNP-HANB11-OKVAL                                    
064670        ELSE                                                              
064671          PERFORM FAA-FLYTTA-ARTNR-TILL-MOD                               
064672          IF RAD-IX = +1                                                  
064673            MOVE 6032-IDARTNR-FOM TO MOD-IDARTNR-ENTER                    
064674          END-IF                                                          
064675          ADD +1 TO RAD-IX                                                
064676          PERFORM IMS-GNP-HANB11-OMR                                      
064680        END-IF                                                            
065600       END-PERFORM                                                        
065700       IF RAD-IX > MAX-INDX                                               
065800         PERFORM FAB-KOLL-ARTNR-SIDBRYTNING                               
065900       END-IF                                                             
066000     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300 FAA-FLYTTA-ARTNR-TILL-MOD SECTION.                                       
066500                                                                          
066600     MOVE 6032-ADINLOMR            TO MOD-ADINLOMR-RAD(RAD-IX)            
066800     MOVE 6032-IDARTNR-FOM         TO MOD-IDARTNR-FOM(RAD-IX)             
067000     MOVE 6032-IDARTNR-TOM         TO MOD-IDARTNR-TOM(RAD-IX)             
067200     .                                                                    
067300     EJECT                                                                
067400 FAB-KOLL-ARTNR-SIDBRYTNING SECTION.                                      
067600                                                                          
067700     MOVE NEJ TO SID-SW                                                   
067800     IF SEGMENT-FINNS                                                     
068400       MOVE JA TO SID-SW                                                  
068410       MOVE 6032-IDARTNR-FOM TO MOD-IDARTNR-NEXT                          
068500       IF NOT MFS-UPDATE                                                  
068600         PERFORM S02-INF-MORE-INFO-EXISTS                                 
068700       END-IF                                                             
068800     ELSE                                                                 
068900       IF URVAL-SAKNAS                                                    
069000         PERFORM IMS-GNP-HANB12-OKVAL                                     
069010       ELSE                                                               
069020         PERFORM IMS-GNP-HANB12-OMR                                       
069030       END-IF                                                             
069040       IF SEGMENT-FINNS                                                   
069050         MOVE JA TO SID-SW                                                
069060         MOVE 6034-IDFKNGRP-FOM TO MOD-IDFKNGRP-NEXT                      
069070         IF NOT MFS-UPDATE                                                
069080           PERFORM S02-INF-MORE-INFO-EXISTS                               
069090         END-IF                                                           
069091       ELSE                                                               
069092         IF URVAL-SAKNAS                                                  
069093           PERFORM IMS-GNP-HANB13-OKVAL                                   
069094         ELSE                                                             
069095           PERFORM IMS-GNP-HANB13-OMR                                     
069096         END-IF                                                           
069097         IF SEGMENT-FINNS                                                 
069098           MOVE JA TO SID-SW                                              
069099           MOVE 6036-IDLEVNR TO MOD-IDLEVNR-NEXT                          
069100           IF NOT MFS-UPDATE                                              
069101             PERFORM S02-INF-MORE-INFO-EXISTS                             
069102           END-IF                                                         
069103         ELSE                                                             
069104           IF URVAL-SAKNAS                                                
069105             PERFORM IMS-GNP-HANB14-OKVAL                                 
069106           ELSE                                                           
069107             PERFORM IMS-GNP-HANB14-OMR                                   
069108           END-IF                                                         
069109           IF SEGMENT-FINNS                                               
069110             MOVE JA TO SID-SW                                            
069111             MOVE 6038-BEFT-FOM TO WS-BEFT-NUM2                           
069112             MOVE WS-BEFT-NUM2  TO MOD-BEFT-NEXT                          
069114             IF NOT MFS-UPDATE                                            
069115                PERFORM S02-INF-MORE-INFO-EXISTS                          
069116             END-IF                                                       
069117           END-IF                                                         
069120         END-IF                                                           
069130       END-IF                                                             
069200     END-IF                                                               
074800     IF FLER-SIDOR-FINNS-EJ AND NOT (MFS-FIRST OR MFS-UPDATE)             
074900       PERFORM S01-INF-LAST-PAGE                                          
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075300 FB-VISA-FKNGRP-TAB SECTION.                                              
075500                                                                          
075600     IF RAD-IX NOT > MAX-INDX                                             
076000                                                                          
076001       IF URVAL-SAKNAS                                                    
076002         PERFORM IMS-GNP-HANB12-OKVAL                                     
076003       ELSE                                                               
076004         PERFORM IMS-GNP-HANB12-OMR                                       
076005       END-IF                                                             
076006                                                                          
076010       PERFORM UNTIL RAD-IX > MAX-INDX                                    
076020                  OR SEGMENT-SAKNAS                                       
076030        IF URVAL-SAKNAS                                                   
076040          PERFORM FBA-FLYTTA-FKNGRP-TILL-MOD                              
076050          IF RAD-IX = +1                                                  
076060            MOVE 6034-IDFKNGRP-FOM TO MOD-IDFKNGRP-ENTER                  
076070          END-IF                                                          
076080          ADD +1 TO RAD-IX                                                
076090          PERFORM IMS-GNP-HANB12-OKVAL                                    
076091        ELSE                                                              
076092          PERFORM FBA-FLYTTA-FKNGRP-TILL-MOD                              
076093          IF RAD-IX = +1                                                  
076094            MOVE 6034-IDFKNGRP-FOM TO MOD-IDFKNGRP-ENTER                  
076095          END-IF                                                          
076096          ADD +1 TO RAD-IX                                                
076097          PERFORM IMS-GNP-HANB12-OMR                                      
076098        END-IF                                                            
076099       END-PERFORM                                                        
077300       IF RAD-IX > MAX-INDX                                               
077400         PERFORM FBB-KOLL-FKNGRP-SIDBRYTNING                              
077500       END-IF                                                             
077600     END-IF                                                               
077700     .                                                                    
077800     EJECT                                                                
077900 FBA-FLYTTA-FKNGRP-TILL-MOD SECTION.                                      
078100                                                                          
078300     MOVE 6034-ADINLOMR            TO MOD-ADINLOMR-RAD(RAD-IX)            
078500     MOVE 6034-IDFKNGRP-FOM        TO MOD-IDFKNGRP-FOM(RAD-IX)            
078700     MOVE 6034-IDFKNGRP-TOM        TO MOD-IDFKNGRP-TOM(RAD-IX)            
078800     .                                                                    
078900     EJECT                                                                
079000 FBB-KOLL-FKNGRP-SIDBRYTNING SECTION.                                     
079200                                                                          
079300     MOVE NEJ TO SID-SW                                                   
079310     IF SEGMENT-FINNS                                                     
079320       MOVE 6034-IDFKNGRP-FOM TO MOD-IDFKNGRP-NEXT                        
079330       MOVE JA  TO SID-SW                                                 
079340       IF NOT MFS-UPDATE                                                  
079350         PERFORM S02-INF-MORE-INFO-EXISTS                                 
079360       END-IF                                                             
079361     ELSE                                                                 
079362       IF URVAL-SAKNAS                                                    
079363         PERFORM IMS-GNP-HANB13-OKVAL                                     
079364       ELSE                                                               
079365         PERFORM IMS-GNP-HANB13-OMR                                       
079366       END-IF                                                             
079367       IF SEGMENT-FINNS                                                   
079368         MOVE 6036-IDLEVNR TO MOD-IDLEVNR-NEXT                            
079369         MOVE JA  TO SID-SW                                               
079370         IF NOT MFS-UPDATE                                                
079371           PERFORM S02-INF-MORE-INFO-EXISTS                               
079372         END-IF                                                           
079373       ELSE                                                               
079374         IF URVAL-SAKNAS                                                  
079375           PERFORM IMS-GNP-HANB14-OKVAL                                   
079376         ELSE                                                             
079377           PERFORM IMS-GNP-HANB14-OMR                                     
079378         END-IF                                                           
079379         IF SEGMENT-FINNS                                                 
079380           MOVE 6038-BEFT-FOM TO WS-BEFT-NUM2                             
079381           MOVE WS-BEFT-NUM2  TO MOD-BEFT-NEXT                            
079383           MOVE JA  TO SID-SW                                             
079384           IF NOT MFS-UPDATE                                              
079385             PERFORM S02-INF-MORE-INFO-EXISTS                             
079386           END-IF                                                         
079387         END-IF                                                           
079388       END-IF                                                             
079390     END-IF                                                               
084600     IF FLER-SIDOR-FINNS-EJ AND NOT (MFS-FIRST OR MFS-UPDATE)             
084700       PERFORM S01-INF-LAST-PAGE                                          
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100 FC-VISA-LEVNR-TAB SECTION.                                               
085300                                                                          
085400     IF RAD-IX NOT > MAX-INDX                                             
085710       IF URVAL-SAKNAS                                                    
085720         PERFORM IMS-GNP-HANB13-OKVAL                                     
085730       ELSE                                                               
085740         PERFORM IMS-GNP-HANB13-OMR                                       
085750       END-IF                                                             
085800                                                                          
085900       PERFORM UNTIL RAD-IX > MAX-INDX OR                                 
086000                  SEGMENT-SAKNAS                                          
086300         PERFORM FCA-FLYTTA-LEVNR-TILL-MOD                                
086400         IF RAD-IX = +1                                                   
086500           MOVE 6036-IDLEVNR TO MOD-IDLEVNR-ENTER                         
086600         END-IF                                                           
086700         ADD +1 TO RAD-IX                                                 
086711         IF URVAL-SAKNAS                                                  
086712           PERFORM IMS-GNP-HANB13-OKVAL                                   
086720         ELSE                                                             
086730           PERFORM IMS-GNP-HANB13-OMR                                     
086800         END-IF                                                           
087000       END-PERFORM                                                        
087100       IF RAD-IX > MAX-INDX                                               
087200         PERFORM FCB-KOLL-LEVNR-SIDBRYTNING                               
087300       ELSE                                                               
087400         IF W-KDINLOMR = 'FB' AND NOT (MFS-FIRST OR MFS-UPDATE)           
087500           PERFORM S01-INF-LAST-PAGE                                      
087600         END-IF                                                           
087700       END-IF                                                             
087800     END-IF                                                               
087900     .                                                                    
088000     EJECT                                                                
088100 FCA-FLYTTA-LEVNR-TILL-MOD SECTION.                                       
088300                                                                          
088500     MOVE 6036-ADINLOMR            TO MOD-ADINLOMR-RAD(RAD-IX)            
088700     MOVE 6036-IDLEVNR             TO MOD-IDLEVNR(RAD-IX)                 
089000     .                                                                    
089100     EJECT                                                                
089200 FCB-KOLL-LEVNR-SIDBRYTNING SECTION.                                      
089400                                                                          
089500     MOVE NEJ TO SID-SW                                                   
089600     IF SEGMENT-FINNS                                                     
090100       MOVE 6036-IDLEVNR TO MOD-IDLEVNR-NEXT                              
090200       MOVE JA TO SID-SW                                                  
090300       IF NOT MFS-UPDATE                                                  
090400         PERFORM S02-INF-MORE-INFO-EXISTS                                 
090500       END-IF                                                             
090600     ELSE                                                                 
090700       IF URVAL-SAKNAS                                                    
090800         PERFORM IMS-GNP-HANB14-OKVAL                                     
090900       ELSE                                                               
091000         PERFORM IMS-GNP-HANB14-OMR                                       
091100       END-IF                                                             
091200       IF SEGMENT-FINNS                                                   
091210         MOVE 6038-BEFT-FOM TO WS-BEFT-NUM2                               
091220         MOVE WS-BEFT-NUM2  TO MOD-BEFT-NEXT                              
091400         MOVE JA TO SID-SW                                                
091500         IF NOT MFS-UPDATE                                                
091600           PERFORM S02-INF-MORE-INFO-EXISTS                               
091700         END-IF                                                           
091800       END-IF                                                             
091900     END-IF                                                               
093000     IF FLER-SIDOR-FINNS-EJ AND NOT (MFS-FIRST OR MFS-UPDATE)             
093100       PERFORM S01-INF-LAST-PAGE                                          
093200     END-IF                                                               
093300     .                                                                    
093400     EJECT                                                                
093500 FD-VISA-FT-TAB SECTION.                                                  
093700                                                                          
093800     IF RAD-IX NOT > MAX-INDX                                             
094110       IF URVAL-SAKNAS                                                    
094120         PERFORM IMS-GNP-HANB14-OKVAL                                     
094130       ELSE                                                               
094140         PERFORM IMS-GNP-HANB14-OMR                                       
094150       END-IF                                                             
094200                                                                          
094300       PERFORM UNTIL RAD-IX > MAX-INDX OR                                 
094400                  SEGMENT-SAKNAS                                          
094700         PERFORM FDA-FLYTTA-FT-TILL-MOD                                   
094800         IF RAD-IX = +1                                                   
094810*** EXTRA FLYTTNING FÖR ATT LÄGGA RÄTT I MODEN (PIC X(2))                 
094900           MOVE 6038-BEFT-FOM TO WS-BEFT-NUM2                             
094910           MOVE WS-BEFT-NUM2  TO MOD-BEFT-ENTER                           
095000         END-IF                                                           
095310         IF URVAL-SAKNAS                                                  
095320           PERFORM IMS-GNP-HANB14-OKVAL                                   
095330         ELSE                                                             
095340           PERFORM IMS-GNP-HANB14-OMR                                     
095350         END-IF                                                           
095360         ADD +1 TO RAD-IX                                                 
095400       END-PERFORM                                                        
095500       IF RAD-IX > MAX-INDX                                               
095600         PERFORM FDB-KOLL-FT-SIDBRYTNING                                  
095700       ELSE                                                               
095800         IF  NOT (MFS-FIRST OR MFS-UPDATE)                                
095900           PERFORM S01-INF-LAST-PAGE                                      
096000         END-IF                                                           
096100       END-IF                                                             
096200     END-IF                                                               
096300     .                                                                    
096400     EJECT                                                                
096500 FDA-FLYTTA-FT-TILL-MOD SECTION.                                          
096700                                                                          
096900     MOVE 6038-ADINLOMR            TO MOD-ADINLOMR-RAD(RAD-IX)            
097100     MOVE 6038-BEFT-FOM            TO MOD-BEFT-FOM(RAD-IX)                
097300     MOVE 6038-BEFT-TOM            TO MOD-BEFT-TOM(RAD-IX)                
097400     .                                                                    
097500     EJECT                                                                
097600 FDB-KOLL-FT-SIDBRYTNING SECTION.                                         
097800                                                                          
097900     MOVE NEJ TO SID-SW                                                   
098000     IF SEGMENT-FINNS                                                     
098001** EN EXTRA FLYTTNING FÖR ATT MOD-FÄLTET SKA BLI RÄTT (PIC X(2))          
098010       MOVE 6038-BEFT-FOM          TO WS-BEFT-NUM2                        
098011       MOVE WS-BEFT-NUM2           TO MOD-BEFT-NEXT                       
098600       MOVE JA TO SID-SW                                                  
098700       IF NOT MFS-UPDATE                                                  
098800         PERFORM S02-INF-MORE-INFO-EXISTS                                 
098900       END-IF                                                             
099200     END-IF                                                               
099400     IF FLER-SIDOR-FINNS-EJ AND NOT (MFS-FIRST OR MFS-UPDATE)             
099500       PERFORM S01-INF-LAST-PAGE                                          
099600     END-IF                                                               
099700     .                                                                    
099800     EJECT                                                                
099900 G-KOLLA-INPUT SECTION.                                                   
100100                                                                          
100200     MOVE MFS-ALFA-FAELT-RAETT  TO MOD-ADINLOMR-INPUT-ATTR                
100210                                   MOD-IDLEVNR-INPUT-ATTR                 
100300     MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDARTNR-FOM-INPUT-ATTR             
100400                                   MOD-IDARTNR-TOM-INPUT-ATTR             
100500                                   MOD-IDFKNGRP-FOM-INPUT-ATTR            
100600                                   MOD-IDFKNGRP-TOM-INPUT-ATTR            
100900                                   MOD-BEFT-FOM-INPUT-ATTR                
101000                                   MOD-BEFT-TOM-INPUT-ATTR                
101100     MOVE JA  TO INDATA-SW                                                
101200     MOVE NEJ TO CMDKOD-SW                                                
101300     MOVE +1  TO RAD-IX                                                   
101310     MOVE +1  TO TAB-IX                                                   
101320     PERFORM UNTIL RAD-IX > MAX-INDX                                      
103300      IF MID-KDCMD-RAD(RAD-IX) NOT = ALL '+'                              
103310        MOVE JA TO CMDKOD-SW                                              
103400        IF MID-KDCMD-RAD (RAD-IX) = 'B' OR 'D'                            
103500          IF MID-INPUT NOT = ALL '+'                                      
103600            PERFORM S03-ERR-MANY-FUNCTIONS                                
103700          ELSE                                                            
103800            IF MID-ADINLOMR-RAD (RAD-IX) = ALL SPACE                      
103900              PERFORM S04-ERR-WRONG-CHOISE                                
104000            ELSE                                                          
104100              PERFORM GA-SPARA-VAERDEN                                    
104110              ADD +1     TO MAXTAB-IX                                     
104120              ADD +1     TO TAB-IX                                        
104200            END-IF                                                        
104300          END-IF                                                          
104400        ELSE                                                              
104500          PERFORM S04-ERR-WRONG-CHOISE                                    
104600        END-IF                                                            
104610      END-IF                                                              
104620      ADD +1 TO RAD-IX                                                    
104630     END-PERFORM                                                          
104710     IF MID-INPUT = ALL '+' AND CMDKOD-SAKNAS                             
104720       PERFORM S07-ERR-PF11-AND-NO-DATA                                   
104730     ELSE                                                                 
104740       IF MID-INPUT NOT = ALL '+' AND CMDKOD-SAKNAS                       
104900         PERFORM GB-KOLL-INPUT-RADEN                                      
104910       END-IF                                                             
105000     END-IF                                                               
105300     .                                                                    
105400     EJECT                                                                
105500 GA-SPARA-VAERDEN SECTION.                                                
105700                                                                          
105800     MOVE MID-ADINLOMR-RAD(RAD-IX)  TO  W-ADINLOMR                        
105810     IF W-ADINLOMR NOT = 'FB  '                                           
105811       PERFORM IMS-GET-PLAA11                                             
105820     END-IF                                                               
106000     IF SEGMENT-FINNS OR W-ADINLOMR = 'FB  '                              
106100       IF 6006-KDINLOMR = 'FB' OR 'F' OR 'FBP' OR                         
106110             W-ADINLOMR = 'FB  '                                          
106200         MOVE 6006-KDINLOMR TO W-KDINLOMR                                 
106300         MOVE MID-ADINLOMR-RAD (RAD-IX) TO SPAR-ADINLOMR (TAB-IX)         
106400         MOVE MID-IDARTNR-FOM (RAD-IX) TO                                 
106410                                         SPAR-IDARTNR-FOM (TAB-IX)        
106500         MOVE MID-IDARTNR-TOM (RAD-IX) TO                                 
106510                                         SPAR-IDARTNR-TOM (TAB-IX)        
106600                                                                          
106900         MOVE MID-IDFKNGRP-FOM (RAD-IX) TO                                
106910                                 SPAR-IDFKNGRP-FOM (TAB-IX)               
107000         MOVE MID-IDFKNGRP-TOM (RAD-IX) TO                                
107010                                 SPAR-IDFKNGRP-TOM (TAB-IX)               
107100                                                                          
107300         MOVE MID-IDLEVNR (RAD-IX)      TO SPAR-IDLEVNR (TAB-IX)          
107500                                                                          
107700         MOVE MID-BEFT-FOM (RAD-IX)     TO SPAR-BEFT-FOM (TAB-IX)         
107800         MOVE MID-BEFT-TOM (RAD-IX)     TO SPAR-BEFT-TOM (TAB-IX)         
107900                                                                          
108000         INSPECT SPAR-IDARTNR-FOM  (TAB-IX)                               
108010                 REPLACING LEADING SPACE BY ZERO                          
108100         INSPECT SPAR-IDARTNR-TOM  (TAB-IX)                               
108110                 REPLACING LEADING SPACE BY ZERO                          
108200         INSPECT SPAR-IDFKNGRP-FOM (TAB-IX)                               
108210                 REPLACING LEADING SPACE BY ZERO                          
108300         INSPECT SPAR-IDFKNGRP-TOM (TAB-IX)                               
108400                 REPLACING LEADING SPACE BY ZERO                          
108600         INSPECT SPAR-BEFT-FOM     (TAB-IX)                               
108610                 REPLACING LEADING SPACE BY ZERO                          
108700         INSPECT SPAR-BEFT-TOM     (TAB-IX)                               
108710                 REPLACING LEADING SPACE BY ZERO                          
108800       END-IF                                                             
108900     ELSE                                                                 
109000       PERFORM S05-ERR-NOT-ON-REGISTER                                    
109100     END-IF                                                               
109200     .                                                                    
109300     EJECT                                                                
109400 GB-KOLL-INPUT-RADEN SECTION.                                             
109600                                                                          
109700     IF MID-ADINLOMR-INPUT = ALL '+'                                      
109800       MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-INPUT-ATTR               
110000       MOVE NEJ TO INDATA-SW                                              
110100     ELSE                                                                 
110200       IF URVAL-FINNS AND MID-ADINLOMR-INPUT NOT = MID-ADINLOMR-UT        
110300         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-INPUT-ATTR             
110500         MOVE NEJ TO INDATA-SW                                            
110600       END-IF                                                             
110700     END-IF                                                               
110800                                                                          
110900     IF INDATA-OK                                                         
111000       MOVE MID-ADINLOMR-INPUT TO W-ADINLOMR                              
111001                                  SPAR-ADINLOMR (1)                       
111010       IF W-ADINLOMR NOT = 'FB  '                                         
111100         PERFORM IMS-GET-PLAA11                                           
111110       END-IF                                                             
111200       IF SEGMENT-FINNS  OR W-ADINLOMR = 'FB  '                           
111300                                                                          
111400         IF 6006-KDINLOMR = 'FB ' OR 'F ' OR 'FBP'                        
111500           MOVE 6006-KDINLOMR TO W-KDINLOMR                               
111600         ELSE                                                             
111610           IF W-ADINLOMR = 'FB  '                                         
111620             MOVE 'FBP' TO W-KDINLOMR                                     
111630           ELSE                                                           
111700             MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-INPUT-ATTR         
111800             MOVE NEJ TO INDATA-SW                                        
111810           END-IF                                                         
111900         END-IF                                                           
112000                                                                          
112100         IF W-KDINLOMR = 'FB'                                             
112200           IF MID-IDARTNR-FOM-INPUT  = ALL '+' AND                        
112300              MID-IDFKNGRP-FOM-INPUT = ALL '+' AND                        
112400              MID-IDLEVNR-INPUT      = ALL '+'                            
112500              MOVE MFS-ALFA-FAELT-FEL TO                                  
112600                           MOD-ADINLOMR-INPUT-ATTR                        
112610                           MOD-IDLEVNR-INPUT-ATTR                         
112700              MOVE MFS-NUM-FAELT-FEL TO                                   
112800                           MOD-IDARTNR-FOM-INPUT-ATTR                     
112900                           MOD-IDFKNGRP-FOM-INPUT-ATTR                    
113100              MOVE NEJ TO INDATA-SW                                       
113200              IF MID-BEFT-FOM-INPUT NOT = ALL '+'                         
113300                MOVE MFS-NUM-FAELT-FEL TO                                 
113400                           MOD-BEFT-FOM-INPUT-ATTR                        
113500              END-IF                                                      
113600              IF MID-BEFT-TOM-INPUT NOT = ALL '+'                         
113700                MOVE MFS-NUM-FAELT-FEL TO                                 
113800                           MOD-BEFT-TOM-INPUT-ATTR                        
113900              END-IF                                                      
114000           ELSE                                                           
114100             PERFORM GBA-KOLL-FT-TAB-TOM                                  
114200             PERFORM GBB-KOLL-ARTNR-TAB-INPUT                             
114300             PERFORM GBC-KOLL-FKNGRP-TAB-INPUT                            
114400             PERFORM GBD-KOLL-LEVNR-TAB-INPUT                             
114500           END-IF                                                         
114600         ELSE                                                             
114700           IF W-KDINLOMR = 'F'                                            
114800             IF MID-BEFT-FOM-INPUT  = ALL '+'                             
114900                MOVE MFS-NUM-FAELT-FEL TO                                 
115000                             MOD-BEFT-FOM-INPUT-ATTR                      
115100                MOVE NEJ TO INDATA-SW                                     
115200             END-IF                                                       
115300             PERFORM GBE-KOLL-TOMMA-TAB                                   
115400             PERFORM GBF-KOLL-FT-TAB-INPUT                                
115500           ELSE                                                           
115600             IF W-KDINLOMR = 'FBP'                                        
115700               IF MID-IDARTNR-FOM-INPUT  = ALL '+' AND                    
115800                  MID-IDFKNGRP-FOM-INPUT = ALL '+' AND                    
115900                  MID-IDLEVNR-INPUT      = ALL '+' AND                    
116000                  MID-BEFT-FOM-INPUT     = ALL '+'                        
116010                  MOVE MFS-ALFA-FAELT-FEL TO                              
116020                               MOD-IDLEVNR-INPUT-ATTR                     
116100                  MOVE MFS-NUM-FAELT-FEL TO                               
116200                               MOD-IDARTNR-FOM-INPUT-ATTR                 
116300                               MOD-IDFKNGRP-FOM-INPUT-ATTR                
116500                               MOD-BEFT-FOM-INPUT-ATTR                    
116600                  MOVE NEJ TO INDATA-SW                                   
116700               ELSE                                                       
116800                 PERFORM GBB-KOLL-ARTNR-TAB-INPUT                         
116900                 PERFORM GBC-KOLL-FKNGRP-TAB-INPUT                        
117000                 PERFORM GBD-KOLL-LEVNR-TAB-INPUT                         
117100                 PERFORM GBF-KOLL-FT-TAB-INPUT                            
117200               END-IF                                                     
117300             END-IF                                                       
117400           END-IF                                                         
117500         END-IF                                                           
117600         IF INDATA-FEL                                                    
117700           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
117800           CALL WMEDKONV USING MED-WMEDAREA                               
117900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
118000           PERFORM S13-ROER-EJ-FAELT-UT                                   
118100           PERFORM S14-ROER-EJ-FAELT-IN                                   
118200           MOVE NEJ TO ALLT-SW                                            
118300         END-IF                                                           
118400       ELSE                                                               
118500         PERFORM S05-ERR-NOT-ON-REGISTER                                  
118600       END-IF                                                             
118700     ELSE                                                                 
118800       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
118900       CALL WMEDKONV USING MED-WMEDAREA                                   
119000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
119100       PERFORM S13-ROER-EJ-FAELT-UT                                       
119200       PERFORM S14-ROER-EJ-FAELT-IN                                       
119300       PERFORM MFS-LAES-IN-IGEN-RAD                                       
119400       MOVE NEJ TO ALLT-SW                                                
119500     END-IF                                                               
119600     .                                                                    
119700     EJECT                                                                
119800 GBA-KOLL-FT-TAB-TOM SECTION.                                             
120000                                                                          
120100     IF MID-BEFT-FOM-INPUT  = ALL '+'                                     
120200       MOVE ZEROS               TO SPAR-BEFT-FOM (1)                      
120300     ELSE                                                                 
120400       MOVE MFS-NUM-FAELT-FEL  TO                                         
120500                     MOD-BEFT-FOM-INPUT-ATTR                              
120600       MOVE NEJ TO INDATA-SW                                              
120700     END-IF                                                               
120800     IF MID-BEFT-TOM-INPUT  = ALL '+'                                     
120900       MOVE ZEROS               TO SPAR-BEFT-TOM (1)                      
121000     ELSE                                                                 
121100       MOVE MFS-NUM-FAELT-FEL  TO                                         
121200                     MOD-BEFT-TOM-INPUT-ATTR                              
121300       MOVE NEJ TO INDATA-SW                                              
121400     END-IF                                                               
121500     .                                                                    
121600     EJECT                                                                
121700 GBB-KOLL-ARTNR-TAB-INPUT SECTION.                                        
121900                                                                          
122000     IF MID-IDARTNR-FOM-INPUT NOT = ALL '+' OR                            
122100        MID-IDARTNR-TOM-INPUT NOT = ALL '+'                               
122200       IF MID-IDARTNR-FOM-INPUT NUMERIC AND                               
122300          MID-IDARTNR-FOM-INPUT > ZEROS                                   
122400          INSPECT MID-IDARTNR-FOM-INPUT REPLACING LEADING                 
122500                                        ZEROS  BY  SPACE                  
122600         IF MID-IDARTNR-TOM-INPUT = ALL '+'                               
122700           MOVE MID-IDARTNR-FOM-INPUT TO MOD-IDARTNR-TOM-INPUT            
122800         ELSE                                                             
122900           IF MID-IDARTNR-TOM-INPUT NUMERIC                               
123000             INSPECT MID-IDARTNR-TOM-INPUT REPLACING LEADING              
123100                                           ZEROS  BY  SPACE               
123200             IF MID-IDARTNR-FOM-INPUT > MID-IDARTNR-TOM-INPUT             
123300               MOVE MFS-NUM-FAELT-FEL  TO                                 
123400                             MOD-IDARTNR-FOM-INPUT-ATTR                   
123500                             MOD-IDARTNR-TOM-INPUT-ATTR                   
123600               MOVE NEJ TO INDATA-SW                                      
123700             ELSE                                                         
123800               MOVE MID-IDARTNR-TOM-INPUT TO MOD-IDARTNR-TOM-INPUT        
123900             END-IF                                                       
124000           ELSE                                                           
124100             MOVE MFS-NUM-FAELT-FEL  TO                                   
124200                           MOD-IDARTNR-TOM-INPUT-ATTR                     
124300             MOVE NEJ TO INDATA-SW                                        
124400           END-IF                                                         
124500         END-IF                                                           
124600       ELSE                                                               
124700         MOVE MFS-NUM-FAELT-FEL  TO                                       
124800                       MOD-IDARTNR-FOM-INPUT-ATTR                         
124900         MOVE NEJ TO INDATA-SW                                            
125000       END-IF                                                             
125100       PERFORM GBBA-KOLL-ARTNR-INTERVALL                                  
125200     ELSE                                                                 
125300       MOVE NEJ TO ARTNR-INPUT-SW                                         
125400     END-IF                                                               
125500     .                                                                    
125600     EJECT                                                                
125700 GBBA-KOLL-ARTNR-INTERVALL SECTION.                                       
125900                                                                          
126100     MOVE NEJ TO ARTNR-TAB-SW                                             
126300     MOVE MID-IDARTNR-FOM-INPUT  TO  SPAR-IDARTNR-FOM (1)                 
126400     MOVE MOD-IDARTNR-TOM-INPUT  TO  SPAR-IDARTNR-TOM (1)                 
126500     INSPECT SPAR-IDARTNR-FOM (1) REPLACING LEADING SPACE                 
126600                                              BY ZERO                     
126700     INSPECT SPAR-IDARTNR-TOM (1) REPLACING LEADING SPACE                 
126800                                              BY ZERO                     
126900     MOVE SPAR-IDARTNR-FOM (1) TO W-IDARTNR-FOM                           
126910     MOVE SPAR-IDARTNR-TOM (1) TO W-IDARTNR-TOM                           
127000                                                                          
127010     PERFORM IMS-GHU-HANB11-KVAL                                          
127100     IF SEGMENT-SAKNAS                                                    
127110*NDC                                                                      
127111      IF DCS-IDDC NOT = W-6031-IDDC                                       
127112         MOVE W-6031-IDDC TO W-IDDC-B6                                    
127113         PERFORM IMS-GU-WDB601                                            
127114      END-IF                                                              
127120      IF DCS-CDC OR DCS-CDC-TR                                            
127200        IF DCS-CDC                                                        
127300          MOVE WC-CDC-TR TO W-6031-IDDC                                   
127400          PERFORM IMS-GHU-HANB11-KVAL                                     
127500          MOVE WC-CDC-SE TO W-6031-IDDC                                   
127600          IF SEGMENT-SAKNAS                                               
128200            MOVE JA TO ARTNR-TAB-SW                                       
128210          ELSE                                                            
128220            PERFORM S06-ERR-WRONG-INTERVAL                                
128230            MOVE MFS-NUM-FAELT-FEL     TO                                 
128240                         MOD-IDARTNR-FOM-INPUT-ATTR                       
128250                         MOD-IDARTNR-TOM-INPUT-ATTR                       
128260            MOVE NEJ TO INDATA-SW                                         
128270          END-IF                                                          
128280        ELSE                                                              
128281          MOVE WC-CDC-SE TO W-6031-IDDC                                   
128282          PERFORM IMS-GHU-HANB11-KVAL                                     
128283          MOVE WC-CDC-TR TO W-6031-IDDC                                   
128284          IF SEGMENT-SAKNAS                                               
128285            MOVE JA TO ARTNR-TAB-SW                                       
128286          ELSE                                                            
128287            PERFORM S06-ERR-WRONG-INTERVAL                                
128288            MOVE MFS-NUM-FAELT-FEL     TO                                 
128289                         MOD-IDARTNR-FOM-INPUT-ATTR                       
128290                         MOD-IDARTNR-TOM-INPUT-ATTR                       
128291            MOVE NEJ TO INDATA-SW                                         
128292          END-IF                                                          
128293        END-IF                                                            
128294      ELSE                                                                
128295        MOVE JA TO ARTNR-TAB-SW                                           
128296      END-IF                                                              
128300     ELSE                                                                 
129400       PERFORM S06-ERR-WRONG-INTERVAL                                     
129500       MOVE MFS-NUM-FAELT-FEL     TO                                      
129600                    MOD-IDARTNR-FOM-INPUT-ATTR                            
129700                    MOD-IDARTNR-TOM-INPUT-ATTR                            
129800       MOVE NEJ TO INDATA-SW                                              
129900     END-IF                                                               
130600     .                                                                    
130700     EJECT                                                                
130800 GBC-KOLL-FKNGRP-TAB-INPUT SECTION.                                       
131000                                                                          
131100     IF MID-IDFKNGRP-FOM-INPUT NOT = ALL '+' OR                           
131200        MID-IDFKNGRP-TOM-INPUT NOT = ALL '+'                              
131300       IF ARTNR-INPUT-SAKNAS                                              
131400         IF MID-IDFKNGRP-FOM-INPUT NUMERIC AND                            
131500            MID-IDFKNGRP-FOM-INPUT > ZEROS                                
131600           INSPECT MID-IDFKNGRP-FOM-INPUT REPLACING LEADING               
131700                                         ZEROS  BY  SPACE                 
131800           IF MID-IDFKNGRP-TOM-INPUT = ALL '+'                            
131900             MOVE MID-IDFKNGRP-FOM-INPUT TO MOD-IDFKNGRP-TOM-INPUT        
132000           ELSE                                                           
132100             IF MID-IDFKNGRP-TOM-INPUT NUMERIC                            
132200               INSPECT MID-IDFKNGRP-TOM-INPUT REPLACING LEADING           
132300                                             ZEROS  BY  SPACE             
132400               IF MID-IDFKNGRP-FOM-INPUT > MID-IDFKNGRP-TOM-INPUT         
132500                 MOVE MFS-NUM-FAELT-FEL  TO                               
132600                               MOD-IDFKNGRP-FOM-INPUT-ATTR                
132700                               MOD-IDFKNGRP-TOM-INPUT-ATTR                
132800                 MOVE NEJ TO INDATA-SW                                    
132900               ELSE                                                       
133000                 MOVE MID-IDFKNGRP-TOM-INPUT TO                           
133100                                        MOD-IDFKNGRP-TOM-INPUT            
133200               END-IF                                                     
133300             ELSE                                                         
133400               MOVE MFS-NUM-FAELT-FEL  TO                                 
133500                             MOD-IDFKNGRP-TOM-INPUT-ATTR                  
133600               MOVE NEJ TO INDATA-SW                                      
133700             END-IF                                                       
133800           END-IF                                                         
133900         ELSE                                                             
134000           MOVE MFS-NUM-FAELT-FEL  TO                                     
134100                         MOD-IDFKNGRP-FOM-INPUT-ATTR                      
134200           MOVE NEJ TO INDATA-SW                                          
134300         END-IF                                                           
134400       ELSE                                                               
134500         MOVE MFS-NUM-FAELT-FEL  TO                                       
134600                       MOD-IDFKNGRP-FOM-INPUT-ATTR                        
134700                       MOD-IDFKNGRP-TOM-INPUT-ATTR                        
134800                       MOD-IDARTNR-FOM-INPUT-ATTR                         
134900                       MOD-IDARTNR-TOM-INPUT-ATTR                         
135000         MOVE NEJ TO INDATA-SW                                            
135100       END-IF                                                             
135200                                                                          
135300       PERFORM GBCA-KOLL-FKNGRP-INTERVALL                                 
135400     ELSE                                                                 
135500       MOVE NEJ TO FKNGRP-INPUT-SW                                        
135600     END-IF                                                               
135700     .                                                                    
135800     EJECT                                                                
135900 GBCA-KOLL-FKNGRP-INTERVALL SECTION.                                      
136100                                                                          
136300     MOVE NEJ TO FKNGRP-TAB-SW                                            
136500     MOVE MID-IDFKNGRP-FOM-INPUT  TO  SPAR-IDFKNGRP-FOM (1)               
136600     MOVE MOD-IDFKNGRP-TOM-INPUT  TO  SPAR-IDFKNGRP-TOM (1)               
136700     INSPECT SPAR-IDFKNGRP-FOM (1) REPLACING LEADING SPACE                
136800                                               BY ZERO                    
136900     INSPECT SPAR-IDFKNGRP-TOM (1) REPLACING LEADING SPACE                
137000                                               BY ZERO                    
137010     MOVE SPAR-IDFKNGRP-FOM (1) TO W-IDFKNGRP-FOM                         
137020     MOVE SPAR-IDFKNGRP-TOM (1) TO W-IDFKNGRP-TOM                         
137200                                                                          
137210     PERFORM IMS-GHU-HANB12-KVAL                                          
137300     IF SEGMENT-SAKNAS                                                    
137301      IF DCS-IDDC NOT = W-6031-IDDC                                       
137302         MOVE W-6031-IDDC    TO W-IDDC-B6                                 
137303         PERFORM IMS-GU-WDB601                                            
137304      END-IF                                                              
137310      IF DCS-CDC OR DCS-CDC-TR                                            
137400        IF DCS-CDC                                                        
137500          MOVE WC-CDC-TR   TO W-6031-IDDC                                 
137501          PERFORM IMS-GHU-HANB12-KVAL                                     
137502          MOVE WC-CDC-SE   TO W-6031-IDDC                                 
137510          IF SEGMENT-SAKNAS                                               
137600            MOVE JA TO FKNGRP-TAB-SW                                      
137700          ELSE                                                            
137800            PERFORM S06-ERR-WRONG-INTERVAL                                
137900            MOVE MFS-NUM-FAELT-FEL     TO                                 
138000                         MOD-IDFKNGRP-FOM-INPUT-ATTR                      
138100                         MOD-IDFKNGRP-TOM-INPUT-ATTR                      
138200            MOVE NEJ TO INDATA-SW                                         
138300          END-IF                                                          
138400        ELSE                                                              
138410          MOVE WC-CDC-SE   TO W-6031-IDDC                                 
138420          PERFORM IMS-GHU-HANB12-KVAL                                     
138430          MOVE WC-CDC-TR   TO W-6031-IDDC                                 
138440          IF SEGMENT-SAKNAS                                               
138450            MOVE JA TO FKNGRP-TAB-SW                                      
138460          ELSE                                                            
138470            PERFORM S06-ERR-WRONG-INTERVAL                                
138480            MOVE MFS-NUM-FAELT-FEL     TO                                 
138490                         MOD-IDFKNGRP-FOM-INPUT-ATTR                      
138491                         MOD-IDFKNGRP-TOM-INPUT-ATTR                      
138492            MOVE NEJ TO INDATA-SW                                         
138493          END-IF                                                          
138500        END-IF                                                            
138600      ELSE                                                                
138700        MOVE JA TO FKNGRP-TAB-SW                                          
138800      END-IF                                                              
139500     ELSE                                                                 
139600        PERFORM S06-ERR-WRONG-INTERVAL                                    
139700        MOVE MFS-NUM-FAELT-FEL     TO                                     
139800                     MOD-IDFKNGRP-FOM-INPUT-ATTR                          
139900                     MOD-IDFKNGRP-TOM-INPUT-ATTR                          
140000        MOVE NEJ TO INDATA-SW                                             
140100     END-IF                                                               
140800     .                                                                    
140900     EJECT                                                                
141000 GBD-KOLL-LEVNR-TAB-INPUT SECTION.                                        
141200                                                                          
141300     IF MID-IDLEVNR-INPUT NOT = ALL '+'                                   
141500       IF ARTNR-INPUT-SAKNAS AND FKNGRP-INPUT-SAKNAS                      
141700         IF MID-IDLEVNR-INPUT > SPACES                                    
141800           MOVE MID-IDLEVNR-INPUT TO MOD-IDLEVNR-INPUT                    
144100         ELSE                                                             
144200           MOVE MFS-ALFA-FAELT-FEL  TO                                    
144300                         MOD-IDLEVNR-INPUT-ATTR                           
144400           MOVE NEJ TO INDATA-SW                                          
144500         END-IF                                                           
144600       ELSE                                                               
144610         MOVE MFS-ALFA-FAELT-FEL  TO                                      
144620                       MOD-IDLEVNR-INPUT-ATTR                             
144700         MOVE MFS-NUM-FAELT-FEL  TO                                       
145000                       MOD-IDARTNR-FOM-INPUT-ATTR                         
145100                       MOD-IDARTNR-TOM-INPUT-ATTR                         
145200                       MOD-IDFKNGRP-FOM-INPUT-ATTR                        
145300                       MOD-IDFKNGRP-TOM-INPUT-ATTR                        
145400         MOVE NEJ TO INDATA-SW                                            
145500       END-IF                                                             
145600                                                                          
145700       PERFORM GBDA-KOLL-LEVNR-INTERVALL                                  
145800     ELSE                                                                 
145900       MOVE NEJ TO LEVNR-INPUT-SW                                         
146000     END-IF                                                               
146100     .                                                                    
146200     EJECT                                                                
146300 GBDA-KOLL-LEVNR-INTERVALL SECTION.                                       
146500                                                                          
146700     MOVE NEJ TO LEVNR-TAB-SW                                             
146900     MOVE MID-IDLEVNR-INPUT  TO  SPAR-IDLEVNR (1)                         
147410     MOVE SPAR-IDLEVNR (1) TO W-IDLEVNR                                   
147420                                                                          
147500     PERFORM IMS-GHU-HANB13-KVAL                                          
147600                                                                          
147700     IF SEGMENT-SAKNAS                                                    
147701*NDC                                                                      
147702      IF DCS-IDDC NOT = W-6031-IDDC                                       
147703         MOVE W-6031-IDDC TO W-IDDC-B6                                    
147704         PERFORM IMS-GU-WDB601                                            
147705      END-IF                                                              
147710      IF DCS-CDC OR DCS-CDC-TR                                            
147800        IF DCS-CDC                                                        
147900          MOVE WC-CDC-TR TO W-6031-IDDC                                   
147901          PERFORM IMS-GHU-HANB13-KVAL                                     
147902          MOVE WC-CDC-SE TO W-6031-IDDC                                   
147910          IF SEGMENT-SAKNAS                                               
148000            MOVE JA TO LEVNR-TAB-SW                                       
148100          ELSE                                                            
148110            PERFORM S06-ERR-WRONG-INTERVAL                                
148120            MOVE MFS-ALFA-FAELT-FEL     TO                                
148130                         MOD-IDLEVNR-INPUT-ATTR                           
148150            MOVE NEJ TO INDATA-SW                                         
148200          END-IF                                                          
148300        ELSE                                                              
148310          MOVE WC-CDC-SE TO W-6031-IDDC                                   
148320          PERFORM IMS-GHU-HANB13-KVAL                                     
148330          MOVE WC-CDC-TR TO W-6031-IDDC                                   
148340          IF SEGMENT-SAKNAS                                               
148350            MOVE JA TO LEVNR-TAB-SW                                       
148360          ELSE                                                            
148370            PERFORM S06-ERR-WRONG-INTERVAL                                
148380            MOVE MFS-ALFA-FAELT-FEL     TO                                
148390                         MOD-IDLEVNR-INPUT-ATTR                           
148394            MOVE NEJ TO INDATA-SW                                         
148395          END-IF                                                          
148400        END-IF                                                            
148500      ELSE                                                                
148600        MOVE JA TO LEVNR-TAB-SW                                           
148700      END-IF                                                              
149900     ELSE                                                                 
150000        PERFORM S06-ERR-WRONG-INTERVAL                                    
150100        MOVE MFS-ALFA-FAELT-FEL     TO                                    
150200                     MOD-IDLEVNR-INPUT-ATTR                               
150400        MOVE NEJ TO INDATA-SW                                             
150500     END-IF                                                               
151200     .                                                                    
151300     EJECT                                                                
151400 GBE-KOLL-TOMMA-TAB SECTION.                                              
151500                                                                          
151600     IF MID-IDARTNR-FOM-INPUT  = ALL '+'                                  
151700       MOVE ZEROS               TO SPAR-IDARTNR-FOM (1)                   
151800     ELSE                                                                 
151900       MOVE MFS-NUM-FAELT-FEL   TO                                        
152000                     MOD-IDARTNR-FOM-INPUT-ATTR                           
152100       MOVE NEJ TO INDATA-SW                                              
152200     END-IF                                                               
152300     IF MID-IDARTNR-TOM-INPUT  = ALL '+'                                  
152400       MOVE ZEROS               TO SPAR-IDARTNR-TOM (1)                   
152500     ELSE                                                                 
152600       MOVE MFS-NUM-FAELT-FEL   TO                                        
152700                     MOD-IDARTNR-TOM-INPUT-ATTR                           
152800       MOVE NEJ TO INDATA-SW                                              
152900     END-IF                                                               
153000                                                                          
153100     IF MID-IDFKNGRP-FOM-INPUT  = ALL '+'                                 
153200       MOVE ZEROS               TO SPAR-IDFKNGRP-FOM (1)                  
153300     ELSE                                                                 
153400       MOVE MFS-NUM-FAELT-FEL   TO                                        
153500                     MOD-IDFKNGRP-FOM-INPUT-ATTR                          
153600       MOVE NEJ TO INDATA-SW                                              
153700     END-IF                                                               
153800     IF MID-IDFKNGRP-TOM-INPUT  = ALL '+'                                 
153900       MOVE ZEROS               TO SPAR-IDFKNGRP-TOM (1)                  
154000     ELSE                                                                 
154100       MOVE MFS-NUM-FAELT-FEL   TO                                        
154200                     MOD-IDFKNGRP-TOM-INPUT-ATTR                          
154300       MOVE NEJ TO INDATA-SW                                              
154400     END-IF                                                               
154500                                                                          
154600     IF MID-IDLEVNR-INPUT       = ALL '+'                                 
154700       MOVE SPACES              TO SPAR-IDLEVNR (1)                       
154800     ELSE                                                                 
154900       MOVE MFS-ALFA-FAELT-FEL   TO                                       
155000                     MOD-IDLEVNR-INPUT-ATTR                               
155100       MOVE NEJ TO INDATA-SW                                              
155200     END-IF                                                               
156000     .                                                                    
156100     EJECT                                                                
156200 GBF-KOLL-FT-TAB-INPUT SECTION.                                           
156400                                                                          
156500     IF MID-BEFT-FOM-INPUT NOT = ALL '+' OR                               
156600        MID-BEFT-TOM-INPUT NOT = ALL '+'                                  
156700       IF (W-KDINLOMR = 'F') OR (W-KDINLOMR = 'FBP' AND                   
156800          (ARTNR-INPUT-SAKNAS AND FKNGRP-INPUT-SAKNAS AND                 
156900           LEVNR-INPUT-SAKNAS))                                           
157000         IF MID-BEFT-FOM-INPUT NUMERIC AND                                
157100            MID-BEFT-FOM-INPUT > ZERO                                     
157200            INSPECT MID-BEFT-FOM-INPUT REPLACING LEADING                  
157300                                       ZEROS  BY  SPACE                   
157400           IF MID-BEFT-TOM-INPUT = ALL '+'                                
157500             MOVE MID-BEFT-FOM-INPUT TO MOD-BEFT-TOM-INPUT                
157600           ELSE                                                           
157700             IF MID-BEFT-TOM-INPUT NUMERIC                                
157800                INSPECT MID-BEFT-TOM-INPUT REPLACING LEADING              
157900                                           ZEROS  BY  SPACE               
158000               IF MID-BEFT-FOM-INPUT > MID-BEFT-TOM-INPUT                 
158100                 MOVE MFS-NUM-FAELT-FEL  TO                               
158200                               MOD-BEFT-FOM-INPUT-ATTR                    
158300                               MOD-BEFT-TOM-INPUT-ATTR                    
158400                 MOVE NEJ TO INDATA-SW                                    
158500               ELSE                                                       
158600                 MOVE MID-BEFT-TOM-INPUT TO MOD-BEFT-TOM-INPUT            
158700               END-IF                                                     
158800             ELSE                                                         
158900               MOVE MFS-NUM-FAELT-FEL  TO                                 
159000                             MOD-BEFT-TOM-INPUT-ATTR                      
159100               MOVE NEJ TO INDATA-SW                                      
159200             END-IF                                                       
159300           END-IF                                                         
159400         ELSE                                                             
159500           MOVE MFS-NUM-FAELT-FEL  TO                                     
159600                         MOD-BEFT-FOM-INPUT-ATTR                          
159700           MOVE NEJ TO INDATA-SW                                          
159800         END-IF                                                           
159900       ELSE                                                               
159910         MOVE MFS-ALFA-FAELT-FEL  TO                                      
159920                       MOD-IDLEVNR-INPUT-ATTR                             
160000         MOVE MFS-NUM-FAELT-FEL   TO                                      
160100                       MOD-IDARTNR-FOM-INPUT-ATTR                         
160200                       MOD-IDARTNR-TOM-INPUT-ATTR                         
160300                       MOD-IDFKNGRP-FOM-INPUT-ATTR                        
160400                       MOD-IDFKNGRP-TOM-INPUT-ATTR                        
160700                       MOD-BEFT-FOM-INPUT-ATTR                            
160800                       MOD-BEFT-TOM-INPUT-ATTR                            
160900         MOVE NEJ TO INDATA-SW                                            
161000       END-IF                                                             
161100                                                                          
161200       PERFORM GBFA-KOLL-FT-INTERVALL                                     
161300     END-IF                                                               
161400     .                                                                    
161500     EJECT                                                                
161600 GBFA-KOLL-FT-INTERVALL SECTION.                                          
161800                                                                          
162000     MOVE NEJ TO FT-TAB-SW                                                
162200     MOVE MID-BEFT-FOM-INPUT  TO  SPAR-BEFT-FOM (1)                       
162300     MOVE MOD-BEFT-TOM-INPUT  TO  SPAR-BEFT-TOM (1)                       
162400     INSPECT SPAR-BEFT-FOM (1) REPLACING LEADING SPACE                    
162500                                           BY ZERO                        
162600     INSPECT SPAR-BEFT-TOM (1) REPLACING LEADING SPACE                    
162700                                           BY ZERO                        
162710     MOVE SPAR-BEFT-FOM (1) TO W-BEFT-FOM                                 
162711     MOVE SPAR-BEFT-TOM (1) TO W-BEFT-TOM                                 
162720                                                                          
162800     PERFORM IMS-GHU-HANB14-KVAL                                          
162900                                                                          
163000     IF SEGMENT-SAKNAS                                                    
163300       MOVE JA TO FT-TAB-SW                                               
165200     ELSE                                                                 
165300       PERFORM S06-ERR-WRONG-INTERVAL                                     
165400       MOVE MFS-NUM-FAELT-FEL     TO                                      
165500                    MOD-BEFT-FOM-INPUT-ATTR                               
165600                    MOD-BEFT-TOM-INPUT-ATTR                               
165700       MOVE NEJ TO INDATA-SW                                              
165800     END-IF                                                               
166500     .                                                                    
166600     EJECT                                                                
166700 H-UPPDATERA SECTION.                                                     
166900                                                                          
167300     IF CMDKOD-FINNS                                                      
167400       PERFORM HB-KOLL-BORTTAG                                            
167500     ELSE                                                                 
167600       PERFORM HC-KOLL-TILLAEGG                                           
167700     END-IF                                                               
168300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
168400     CALL WMEDKONV USING MED-WMEDAREA                                     
168500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
168600     PERFORM MFS-FORM-ATTR                                                
168700     MOVE JA TO ALLT-SW                                                   
168800     .                                                                    
168900     EJECT                                                                
173400 HB-KOLL-BORTTAG SECTION.                                                 
173600                                                                          
173700     MOVE +1      TO TAB-IX                                               
173800     PERFORM UNTIL TAB-IX > MAXTAB-IX                                     
174300       IF SPAR-IDARTNR-FOM (TAB-IX) > ZERO                                
174400         MOVE SPAR-IDARTNR-FOM (TAB-IX) TO W-IDARTNR-FOM                  
174500         MOVE SPAR-IDARTNR-TOM (TAB-IX) TO W-IDARTNR-TOM                  
174600         PERFORM IMS-GHU-HANB11-KVAL                                      
174700         IF SEGMENT-FINNS                                                 
174800           PERFORM IMS-DLET-HANB11                                        
174900         END-IF                                                           
175000       END-IF                                                             
175100       IF SPAR-IDFKNGRP-FOM (TAB-IX) > ZERO                               
175200         MOVE SPAR-IDFKNGRP-FOM (TAB-IX) TO W-IDFKNGRP-FOM                
175300         MOVE SPAR-IDFKNGRP-TOM (TAB-IX) TO W-IDFKNGRP-TOM                
175400         PERFORM IMS-GHU-HANB12-KVAL                                      
175500         IF SEGMENT-FINNS                                                 
175600           PERFORM IMS-DLET-HANB12                                        
175700         END-IF                                                           
175800       END-IF                                                             
175900       IF SPAR-IDLEVNR (TAB-IX) > SPACE                                   
176000         MOVE SPAR-IDLEVNR (TAB-IX) TO W-IDLEVNR                          
176200         PERFORM IMS-GHU-HANB13-KVAL                                      
176300         IF SEGMENT-FINNS                                                 
176400           PERFORM IMS-DLET-HANB13                                        
176500         END-IF                                                           
176600       END-IF                                                             
176700       IF SPAR-BEFT-FOM (TAB-IX) > ZERO                                   
176800         MOVE SPAR-BEFT-FOM (TAB-IX) TO W-BEFT-FOM                        
176900         MOVE SPAR-BEFT-TOM (TAB-IX) TO W-BEFT-TOM                        
177000         PERFORM IMS-GHU-HANB14-KVAL                                      
177100         IF SEGMENT-FINNS                                                 
177200           PERFORM IMS-DLET-HANB14                                        
177300         END-IF                                                           
177400       END-IF                                                             
177500       ADD +1 TO TAB-IX                                                   
177600     END-PERFORM                                                          
187700     .                                                                    
187800     EJECT                                                                
190700 HC-KOLL-TILLAEGG SECTION.                                                
190900                                                                          
191200     IF SPAR-IDARTNR-FOM (1) > ZERO                                       
191300       MOVE SPAR-IDARTNR-FOM (1) TO 6032-IDARTNR-FOM                      
191310       MOVE SPAR-IDARTNR-TOM (1) TO 6032-IDARTNR-TOM                      
191400       MOVE SPAR-ADINLOMR    (1) TO 6032-ADINLOMR                         
191500                                                                          
191600       PERFORM IMS-ISRT-HANB11                                            
191700     END-IF                                                               
191800     IF SPAR-IDFKNGRP-FOM (1) > ZERO                                      
191900       MOVE SPAR-IDFKNGRP-FOM (1) TO 6034-IDFKNGRP-FOM                    
192000       MOVE SPAR-IDFKNGRP-TOM (1) TO 6034-IDFKNGRP-TOM                    
192100       MOVE SPAR-ADINLOMR    (1) TO 6034-ADINLOMR                         
192200                                                                          
192300       PERFORM IMS-ISRT-HANB12                                            
192400     END-IF                                                               
192500     IF SPAR-IDLEVNR (1) > SPACE                                          
192600       MOVE SPAR-IDLEVNR (1) TO 6036-IDLEVNR                              
192800       MOVE SPAR-ADINLOMR    (1) TO 6036-ADINLOMR                         
192900                                                                          
193000       PERFORM IMS-ISRT-HANB13                                            
193100     END-IF                                                               
193200     IF SPAR-BEFT-FOM (1) > ZERO                                          
193300       MOVE SPAR-BEFT-FOM (1) TO 6038-BEFT-FOM                            
193400       MOVE SPAR-BEFT-TOM (1) TO 6038-BEFT-TOM                            
193500       MOVE SPAR-ADINLOMR (1) TO 6038-ADINLOMR                            
193600                                                                          
193700       PERFORM IMS-ISRT-HANB14                                            
193800     END-IF                                                               
193900     .                                                                    
194000     EJECT                                                                
215900 S01-INF-LAST-PAGE SECTION.                                               
216100                                                                          
216200     MOVE INF-LAST-PAGE TO MED-IDMFSFEL                                   
216300     CALL WMEDKONV USING MED-WMEDAREA                                     
216400     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
216600     .                                                                    
216700     SKIP3                                                                
216800 S02-INF-MORE-INFO-EXISTS SECTION.                                        
217000                                                                          
217100     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
217200     CALL WMEDKONV USING MED-WMEDAREA                                     
217300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
217400     .                                                                    
217500     SKIP3                                                                
217600 S03-ERR-MANY-FUNCTIONS SECTION.                                          
217800                                                                          
217900     MOVE ERR-MANY-FUNCTIONS TO MED-IDMFSFEL                              
218000     CALL WMEDKONV USING MED-WMEDAREA                                     
218100     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
218200     PERFORM S14-ROER-EJ-FAELT-IN                                         
218300     PERFORM S13-ROER-EJ-FAELT-UT                                         
218400     PERFORM MFS-LAES-IN-IGEN                                             
218500     MOVE NEJ TO INDATA-SW                                                
218600                 ALLT-SW                                                  
218700     .                                                                    
218800     EJECT                                                                
218900 S04-ERR-WRONG-CHOISE SECTION.                                            
219100                                                                          
219200     MOVE ERR-WRONG-CHOISE TO MED-IDMFSFEL                                
219300     CALL WMEDKONV USING MED-WMEDAREA                                     
219400     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
219500     PERFORM S14-ROER-EJ-FAELT-IN                                         
219600     PERFORM S13-ROER-EJ-FAELT-UT                                         
219700     MOVE NEJ TO INDATA-SW                                                
219800                 ALLT-SW                                                  
219900     .                                                                    
220000     SKIP3                                                                
220100 S05-ERR-NOT-ON-REGISTER SECTION.                                         
220300                                                                          
220400     MOVE ERR-NOT-ON-REGISTER TO MED-IDMFSFEL                             
220500     CALL WMEDKONV USING MED-WMEDAREA                                     
220600     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
220700     PERFORM S14-ROER-EJ-FAELT-IN                                         
220800     PERFORM S13-ROER-EJ-FAELT-UT                                         
220900     MOVE NEJ TO INDATA-SW                                                
221000                 ALLT-SW                                                  
221100     .                                                                    
221200     SKIP3                                                                
221300 S06-ERR-WRONG-INTERVAL SECTION.                                          
221500                                                                          
221600     MOVE ERR-WRONG-INTERVAL TO MED-IDMFSFEL                              
221700     CALL WMEDKONV USING MED-WMEDAREA                                     
221800     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
221900     PERFORM S14-ROER-EJ-FAELT-IN                                         
222000     PERFORM S13-ROER-EJ-FAELT-UT                                         
222100     MOVE NEJ TO ALLT-SW                                                  
222200     .                                                                    
222300     EJECT                                                                
222400 S07-ERR-PF11-AND-NO-DATA SECTION.                                        
222600                                                                          
222700     MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                            
222800     CALL WMEDKONV USING MED-WMEDAREA                                     
222900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
223000     PERFORM S13-ROER-EJ-FAELT-UT                                         
223100     MOVE NEJ TO INDATA-SW                                                
223200                 ALLT-SW                                                  
223300     .                                                                    
223400     SKIP3                                                                
223500 S08-INF-PRESS-PF11 SECTION.                                              
223700                                                                          
223800     MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                  
223900     CALL WMEDKONV USING MED-WMEDAREA                                     
224000     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
224200     PERFORM S13-ROER-EJ-FAELT-UT                                         
224300     PERFORM MFS-LAES-IN-IGEN                                             
224400     MOVE NEJ TO INDATA-SW                                                
224500     .                                                                    
224600     SKIP3                                                                
224700 S09-ERR-WRONG-KEY SECTION.                                               
224900                                                                          
225000     MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                   
225100     CALL WMEDKONV USING MED-WMEDAREA                                     
225200     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
225300     PERFORM S11-RENSA-FAELT-UT                                           
225400     PERFORM S12-RENSA-FAELT-IN                                           
225500     .                                                                    
225600     EJECT                                                                
225700 S10-RENSA-RADER SECTION.                                                 
225900                                                                          
226000     MOVE +1  TO  RAD-IX                                                  
226100     PERFORM UNTIL  RAD-IX  >  MAX-INDX                                   
226200       MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-RAD (RAD-IX)                  
226300                               MOD-IDARTNR-FOM (RAD-IX)                   
226400                               MOD-IDARTNR-TOM (RAD-IX)                   
226500                               MOD-IDFKNGRP-FOM (RAD-IX)                  
226600                               MOD-IDFKNGRP-TOM (RAD-IX)                  
226700                               MOD-IDLEVNR (RAD-IX)                       
226900                               MOD-BEFT-FOM (RAD-IX)                      
227000                               MOD-BEFT-TOM (RAD-IX)                      
227100       ADD +1 TO RAD-IX                                                   
227200     END-PERFORM                                                          
227300     MOVE +1 TO  RAD-IX                                                   
227400     .                                                                    
227500     SKIP3                                                                
227600 S11-RENSA-FAELT-UT SECTION.                                              
227800                                                                          
227900*    --- ALLA UTDATA-FÄLT                                                 
228000*    --- INKL. BLÄDDRINGSNYCKLAR                                          
228010     MOVE SPACES TO          MOD-IDLEVNR-ENTER                            
228020                             MOD-IDLEVNR-NEXT                             
228100     MOVE ZEROS  TO          MOD-IDARTNR-ENTER                            
228200                             MOD-IDARTNR-NEXT                             
228300                             MOD-IDFKNGRP-ENTER                           
228400                             MOD-IDFKNGRP-NEXT                            
228700                             MOD-BEFT-ENTER                               
228800                             MOD-BEFT-NEXT                                
228900     PERFORM S10-RENSA-RADER                                              
229100     .                                                                    
229200     EJECT                                                                
229300 S12-RENSA-FAELT-IN SECTION.                                              
229500                                                                          
229600*    --- ALLA INDATA-FÄLT                                                 
229700     MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-INPUT                           
229800                             MOD-IDARTNR-FOM-INPUT                        
229900                             MOD-IDARTNR-TOM-INPUT                        
230000                             MOD-IDFKNGRP-FOM-INPUT                       
230100                             MOD-IDFKNGRP-TOM-INPUT                       
230200                             MOD-IDLEVNR-INPUT                            
230400                             MOD-BEFT-FOM-INPUT                           
230500                             MOD-BEFT-TOM-INPUT                           
230700     .                                                                    
230800     EJECT                                                                
230900 S13-ROER-EJ-FAELT-UT  SECTION.                                           
231100                                                                          
231200*    --- ALLA UTDATA-FÄLT                                                 
231300*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
231400     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-ENTER                          
231500                               MOD-IDARTNR-NEXT                           
231600                               MOD-IDFKNGRP-ENTER                         
231700                               MOD-IDFKNGRP-NEXT                          
231800                               MOD-IDLEVNR-ENTER                          
231900                               MOD-IDLEVNR-NEXT                           
232000                               MOD-BEFT-ENTER                             
232100                               MOD-BEFT-NEXT                              
232200                                                                          
232300     MOVE +1 TO RAD-IX                                                    
232400     PERFORM UNTIL RAD-IX > MAX-INDX                                      
232500      MOVE MFS-ROER-EJ-FAELT          TO MOD-ADINLOMR-RAD (RAD-IX)        
232600                                         MOD-IDARTNR-FOM (RAD-IX)         
232700                                         MOD-IDARTNR-TOM (RAD-IX)         
232800                                         MOD-IDFKNGRP-FOM (RAD-IX)        
232900                                         MOD-IDFKNGRP-TOM (RAD-IX)        
233000                                         MOD-IDLEVNR (RAD-IX)             
233200                                         MOD-BEFT-FOM (RAD-IX)            
233300                                         MOD-BEFT-TOM (RAD-IX)            
233400       ADD +1 TO RAD-IX                                                   
233500     END-PERFORM                                                          
233600     MOVE +1 TO RAD-IX                                                    
233700     .                                                                    
233800     EJECT                                                                
233900 S14-ROER-EJ-FAELT-IN  SECTION.                                           
234100                                                                          
234200*    --- ALLA INDATA-FÄLT                                                 
234300     MOVE MFS-ROER-EJ-FAELT      TO MOD-ADINLOMR-INPUT                    
234400                                    MOD-IDARTNR-FOM-INPUT                 
234500                                    MOD-IDARTNR-TOM-INPUT                 
234600                                    MOD-IDFKNGRP-FOM-INPUT                
234700                                    MOD-IDFKNGRP-TOM-INPUT                
234800                                    MOD-IDLEVNR-INPUT                     
235000                                    MOD-BEFT-FOM-INPUT                    
235100                                    MOD-BEFT-TOM-INPUT                    
235200     MOVE +1  TO  RAD-IX                                                  
235300     PERFORM UNTIL  RAD-IX  >  MAX-INDX                                   
235400       MOVE   MFS-ROER-EJ-FAELT TO MOD-KDCMD-RAD(RAD-IX)                  
235500       ADD +1 TO RAD-IX                                                   
235600     END-PERFORM                                                          
235700     MOVE +1 TO  RAD-IX                                                   
235800     .                                                                    
235900     SKIP3                                                                
237100* --- MFS SEKTIONER ---                                                   
237200     SKIP3                                                                
237300 MFS-FORM-ATTR SECTION.                                                   
237500                                                                          
237600*    --- ALLA INDATA-FÄLT                                                 
237700     MOVE MFS-FORMATETS-ATTR    TO MOD-ADINLOMR-INPUT-ATTR                
237800                                   MOD-IDARTNR-FOM-INPUT-ATTR             
237900                                   MOD-IDARTNR-TOM-INPUT-ATTR             
238000                                   MOD-IDFKNGRP-FOM-INPUT-ATTR            
238100                                   MOD-IDFKNGRP-TOM-INPUT-ATTR            
238200                                   MOD-IDLEVNR-INPUT-ATTR                 
238400                                   MOD-BEFT-FOM-INPUT-ATTR                
238500                                   MOD-BEFT-TOM-INPUT-ATTR                
238600     MOVE +1 TO RAD-IX                                                    
238700     PERFORM UNTIL RAD-IX > MAX-INDX                                      
238800       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-RAD-ATTR (RAD-IX)             
238900       ADD +1 TO RAD-IX                                                   
239000     END-PERFORM                                                          
239100     MOVE +1 TO  RAD-IX                                                   
239200     .                                                                    
239300     EJECT                                                                
239400 MFS-LAES-IN-IGEN SECTION.                                                
239600                                                                          
239700*    --- ALLA INDATA-FÄLT                                                 
239800     PERFORM MFS-LAES-IN-IGEN-RAD                                         
239900     MOVE +1   TO RAD-IX                                                  
240000     PERFORM UNTIL RAD-IX > MAX-INDX                                      
240100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-RAD-ATTR(RAD-IX)          
240200      ADD +1 TO RAD-IX                                                    
240300     END-PERFORM                                                          
240400     .                                                                    
240500     EJECT                                                                
240600 MFS-LAES-IN-IGEN-RAD SECTION.                                            
240800                                                                          
240900*    --- ALLA TABELL-INDATA-FÄLT RAD 19                                   
241100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-FOM-INPUT-ATTR           
241200                                     MOD-IDARTNR-TOM-INPUT-ATTR           
241300                                     MOD-IDFKNGRP-FOM-INPUT-ATTR          
241400                                     MOD-IDFKNGRP-TOM-INPUT-ATTR          
241500                                     MOD-IDLEVNR-INPUT-ATTR               
241700                                     MOD-BEFT-FOM-INPUT-ATTR              
241800                                     MOD-BEFT-TOM-INPUT-ATTR              
241900     .                                                                    
242000     EJECT                                                                
242100* --- IMS SEKTIONER ---                                                   
242200     SKIP3                                                                
242300 IMS-GET-MSG SECTION.                                                     
242500                                                                          
242600     MOVE '  QC' TO GODK-STATUSKODER                                      
242700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
242800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
242900     PERFORM IMS-STATUSKONTROLL                                           
243000     .                                                                    
243100     SKIP3                                                                
243200 IMS-INSERT-MSG SECTION.                                                  
243400                                                                          
243410     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
243420       MOVE '0' TO MFS-KDHUVOMR                                           
243700     END-IF                                                               
243800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
243900     MOVE SPACE TO GODK-STATUSKODER                                       
244000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
244100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
244200     PERFORM IMS-STATUSKONTROLL                                           
244300     .                                                                    
244400     EJECT                                                                
244500 IMS-GET-PLAA11 SECTION.                                                  
244700                                                                          
244800     STRING 'W6PLAA01(W6GXKEY  =' W-6005KEY-X ')'                         
244900          DELIMITED BY SIZE INTO SSA1                                     
245000     STRING 'W6PLAA11(ADINLOMR =' W-ADINLOMR-X ')'                        
245100          DELIMITED BY SIZE INTO SSA2                                     
245200     MOVE '  GE' TO GODK-STATUSKODER                                      
245300     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA1 SSA1 SSA2                
245400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
245500     PERFORM IMS-STATUSKONTROLL                                           
245600     .                                                                    
245700     EJECT                                                                
245800 IMS-GU-HANB01 SECTION.                                                   
245900                                                                          
246000     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
246100          DELIMITED BY SIZE INTO SSA1                                     
246200     MOVE '  GE' TO GODK-STATUSKODER                                      
246300     CALL CBLTDLI USING GU HANB-PCB DLI-IO-AREA1 SSA1                     
246400     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
246500     PERFORM IMS-STATUSKONTROLL                                           
246600     .                                                                    
246700     EJECT                                                                
246800 IMS-GHU-HANB11-KVAL SECTION.                                             
246900                                                                          
247000     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
247100          DELIMITED BY SIZE INTO SSA1                                     
247110     STRING 'W6HANB11(IDARTNRF<=' W-IDARTNR-FOM-X                         
247111                    '&IDARTNRT>=' W-IDARTNR-TOM-X ')'                     
247120          DELIMITED BY SIZE INTO SSA2                                     
247200     MOVE '  GE' TO GODK-STATUSKODER                                      
247300     CALL CBLTDLI USING GHU HANB-PCB DLI-IO-AREA2 SSA1 SSA2               
247400     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
247500     PERFORM IMS-STATUSKONTROLL                                           
247600     .                                                                    
247700     SKIP2                                                                
247800 IMS-GHU-HANB12-KVAL SECTION.                                             
247900                                                                          
248000     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
248100          DELIMITED BY SIZE INTO SSA1                                     
248200     STRING 'W6HANB12(IDFKNGRF<=' W-IDFKNGRP-FOM-X                        
248300                    '&IDFKNGRT>=' W-IDFKNGRP-TOM-X ')'                    
248400          DELIMITED BY SIZE INTO SSA2                                     
248500     MOVE '  GE' TO GODK-STATUSKODER                                      
248600     CALL CBLTDLI USING GHU HANB-PCB DLI-IO-AREA2 SSA1 SSA2               
248700     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
248800     PERFORM IMS-STATUSKONTROLL                                           
248900     .                                                                    
249000     SKIP2                                                                
249100 IMS-GHU-HANB13-KVAL SECTION.                                             
249200                                                                          
249300     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
249400          DELIMITED BY SIZE INTO SSA1                                     
249500     STRING 'W6HANB13(IDLEVNR  =' W-IDLEVNR-X ')'                         
249700          DELIMITED BY SIZE INTO SSA2                                     
249800     MOVE '  GE' TO GODK-STATUSKODER                                      
249900     CALL CBLTDLI USING GHU HANB-PCB DLI-IO-AREA2 SSA1 SSA2               
250000     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250100     PERFORM IMS-STATUSKONTROLL                                           
250200     .                                                                    
250300     SKIP2                                                                
250400 IMS-GHU-HANB14-KVAL SECTION.                                             
250410                                                                          
250420     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
250430          DELIMITED BY SIZE INTO SSA1                                     
250440     STRING 'W6HANB14(BEFTF   <=' W-BEFT-FOM-X                            
250450                    '&BEFTT   >=' W-BEFT-TOM-X ')'                        
250460          DELIMITED BY SIZE INTO SSA2                                     
250470     MOVE '  GE' TO GODK-STATUSKODER                                      
250480     CALL CBLTDLI USING GHU HANB-PCB DLI-IO-AREA2 SSA1 SSA2               
250490     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250491     PERFORM IMS-STATUSKONTROLL                                           
250492     .                                                                    
250493     EJECT                                                                
250494 IMS-GNP-HANB11-OKVAL SECTION.                                            
250495                                                                          
250496     STRING 'W6HANB11(IDARTNRF=>' W-IDARTNR-FOM-X ')'                     
250497          DELIMITED BY SIZE INTO SSA1                                     
250510     MOVE '  GE' TO GODK-STATUSKODER                                      
250520     CALL CBLTDLI USING GNP HANB-PCB DLI-IO-AREA2 SSA1                    
250530     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250540     PERFORM IMS-STATUSKONTROLL                                           
250550     .                                                                    
250560     SKIP2                                                                
250570 IMS-GNP-HANB12-OKVAL SECTION.                                            
250580                                                                          
250581     STRING 'W6HANB12(IDFKNGRF=>' W-IDFKNGRP-FOM-X ')'                    
250582          DELIMITED BY SIZE INTO SSA1                                     
250593     MOVE '  GE' TO GODK-STATUSKODER                                      
250594     CALL CBLTDLI USING GNP HANB-PCB DLI-IO-AREA2 SSA1                    
250595     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250596     PERFORM IMS-STATUSKONTROLL                                           
250597     .                                                                    
250598     SKIP2                                                                
250599 IMS-GNP-HANB13-OKVAL SECTION.                                            
250600                                                                          
250601     STRING 'W6HANB13(IDLEVNR =>' W-IDLEVNR-X ')'                         
250602          DELIMITED BY SIZE INTO SSA1                                     
250604     MOVE '  GE' TO GODK-STATUSKODER                                      
250605     CALL CBLTDLI USING GNP HANB-PCB DLI-IO-AREA2 SSA1                    
250606     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250607     PERFORM IMS-STATUSKONTROLL                                           
250608     .                                                                    
250609     SKIP2                                                                
250610 IMS-GNP-HANB14-OKVAL SECTION.                                            
250611                                                                          
250612     STRING 'W6HANB14(BEFTF   =>' W-BEFT-FOM-X ')'                        
250613          DELIMITED BY SIZE INTO SSA1                                     
250615     MOVE '  GE' TO GODK-STATUSKODER                                      
250616     CALL CBLTDLI USING GNP HANB-PCB DLI-IO-AREA2 SSA1                    
250617     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250618     PERFORM IMS-STATUSKONTROLL                                           
250619     .                                                                    
250620     EJECT                                                                
250621 IMS-DLET-HANB11       SECTION.                                           
250622                                                                          
250625     MOVE 'W6HANB11' TO SSA1                                              
250626     MOVE '  GE' TO GODK-STATUSKODER                                      
250627     CALL CBLTDLI USING DLET HANB-PCB DLI-IO-AREA2 SSA1                   
250628     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250629     PERFORM IMS-STATUSKONTROLL                                           
250630     .                                                                    
250631     SKIP2                                                                
250632 IMS-DLET-HANB12       SECTION.                                           
250633                                                                          
250636     MOVE 'W6HANB12' TO SSA1                                              
250637     MOVE '  GE' TO GODK-STATUSKODER                                      
250638     CALL CBLTDLI USING DLET HANB-PCB DLI-IO-AREA2 SSA1                   
250639     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250640     PERFORM IMS-STATUSKONTROLL                                           
250641     .                                                                    
250642     SKIP2                                                                
250643 IMS-DLET-HANB13       SECTION.                                           
250644                                                                          
250647     MOVE 'W6HANB13' TO SSA1                                              
250648     MOVE '  GE' TO GODK-STATUSKODER                                      
250649     CALL CBLTDLI USING DLET HANB-PCB DLI-IO-AREA2 SSA1                   
250650     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250651     PERFORM IMS-STATUSKONTROLL                                           
250652     .                                                                    
250653     SKIP2                                                                
250654 IMS-DLET-HANB14       SECTION.                                           
250655                                                                          
250658     MOVE 'W6HANB14' TO SSA1                                              
250659     MOVE '  GE' TO GODK-STATUSKODER                                      
250660     CALL CBLTDLI USING DLET HANB-PCB DLI-IO-AREA2 SSA1                   
250661     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250662     PERFORM IMS-STATUSKONTROLL                                           
250663     .                                                                    
250664     EJECT                                                                
250701 IMS-ISRT-HANB11       SECTION.                                           
250702                                                                          
250703     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
250704          DELIMITED BY SIZE INTO SSA1                                     
250705     MOVE 'W6HANB11' TO SSA2                                              
250706     MOVE '  GEII' TO GODK-STATUSKODER                                    
250707     CALL CBLTDLI USING ISRT HANB-PCB DLI-IO-AREA2 SSA1 SSA2              
250708     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250709     PERFORM IMS-STATUSKONTROLL                                           
250710     .                                                                    
250711     SKIP2                                                                
250712 IMS-ISRT-HANB12       SECTION.                                           
250713                                                                          
250714     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
250715          DELIMITED BY SIZE INTO SSA1                                     
250716     MOVE 'W6HANB12' TO SSA2                                              
250717     MOVE '  GEII' TO GODK-STATUSKODER                                    
250718     CALL CBLTDLI USING ISRT HANB-PCB DLI-IO-AREA2 SSA1 SSA2              
250719     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250720     PERFORM IMS-STATUSKONTROLL                                           
250721     .                                                                    
250722     SKIP2                                                                
250723 IMS-ISRT-HANB13       SECTION.                                           
250724                                                                          
250725     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
250726          DELIMITED BY SIZE INTO SSA1                                     
250727     MOVE 'W6HANB13' TO SSA2                                              
250728     MOVE '  GEII' TO GODK-STATUSKODER                                    
250729     CALL CBLTDLI USING ISRT HANB-PCB DLI-IO-AREA2 SSA1 SSA2              
250730     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250731     PERFORM IMS-STATUSKONTROLL                                           
250732     .                                                                    
250733     SKIP2                                                                
250734 IMS-ISRT-HANB14       SECTION.                                           
250735                                                                          
250736     STRING 'W6HANB01(W6GXKEY  =' W-6031KEY-X ')'                         
250737          DELIMITED BY SIZE INTO SSA1                                     
250738     MOVE 'W6HANB14' TO SSA2                                              
250739     MOVE '  GEII' TO GODK-STATUSKODER                                    
250740     CALL CBLTDLI USING ISRT HANB-PCB DLI-IO-AREA2 SSA1 SSA2              
250741     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250742     PERFORM IMS-STATUSKONTROLL                                           
250743     .                                                                    
250744     EJECT                                                                
250745 IMS-GNP-HANB11-OMR   SECTION.                                            
250746                                                                          
250747     STRING 'W6HANB11(IDARTNRF=>' W-IDARTNR-FOM-X                         
250748                    '&ADINLOMR =' W-ADINLOMR-X ')'                        
250749          DELIMITED BY SIZE INTO SSA1                                     
250752     MOVE '  GE' TO GODK-STATUSKODER                                      
250753     CALL CBLTDLI USING GNP  HANB-PCB DLI-IO-AREA2 SSA1                   
250754     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250755     PERFORM IMS-STATUSKONTROLL                                           
250756     .                                                                    
250757     SKIP2                                                                
250758 IMS-GNP-HANB12-OMR   SECTION.                                            
250759                                                                          
250760     STRING 'W6HANB12(IDFKNGRF=>' W-IDFKNGRP-FOM-X                        
250761                    '&ADINLOMR =' W-ADINLOMR-X ')'                        
250762          DELIMITED BY SIZE INTO SSA1                                     
250763     MOVE '  GE' TO GODK-STATUSKODER                                      
250764     CALL CBLTDLI USING GNP  HANB-PCB DLI-IO-AREA2 SSA1                   
250765     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250766     PERFORM IMS-STATUSKONTROLL                                           
250767     .                                                                    
250768     SKIP2                                                                
250769 IMS-GNP-HANB13-OMR   SECTION.                                            
250770                                                                          
250773     STRING 'W6HANB13(IDLEVNR =>' W-IDLEVNR-X                             
250774                    '&ADINLOMR =' W-ADINLOMR-X ')'                        
250775          DELIMITED BY SIZE INTO SSA1                                     
250776     MOVE '  GE' TO GODK-STATUSKODER                                      
250777     CALL CBLTDLI USING GNP  HANB-PCB DLI-IO-AREA2 SSA1                   
250778     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250779     PERFORM IMS-STATUSKONTROLL                                           
250780     .                                                                    
250781     SKIP2                                                                
250782 IMS-GNP-HANB14-OMR   SECTION.                                            
250783                                                                          
250784     STRING 'W6HANB14(BEFTF   =>' W-BEFT-FOM-X                            
250785                    '&ADINLOMR =' W-ADINLOMR-X ')'                        
250786          DELIMITED BY SIZE INTO SSA1                                     
250787     MOVE '  GE' TO GODK-STATUSKODER                                      
250788     CALL CBLTDLI USING GNP  HANB-PCB DLI-IO-AREA2 SSA1                   
250789     MOVE HANB-STATUS-CODE TO STATUS-WS                                   
250790     PERFORM IMS-STATUSKONTROLL                                           
250791     .                                                                    
250792     EJECT                                                                
250793 IMS-GU-WDB601    SECTION.                                                
250794     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
250795          DELIMITED BY SIZE INTO SSA1                                     
250796     MOVE '  GE' TO GODK-STATUSKODER                                      
250797     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
250798     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
250799     PERFORM IMS-STATUSKONTROLL                                           
250800     IF SEGMENT-SAKNAS                                                    
250801         MOVE SPACE TO DCS-KDDC                                           
250802     END-IF                                                               
250803     .                                                                    
250804 IMS-STATUSKONTROLL SECTION.                                              
250805                                                                          
250810     SET STATUS-IX TO 1                                                   
250900     SEARCH GODK-STATUS                                                   
251000       AT END                                                             
251100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
251200         DELIMITED BY SIZE INTO FELTEXT                                   
251300         CALL FELLOG                                                      
251400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
251500     END-SEARCH                                                           
251600     .                                                                    
