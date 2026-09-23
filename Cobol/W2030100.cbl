000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2030100.                                                
000400 AUTHOR.         LARS THELL      (MG).                                    
000500 DATE-WRITTEN.   90/12/11.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        VISAR SATSER I ORDERKÖ MED STATUS R                              
001100*        FÖR ANGIVEN ANSKAFFARE ELLER ALLA                                
001200*        OM MAN BARA ANGER ARTIKELNUMMER                                  
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLSATG (WDJ2)                              
001500*                                                                         
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T301                                              
001900*        MID:         W2I30101                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O30101                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002801                                                                          
002810*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W2030100'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +945  COMP SYNC.        
004200                                                                          
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400 77  WS-IDANSK-FOM               PIC X(03)   VALUE SPACE.                 
004500 77  WS-IDANSK-TOM               PIC X(03)   VALUE SPACE.                 
004600 77  WS-FLBYGGB                  PIC X(01)   VALUE SPACE.                 
004700 77  WS-IDARTNR                  PIC X(09)   VALUE SPACE.                 
004800 77  WS-IDORDNST                 PIC X(05)   VALUE SPACE.                 
004900 77  WS-ING-IDARTNR              PIC X(09)   VALUE SPACE.                 
005000 77  WS-KDSATKMB                 PIC X(01)   VALUE SPACE.                 
005010 77  UTSKR                       PIC X(01)   VALUE 'U'.                   
005020 77  FL-UTSKRIVEN-FINNS          PIC X(01)   VALUE 'N'.                   
005100                                                                          
005200 01  W-IDARTNR-11.                                                        
005300     03 W-IDARTNR                PIC  9(9).                               
005400     03 FILLER                   PIC  X(1)   VALUE '-'.                   
005500     03 W-REKSIFFR               PIC  9(1).                               
005600                                                                          
005700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005800     88  NYCKLAR-OK                          VALUE 'J'.                   
005900     88  NYCKLAR-FEL                         VALUE 'N'.                   
006000                                                                          
006100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006200     88  ALLT-OK                             VALUE 'J'.                   
006300                                                                          
006400 77  LAS-ART-INDEX-SW            PIC X       VALUE 'N'.                   
006500     88  LAS-ART-INDEX                       VALUE 'J'.                   
006600                                                                          
006700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006800     88  EGEN-MID                            VALUE '2301'.                
006900     88  GODK-MID                            VALUE '2301' '2302'          
007000                                                   '2303'.                
007100     EJECT                                                                
007200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007610     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007900*   -COPY WMEDAREA                                                        
007901     EJECT                                                                
007910*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007920*   -COPY WMSGINIT                                                        
007930     EJECT                                                                
008100 01  MESSAGE-CODES.                                                       
008200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008500     EJECT                                                                
008600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008900     SKIP3                                                                
009000*01  MID -COPY W2I30101                                                   
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009300     SKIP3                                                                
009400*01  -COPY WMSGAREA                                                       
009500     EJECT                                                                
009600     03  MOD REDEFINES MSG-AREA.                                          
009700*      05  -COPY W2O30101                                                 
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010000     SKIP3                                                                
010100*01  -COPY WMFSAREA                                                       
010200     EJECT                                                                
010300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700 01  NYCKLAR-TILL-DLI.                                                    
010800     03  W-WDJ2BSEQ-X.                                                    
010900         05  W-J2BSEQ-IDANSK        PIC S9(3)  VALUE ZERO COMP-3.         
011000         05  W-J2BSEQ-FLBYGGB       PIC  X(1)  VALUE SPACE.               
011100         05  W-J2BSEQ-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.         
011200         05  W-J2BSEQ-DAREGDAT      PIC  9(8)  VALUE ZERO.                
011300     SKIP2                                                                
011400     03  W-WDJ2BSEQ-MIN-X.                                                
011500         05  W-J2BSEQ-MIN-IDANSK    PIC S9(3)  VALUE ZERO COMP-3.         
011600         05  W-J2BSEQ-MIN-FLBYGGB   PIC  X(1)  VALUE SPACE.               
011700         05  W-J2BSEQ-MIN-IDARTNR   PIC S9(9)  VALUE ZERO COMP-3.         
011800         05  W-J2BSEQ-MIN-DAREGDAT  PIC  9(8)  VALUE ZERO.                
011900     SKIP2                                                                
012000     03  W-WDJ2BSEQ-MAX-X.                                                
012100         05  W-J2BSEQ-MAX-IDANSK    PIC S9(3)  VALUE ZERO COMP-3.         
012200         05  W-J2BSEQ-MAX-FLBYGGB   PIC  X(1)  VALUE SPACE.               
012300         05  W-J2BSEQ-MAX-IDARTNR   PIC S9(9)  VALUE ZERO COMP-3.         
012400         05  W-J2BSEQ-MAX-DAREGDAT  PIC  9(8)  VALUE ZERO.                
012500     SKIP2                                                                
012600     03  W-WDJ2CSEQ-X.                                                    
012700         05  W-J2CSEQ-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.         
012800         05  W-J2CSEQ-DAREGDAT      PIC  9(8)  VALUE ZERO.                
012900     SKIP2                                                                
013000     03  W-WDJ2CSEQ-MIN-X.                                                
013100         05  W-J2CSEQ-MIN-IDARTNR   PIC S9(9)  VALUE ZERO COMP-3.         
013200         05  W-J2CSEQ-MIN-DAREGDAT  PIC  9(8)  VALUE ZERO.                
013300     SKIP2                                                                
013400     03  W-WDJ2CSEQ-MAX-X.                                                
013500         05  W-J2CSEQ-MAX-IDARTNR   PIC S9(9)  VALUE ZERO COMP-3.         
013600         05  W-J2CSEQ-MAX-DAREGDAT  PIC  9(8)  VALUE ZERO.                
013700     SKIP2                                                                
013800     03  W-IDORDNST-X.                                                    
013900         05  W-IDORDNSB             PIC S9(5)  VALUE ZERO COMP-3.         
014000         05  W-IDORDNSS             PIC S9(1)  VALUE ZERO COMP-3.         
014100     SKIP2                                                                
014200     03  W-IDARTNR-MIN-X.                                                 
014300         05  W-IDARTNR-MIN         PIC  S9(9)  VALUE ZERO COMP-3.         
014400                                                                          
014500     03  W-IDARTNR-MAX-X.                                                 
014600         05  W-IDARTNR-MAX         PIC  S9(9)  VALUE ZERO COMP-3.         
014700                                                                          
014800     03  W-FLBYGGB-MIN-X.                                                 
014900         05  W-FLBYGGB-MIN         PIC   X(1)  VALUE SPACE.               
015000                                                                          
015100     03  W-FLBYGGB-MAX-X.                                                 
015200         05  W-FLBYGGB-MAX         PIC   X(1)  VALUE SPACE.               
015300                                                                          
015400     EJECT                                                                
015500*    --- STATUS-KOD FRÅN IMS                                              
015600 01  STATUS-WS                   PIC XX.                                  
015700     88  SEGMENT-FINNS                       VALUE '  '.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016000     SKIP2                                                                
016100 01  GODK-STATUSKODER.                                                    
016200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016300     SKIP3                                                                
016400 01  SSA1                        PIC X(128).                              
016500     EJECT                                                                
016600*    --- IMS FUNKTIONSKODER                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900*    ---  DLI INPUT-OUTPUT AREA                                           
017000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017100     SKIP3                                                                
017200 01  DLI-IO-AREA.                                                         
017300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
017400     SKIP3                                                                
017500     03  WLSATG01 REDEFINES IO-AREA.                                      
017600*        05  -COPY WDJ201                                                 
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900                                                                          
018000*01  -COPY W0009      -PRE MSG-                                           
018100     EJECT                                                                
018110*01  -COPY W0008      -PRE USEA-                                          
018120     05  FILLER                  PIC X.                                   
018130     EJECT                                                                
018200*01  -COPY W0008      -PRE SATI-                                          
018300     05  FILLER                  PIC X.                                   
018400*01  -COPY W0008      -PRE SATJ-                                          
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB SATI-PCB SATJ-PCB.            
018800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB SATI-PCB SATJ-PCB.            
018900                                                                          
019000     PERFORM IMS-GET-MSG                                                  
019100     IF SEGMENT-FINNS                                                     
019200       PERFORM A-INIT                                                     
019300       PERFORM B-KOLLA-NYCKLAR                                            
019400       IF NYCKLAR-OK                                                      
019500           IF MFS-FIRST                                                   
019600             PERFORM C-FOERSTA-SIDA                                       
019700           ELSE                                                           
019800             IF MFS-NEXT                                                  
019900               PERFORM D-NAESTA-SIDA                                      
020000             ELSE                                                         
020100               PERFORM E-SAMMA-SIDA                                       
020200             END-IF                                                       
020300           END-IF                                                         
020400           IF ALLT-OK                                                     
020500             PERFORM F-LAES-VISA-INFO                                     
020600           END-IF                                                         
020700       END-IF                                                             
020800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
020900       PERFORM IMS-INSERT-MSG                                             
021000     END-IF                                                               
021100                                                                          
021200     MOVE ZERO TO RETURN-CODE                                             
021300     GOBACK                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 A-INIT SECTION.                                                          
021700                                                                          
021800     IF MSG-DUBBLA-TRANSKODER                                             
021900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I30101                 
022000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022200     ELSE                                                                 
022300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I30101                  
022400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
022500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022600     END-IF                                                               
022700                                                                          
022800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
022900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
023000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
023100                                                                          
023200     MOVE LOW-VALUE TO MSG-AREA                                           
023300     MOVE 'W2O30101' TO MFS-IDMOD                                         
023400     MOVE '2301' TO MOD-IDTRANS                                           
023500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023600                                                                          
023700     IF NOT EGEN-MID                                                      
023800       MOVE SPACE TO MFS-KDTRTYP                                          
023900       MOVE '7' TO MFS-IDPFK                                              
024000     END-IF                                                               
024100                                                                          
024200     IF ENGLISH-TEXT                                                      
024300       MOVE +2 TO SPRAK-IX                                                
024400       MOVE 'GB ' TO MED-IDSKYLT                                          
024500     ELSE                                                                 
024600       MOVE +1 TO SPRAK-IX                                                
024700       MOVE 'S  ' TO MED-IDSKYLT                                          
024800     END-IF                                                               
024900                                                                          
025000     MOVE NEJ                  TO LAS-ART-INDEX-SW                        
025100     .                                                                    
025200     EJECT                                                                
025300 B-KOLLA-NYCKLAR SECTION.                                                 
025400                                                                          
025500     MOVE JA TO NYCKLAR-SW                                                
025600                                                                          
025700     MOVE LOW-VALUE            TO W-WDJ2BSEQ-MIN-X                        
025800                                  W-WDJ2CSEQ-MIN-X                        
025900                                  W-IDARTNR-MIN-X                         
026000                                  W-FLBYGGB-MIN-X                         
026100                                                                          
026200     MOVE HIGH-VALUE           TO W-WDJ2BSEQ-MAX-X                        
026300                                  W-WDJ2CSEQ-MAX-X                        
026400                                  W-IDARTNR-MAX-X                         
026500                                  W-FLBYGGB-MAX-X                         
026510                                                                          
026520     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026530     MOVE '001'             TO MSGI-KDCALL                                
026540     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026550     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026560     MOVE '2301'            TO MSGI-IDTRANS                               
026600                                                                          
026700     PERFORM BA-KOLLA-IDANSK-FOM                                          
026900     PERFORM BC-KOLLA-FLBYGGB                                             
026901     PERFORM BD-KOLLA-IDARTNR                                             
026903     PERFORM BE-FLYTTA-IDORDNST                                           
026904     PERFORM BF-FLYTTA-ING-IDARTNR                                        
026905     PERFORM BG-FLYTTA-KDSATKMB                                           
027051                                                                          
027052     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027053                                                                          
027054     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
027055     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
027056     MOVE MSGI-IDANSK TO WS-IDANSK-FOM                                    
027057     INSPECT WS-IDANSK-FOM REPLACING LEADING SPACE BY ZERO                
027058                                                                          
027059     PERFORM BB-KOLLA-IDANSK-TOM                                          
027060                                                                          
027061     IF WS-IDARTNR             NUMERIC AND                                
027062        WS-IDARTNR             >  ZERO                                    
027063         MOVE WS-IDARTNR       TO W-J2BSEQ-MIN-IDARTNR                    
027064                                  W-J2BSEQ-MAX-IDARTNR                    
027065                                  W-J2CSEQ-MIN-IDARTNR                    
027066                                  W-J2CSEQ-MAX-IDARTNR                    
027067                                  W-IDARTNR-MIN                           
027068                                  W-IDARTNR-MAX                           
027069     END-IF                                                               
027070                                                                          
027071     IF WS-IDANSK-FOM          NUMERIC AND                                
027072        WS-IDANSK-FOM          > ZERO                                     
027073         MOVE WS-IDANSK-FOM    TO W-J2BSEQ-MIN-IDANSK                     
027074     END-IF                                                               
027075                                                                          
027500     IF WS-IDANSK-FOM          NOT NUMERIC OR                             
027600        WS-IDANSK-FOM          =   ZERO                                   
027700         IF WS-IDARTNR         NUMERIC AND                                
027800            WS-IDARTNR         > ZERO                                     
027900             MOVE JA           TO LAS-ART-INDEX-SW                        
028000          ELSE                                                            
028100             MOVE NEJ          TO NYCKLAR-SW                              
028200         END-IF                                                           
028300     END-IF                                                               
028400                                                                          
028500     IF GODK-MID OR NYCKLAR-OK                                            
028510       IF GODK-MID                                                        
028600          MOVE WS-IDANSK-FOM      TO MOD-IDANSK-FOM-UT                    
028700          INSPECT MOD-IDANSK-FOM-UT REPLACING LEADING ZERO                
028710                                  BY SPACE                                
028800          MOVE WS-IDANSK-TOM      TO MOD-IDANSK-TOM-UT                    
028900          INSPECT MOD-IDANSK-TOM-UT REPLACING LEADING ZERO                
028910                                  BY SPACE                                
029000          MOVE WS-FLBYGGB         TO MOD-FLBYGGB-UT                       
029100          MOVE WS-IDARTNR         TO MOD-IDARTNR-UT                       
029200          INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE         
029300          MOVE WS-IDORDNST (1:4)  TO MOD-IDORDNSB-UT                      
029400          INSPECT MOD-IDORDNSB-UT REPLACING LEADING ZERO BY SPACE         
029500          MOVE WS-IDORDNST (5:1)  TO MOD-IDORDNSS-UT                      
029600          MOVE WS-ING-IDARTNR     TO MOD-ING-IDARTNR-UT                   
029700          INSPECT MOD-ING-IDARTNR-UT REPLACING LEADING ZERO               
029710                                  BY SPACE                                
029800          MOVE WS-KDSATKMB        TO MOD-KDSATKMB-UT                      
029810       ELSE                                                               
029820          IF NYCKLAR-OK                                                   
029821            MOVE WS-IDANSK-FOM   TO MOD-IDANSK-FOM-UT                     
029822            INSPECT MOD-IDANSK-FOM-UT REPLACING LEADING ZERO              
029823                                  BY SPACE                                
029824            MOVE WS-IDANSK-TOM   TO MOD-IDANSK-TOM-UT                     
029825            INSPECT MOD-IDANSK-TOM-UT REPLACING LEADING ZERO              
029826                                  BY SPACE                                
029827            MOVE WS-FLBYGGB      TO MOD-FLBYGGB-UT                        
029828            MOVE WS-IDARTNR      TO MOD-IDARTNR-UT                        
029829            INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO                 
029830                                   BY SPACE                               
029870            MOVE MFS-RENSA-FAELT TO MOD-IDORDNSB-UT                       
029880                                    MOD-IDORDNSS-UT                       
029890                                    MOD-ING-IDARTNR-UT                    
029891                                    MOD-KDSATKMB-UT                       
029892          END-IF                                                          
029893       END-IF                                                             
029900     ELSE                                                                 
030000       MOVE MFS-RENSA-FAELT TO MOD-IDANSK-FOM-UT                          
030100                               MOD-IDANSK-TOM-UT                          
030200                               MOD-FLBYGGB-UT                             
030300                               MOD-IDARTNR-UT                             
030400                               MOD-IDORDNSB-UT                            
030500                               MOD-IDORDNSS-UT                            
030600                               MOD-ING-IDARTNR-UT                         
030700                               MOD-KDSATKMB-UT                            
030800     END-IF                                                               
030900                                                                          
031000     IF NYCKLAR-FEL                                                       
031100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
031200       CALL WMEDKONV USING MED-WMEDAREA                                   
031300       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
031400       PERFORM MFS-RENSA-FAELT-UT                                         
031500     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 BA-KOLLA-IDANSK-FOM     SECTION.                                         
031900                                                                          
032000*    -- KONTROLL AV IDANSK-FOM                                            
032100     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-FOM-IN                            
032200                                                                          
032300     IF MID-IDANSK-FOM-IN = ALL '+'                                       
032310       IF MFS-IDTRANS = '2302' OR '2303'                                  
032330          MOVE MID-IDANSK-FOM-UT TO WS-IDANSK-FOM                         
032331          INSPECT WS-IDANSK-FOM REPLACING LEADING SPACE BY ZERO           
032332          IF WS-IDANSK-FOM NUMERIC                                        
032334             MOVE WS-IDANSK-FOM TO MSGI-IDANSK                            
032335          END-IF                                                          
032500       END-IF                                                             
032600     ELSE                                                                 
032610       IF MFS-IDTRANS = '2301'                                            
032620       OR MID-IDANSK-FOM-IN NUMERIC                                       
032700          MOVE MID-IDANSK-FOM-IN TO MSGI-IDANSK                           
032800       END-IF                                                             
033000     END-IF                                                               
033001                                                                          
033010     IF MID-IDANSK-FOM-IN = ALL '+'                                       
033020       CONTINUE                                                           
033040     ELSE                                                                 
033060       MOVE '7'                TO MFS-IDPFK                               
033070       MOVE SPACE              TO MFS-KDTRTYP                             
033080     END-IF                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 BB-KOLLA-IDANSK-TOM      SECTION.                                        
034000                                                                          
034100*    -- KONTROLL AV IDANSK-TOM                                            
034200     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-TOM-IN                            
034300                                                                          
034400     IF MID-IDANSK-TOM-IN      = ALL '+'                                  
034500       MOVE MID-IDANSK-TOM-UT  TO WS-IDANSK-TOM                           
034600       INSPECT WS-IDANSK-TOM REPLACING LEADING SPACE BY ZERO              
034700     ELSE                                                                 
034800       MOVE MID-IDANSK-TOM-IN  TO WS-IDANSK-TOM                           
034900       MOVE '7'                TO MFS-IDPFK                               
035000       MOVE SPACE              TO MFS-KDTRTYP                             
035100     END-IF                                                               
035200                                                                          
035210     IF GODK-MID                                                          
035220        CONTINUE                                                          
035230     ELSE                                                                 
035240        MOVE ZERO TO WS-IDANSK-TOM                                        
035250     END-IF                                                               
035260                                                                          
035300     IF WS-IDANSK-TOM          NUMERIC AND                                
035400        WS-IDANSK-TOM          > ZERO                                     
035500        MOVE WS-IDANSK-TOM    TO W-J2BSEQ-MAX-IDANSK                      
035600     ELSE                                                                 
035610       IF WS-IDANSK-FOM NUMERIC                                           
035700          MOVE WS-IDANSK-FOM    TO WS-IDANSK-TOM                          
035800                                   W-J2BSEQ-MAX-IDANSK                    
035810       END-IF                                                             
035900     END-IF                                                               
036000                                                                          
036100     .                                                                    
036200     EJECT                                                                
036300 BC-KOLLA-FLBYGGB         SECTION.                                        
036400                                                                          
036500*    -- KONTROLL AV FLAGGA BYGGBAR                                        
036600     MOVE MFS-RENSA-FAELT TO MOD-FLBYGGB-IN                               
036700                                                                          
036800     IF MID-FLBYGGB-IN         = ALL '+'                                  
036900       MOVE MID-FLBYGGB-UT     TO WS-FLBYGGB                              
037000     ELSE                                                                 
037100       MOVE MID-FLBYGGB-IN     TO WS-FLBYGGB                              
037200       MOVE '7'                TO MFS-IDPFK                               
037300       MOVE SPACE              TO MFS-KDTRTYP                             
037400     END-IF                                                               
037410                                                                          
037420     IF GODK-MID                                                          
037430        CONTINUE                                                          
037440     ELSE                                                                 
037450        MOVE SPACE TO WS-FLBYGGB                                          
037460     END-IF                                                               
037500                                                                          
037600     IF WS-FLBYGGB             = JA OR NEJ OR UTSKR OR SPACE              
037700         IF WS-FLBYGGB         = JA OR NEJ                                
037800            MOVE WS-FLBYGGB       TO W-J2BSEQ-MIN-FLBYGGB                 
037900                                     W-J2BSEQ-MAX-FLBYGGB                 
038000                                     W-FLBYGGB-MIN-X                      
038100                                     W-FLBYGGB-MAX-X                      
038110         ELSE                                                             
038120            IF WS-FLBYGGB      = UTSKR                                    
038121               MOVE JA            TO W-J2BSEQ-MIN-FLBYGGB                 
038122                                     W-J2BSEQ-MAX-FLBYGGB                 
038123                                     W-FLBYGGB-MIN-X                      
038124                                     W-FLBYGGB-MAX-X                      
038200            END-IF                                                        
038210         END-IF                                                           
038300     ELSE                                                                 
038311         MOVE NEJ TO NYCKLAR-SW                                           
038500     END-IF                                                               
038600                                                                          
038700     .                                                                    
038800     EJECT                                                                
038900 BD-KOLLA-IDARTNR         SECTION.                                        
039000                                                                          
039100*    -- KONTROLL AV ARTIKEL NUMMER                                        
039200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
039300                                                                          
039400     IF MID-IDARTNR-IN = ALL '+'                                          
039410       IF MFS-IDTRANS = '2302' OR '2303'                                  
039430          MOVE MID-IDARTNR-UT TO WS-IDARTNR                               
039440          INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO              
039450          IF (WS-IDARTNR NUMERIC                                          
039460          AND WS-IDARTNR > ZERO)                                          
039470             MOVE WS-IDARTNR TO MSGI-IDARTNR                              
039480          END-IF                                                          
039490       END-IF                                                             
039700     ELSE                                                                 
039710       IF MFS-IDTRANS = '2301'                                            
039720       OR (WS-IDARTNR NUMERIC                                             
039730       AND WS-IDARTNR > ZERO)                                             
039800          MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                             
039900       END-IF                                                             
040100     END-IF                                                               
041200                                                                          
041210     IF MID-IDARTNR-IN         = ALL '+'                                  
041220       CONTINUE                                                           
041240     ELSE                                                                 
041260       MOVE '7'                TO MFS-IDPFK                               
041270       MOVE SPACE              TO MFS-KDTRTYP                             
041280     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 BE-FLYTTA-IDORDNST       SECTION.                                        
041600                                                                          
041700*    -- FLYTTNING AV SATSORDERNUMMER                                      
041800     MOVE MFS-RENSA-FAELT      TO MOD-IDORDNSB-IN                         
041900                                  MOD-IDORDNSS-IN                         
042000                                                                          
042100     IF MID-IDORDNSB-IN        = ALL '+'                                  
042200       MOVE MID-IDORDNSB-UT    TO WS-IDORDNST (1:4)                       
042300       INSPECT WS-IDORDNST   REPLACING LEADING SPACE BY ZERO              
042400       MOVE MID-IDORDNSS-UT    TO WS-IDORDNST (5:1)                       
042500     ELSE                                                                 
042600       MOVE MID-IDORDNSB-IN    TO WS-IDORDNST  (1:4)                      
042700       MOVE MID-IDORDNSS-IN    TO WS-IDORDNST  (5:1)                      
042800     END-IF                                                               
042900                                                                          
043000     .                                                                    
043100     EJECT                                                                
043200 BF-FLYTTA-ING-IDARTNR    SECTION.                                        
043300                                                                          
043400*    -- FLYTTNING AV INGÅENDE ARTIKELNUMMER                               
043500     MOVE MFS-RENSA-FAELT      TO MOD-ING-IDARTNR-IN                      
043600                                                                          
043700     IF MID-ING-IDARTNR-IN     = ALL '+'                                  
043800       MOVE MID-ING-IDARTNR-UT TO WS-ING-IDARTNR                          
043900       INSPECT WS-ING-IDARTNR   REPLACING LEADING SPACE BY ZERO           
044000     ELSE                                                                 
044100       MOVE MID-ING-IDARTNR-IN  TO WS-ING-IDARTNR                         
044200     END-IF                                                               
044300                                                                          
044400     .                                                                    
044500     EJECT                                                                
044600 BG-FLYTTA-KDSATKMB       SECTION.                                        
044700                                                                          
044800*    -- FLYTTNING AV SATS KOMBINATION                                     
044900     MOVE MFS-RENSA-FAELT      TO MOD-KDSATKMB-IN                         
045000                                                                          
045100     IF MID-KDSATKMB-IN        = ALL '+'                                  
045200       MOVE MID-KDSATKMB-UT    TO WS-KDSATKMB                             
045300     ELSE                                                                 
045400       MOVE MID-KDSATKMB-IN    TO WS-KDSATKMB                             
045500     END-IF                                                               
045600                                                                          
045700     .                                                                    
045800     EJECT                                                                
045900 C-FOERSTA-SIDA SECTION.                                                  
046000                                                                          
046100     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
046200     CALL WMEDKONV USING MED-WMEDAREA                                     
046300     MOVE MED-TEMFSINF         TO MOD-TEMFSINF                            
046400                                                                          
046500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
046600     MOVE JA                   TO ALLT-SW                                 
046700     .                                                                    
046800     EJECT                                                                
046900 D-NAESTA-SIDA SECTION.                                                   
047000                                                                          
047100     MOVE MID-IDANSK-NEXT      TO W-J2BSEQ-IDANSK                         
047200     MOVE MID-IDARTNR-NEXT     TO W-J2BSEQ-IDARTNR                        
047300                                  W-J2CSEQ-IDARTNR                        
047400     MOVE MID-FLBYGGB-NEXT     TO W-J2BSEQ-FLBYGGB                        
047500     MOVE MID-TIREGDAT-NEXT    TO W-J2BSEQ-DAREGDAT                       
047600                                  W-J2CSEQ-DAREGDAT                       
047610     IF MID-TIREGDAT-NEXT NOT ZERO                                        
047620       IF MID-TIREGDAT-NEXT < 500000                                      
047630         MOVE 20               TO W-J2BSEQ-DAREGDAT (1:2)                 
047640                                  W-J2CSEQ-DAREGDAT (1:2)                 
047650       ELSE                                                               
047660         IF MID-TIREGDAT-NEXT < 999999                                    
047670           MOVE 19             TO W-J2BSEQ-DAREGDAT (1:2)                 
047680                                  W-J2CSEQ-DAREGDAT (1:2)                 
047690         ELSE                                                             
047691           MOVE 99999999       TO W-J2BSEQ-DAREGDAT                       
047692                                  W-J2CSEQ-DAREGDAT                       
047693         END-IF                                                           
047694       END-IF                                                             
047695     END-IF                                                               
047700     MOVE MID-IDORDNSB-NEXT    TO W-IDORDNSB                              
047800     MOVE MID-IDORDNSS-NEXT    TO W-IDORDNSS                              
047900     MOVE JA TO ALLT-SW                                                   
048000     .                                                                    
048100     EJECT                                                                
048200 E-SAMMA-SIDA SECTION.                                                    
048300                                                                          
048400     MOVE MID-IDANSK-ENTER     TO W-J2BSEQ-IDANSK                         
048500     MOVE MID-IDARTNR-ENTER    TO W-J2BSEQ-IDARTNR                        
048600                                  W-J2CSEQ-IDARTNR                        
048700     MOVE MID-FLBYGGB-ENTER    TO W-J2BSEQ-FLBYGGB                        
048800     MOVE MID-TIREGDAT-ENTER   TO W-J2BSEQ-DAREGDAT                       
048900                                  W-J2CSEQ-DAREGDAT                       
048910     IF MID-TIREGDAT-ENTER NOT = ZERO                                     
048920       IF MID-TIREGDAT-ENTER < 500000                                     
048930         MOVE 20               TO W-J2BSEQ-DAREGDAT (1:2)                 
048940                                  W-J2CSEQ-DAREGDAT (1:2)                 
048950       ELSE                                                               
048960         IF MID-TIREGDAT-ENTER < 999999                                   
048970           MOVE 19             TO W-J2BSEQ-DAREGDAT (1:2)                 
048980                                  W-J2CSEQ-DAREGDAT (1:2)                 
048990         ELSE                                                             
048991           MOVE 99999999       TO W-J2BSEQ-DAREGDAT                       
048992                                  W-J2CSEQ-DAREGDAT                       
048993         END-IF                                                           
048994       END-IF                                                             
048995     END-IF                                                               
049000     MOVE MID-IDORDNSB-ENTER   TO W-IDORDNSB                              
049100     MOVE MID-IDORDNSS-ENTER   TO W-IDORDNSS                              
049200     MOVE JA                   TO ALLT-SW                                 
049300     .                                                                    
049400     EJECT                                                                
049500 F-LAES-VISA-INFO SECTION.                                                
049600                                                                          
049700     IF LAS-ART-INDEX                                                     
049800         IF MFS-FIRST                                                     
049900             PERFORM IMS-GU-SATJ01-OKVAL                                  
050000          ELSE                                                            
050100             PERFORM IMS-GU-SATJ01-KVAL                                   
050200         END-IF                                                           
050300      ELSE                                                                
050400         IF MFS-FIRST                                                     
050500             PERFORM IMS-GU-SATI01-OKVAL                                  
050600          ELSE                                                            
050700             PERFORM IMS-GU-SATI01-KVAL                                   
050800         END-IF                                                           
050900     END-IF                                                               
051000                                                                          
051100     IF SEGMENT-SAKNAS                                                    
051200         IF MFS-NEXT                                                      
051300             MOVE '115'        TO MED-IDMFSFEL                            
051400          ELSE                                                            
051500             MOVE '005'        TO MED-IDMFSFEL                            
051600         END-IF                                                           
051700         CALL WMEDKONV USING MED-WMEDAREA                                 
051800         MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                            
051900         PERFORM MFS-RENSA-FAELT-UT                                       
052000      ELSE                                                                
052100         MOVE +1               TO INDX                                    
052200                                                                          
052300         PERFORM FA-SKAPA-NYCKLAR-ENTER                                   
052400                                                                          
052500         PERFORM UNTIL INDX    > MAX-INDX                                 
052600             IF SEGMENT-FINNS                                             
052700                 PERFORM FB-RAD                                           
052800                 IF LAS-ART-INDEX                                         
052900                     PERFORM IMS-GN-SATJ01-OKVAL                          
053000                  ELSE                                                    
053100                     PERFORM IMS-GN-SATI01-OKVAL                          
053200                 END-IF                                                   
053300              ELSE                                                        
053400                 PERFORM MFS-RENSA-RAD-FAELT-UT                           
053410                 ADD +1              TO INDX                              
053500             END-IF                                                       
053700         END-PERFORM                                                      
053710                                                                          
053720         IF WS-FLBYGGB = UTSKR AND FL-UTSKRIVEN-FINNS = NEJ               
053770            MOVE '005'        TO MED-IDMFSFEL                             
053790            CALL WMEDKONV USING MED-WMEDAREA                              
053791            MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                         
053792            PERFORM MFS-RENSA-FAELT-UT                                    
053800         END-IF                                                           
053900         PERFORM FC-SKAPA-NYCKLAR-NEXT                                    
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 FA-SKAPA-NYCKLAR-ENTER  SECTION.                                         
054400                                                                          
054500     IF SEGMENT-FINNS                                                     
054600         MOVE SHUV-IDANSK      TO MOD-IDANSK-ENTER                        
054700         MOVE SHUV-IDARTNR     TO MOD-IDARTNR-ENTER                       
054800         MOVE SHUV-FLBYGGB     TO MOD-FLBYGGB-ENTER                       
054900         MOVE SHUV-DAREGDAT (3:6) TO MOD-TIREGDAT-ENTER                   
055000         MOVE SHUV-IDORDNSB    TO MOD-IDORDNSB-ENTER                      
055100         MOVE SHUV-IDORDNSS    TO MOD-IDORDNSS-ENTER                      
055200      ELSE                                                                
055300         MOVE ZERO             TO MOD-IDANSK-ENTER                        
055400                                  MOD-IDARTNR-ENTER                       
055500                                  MOD-FLBYGGB-ENTER                       
055600                                  MOD-TIREGDAT-ENTER                      
055700                                  MOD-IDORDNSB-ENTER                      
055800                                  MOD-IDORDNSS-ENTER                      
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200 FB-RAD SECTION.                                                          
056300                                                                          
056310     IF WS-FLBYGGB = NEJ                                                  
056311         PERFORM FBA-REDIGERA-RAD                                         
056312         ADD +1 TO INDX                                                   
056320     ELSE                                                                 
056330        IF WS-FLBYGGB = JA OR SPACE                                       
056331           IF SHUV-KDSATSTA = UTSKR                                       
056332              CONTINUE                                                    
056333           ELSE                                                           
056334              PERFORM FBA-REDIGERA-RAD                                    
056335              ADD +1 TO INDX                                              
056336           END-IF                                                         
056340        ELSE                                                              
056350           IF WS-FLBYGGB = UTSKR                                          
056351              IF SHUV-KDSATSTA = UTSKR                                    
056352                 PERFORM FBA-REDIGERA-RAD                                 
056353                 ADD +1 TO INDX                                           
056354                 MOVE JA TO FL-UTSKRIVEN-FINNS                            
056355              ELSE                                                        
056357                 CONTINUE                                                 
056358              END-IF                                                      
056360           END-IF                                                         
056370        END-IF                                                            
056380     END-IF                                                               
056381     .                                                                    
056382     EJECT                                                                
056383                                                                          
056384                                                                          
056390 FBA-REDIGERA-RAD SECTION.                                                
056391                                                                          
056400     IF SEGMENT-FINNS                                                     
056500         MOVE SHUV-IDANSK        TO MOD-IDANSK-RAD   (INDX)               
056600         MOVE SHUV-IDARTNR       TO W-IDARTNR                             
056700         MOVE SHUV-REKSIFFR      TO W-REKSIFFR                            
056800         INSPECT W-IDARTNR-11                                             
056900         REPLACING LEADING ZERO BY SPACE                                  
057000         MOVE W-IDARTNR-11       TO MOD-IDARTNR-RAD  (INDX)               
057100         MOVE SHUV-IDORDNSB      TO MOD-IDORDNSB                          
057200                             IN MOD-IDORDNST-RAD (INDX)                   
057300         MOVE SHUV-IDORDNSS      TO MOD-IDORDNSS                          
057400                             IN MOD-IDORDNST-RAD (INDX)                   
057500         MOVE SHUV-DAREGDAT (3:6) TO MOD-TIREGDAT-RAD (INDX)              
057600         MOVE SHUV-TIBEGPAC      TO MOD-TIBEGPAC-RAD (INDX)               
057700         MOVE SHUV-KVBEART       TO MOD-KVBEART-RAD  (INDX)               
057800         MOVE SHUV-KVBYGGB       TO MOD-KVBYGGB-RAD  (INDX)               
057900         MOVE SHUV-KDCLAGER      TO MOD-KDCLAGER-RAD (INDX)               
058000         MOVE SHUV-IDPRC         TO MOD-IDPRC-RAD    (INDX)               
058100         IF SHUV-KDSATSTA        =  'U'                                   
058200             MOVE SHUV-KDSATSTA TO MOD-FLBYGGB-RAD (INDX)                 
058300          ELSE                                                            
058400             MOVE SHUV-FLBYGGB      TO MOD-FLBYGGB-RAD (INDX)             
058500         END-IF                                                           
058600     END-IF                                                               
058700     .                                                                    
058800     EJECT                                                                
058900 FC-SKAPA-NYCKLAR-NEXT      SECTION.                                      
059000                                                                          
059100     IF SEGMENT-FINNS                                                     
059200         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
059300         CALL WMEDKONV USING MED-WMEDAREA                                 
059400         MOVE MED-TEMFSINF     TO MOD-TEMFSINF                            
059500         MOVE SHUV-IDANSK      TO MOD-IDANSK-NEXT                         
059600         MOVE SHUV-IDARTNR     TO MOD-IDARTNR-NEXT                        
059700         MOVE SHUV-FLBYGGB     TO MOD-FLBYGGB-NEXT                        
059800         MOVE SHUV-IDORDNSB    TO MOD-IDORDNSB-NEXT                       
059900         MOVE SHUV-IDORDNSS    TO MOD-IDORDNSS-NEXT                       
060000         MOVE SHUV-DAREGDAT (3:6) TO MOD-TIREGDAT-NEXT                    
060100     ELSE                                                                 
060200         MOVE ZERO             TO MOD-IDANSK-NEXT                         
060300                                  MOD-IDARTNR-NEXT                        
060400                                  MOD-FLBYGGB-NEXT                        
060500                                  MOD-IDORDNSB-NEXT                       
060600                                  MOD-IDORDNSS-NEXT                       
060700                                  MOD-TIREGDAT-NEXT                       
060800     END-IF                                                               
060900     .                                                                    
061000     EJECT                                                                
061100 MFS-RENSA-FAELT-UT SECTION.                                              
061200                                                                          
061300*    --- ALLA UTDATA-FÄLT                                                 
061400*    --- INKL. BLÄDDRINGSNYCKLAR                                          
061500     MOVE MFS-RENSA-FAELT      TO MOD-IDANSK-ENTER                        
061600                                  MOD-IDANSK-NEXT                         
061700                                  MOD-IDARTNR-ENTER                       
061800                                  MOD-IDARTNR-NEXT                        
061900                                  MOD-IDORDNSB-ENTER                      
062000                                  MOD-IDORDNSS-ENTER                      
062100                                  MOD-IDORDNSB-NEXT                       
062200                                  MOD-IDORDNSS-NEXT                       
062300                                  MOD-TIREGDAT-ENTER                      
062400                                  MOD-TIREGDAT-NEXT                       
062500                                                                          
062600     MOVE +1                   TO INDX                                    
062700     PERFORM UNTIL             INDX > MAX-INDX                            
062800         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
062900         ADD +1                TO INDX                                    
063000     END-PERFORM                                                          
063100     .                                                                    
063200     SKIP2                                                                
063300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
063400                                                                          
063500     MOVE MFS-RENSA-FAELT      TO MOD-IDANSK-RAD   (INDX)                 
063600                                  MOD-IDARTNR-RAD  (INDX)                 
063700                                  MOD-IDORDNST-RAD (INDX)                 
063800                                  MOD-TIREGDAT-RAD (INDX)                 
063900                                  MOD-TIBEGPAC-RAD (INDX)                 
064000                                  MOD-KVBEART-RAD  (INDX)                 
064100                                  MOD-KVBYGGB-RAD  (INDX)                 
064200                                  MOD-KDCLAGER-RAD (INDX)                 
064300                                  MOD-IDPRC-RAD    (INDX)                 
064400                                  MOD-FLBYGGB-RAD  (INDX)                 
064500     .                                                                    
064600     EJECT                                                                
064700* --- IMS SEKTIONER ---                                                   
064800     SKIP3                                                                
064900 IMS-GET-MSG SECTION.                                                     
065000                                                                          
065100     MOVE '  QC' TO GODK-STATUSKODER                                      
065200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
065300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600     SKIP3                                                                
065700 IMS-INSERT-MSG SECTION.                                                  
065800                                                                          
065900     IF ENGLISH-TEXT                                                      
066000       MOVE 'N' TO MFS-KDHUVOMR                                           
066100     END-IF                                                               
066110                                                                          
066200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
066300     MOVE SPACE TO GODK-STATUSKODER                                       
066400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
066500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066600     PERFORM IMS-STATUSKONTROLL                                           
066700     .                                                                    
066800     EJECT                                                                
066900 IMS-GU-SATI01-KVAL    SECTION.                                           
067000     STRING 'WLSATG01(WDJ2BSEQ =' W-WDJ2BSEQ-X                            
067100                    '&IDORDNST =' W-IDORDNST-X ')'                        
067200          DELIMITED BY SIZE INTO SSA1                                     
067300     MOVE '  GE' TO GODK-STATUSKODER                                      
067400     CALL CBLTDLI USING GU SATI-PCB DLI-IO-AREA SSA1                      
067500     MOVE SATI-STATUS-CODE TO STATUS-WS                                   
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     .                                                                    
067800     SKIP3                                                                
067900 IMS-GU-SATI01-OKVAL   SECTION.                                           
068000     STRING 'WLSATG01(WDJ2BSEQ>=' W-WDJ2BSEQ-MIN-X                        
068100                    '&WDJ2BSEQ<=' W-WDJ2BSEQ-MAX-X                        
068200                    '&FLBYGGB >=' W-FLBYGGB-MIN-X                         
068300                    '&FLBYGGB <=' W-FLBYGGB-MAX-X                         
068400                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
068500                    '&IDARTNR <=' W-IDARTNR-MAX-X ')'                     
068600          DELIMITED BY SIZE INTO SSA1                                     
068700     MOVE '  GE' TO GODK-STATUSKODER                                      
068800     CALL CBLTDLI USING GU SATI-PCB DLI-IO-AREA SSA1                      
068900     MOVE SATI-STATUS-CODE TO STATUS-WS                                   
069000     PERFORM IMS-STATUSKONTROLL                                           
069100     .                                                                    
069200     SKIP3                                                                
069300 IMS-GN-SATI01-OKVAL   SECTION.                                           
069400     STRING 'WLSATG01(WDJ2BSEQ>=' W-WDJ2BSEQ-MIN-X                        
069500                    '&WDJ2BSEQ<=' W-WDJ2BSEQ-MAX-X                        
069600                    '&FLBYGGB >=' W-FLBYGGB-MIN-X                         
069700                    '&FLBYGGB <=' W-FLBYGGB-MAX-X                         
069800                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
069900                    '&IDARTNR <=' W-IDARTNR-MAX-X ')'                     
070000          DELIMITED BY SIZE INTO SSA1                                     
070100     MOVE '  GE' TO GODK-STATUSKODER                                      
070200     CALL CBLTDLI USING GN SATI-PCB DLI-IO-AREA SSA1                      
070300     MOVE SATI-STATUS-CODE TO STATUS-WS                                   
070400     PERFORM IMS-STATUSKONTROLL                                           
070500     .                                                                    
070600     EJECT                                                                
070700 IMS-GU-SATJ01-KVAL    SECTION.                                           
070800     STRING 'WLSATG01(WDJ2CSEQ =' W-WDJ2CSEQ-X                            
070900                    '&IDORDNST =' W-IDORDNST-X ')'                        
071000          DELIMITED BY SIZE INTO SSA1                                     
071100     MOVE '  GE' TO GODK-STATUSKODER                                      
071200     CALL CBLTDLI USING GU SATJ-PCB DLI-IO-AREA SSA1                      
071300     MOVE SATJ-STATUS-CODE TO STATUS-WS                                   
071400     PERFORM IMS-STATUSKONTROLL                                           
071500     .                                                                    
071600     SKIP3                                                                
071700 IMS-GU-SATJ01-OKVAL   SECTION.                                           
071800     STRING 'WLSATG01(WDJ2CSEQ>=' W-WDJ2CSEQ-MIN-X                        
071900                    '&WDJ2CSEQ<=' W-WDJ2CSEQ-MAX-X ')'                    
072000          DELIMITED BY SIZE INTO SSA1                                     
072100     MOVE '  GE' TO GODK-STATUSKODER                                      
072200     CALL CBLTDLI USING GU SATJ-PCB DLI-IO-AREA SSA1                      
072300     MOVE SATJ-STATUS-CODE TO STATUS-WS                                   
072400     PERFORM IMS-STATUSKONTROLL                                           
072500     .                                                                    
072600     SKIP3                                                                
072700 IMS-GN-SATJ01-OKVAL   SECTION.                                           
072800     STRING 'WLSATG01(WDJ2CSEQ>=' W-WDJ2CSEQ-MIN-X                        
072900                    '&WDJ2CSEQ<=' W-WDJ2CSEQ-MAX-X ')'                    
073000          DELIMITED BY SIZE INTO SSA1                                     
073100     MOVE '  GE' TO GODK-STATUSKODER                                      
073200     CALL CBLTDLI USING GN SATJ-PCB DLI-IO-AREA SSA1                      
073300     MOVE SATJ-STATUS-CODE TO STATUS-WS                                   
073400     PERFORM IMS-STATUSKONTROLL                                           
073500     .                                                                    
073600     EJECT                                                                
073700 IMS-STATUSKONTROLL SECTION.                                              
073800                                                                          
073900     SET STATUS-IX TO 1                                                   
074000     SEARCH GODK-STATUS                                                   
074100       AT END                                                             
074200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
074300         DELIMITED BY SIZE INTO FELTEXT                                   
074400         CALL FELLOG                                                      
074500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
074600     END-SEARCH                                                           
074700     .                                                                    
